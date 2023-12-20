use std::collections::{HashMap, VecDeque};

use anyhow::{anyhow, Result};

const TEST: &str = include_str!("../test_input.txt");

#[derive(Debug, Clone)]
enum ModuleType<'a> {
    FlipFlop(FlipFlop<'a>),
    Conjunction(Conjunction<'a>),
    Broadcaster(Vec<&'a str>),
}

impl<'a> ModuleType<'a> {
    fn receive_pulse(&mut self, origin: &'a str, pulse: PulseType) -> Option<PulseDelivery> {
        match self {
            ModuleType::FlipFlop(ff) => ff.receive_pulse(pulse),
            ModuleType::Conjunction(c) => Some(c.receive_pulse(origin, pulse)),
            ModuleType::Broadcaster(destinations) => Some(PulseDelivery {
                origin: "broadcaster",
                destinations,
                pulse_type: PulseType::Low,
            }),
        }
    }

    fn get_destinations(&self) -> &[&'a str] {
        match self {
            Self::Broadcaster(ds) => ds,
            Self::FlipFlop(ff) => &ff.destinations,
            Self::Conjunction(c) => &c.destinations,
        }
    }

    fn get_name(&self) -> &'a str {
        match self {
            ModuleType::FlipFlop(FlipFlop { name, .. }) => name,
            ModuleType::Conjunction(Conjunction { name, .. }) => name,
            ModuleType::Broadcaster(_) => "broadcaster",
        }
    }
}

#[derive(Debug, Clone)]
struct FlipFlop<'a> {
    name: &'a str,
    status: FlipFlopStatus,
    destinations: Vec<&'a str>,
}

impl<'a> FlipFlop<'a> {
    fn new(name: &'a str, destinations: Vec<&'a str>) -> FlipFlop<'a> {
        FlipFlop {
            name,
            status: FlipFlopStatus::Off,
            destinations,
        }
    }

    fn receive_pulse(&mut self, pulse: PulseType) -> Option<PulseDelivery> {
        if let PulseType::Low = pulse {
            match self.status {
                FlipFlopStatus::Off => {
                    self.status = FlipFlopStatus::On;
                    Some(PulseDelivery {
                        origin: self.name,
                        destinations: &self.destinations,
                        pulse_type: PulseType::High,
                    })
                }
                FlipFlopStatus::On => {
                    self.status = FlipFlopStatus::Off;
                    Some(PulseDelivery {
                        origin: self.name,
                        destinations: &self.destinations,
                        pulse_type: PulseType::Low,
                    })
                }
            }
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum FlipFlopStatus {
    On,
    Off,
}

#[derive(Debug, Clone, Copy)]
struct PulseDelivery<'a> {
    origin: &'a str,
    destinations: &'a [&'a str],
    pulse_type: PulseType,
}

#[derive(Debug, Clone, Copy)]
enum PulseType {
    Low,
    High,
}

#[derive(Debug, Clone)]
struct Conjunction<'a> {
    name: &'a str,
    most_recent_pulses: HashMap<&'a str, PulseType>,
    destinations: Vec<&'a str>,
}

impl<'a> Conjunction<'a> {
    fn new(name: &'a str, destinations: Vec<&'a str>) -> Conjunction<'a> {
        Conjunction {
            name,
            most_recent_pulses: HashMap::new(),
            destinations,
        }
    }

    fn initialize_input(&mut self, name: &'a str) {
        self.most_recent_pulses.insert(name, PulseType::Low);
    }

    fn receive_pulse(&mut self, origin: &'a str, pulse: PulseType) -> PulseDelivery {
        self.most_recent_pulses.insert(origin, pulse);

        if self
            .most_recent_pulses
            .values()
            .all(|pt| matches!(pt, PulseType::High))
        {
            PulseDelivery {
                origin: self.name,
                pulse_type: PulseType::Low,
                destinations: &self.destinations,
            }
        } else {
            PulseDelivery {
                origin: self.name,
                pulse_type: PulseType::High,
                destinations: &self.destinations,
            }
        }
    }
}

fn parse_line(line: &str) -> Result<ModuleType> {
    if line.starts_with("broadcaster") {
        let (_, destinations) = line.split_once(" -> ").ok_or(anyhow!("Syntax error"))?;
        let destinations = destinations.split(", ").collect();

        Ok(ModuleType::Broadcaster(destinations))
    } else if line.starts_with('%') {
        let (name, destinations) = line.split_once(" -> ").ok_or(anyhow!("Syntax error"))?;
        let name = name.trim_start_matches('%');
        let destinations = destinations.split(", ").collect();

        Ok(ModuleType::FlipFlop(FlipFlop::new(name, destinations)))
    } else if line.starts_with('&') {
        let (name, destinations) = line.split_once(" -> ").ok_or(anyhow!("Syntax error"))?;
        let name = name.trim_start_matches('&');
        let destinations = destinations.split(", ").collect();

        Ok(ModuleType::Conjunction(Conjunction::new(
            name,
            destinations,
        )))
    } else {
        Err(anyhow!("Syntax error"))
    }
}

fn parse_contents(contents: &str) -> Result<Contraption> {
    let mut module_types = contents
        .lines()
        .map(parse_line)
        .collect::<Result<Vec<_>>>()?;
    let reference_module_types = module_types.clone();

    for (i, module_type) in module_types.iter_mut().enumerate() {
        if let ModuleType::Conjunction(c) = module_type {
            for j in (0..reference_module_types.len()).filter(|&x| x != i) {
                match &reference_module_types[j] {
                    ModuleType::FlipFlop(FlipFlop {
                        name, destinations, ..
                    }) => {
                        if destinations.contains(&c.name) {
                            c.initialize_input(name)
                        }
                    }
                    ModuleType::Conjunction(Conjunction {
                        name, destinations, ..
                    }) => {
                        if destinations.contains(&c.name) {
                            c.initialize_input(name)
                        }
                    }
                    _ => (),
                }
            }
        }
    }

    let mut modules = HashMap::new();

    for module in module_types {
        modules.insert(module.get_name(), module);
    }

    Ok(Contraption { modules })
}

#[derive(Debug)]
struct Contraption<'a> {
    modules: HashMap<&'a str, ModuleType<'a>>,
}

impl<'a> Contraption<'a> {
    fn run(&'a mut self) -> Result<()> {
        let broadcaster = &self.modules["broadcaster"];

        let mut queue = VecDeque::new();

        let initial_destinations = broadcaster.get_destinations();

        queue.push_back(PulseDelivery {
            origin: "broadcaster",
            destinations: initial_destinations,
            pulse_type: PulseType::Low,
        });

        while let Some(pd) = queue.pop_front() {
            for destination in pd.destinations {
                let destination = self
                    .modules
                    .get_mut(destination)
                    .expect("Should have destination {destination}");

                let output_pulse = destination.receive_pulse(pd.origin, pd.pulse_type);

                if let Some(output_pulse) = output_pulse {
                    queue.push_back(output_pulse);
                }
            }
        }

        Ok(())
    }
}

fn main() -> Result<()> {
    println!("{:?}", parse_contents(TEST));

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {}
}
