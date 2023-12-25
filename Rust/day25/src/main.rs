use anyhow::{anyhow, Result};
use itertools::Itertools;
use petgraph::{
    dot::{Config, Dot},
    graphmap::UnGraphMap,
};

const TEST: &str = include_str!("../test_input.txt");
const INPUT: &str = include_str!("../input.txt");

fn parse_line(line: &str) -> Result<(&str, Vec<&str>)> {
    let (origin, destinations) = line.split_once(": ").ok_or(anyhow!("Syntax error"))?;

    let destinations = destinations.split_whitespace().collect::<Vec<_>>();

    Ok((origin, destinations))
}

fn parse_contents(contents: &str) -> Result<UnGraphMap<&str, ()>> {
    let mut graph = UnGraphMap::new();

    for parsed_line in contents.lines().map(parse_line) {
        let (origin, destinations) = parsed_line?;

        for destination in destinations {
            graph.add_edge(origin, destination, ());
        }
    }

    Ok(graph)
}

fn solve_part_one(contents: &str) -> Result<usize> {
    let graph = parse_contents(contents)?;

    let mut test_graph = graph.clone();

    // Found by graphing the output of the print below with graphviz.
    // println!("{:?}", Dot::with_config(&graph, &[Config::EdgeNoLabel]));
    test_graph.remove_edge("cfn", "jkn");
    test_graph.remove_edge("ljm", "sfd");
    test_graph.remove_edge("gst", "rph");

    if petgraph::algo::connected_components(&test_graph) == 2 {
        let result = petgraph::algo::tarjan_scc(&test_graph);

        return Ok(result[0].len() * result[1].len());
    }

    for comb in graph.all_edges().combinations(3) {
        let (a, b, c) = comb
            .iter()
            .collect_tuple()
            .expect("Combinations should have 3 elements");

        let (a1, a2, _) = a;
        let (b1, b2, _) = b;
        let (c1, c2, _) = c;

        let mut graph = graph.clone();
        graph.remove_edge(a1, a2);
        graph.remove_edge(b1, b2);
        graph.remove_edge(c1, c2);

        if petgraph::algo::connected_components(&graph) == 2 {
            let result = petgraph::algo::tarjan_scc(&graph);

            return Ok(result[0].len() * result[1].len());
        }
    }

    Err(anyhow!("Unable to find a valid 2-component division"))
}

fn main() -> Result<()> {
    println!("{}", solve_part_one(INPUT)?);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part_one() {
        assert_eq!(solve_part_one(TEST).unwrap(), 54);
    }
}
