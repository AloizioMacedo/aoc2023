use anyhow::{anyhow, Result};
use itertools::Itertools;
use num_bigint::BigInt;

#[derive(Debug, Clone, Copy)]
pub(crate) struct Position {
    pub(crate) x: i64,
    pub(crate) y: i64,
    pub(crate) z: i64,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct PositionGround {
    pub(crate) x: f64,
    pub(crate) y: f64,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct Vector {
    pub(crate) x: i64,
    pub(crate) y: i64,
    pub(crate) z: i64,
}

impl std::ops::Add<Vector> for Position {
    type Output = Position;

    fn add(self, rhs: Vector) -> Self::Output {
        Position {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
            z: self.z + rhs.z,
        }
    }
}

impl std::ops::Sub for Position {
    type Output = Vector;

    fn sub(self, rhs: Position) -> Self::Output {
        Vector {
            x: self.x - rhs.x,
            y: self.y - rhs.y,
            z: self.z - rhs.z,
        }
    }
}

enum VectorComparison {
    PositiveScaling,
    NegativeScaling,
    Incomparable,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct Hailstone {
    position: Position,
    velocity: Vector,
}

pub(crate) fn parse_line(line: &str) -> Result<Hailstone> {
    let (position, velocity) = line.split_once(" @ ").ok_or(anyhow!("Syntax error"))?;

    let (px, py, pz) = position
        .splitn(3, ", ")
        .map(|s| s.trim())
        .collect_tuple()
        .ok_or(anyhow!("Syntax error"))?;

    let (vx, vy, vz) = velocity
        .splitn(3, ", ")
        .map(|s| s.trim())
        .collect_tuple()
        .ok_or(anyhow!("Syntax error"))?;

    Ok(Hailstone {
        position: Position {
            x: px.parse()?,
            y: py.parse()?,
            z: pz.parse()?,
        },
        velocity: Vector {
            x: vx.parse()?,
            y: vy.parse()?,
            z: vz.parse()?,
        },
    })
}

#[derive(Debug)]
pub(crate) enum Intersection {
    Coincident,
    One((f64, f64)),
    None,
}

pub(crate) fn get_intersection_at_xy(hailstone: Hailstone, other: Hailstone) -> Intersection {
    let Hailstone {
        position: Position { x: x1, y: y1, .. },
        velocity: Vector { x: vx, y: vy, .. },
    } = hailstone;
    let x2 = x1 + 100_000 * vx; // Velocities are very small compared to positions.
                                // Scaling to prevent accuracy issues.
    let y2 = y1 + 100_000 * vy; // Velocities are very small compared to positions.
                                // Scaling to prevent accuracy issues.

    let Hailstone {
        position: Position { x: x3, y: y3, .. },
        velocity: Vector { x: vx, y: vy, .. },
    } = other;
    let x4 = x3 + 100_000 * vx;
    let y4 = y3 + 100_000 * vy;

    let x1 = x1 as f64;
    let x2 = x2 as f64;
    let x3 = x3 as f64;
    let x4 = x4 as f64;

    let y1 = y1 as f64;
    let y2 = y2 as f64;
    let y3 = y3 as f64;
    let y4 = y4 as f64;

    let px = ((x1 * y2 - y1 * x2) * (x3 - x4) - (x1 - x2) * (x3 * y4 - y3 * x4))
        / ((x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4));
    let py = ((x1 * y2 - y1 * x2) * (y3 - y4) - (y1 - y2) * (x3 * y4 - y3 * x4))
        / ((x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4));

    if !px.is_finite() || !py.is_finite() {
        let x1 = BigInt::from(x1 as i64);
        let x3 = BigInt::from(x3 as i64);
        let x4 = BigInt::from(x4 as i64);

        let y1 = BigInt::from(y1 as i64);
        let y3 = BigInt::from(y3 as i64);
        let y4 = BigInt::from(y4 as i64);

        let det = (x3 - x1.clone()) * (y4 - y1.clone()) - (x4 - x1) * (y3 - y1);
        if det == BigInt::from(0) {
            return Intersection::Coincident;
        } else {
            return Intersection::None;
        }
    }

    Intersection::One((px, py))
}

pub(crate) fn is_point_in_past(hailstone: Hailstone, point: PositionGround) -> bool {
    let Hailstone { position, velocity } = hailstone;

    let diff = (point.x - position.x as f64, point.y - position.y as f64);
    // eprintln!("Diff: {diff:?}");
    // eprintln!("Vel: {velocity:?}");

    (diff.0 * velocity.x as f64 + diff.1 * velocity.y as f64) < 0.0
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let hailstone = Hailstone {
            position: Position { x: 1, y: 1, z: 1 },
            velocity: Vector { x: 1, y: 1, z: 1 },
        };

        assert!(matches!(
            get_intersection_at_xy(hailstone, hailstone),
            Intersection::Coincident
        ));

        let hailstone2 = Hailstone {
            position: Position { x: 2, y: 1, z: 1 },
            velocity: Vector { x: 1, y: 1, z: 1 },
        };

        assert!(matches!(
            get_intersection_at_xy(hailstone, hailstone2),
            Intersection::None
        ));
    }
}
