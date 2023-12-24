use anyhow::{anyhow, Result};
use itertools::Itertools;
use nalgebra::{RowVector3, Vector3};
use num_bigint::BigInt;

use crate::linalg::{cross_product, dot_product};

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
    pub(crate) position: Position,
    pub(crate) velocity: Vector,
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
    One((f64, f64, f64)),
    None,
}

pub(crate) fn get_intersection_at_xy(hailstone: Hailstone, other: Hailstone) -> Intersection {
    let Hailstone {
        position: Position {
            x: x1,
            y: y1,
            z: z1,
        },
        velocity: Vector {
            x: vx,
            y: vy,
            z: vz,
        },
    } = hailstone;
    let x2 = x1 + 100_000 * vx; // Velocities are very small compared to positions.
                                // Scaling to prevent accuracy issues.
    let y2 = y1 + 100_000 * vy; // Velocities are very small compared to positions.
                                // Scaling to prevent accuracy issues.

    let Hailstone {
        position: Position { x: x3, y: y3, .. },
        velocity: Vector { x: vx2, y: vy2, .. },
    } = other;
    let x4 = x3 + 100_000 * vx2;
    let y4 = y3 + 100_000 * vy2;

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

    let time = (px - x1) / (100_000.0 * vx as f64);
    let pz = z1 as f64 + time * 100_000.0 * vz as f64;

    Intersection::One((px, py, pz))
}

pub(crate) fn is_point_in_past(hailstone: Hailstone, point: PositionGround) -> bool {
    let Hailstone { position, velocity } = hailstone;

    let diff = (point.x - position.x as f64, point.y - position.y as f64);
    // eprintln!("Diff: {diff:?}");
    // eprintln!("Vel: {velocity:?}");

    (diff.0 * velocity.x as f64 + diff.1 * velocity.y as f64) < 0.0
}

pub fn get_matrix_coefficients_for_ray(hailstone1: Hailstone, hailstone2: Hailstone) -> [f64; 4] {
    let Hailstone {
        position: Position {
            x: x1,
            y: y1,
            z: z1,
        },
        velocity: Vector {
            x: vx1,
            y: vy1,
            z: vz1,
        },
    } = hailstone1;

    let Hailstone {
        position: Position {
            x: x2,
            y: y2,
            z: z2,
        },
        velocity: Vector {
            x: vx2,
            y: vy2,
            z: vz2,
        },
    } = hailstone2;

    //    let x1 = x1 as f64;
    //    let x2 = x2 as f64;
    //    let y1 = y1 as f64;
    //    let y2 = y2 as f64;
    //    let z1 = z1 as f64;
    //    let z2 = z2 as f64;

    // let vx1 = 100_000.0 * vx1 as f64;
    // let vx2 = 100_000.0 * vx2 as f64;
    // let vy1 = 100_000.0 * vy1 as f64;
    // let vy2 = 100_000.0 * vy2 as f64;
    // let vz1 = 100_000.0 * vz1 as f64;
    // let vz2 = 100_000.0 * vz2 as f64;

    let b = dot_product(
        [x1 - x2, y1 - y2, z1 - z2],
        cross_product([vx1, vy1, vz1], [vx2, vy2, vz2]),
    );

    let coefficients_for_x = cross_product(
        [x1 - x2, y1 - y2, z1 - z2],
        [vx1 - vx2, vy1 - vy2, vz1 - vz2],
    );

    [
        coefficients_for_x[0] as f64,
        coefficients_for_x[1] as f64,
        coefficients_for_x[2] as f64,
        b as f64,
    ]
}

pub(crate) fn get_shift_velocity(
    hailstone1: Hailstone,
    hailstone2: Hailstone,
    hailstone3: Hailstone,
) -> Result<[i64; 3]> {
    let first_line = get_matrix_coefficients_for_ray(hailstone1, hailstone2);
    let second_line = get_matrix_coefficients_for_ray(hailstone1, hailstone3);
    let third_line = get_matrix_coefficients_for_ray(hailstone2, hailstone3);

    let m = nalgebra::Matrix3::from_rows(&[
        RowVector3::new(
            first_line[0] as f64,
            first_line[1] as f64,
            first_line[2] as f64,
        ),
        RowVector3::new(
            second_line[0] as f64,
            second_line[1] as f64,
            second_line[2] as f64,
        ),
        RowVector3::new(
            third_line[0] as f64,
            third_line[1] as f64,
            third_line[2] as f64,
        ),
    ]);

    let b = Vector3::new(
        first_line[3] as f64,
        second_line[3] as f64,
        third_line[3] as f64,
    );

    let sol = m.qr().solve(&b).ok_or(anyhow!("No solution"))?;

    Ok([sol.x as i64, sol.y as i64, sol.z as i64])
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
