pub(crate) fn interpolate_quadratically(values: &[(f64, f64)], x: f64) -> f64 {
    let x1 = values[0].0;
    let x2 = values[1].0;
    let x3 = values[2].0;

    let y1 = values[0].1;
    let y2 = values[1].1;
    let y3 = values[2].1;

    let first_factor = y1 * (x - x2) * (x - x3) / ((x1 - x2) * (x1 - x3));
    let second_factor = y2 * (x - x1) * (x - x3) / ((x2 - x1) * (x2 - x3));
    let third_factor = y3 * (x - x1) * (x - x2) / ((x3 - x1) * (x3 - x2));

    first_factor + second_factor + third_factor
}
