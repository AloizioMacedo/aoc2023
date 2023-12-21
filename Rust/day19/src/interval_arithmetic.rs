fn diff_single(a: (i64, i64), b: (i64, i64)) -> Vec<(i64, i64)> {
    let mut results = Vec::new();

    if b.0 > a.0 && b.1 < a.1 {
        results.push((a.0, b.0 - 1));
        results.push((b.1 + 1, a.1));
    } else if b.0 <= a.0 && b.1 < a.1 {
        results.push((b.1 + 1, a.1));
    } else if b.0 > a.0 && b.1 >= a.1 {
        results.push((a.0, b.0 - 1))
    } else if b.0 <= a.0 && b.1 >= a.1 {
    }

    results
}

pub(crate) fn intersection(a: (i64, i64), b: (i64, i64)) -> (i64, i64) {
    let c0 = a.0.max(b.0);
    let c1 = a.1.min(b.1);

    (c0, c1)
}

fn diff_multiple_b(a: (i64, i64), bs: &[(i64, i64)]) -> Vec<(i64, i64)> {
    let mut diffs = Vec::new();

    for &b in bs {
        for d in diff_single(a, b) {
            diffs.push(d)
        }
    }

    diffs
}

pub(crate) fn interval_diff(values: &[(i64, i64)], bs: &[(i64, i64)]) -> Vec<(i64, i64)> {
    let mut diffs = Vec::new();

    for &value in values {
        for d in diff_multiple_b(value, bs) {
            diffs.push(d);
        }
    }

    diffs
}
