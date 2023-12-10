use std::collections::HashSet;

use petgraph::graphmap::UnGraphMap;

use ndarray::Array2;

use crate::pipe;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Filled {
    Loop,
    Filled,

    #[default]
    Empty,
}

#[derive(Debug)]
pub(crate) struct Grid {
    pub(crate) origin: (usize, usize),
    pub(crate) matrix: Array2<pipe::Pipe>,
}

impl Grid {
    pub(crate) fn get_graph(&self) -> UnGraphMap<(usize, usize), ()> {
        let mut graph = UnGraphMap::new();

        for ((i, j), p) in self.matrix.indexed_iter() {
            if i > 0 && p.get_possible_up().contains(&self.matrix[(i - 1, j)]) {
                graph.add_edge((i, j), (i - 1, j), ());
            }
            if i < self.matrix.nrows() - 1
                && p.get_possible_down().contains(&self.matrix[(i + 1, j)])
            {
                graph.add_edge((i, j), (i + 1, j), ());
            }

            if j > 0 && p.get_possible_left().contains(&self.matrix[(i, j - 1)]) {
                graph.add_edge((i, j), (i, j - 1), ());
            }
            if j < self.matrix.ncols() - 1
                && p.get_possible_right().contains(&self.matrix[(i, j + 1)])
            {
                graph.add_edge((i, j), (i, j + 1), ());
            }
        }

        graph
    }
}

pub(crate) fn scale_up(matrix: &Array2<pipe::Pipe>) -> Array2<pipe::Pipe> {
    let n_rows = matrix.nrows();
    let n_cols = matrix.ncols();

    let mut new_array: Array2<pipe::Pipe> = Array2::default((3 * n_rows, 3 * n_cols));

    for ((i, j), p) in matrix.indexed_iter() {
        let scaled_up = p.get_scaled_up_version();

        for h1 in [3 * i, 3 * i + 1, 3 * i + 2] {
            for h2 in [3 * j, 3 * j + 1, 3 * j + 2] {
                new_array[(h1, h2)] = scaled_up[(h1 - 3 * i, h2 - 3 * j)]
            }
        }
    }

    new_array
}

pub(crate) fn scale_down<T>(matrix: &Array2<T>) -> Array2<T>
where
    T: Clone + Copy + Default,
{
    let n_rows = matrix.nrows() / 3;
    let n_cols = matrix.ncols() / 3;

    let mut new_array = Array2::default((n_rows, n_cols));

    for i in 0..n_rows {
        for j in 0..n_cols {
            new_array[(i, j)] = matrix[(3 * i + 1, 3 * j + 1)]
        }
    }

    new_array
}

pub(crate) fn transform_matrix(
    matrix: &Array2<pipe::Pipe>,
    lp: &[(usize, usize)],
) -> Array2<Filled> {
    let mut new_matrix: Array2<Filled> = Array2::default((matrix.nrows(), matrix.ncols()));

    for (i, j) in lp {
        for h1 in [3 * i, 3 * i + 1, 3 * i + 2] {
            for h2 in [3 * j, 3 * j + 1, 3 * j + 2] {
                if !matches!(matrix[(h1, h2)], pipe::Pipe::Empty) {
                    new_matrix[(h1, h2)] = Filled::Loop
                }
            }
        }
    }

    new_matrix
}

pub(crate) fn flood_fill(matrix: &Array2<Filled>) -> Array2<Filled> {
    let mut visited: HashSet<(usize, usize)> = HashSet::new();
    let mut queue: Vec<(usize, usize)> = vec![(0, 0)];

    let mut new_array: Array2<Filled> = Array2::default((matrix.nrows(), matrix.ncols()));

    for ((i, j), v) in matrix.indexed_iter() {
        if let Filled::Loop = v {
            new_array[(i, j)] = *v
        }
    }

    while let Some(next) = queue.pop() {
        visited.insert(next);
        new_array[next] = Filled::Filled;

        let (i, j) = next;

        if i > 0 && !matches!(matrix[(i - 1, j)], Filled::Loop) && !visited.contains(&(i - 1, j)) {
            queue.push((i - 1, j))
        }
        if i < matrix.nrows() - 1
            && !matches!(matrix[(i + 1, j)], Filled::Loop)
            && !visited.contains(&(i + 1, j))
        {
            queue.push((i + 1, j))
        }
        if j > 0 && !matches!(matrix[(i, j - 1)], Filled::Loop) && !visited.contains(&(i, j - 1)) {
            queue.push((i, j - 1))
        }
        if j < matrix.ncols() - 1
            && !matches!(matrix[(i, j + 1)], Filled::Loop)
            && !visited.contains(&(i, j + 1))
        {
            queue.push((i, j + 1))
        }
    }

    new_array
}
