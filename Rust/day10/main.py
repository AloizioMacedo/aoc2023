import networkx as nx
from dataclasses import dataclass


@dataclass
class Grid:
    origin: tuple[int, int]
    graph: nx.Graph
    matrix: list[list]


def flood_fill(
    matrix: list[list[str]],
    ref: tuple[int, int],
    visited: set[tuple[int, int]],
):
    matrix[ref[0]][ref[1]] = "O"

    n_rows = len(matrix)
    n_cols = len(matrix[0])

    nexts = []
    i, j = ref

    if i - 1 >= 0:
        nexts.append((i - 1, j))
    if i + 1 < n_rows:
        nexts.append((i + 1, j))
    if j - 1 >= 0:
        nexts.append((i, j - 1))
    if j + 1 < n_cols:
        nexts.append((i, j + 1))

    nexts = [x for x in nexts if x not in visited and matrix[x[0]][x[1]] == "."]
    print(nexts)
    visited.update(nexts)

    for e in nexts:
        flood_fill(matrix, e, visited)


def scale_down_matrix(matrix: list[list[str]]) -> list[list[str]]:
    scaled_down = []
    n_rows = len(matrix)
    n_cols = len(matrix[0])

    for i in range(1, n_rows, 3):
        scaled_down.append([])
        for j in range(1, n_cols, 3):
            scaled_down[-1].append(matrix[i][j])

    return scaled_down


def scale_up_matrix(matrix: list[list[str]]) -> list[list[str]]:
    scaled_up = []

    for k, line in enumerate(matrix):
        scaled_up.append([])
        scaled_up.append([])
        scaled_up.append([])

        scaled_up_chars = [scale_up(c) for c in line]
        for i in range(3):
            for scaled_entry in [s[i] for s in scaled_up_chars]:
                scaled_up[3 * k + i].extend(scaled_entry)

    return scaled_up


def scale_up(c: str) -> list[list[str]]:
    if c == "|":
        return [[".", "|", "."], [".", "|", "."], [".", "|", "."]]
    elif c == "-":
        return [[".", ".", "."], ["-", "-", "-"], [".", ".", "."]]
    elif c == "L":
        return [[".", "|", "."], [".", "L", "-"], [".", ".", "."]]
    elif c == "J":
        return [[".", "|", "."], ["-", "J", "."], [".", ".", "."]]
    elif c == "7":
        return [[".", ".", "."], ["-", "7", "."], [".", "|", "."]]
    elif c == "F":
        return [[".", ".", "."], [".", "F", "-"], [".", "|", "."]]
    elif c == "S":
        return [[".", "|", "."], ["-", "S", "-"], [".", "|", "."]]
    else:
        return [[".", ".", "."], [".", ".", "."], [".", ".", "."]]


def parse_contents(contents: list[list[str]]) -> Grid:
    graph = nx.Graph()
    lines = contents

    n_rows = len(lines)
    n_cols = len(lines[0])

    matrix = []

    origin = (0, 0)

    for i, line in enumerate(lines):
        new_line = []
        matrix.append(new_line)

        for j, c in enumerate(line):
            new_line.append(c)

            coords = []
            if c == "|":
                if (0 <= i - 1 < n_rows) and lines[i - 1][j] in ["7", "F", "|", "S"]:
                    coords.append((i - 1, j))
                if (0 <= i + 1 < n_rows) and lines[i + 1][j] in ["L", "J", "|", "S"]:
                    coords.append((i + 1, j))
            elif c == "-":
                if (0 <= j - 1 < n_cols) and lines[i][j - 1] in ["-", "F", "L", "S"]:
                    coords.append((i, j - 1))
                if (0 <= j + 1 < n_cols) and lines[i][j + 1] in ["-", "J", "7", "S"]:
                    coords.append((i, j + 1))
            elif c == "L":
                if (0 <= j + 1 < n_cols) and lines[i][j + 1] in ["-", "J", "7", "S"]:
                    coords.append((i, j + 1))
                if (0 <= i - 1 < n_rows) and lines[i - 1][j] in ["|", "F", "7", "S"]:
                    coords.append((i - 1, j))
            elif c == "J":
                if (0 <= j - 1 < n_cols) and lines[i][j - 1] in ["-", "F", "L", "S"]:
                    coords.append((i, j - 1))
                if (0 <= i - 1 < n_rows) and lines[i - 1][j] in ["|", "F", "7", "S"]:
                    coords.append((i - 1, j))
            elif c == "7":
                if (0 <= i + 1 < n_rows) and lines[i + 1][j] in ["L", "J", "|", "S"]:
                    coords.append((i + 1, j))
                if (0 <= j - 1 < n_cols) and lines[i][j - 1] in ["-", "F", "L", "S"]:
                    coords.append((i, j - 1))
            elif c == "F":
                if (0 <= i + 1 < n_rows) and lines[i + 1][j] in ["L", "J", "|", "S"]:
                    coords.append((i + 1, j))
                if (0 <= j + 1 < n_cols) and lines[i][j + 1] in ["-", "J", "7", "S"]:
                    coords.append((i, j + 1))
            elif c == "S":
                coords = [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]
            else:
                continue

            if len(coords) == 4:
                origin = (i, j)
                continue

            for i1, j1 in coords:
                if (0 <= i1 < n_rows) and (0 <= j1 < n_cols):
                    graph.add_edge((i, j), (i1, j1))

    return Grid(origin, graph, matrix)


def solve_part_one(contents_as_lst: list[list[str]]) -> int:
    grid = parse_contents(contents_as_lst)

    cycle_with_origin = next(
        cycle for cycle in nx.cycle_basis(grid.graph) if grid.origin in cycle
    )

    return sum(1 for _ in cycle_with_origin) // 2


def solve_part_two(contents_as_lst: list[list[str]]) -> int:
    scaled_up = scale_up_matrix(contents_as_lst)

    grid = parse_contents(scaled_up)

    matrix = grid.matrix.copy()

    flood_fill(matrix, (0, 0), set())

    scaled_down = scale_down_matrix(matrix)

    for line in scaled_down:
        print(line)

    counter = 0
    for line in scaled_down:
        for c in line:
            if c == ".":
                counter += 1

    return counter


def remove_frame(matrix: list[list[str]]) -> list[list[str]]:
    matrix = matrix[1:-1]
    for i in range(len(matrix)):
        matrix[i] = matrix[i][1:-1]

    return matrix


def main():
    with open("input.txt") as file:
        contents = file.read()

    contents_as_lst = [list(line) for line in contents.splitlines()]

    print(solve_part_one(contents_as_lst))
    print(solve_part_two(contents_as_lst))


if __name__ == "__main__":
    main()
