import networkx as nx
from dataclasses import dataclass


@dataclass
class Grid:
    origin: tuple[int, int]
    graph: nx.Graph
    matrix: list[list]


def parse_contents(contents: str) -> Grid:
    graph = nx.Graph()

    lines = contents.splitlines()

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


def solve_part_one(contents: str) -> int:
    grid = parse_contents(contents)

    cycle_with_origin = next(
        cycle for cycle in nx.cycle_basis(grid.graph) if grid.origin in cycle
    )

    return sum(1 for _ in cycle_with_origin) // 2


def main():
    with open("input.txt") as file:
        contents = file.read()

    print(solve_part_one(contents))


if __name__ == "__main__":
    main()
