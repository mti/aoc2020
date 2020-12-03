import math

def solve_part1(fname):
    width = None
    pos   = 0
    trees = 0
    with open(fname) as f:
        for line in f:
            if width is None:
                width = len(line) - 1
                continue
            pos = (pos + 3) % width
            if line[pos] == '#':
                trees += 1
    return trees

def solve_part2(fname, slopes=[(1,1), (3,1), (5,1), (7,1), (1,2)]):
    width = None
    pos   = [0 for _ in slopes]
    trees = [0 for _ in slopes]
    lineno= 1
    with open(fname) as f:
        for line in f:
            if width is None:
                width = len(line) - 1
                continue
            for i in range(len(slopes)):
                if lineno % slopes[i][1] != 0:
                    continue
                pos[i] = (pos[i] + slopes[i][0]) % width
                if line[pos[i]] == '#':
                    trees[i] += 1
            lineno += 1
    return trees, math.prod(trees)

