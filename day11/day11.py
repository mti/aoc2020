import copy
import operator

def do_round1(layout):
    w = len(layout[0])
    h = len(layout)
    newlayout = copy.deepcopy(layout)

    def adjacent(x,y):
        return [layout[v][u] for u in [x-1,x,x+1] if 0<=u and u<w \
                             for v in [y-1,y,y+1] if 0<=v and v<h]
    for x in range(w):
        for y in range(h):
            if layout[y][x] == 'L':
                if len([s for s in adjacent(x,y) if s=='#']) == 0:
                    newlayout[y][x] = '#'
            elif layout[y][x] == '#':
                if len([s for s in adjacent(x,y) if s=='#']) >= 5:
                    newlayout[y][x] = 'L'

    return newlayout

def do_round2(layout):
    w = len(layout[0])
    h = len(layout)
    newlayout = copy.deepcopy(layout)

    def visible(x,y):
        r = []
        for step in [(1,0), (1,1), (0,1), (-1,1), \
                        (-1,0), (-1,-1), (0,-1), (1,-1)]:
            (u,v) = tuple(map(operator.add, (x,y), step))
            while 0<=u and u<w and 0<=v and v<h:
                if layout[v][u] != '.':
                    r += [layout[v][u]]
                    break
                (u,v) = tuple(map(operator.add, (u,v), step))
        return r

    for x in range(w):
        for y in range(h):
            if layout[y][x] == 'L':
                if len([s for s in visible(x,y) if s=='#']) == 0:
                    newlayout[y][x] = '#'
            elif layout[y][x] == '#':
                if len([s for s in visible(x,y) if s=='#']) >= 5:
                    newlayout[y][x] = 'L'

    return newlayout

def printlayout(layout):
    for row in layout:
        print("".join(row))
    print ""

def solve_part1(fname, verbose=False):
    with open(fname) as f:
        newlayout = [list(l)[:-1] for l in f.readlines()]

    layout = None
    while newlayout != layout:
        layout = copy.deepcopy(newlayout)
        if verbose:
            printlayout(layout)
        newlayout = do_round1(layout)

    return sum([len([s for s in row if s=='#']) for row in layout])

def solve_part2(fname, verbose=False):
    with open(fname) as f:
        newlayout = [list(l)[:-1] for l in f.readlines()]

    layout = None
    while newlayout != layout:
        layout = copy.deepcopy(newlayout)
        if verbose:
            printlayout(layout)
        newlayout = do_round2(layout)

    return sum([len([s for s in row if s=='#']) for row in layout])

