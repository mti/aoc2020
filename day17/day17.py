import copy
import operator

def do_round1(cubes):
    def neigh(c):
        x,y,z = c
        return [(u,v,w) for u in [x-1,x,x+1]
                        for v in [y-1,y,y+1]
                        for w in [z-1,z,z+1]]
    neighbours = set()
    for c in cubes:
        neighbours = neighbours.union(set(neigh(c)))
    
    newcubes = dict()
    for c in neighbours:
        actn = sum([cubes.get(d,0) for d in neigh(c)])
        if (c in cubes) and (actn == 3 or actn == 4):
            newcubes[c] = 1
        elif (c not in cubes) and actn == 3:
            newcubes[c] = 1
    return newcubes

def do_round2(cubes):
    def neigh(c):
        x,y,z,t = c
        return [(u,v,w,s) for u in [x-1,x,x+1] 
                          for v in [y-1,y,y+1] 
                          for w in [z-1,z,z+1]
                          for s in [t-1,t,t+1]]
    neighbours = set()
    for c in cubes:
        neighbours = neighbours.union(set(neigh(c)))
    
    newcubes = dict()
    for c in neighbours:
        actn = sum([cubes.get(d,0) for d in neigh(c)])
        if (c in cubes) and (actn == 3 or actn == 4):
            newcubes[c] = 1
        elif (c not in cubes) and actn == 3:
            newcubes[c] = 1
    return newcubes


def solve_part1(fname, n=6):
    with open(fname) as f:
        layout = [list(l)[:-1] for l in f.readlines()]

    active = dict()
    for y in range(len(layout)):
        for x in range(len(layout[y])):
            if layout[y][x] == '#':
                active[(x,y,0)] = 1
    
    for _ in range(n):
        active = do_round1(active)

    return len(active)

def solve_part2(fname, n=6):
    with open(fname) as f:
        layout = [list(l)[:-1] for l in f.readlines()]

    active = dict()
    for y in range(len(layout)):
        for x in range(len(layout[y])):
            if layout[y][x] == '#':
                active[(x,y,0,0)] = 1
    
    for _ in range(n):
        active = do_round2(active)

    return len(active)
