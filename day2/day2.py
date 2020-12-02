import re

def solve_part1(fname):
    valid = 0
    p = re.compile("^([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)$")
    with open(fname) as f:
        for line in f:
            m = p.match(line)
            lmin = int(m[1])
            lmax = int(m[2])
            c = m[3]
            s = m[4]
            occ = s.count(c)
            if occ >= lmin and occ <= lmax:
                valid += 1
    return valid

def solve_part2(fname):
    valid = 0
    p = re.compile("^([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)$")
    with open(fname) as f:
        for line in f:
            m = p.match(line)
            pos1 = int(m[1])-1
            pos2 = int(m[2])-1
            c = m[3]
            s = m[4]
            if (s[pos1] == c) != (s[pos2] == c):
                valid += 1
    return valid

