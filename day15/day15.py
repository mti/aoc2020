import itertools

def solve_part1(starting,n=2020):
    seq = starting
    last= dict()
    for (i,x) in zip(itertools.count(0), starting):
        last[x] = [i]

    m = len(starting)
    for i in range(m,n):
        lasts = last[seq[i-1]]
        if len(lasts) <= 1:
            seq += [0]
        else:
            seq += [lasts[0] - lasts[1]]
        last[seq[i]] = [i] + last.get(seq[i], [])

    return seq

def solve_part2(starting,n=2020):
    last = dict()
    for (i,x) in zip(itertools.count(0), starting):
        last[x] = [i]

    m = len(starting)
    prev = starting[-1]
    for i in range(m,n):
        lasts = last[prev]
        if len(lasts) <= 1:
            prev = 0
        else:
            prev = lasts[0] - lasts[1]
        last[prev] = [i] + last.get(prev, [])[:1]

    return prev

