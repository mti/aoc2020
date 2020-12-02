def findsumpair(xs, s):
    """
    In the *sorted* list of integers xs, find a pair of elements (x,y)
    that sums to s if it exists (in linear time), or return None otherwise.
    """
    ys = [s - x for x in xs[::-1]]
    n = len(xs)
    i = 0
    j = 0
    while max(i,j) < n:
        if xs[i] == ys[j]:
            return (xs[i], s-ys[j])
        elif xs[i] < ys[j]:
            i += 1
        else:
            j += 1
    return None

def solve_part1(fname):
    with open(fname) as f:
        xs = [int(x) for x in f.readlines()]
    xs.sort()
    w = findsumpair(xs, 2020)
    if w is not None:
        (x,y) = w
        return x*y
    return None

def solve_part2(fname):
    with open(fname) as f:
        xs = [int(x) for x in f.readlines()]
    xs.sort()
    for i in range(len(xs)):
        z = xs[i]
        w = findsumpair(xs[i+1:], 2020-z)
        if w is not None:
            (x,y) = w
            return x*y*z
    return None

