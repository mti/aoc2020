def is_pairsum(x,ys):
    return not set([x-y for y in ys]).isdisjoint(ys)

def solve_part1(fname,plen):
    with open(fname) as f:
        data = [int(x) for x in f.readlines()]

    for i in range(plen,len(data)):
        if not is_pairsum(data[i], data[i-plen:i]):
            return data[i]
    return None

def solve_part2(fname,target):
    with open(fname) as f:
        data = [int(x) for x in f.readlines()]
    n = len(data)
    i = 0
    j = 0
    s = 0
    while i < n and j < n:
        if s < target:
            s += data[j]
            j +=1
        elif s > target:
            s -= data[i]
            i += 1
        else:
            return min(data[i:j]) + max(data[i:j])
    return None

