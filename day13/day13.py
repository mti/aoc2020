import itertools
from functools import reduce

def chinese_remainder(n, a):
    sum = 0
    prod = reduce(lambda a, b: a*b, n)
    for n_i, a_i in zip(n, a):
        p = prod // n_i
        sum += a_i * mul_inv(p, n_i) * p
    return sum % prod
 
def mul_inv(a, b):
    b0 = b
    x0, x1 = 0, 1
    if b == 1: return 1
    while a > 1:
        q = a // b
        a, b = b, a%b
        x0, x1 = x1 - q * x0, x0
    if x1 < 0: x1 += b0
    return x1

def solve_part1(fname):
    with open(fname) as f:
        mintime = int(f.readline())
        busstr  = f.readline()

    busids = [int(n) for n in busstr.split(",") if n != 'x']
    waits  = [((-mintime)%bus, bus) for bus in busids]
    waits.sort()
    
    wait, bus = waits[0]
    return wait*bus

def solve_part2(fname):
    with open(fname) as f:
        mintime = int(f.readline())
        busstr  = f.readline()

    pairs = [(int(n),i) for (n,i) in zip(busstr.split(","),itertools.count(0))
                if n != 'x']
        
    return chinese_remainder([p[0] for p in pairs], [p[0]-p[1] for p in pairs])

