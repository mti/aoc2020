import math

def part3(n):
     if n<=1: 
         return 1 
     if n==2: 
         return 2 
     if n==3: 
         return 4 
     return part3(n-1) + part3(n-2) + part3(n-3) 

def decomp(l): 
     r = [] 
     c = 0 
     for x in l: 
         if x == 1: 
             c+=1 
         else: 
             if c>0: 
                 r+=[c] 
             c=0 
     return r

def solve_part1(fname):
    with open(fname) as f:
        data = [int(x) for x in f.readlines()]
    data.sort()
    jolts = [0] + data + [data[-1]+3]
    diffs = [jolts[i+1] - jolts[i] for i in range(len(jolts)-1)]
    return len([x for x in diffs if x==1]) * len([x for x in diffs if x==3])

def solve_part2(fname):
    with open(fname) as f:
        data = [int(x) for x in f.readlines()]
    data.sort()
    jolts = [0] + data + [data[-1]+3]
    diffs = [jolts[i+1] - jolts[i] for i in range(len(jolts)-1)]
    decos = decomp(diffs)
    return math.prod([part3(x) for x in decos])

