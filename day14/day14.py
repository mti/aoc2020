import re
import itertools

def solve_part1(fname):
    memvals = dict()
    mskre = re.compile("^mask = ([01X]+)$")
    memre = re.compile("^mem\[([0-9]+)\] = ([0-9]+)$")
    andmsk = ~0
    ormask = 0
    with open(fname) as f:
        for line in f:
            msk = mskre.match(line)
            if msk is not None:
                mskstr = msk[1]
                andmsk = int(mskstr.replace("X","1"),2)
                ormask = int(mskstr.replace("X","0"),2)
                continue
            mem = memre.match(line)
            if mem is not None:
                addr = mem[1]
                val  = (int(mem[2]) | ormask) & andmsk
                #print("set address %s to %d" % (addr, val))
                memvals[addr] = val
    return sum(memvals.values())

def solve_part2(fname):
    memvals = dict()
    mskre = re.compile("^mask = ([01X]+)$")
    memre = re.compile("^mem\[([0-9]+)\] = ([0-9]+)$")
    mskstr = "0"
    with open(fname) as f:
        for line in f:
            msk = mskre.match(line)
            if msk is not None:
                mskstr = msk[1]
            mem = memre.match(line)
            if mem is not None:
                baseaddr = bin(int(mem[1]))[2:]
                baseaddr = "0"*(36-len(baseaddr)) + baseaddr
                val  = int(mem[2])
                maskaddr = [x if y=='0' else y for (x,y) in zip(baseaddr,mskstr)]
                addrs = ("".join(list(binaddr)) for binaddr in \
                    itertools.product(*[x if x!='X' else '01' for x in maskaddr]))
                for addr in addrs:
                    memvals[addr] = val
    return sum(memvals.values())
