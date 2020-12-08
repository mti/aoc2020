def run_until_repeat(program):
    acc = 0
    ins = 0
    exc = [False for _ in program]
    while exc[ins] is False:
        exc[ins] = True
        opn = program[ins][:3]
        arg = int(program[ins][4:])
        if opn == 'jmp':
            ins += arg
            continue
        elif opn == 'acc':
            acc += arg
        ins += 1

    return acc

def is_terminating(program):
    acc = 0
    ins = 0
    exc = [False for _ in program]
    pln = len(program)
    while ins < pln and exc[ins] is False:
        exc[ins] = True
        opn = program[ins][:3]
        arg = int(program[ins][4:])
        if opn == 'jmp':
            ins += arg
            continue
        elif opn == 'acc':
            acc += arg
        ins += 1
    return (ins == pln, acc)

def fix_termination(program):
    cands = []

    def swap_jmpnop(s):
        if s[:3] == 'jmp':
            return 'nop' + s[3:]
        if s[:3] == 'nop':
            return 'jmp' + s[3:]
        return s
    for ins in range(len(program)):
        if program[ins] == 'acc':
            continue
        program[ins] = swap_jmpnop(program[ins])
        term, acc = is_terminating(program)
        if term:
            cands += [acc]
        program[ins] = swap_jmpnop(program[ins])
    return cands
        
def solve_part1(fname):
    with open(fname) as f:
        program = f.readlines()

    return run_until_repeat(program)

def solve_part2(fname):
    with open(fname) as f:
        program = f.readlines()

    return fix_termination(program)

