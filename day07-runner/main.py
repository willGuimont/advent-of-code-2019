import subprocess
import itertools
import time


def make_machine(phase: str):
    m = subprocess.Popen(["./day07"],
                         stdin=subprocess.PIPE,
                         stdout=subprocess.PIPE,
                         stderr=subprocess.PIPE,
                         universal_newlines=True,
                         bufsize=0)
    m.stdin.write(phase + '\n')
    return m


def make_all_machines(phases: str):
    return list(map(lambda x: make_machine(x), phases))


def generate_all_permutations(xs: str):
    return itertools.permutations(xs)


def solve1():
    best_value = -1
    for p in generate_all_permutations('01234'):
        machines = make_all_machines(p)
        output = '0'
        for m in machines:
            m.stdin.write(output + '\n')
            output = m.stdout.readline()
            out_int = int(output)
        for m in machines:
            m.terminate()
        if out_int > best_value:
            best_value = out_int

    print(best_value)


def run(p):
    machines = make_all_machines(p)
    out_int = 0
    running = True
    while running:
        for i, m in enumerate(machines):
            if m.poll() is not None:
                running = False
                break
            m.stdin.write(str(out_int) + '\n')
            out_int = m.stdout.readline()
            out_int = int(out_int)
            if i == 4:
                e_out = out_int
    for m in machines:
        m.terminate()
    time.sleep(0.1)
    return e_out


def solve2():
    best = -1
    for p in generate_all_permutations('98765'):
        try:
            a = run(p)
            if a > best:
                best = a
        except:
            ...
    print(best)


if __name__ == '__main__':
    solve2()
