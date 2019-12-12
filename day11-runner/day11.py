import subprocess
import itertools
import time
import collections


def make_machine():
    m = subprocess.Popen(["./day11"],
                         stdin=subprocess.PIPE,
                         stdout=subprocess.PIPE,
                         stderr=subprocess.PIPE,
                         universal_newlines=True,
                         bufsize=0)
    return m


def turn(dp, t):
    x, y = dp
    if t == 0:
        return -y, x
    else:
        return y, -x


def run(grid, x, y, dx, dy):
    m = make_machine()
    while m.poll() is None:
        value = grid[x, y]
        m.stdin.write(f'{value}\n')
        grid[x, y] = int(m.stdout.readline())
        dx, dy = turn((dx, dy), int(m.stdout.readline()))
        x += dx
        y += dy
        time.sleep(0.0001)
    return grid


def solve1():
    grid = collections.defaultdict(int)
    x, y = 0, 0
    dx, dy = 0, 1
    grid = run(grid, x, y, dx, dy)
    print(len(grid.values()))


def solve2():
    grid = collections.defaultdict(int)
    x, y = 0, 0
    dx, dy = 0, 1
    grid[x, y] = 1
    run(grid, x, y, dx, dy)
    keys = grid.keys()
    min_x = min(keys, key=lambda z: z[0])[0]
    max_x = max(keys, key=lambda z: z[0])[0]
    min_y = min(keys, key=lambda z: z[1])[1]
    max_y = max(keys, key=lambda z: z[1])[1]
    for x in range(min_x, max_x + 1):
        for y in range(min_y, max_y + 1):
            print('.' if grid[x, y] == 0 else '#', end='')
        print()


if __name__ == '__main__':
    solve2()
