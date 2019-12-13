import subprocess
import itertools
import time
import collections


def make_machine():
    m = subprocess.Popen(["./day13"],
                         stdin=subprocess.PIPE,
                         stdout=subprocess.PIPE,
                         stderr=subprocess.PIPE,
                         universal_newlines=True,
                         bufsize=0)
    return m


def run(grid):
    m = make_machine()
    lines = m.stdout.readlines()
    for (x, y, tile) in list(zip(*[iter(lines)]*3)):
        grid[int(x), int(y)] = int(tile)
    return grid


def symbol(x):
    xs = dict()
    xs[0] = ' '
    xs[1] = '#'
    xs[2] = 'W'
    xs[3] = '-'
    xs[4] = 'o'
    return xs[x]


def draw(grid):
    min_x = min(map(lambda x: x[0], grid.keys()))
    max_x = max(map(lambda x: x[0], grid.keys()))
    min_y = min(map(lambda x: x[1], grid.keys()))
    max_y = max(map(lambda x: x[1], grid.keys()))
    for y in range(min_y, max_y + 1):
        for x in range(min_x, max_x + 1):
            print(symbol(grid[x, y]), end='')
        print()


def solve1():
    grid = collections.defaultdict(int)
    run(grid)
    draw(grid)
    print(sum(map(lambda x: x == 2, grid.values())))


if __name__ == '__main__':
    solve1()
