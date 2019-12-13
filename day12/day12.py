import numpy as np
import itertools


class Moon:
    def __init__(self, p, v):
        self.position = np.array(p)
        self.velocity = np.array(v)

    def update(self):
        self.position += self.velocity

    def attract(self, other):
        for i, (x, y) in enumerate(zip(self.position, other.position)):
            if y < x:
                other.velocity[i] += 1
            elif y > x:
                other.velocity[i] += -1

    def kin(self):
        return sum(map(abs, self.position))

    def pot(self):
        return sum(map(abs, self.velocity))

    def tot(self):
        return self.kin() * self.pot()


def solve1():
    a = Moon([-10, -13, 7], [0, 0, 0])
    b = Moon([1, 2, 1], [0, 0, 0])
    c = Moon([-15, -3, 13], [0, 0, 0])
    d = Moon([3, 7, -4], [0, 0, 0])

    moons = [a, b, c, d]

    for _ in range(1000):
        for m1 in moons:
            for m2 in moons:
                m1.attract(m2)
        for m in moons:
            m.update()

    tot = sum(map(lambda x: x.tot(), moons))
    print(tot)


def gcd(a, b):
    while b > 0:
        a, b = b, a % b
    return a


def lcm(a, b):
    return a * b / gcd(a, b)


def find_cycle(start):
    n = len(start)
    moons = list(map(lambda x: Moon([x], [0]), start))

    for i in itertools.count(1, 1):
        for m1 in moons:
            for m2 in moons:
                m1.attract(m2)
        for m in moons:
            m.update()
        if list(map(lambda x: x.position, moons)) == start and list(map(lambda x: x.velocity, moons)) == [0] * n:
            return i


def solve2():
    a = find_cycle([-10, 1, -15, 3])
    b = find_cycle([-13, 2, -3, 7])
    c = find_cycle([7, 1, 13, -4])

    print(lcm(a, lcm(b, c)))


if __name__ == '__main__':
    solve2()
