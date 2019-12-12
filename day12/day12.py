import numpy as np


class Moon:
    def __init__(self, x, y, z):
        self.position = np.array([x, y, z])
        self.velocity = np.array([0, 0, 0])

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


if __name__ == '__main__':
    a = Moon(-10, -13, 7)
    b = Moon(1, 2, 1)
    c = Moon(-15, -3, 13)
    d = Moon(3, 7, -4)

    moons = [a, b, c, d]

    for _ in range(1000):
        for m1 in moons:
            for m2 in moons:
                m1.attract(m2)
        for m in moons:
            m.update()

    tot = sum(map(lambda x: x.tot(), moons))
    print(tot)
