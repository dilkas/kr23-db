#!/usr/bin/env python3

import math

# ================================= FINAL =================================


def one_dimensional_bijections1(n):
    def g(n, m):
        if m == 0:
            return 1
        return g(n, m - 1) + n * g(n - 1, m - 1)

    return sum(math.comb(n, m) * (-1) ** (n - m) * g(n, m) for m in range(n + 1))


def one_dimensional_bijections2(n):
    def g(n, m):
        if n == 0:
            return 1
        return m * g(n - 1, m - 1)

    return sum(math.comb(n, m) * (-1) ** (n - m) * g(n, m) for m in range(n + 1))


def one_dimensional_bijections3(n):
    def g(m, l):
        if l == 0:
            return 1
        return g(m, l - 1) + m * g(m - 1, l - 1)

    return sum(
        math.comb(n, m)
        * (-1) ** (n - m)
        * sum(math.comb(n, l) * (-1) ** (n - l) * g(m, l) for l in range(n + 1))
        for m in range(n + 1)
    )


def one_dimensional_bijections4(n):
    def g(n, m):
        if n == 0:
            return 1
        return m * g(n - 1, m - 1)

    return sum(math.comb(n, m) * (-1) ** (n - m) * g(n, m) for m in range(n + 1))


def one_dimensional_injections1(m):
    def g(m, l):
        if m == 0:
            return 1
        return g(m - 1, l) + l * g(m - 1, l - 1)

    return sum(math.comb(m, l) * (-1) ** (m - l) * g(m, l) for l in range(m + 1))


def one_dimensional_injections2(n):
    def g(j, k):
        if j == 0:
            return 1
        return g(j - 1, k) + k * g(j - 1, k - 1)

    return sum(
        math.comb(n, m) * (-1) ** (n - m) * (g(n - 1, m) + m * g(n - 1, m - 1))
        for m in range(n + 1)
    )


def one_dimensional_injections3(n):
    def g(j, k):
        if k == 0:
            return 1
        return g(j, k - 1) + j * g(j - 1, k - 1)

    return sum(
        math.comb(n, m) * (-1) ** (n - m) * (g(m - 1, n) + n * g(m - 1, n - 1))
        for m in range(n + 1)
    )


def one_dimensional_partial_injections(n):
    def e(j, k):
        if k <= 0 or j <= 0:
            return 1
        return e(j, k - 1) + e(j - 1, k - 1)

    def h(n, m, l):
        if l < 2 and m < 1:
            return 2 * e(n - m, n - l - 1)
        return e(n - m, n - l - 1)

    def g(n, m):
        return (
            h(n, m, 0)
            + n * h(n, m, 1)
            + (n - 1) * h(n, m, 2)
            + (n - 1) * (n - 2) // 2 * h(n, m, 3)
        )

    return g(n, 0) + n * g(n, 1) + (n - 1) * g(n, 2) + (n - 1) * (n - 2) // 2 * g(n, 3)


def two_dimensional_bijections1(m, n):
    if m == 0 and n == 0:
        return 1
    if m == 0 or n == 0:
        return 0
    return m * two_dimensional_bijections1(m - 1, n - 1)


def two_dimensional_bijections2(m, n):
    def g(l, n):
        if n == 0:
            return 1
        if l == 0:
            return 0
        return g(l - 1, n) + n * g(l - 1, n - 1)

    return sum(math.comb(m, l) * (-1) ** (m - l) * g(l, n) for l in range(m + 1))


def two_dimensional_injections(m, n):
    if m == 0 and n == 0:
        return 1
    if n == 0:
        return 0
    return two_dimensional_injections(m, n - 1) + m * two_dimensional_injections(
        m - 1, n - 1
    )


def two_dimensional_partial_injections(m, n):
    if n == 0:
        return 1
    return two_dimensional_partial_injections(
        m, n - 1
    ) + m * two_dimensional_partial_injections(m - 1, n - 1)


def my_one_dimensional_surjections(n):
    return sum((-1) ** k * math.comb(n, k) * (n - k) ** n for k in range(n + 1))


def forclift_one_dimensional_surjections(n):
    return sum(
        math.comb(n, m)
        * (-1) ** (n - m)
        * sum(math.comb(n, l) * (-1) ** (n - l) * (l + 1) ** m for l in range(n + 1))
        for m in range(n + 1)
    )


def crane_one_dimensional_surjections(n):
    return sum(
        math.comb(n, m)
        * (-1) ** (n - m)
        * sum(
            math.comb(n, l) * (-1) ** (n - l) * crane_one_dimensional_surjections2(l, m)
            for l in range(n + 1)
        )
        for m in range(n + 1)
    )


def crane_one_dimensional_surjections2(l, m):
    if m == 0:
        return 1
    return (l + 1) * crane_one_dimensional_surjections2(l, m - 1)


def crane_two_dimensional_surjections(m, n):
    return sum(
        math.comb(m, l)
        * (-1) ** (m - l)
        * sum(math.comb(n, k) * (-1) ** (n - k) * (k + 1) ** l for k in range(n + 1))
        for l in range(m + 1)
    )


def my_two_dimensional_surjections(m, n):
    return sum((-1) ** i * math.comb(n, i) * (n - i) ** m for i in range(n + 1))

def crane_one_dimensional_partial_injections(m):
    if m == 0:
        return 1
    return (crane_one_dimensional_partial_injections(m-1) +
            m*2**(m-1)*crane_one_dimensional_partial_injections2(m-1, m-1) +
            m*m*2**(m-1)*crane_one_dimensional_partial_injections2(m-1, m-2) +
            math.comb(m, 2)*2**(m-2)*crane_one_dimensional_partial_injections2(m-2, m-1) +
            math.comb(m, 2)*m*2**(m-2)*crane_one_dimensional_partial_injections2(m-2, m-2))

def crane_one_dimensional_partial_injections2(i, j):
    if i < 0 or j < 0:
        return 0
    if i == 0:
        return 1
    return crane_one_dimensional_partial_injections2(i-1, j) + j * crane_one_dimensional_partial_injections2(i-1, j-1)

# ================================= MISC =================================


def my_partial_injections(m, n):
    return sum(
        math.factorial(k) * math.comb(m, k) * math.comb(n, k)
        for k in range(min(m, n) + 1)
    )


def forclift_partial_injections(m, n):
    return (
        1
        if m == 0
        else sum(
            math.comb(n, k) * forclift_partial_injections(m - 1, n - k)
            for k in range(min(n, 1) + 1)
        )
    )


def injections(m, n):
    return sum(
        math.comb(m, k) * (-1) ** (m - k) * injections2(k, n) for k in range(m + 1)
    )


def injections2(k, n):
    return 1 if k == 0 else injections2(k - 1, n) + n * injections2(k - 1, n - 1)


def forclift_injections(m, n):
    return sum(
        math.comb(m, k) * (-1) ** (m - k) * forclift_injections2(k, n)
        for k in range(m + 1)
    )


def forclift_injections2(a, b):
    if a <= 0:
        return 1
    if a == 1:
        return b + 1
    return sum(
        int(math.factorial(b) / math.factorial(b - m - l))
        * forclift_injections2(a - 2, b - m - l)
        for m in range(min(b, 1) + 1)
        for l in range(min(b - m, 1) + 1)
    )


def forclift2_injections(m, n):
    if m == 0:
        return 1
    if n == 0:
        return forclift2_injections(m - 1, n)
    return forclift2_injections(m - 1, n) + n * forclift2_injections(m - 1, n - 1)


def partial_injections_final(m, n):
    if m == 0:
        return 1
    return partial_injections_final(m - 1, n) + n * partial_injections_final(
        m - 1, n - 1
    )

def factorial(n):
    return sum(math.comb(n, k) * sum(math.comb(n, l) * (l + 1) ** k * (-1) ** (2 * n - k - l) for l in range(n + 1)) for k in range(n + 1))

def friendsmoker(n):
    return 2**n * sum(math.comb(n, m) * 2**((n-m)*(n-m-1) + (n-m)*m + m*(m-1)) for m in range(n + 1))

MAX = 10

# for m in range(1, MAX):
#     for n in range(1, MAX):
#         print("f({}, {}) = {}".format(m, n, my_partial_injections(m, n)))

# print()
# for m in range(1, MAX):
#     for n in range(1, MAX):
#         print("f({}, {}) = {}".format(m, n, forclift2_injections(m, n)))

# for m in range(0, MAX):
#     for n in range(0, MAX):
#         print(
#             "f({}, {}) = {} ({})".format(
#                 m,
#                 n,
#                 my_two_dimensional_surjections(m, n),
#                 crane_two_dimensional_surjections(m, n),
#             )
#         )

# for n in range(0, MAX):
#     print(
#         "f({}) = {} ({}, {})".format(
#             n,
#             crane_one_dimensional_partial_injections(n),
#             two_dimensional_partial_injections(n, n),
#             partial_injections_final(n, n)
#         )
#     )

for n in range(0, MAX):
    print(
        "f({}) = {}".format(
            n,
            friendsmoker(n)
        )
    )
