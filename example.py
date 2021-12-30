import math

def my_partial_injections(m, n):
    return sum(math.factorial(k) * math.comb(m, k) * math.comb(n, k) for k in range(min(m, n) + 1))

def forclift_partial_injections(m, n):
    return 1 if m == 0 else sum(math.comb(n, k) * forclift_partial_injections(m-1, n-k) for k in range(min(n, 1) + 1))

# for m in range(1, 5):
#     for n in range(1, 5):
#         print("m =", m, ", n =", n, "mine =", my_partial_injections(m, n), ", forclift =", forclift_partial_injections(m, n))

def injections(m, n):
    return sum(math.comb(m, k) * (-1)**(m-k) * injections2(k, n) for k in range(m + 1))

def injections2(k, n):
    return 1 if k == 0 else injections2(k-1, n) + n * injections2(k-1, n-1)

def forclift_injections(m, n):
    return sum(math.comb(m, k) * (-1)**(m-k) * forclift_injections2(k, n) for k in range(m + 1))

def forclift_injections2(a, b):
    if a <= 0:
        return 1
    if a == 1:
        return b+1
    return sum(int(math.factorial(b) / math.factorial(b - m - l)) * forclift_injections2(a - 2, b - m - l) for m in range(min(b, 1) + 1) for l in range(min(b - m, 1) + 1))

MAX = 5

for m in range(1, MAX):
    for n in range(1, MAX):
        print("f({}, {}) = {}".format(m, n, injections(m, n)))

print()
for m in range(MAX):
    for n in range(MAX):
        print("g({}, {}) = {}".format(m, n, injections2(m, n)))

print()
for m in range(1, MAX):
    for n in range(1, MAX):
        print("f({}, {}) = {}".format(m, n, forclift_injections(m, n)))

print()
for m in range(MAX):
    for n in range(MAX):
        print("g({}, {}) = {}".format(m, n, forclift_injections2(m, n)))
