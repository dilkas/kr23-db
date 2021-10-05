import math

def my_partial_injections(m, n):
    return sum(math.factorial(k) * math.comb(m, k) * math.comb(n, k) for k in range(min(m, n) + 1))

def forclift_partial_injections(m, n):
    return 1 if m == 0 else sum(math.comb(n, k) * forclift_partial_injections(m-1, n-k) for k in range(min(n, 1) + 1))

for m in range(1, 5):
    for n in range(1, 5):
        print("m =", m, ", n =", n, "mine =", my_partial_injections(m, n), ", forclift =", forclift_partial_injections(m, n))
