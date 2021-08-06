import math

def f(n):
    return (sum(math.comb(n, k) * math.comb(n, m) * 2**(n*(k+m)-k*m) for k in range(n+1) for m in range(n+1)))**n

def g(n):
    return (4**n + 2**(n+1) + 3**n)**n

print(g(1))
print(g(2))
print(g(3))

