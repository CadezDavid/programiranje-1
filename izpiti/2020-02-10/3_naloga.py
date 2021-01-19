from functools import lru_cache

@lru_cache(maxsize=None)
def f(k, n, zacetna_vrednost=0):
    if n == 0:
        return 1
    else:
        sum = 0
        for i in range(max(zacetna_vrednost - k, 0), zacetna_vrednost + k + 1):
            sum += f(k, n - 1, i)
        return sum
