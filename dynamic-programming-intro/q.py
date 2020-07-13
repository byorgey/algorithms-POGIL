qtab = [0,1,1]

def q(n):
    while len(qtab) < n+1:
        qtab.append(-1)
    if qtab[n] == -1:
        qtab[n] = max(q(n-3) ** 2, q(n-1) + q(n-2))
    return qtab[n]
