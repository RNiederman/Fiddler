# https://thefiddler.substack.com/p/can-your-team-self-organize

import numpy as np
from joblib import Parallel, delayed
from tqdm import trange


def fiddler_sorter(players=4, max_value=100):
    v = np.random.randint(max_value, size=players).tolist()
    
    n = len(v)
    dp = [1] * n
    parent = [-1] * n

    for i in range(n):
        for j in range(i):
            if v[j] < v[i] and dp[j] + 1 > dp[i]:
                dp[i] = dp[j] + 1
                parent[i] = j

    length = max(dp)
    idx = dp.index(length)
    seq = []
    while idx != -1:
        seq.append(v[idx])
        idx = parent[idx]
    
    score = len(seq[::-1])
    return score


trials = 10**7
mc = Parallel(n_jobs=-2)(delayed(fiddler_sorter)(4)
                         for _ in trange(trials, unit_scale=True, colour="blue"))

xc_mc = Parallel(n_jobs=-2)(delayed(fiddler_sorter)(10)
                            for _ in trange(trials, unit_scale=True, colour="green"))

soln = sum(mc) / trials
xc_soln = sum(xc_mc) / trials

print(f'{soln:.2f}')
# 2.40
print(f'{xc_soln:.2f}')
# 4.28

