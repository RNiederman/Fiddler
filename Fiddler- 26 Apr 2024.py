# https://thefiddler.substack.com/p/can-you-play-the-price-is-right-continuously

import numpy as np
from joblib import Parallel, delayed
from tqdm import trange


def fiddler_tpir_wheel(intial_threshold=1/2):

    spins_a = np.random.uniform(low=0, high=1, size=2)
    spins_b = np.random.uniform(low=0, high=1, size=2)

    spin_threshold = intial_threshold

    player_a = spins_a[0]
    if player_a < spin_threshold:
        player_a += spins_a[1]
    if player_a > 1:
        player_a = 0

    spin_threshold = player_a

    player_b = spins_b[0]
    if player_b < spin_threshold:
        player_b += spins_b[1]
    if player_b > 1:
        player_b = 0

    a_wins = player_a > player_b
    return a_wins


N = 10**7

mc = Parallel(n_jobs=-2)(delayed(fiddler_tpir_wheel)(.5)
                         for _ in trange(N, unit_scale=True))

wins = sum(mc)
print(f'{wins/N:.3%}')
