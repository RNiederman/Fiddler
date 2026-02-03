# https://thefiddler.substack.com/p/can-you-play-the-price-is-right-continuously

import numpy as np
import pandas as pd
from joblib import Parallel, delayed
from tqdm import trange


def fiddler_tpir_wheel_xc(players=3, intial_threshold=1/2):

    spins_a = np.random.uniform(low=0, high=1, size=2)
    threshold = intial_threshold

    ledger = []

    player_a = spins_a[0]
    if player_a < threshold:
        player_a += spins_a[1]
    if player_a > 1:
        player_a = 0

    ledger.append(player_a)

    threshold = max(intial_threshold, player_a)
    player_set = range(1, players)

    for p in player_set:
        spins_z = np.random.uniform(low=0, high=1, size=2)

        # The last contestant only needs to beat the highest score
        if p == max(player_set):
            threshold = max(ledger)

        player_z = spins_z[0]
        if player_z < threshold:
            player_z += spins_z[1]
        if player_z > 1:
            player_z = 0

        ledger.append(player_z)

        threshold = max(ledger)
        threshold = max(threshold, intial_threshold)

    winner = max(ledger)
    a_wins = player_a == winner
    return a_wins


TRIALS = 10**6
contestant_set = range(2, 31)
contestants = []
register = []

for c in contestant_set:

    mc_xc = Parallel(n_jobs=-2)(delayed(fiddler_tpir_wheel_xc)(c, 1/2)
                                for _ in trange(TRIALS, unit_scale=True,
                                                desc=str(c) + ' Contestants'))

    win_perc = sum(mc_xc)/TRIALS
    contestants.append(c)
    register.append(win_perc)

    print(f'{win_perc:.3%}')

tpir_df = pd.DataFrame(list(zip(contestants, register)),
                       columns=['Contestants', 'Win_Perc'])

print(tpir_df.to_string(index=False))
