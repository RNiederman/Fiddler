# https://thefiddler.substack.com/p/can-you-race-around-the-monopoly

import numpy as np
import itertools
from joblib import Parallel, delayed
from tqdm import trange


def oligopoly(n_dice=2):
    spaces = []
    posn = 0
    while posn < 40:
        roll = sum(np.random.randint(low=1, high=7, size=n_dice))
        posn += roll
        spaces.append(posn)
    spaces = spaces[:-1]  # Remove the last entry

    return spaces


def flatten(list_of_lists):
    flattened_list = list(itertools.chain.from_iterable(list_of_lists))
    bins = np.bincount(flattened_list, None, 40)
    tupl = tuple(bins)

    return tupl


games = 10 ** 6
multi = 10
games *= multi

z2 = Parallel(n_jobs=-2)(delayed(oligopoly)(2) for _ in
                         trange(games, desc='2 Dice', unit_scale=True))
z3 = Parallel(n_jobs=-2)(delayed(oligopoly)(3) for _ in
                         trange(games, desc='3 Dice', unit_scale=True))

space_counts2 = flatten(z2)
space_counts3 = flatten(z3)

del z2, z3

# Standard Fiddler
most_freq2 = max(space_counts2)
max_number2 = space_counts2.index(most_freq2)
prob2 = most_freq2 / games

print()
print(max_number2)
print(f'{prob2:.2%}')
# 7

# Extra Credit
least_freq2 = min(space_counts2[10:40])
min_number2 = space_counts2.index(least_freq2)

least_freq3 = min(space_counts3[10:40])
min_number3 = space_counts3.index(least_freq3)

print()
print(min_number2)
# 13
print(min_number3)
# 17

# https://thefiddler.substack.com/p/can-you-make-the-connection
