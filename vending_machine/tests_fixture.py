#!/usr/bin/env python

from coin import Coin
from display import Display
from coin_manager import CoinManager


class TestsFixture(object):
    COIN_1 = Coin('NICKEL', 1, 1)
    COIN_2 = Coin('DIME', 1, 2)
    COIN_3 = Coin('QUARTER', 2, 1)
    COIN_VALUE_1 = 0.05
    COIN_VALUE_2 = 0.1
    COIN_VALUE_3 = 0.25
    VALID_COINS = [COIN_1, COIN_2, COIN_3]
    INVALID_COIN = Coin('PENNY', 15, 20)
    INVALID_COINS = [INVALID_COIN]
    COIN_VALUES = {COIN_1: COIN_VALUE_1, COIN_2: COIN_VALUE_2, COIN_3: COIN_VALUE_3}
    NO_COINS_MSG = 'INSERT_COIN'
    AMOUNT_TPL = 'Current amount: %.2f$'
    RETURN_TPL = '{}:\t{}'
    DISPLAY = Display(NO_COINS_MSG, AMOUNT_TPL, RETURN_TPL)
    COIN_MANAGER = CoinManager(VALID_COINS, COIN_VALUES)
