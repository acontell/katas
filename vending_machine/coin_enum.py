#!/usr/bin/env python
from coin import Coin

from enum import Enum


class CoinEnum(Enum):
    NICKELS = Coin(1, 1)
    DIMES = Coin(2, 2)
    QUARTERS = Coin(3, 3)
