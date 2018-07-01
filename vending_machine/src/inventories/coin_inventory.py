#!/usr/bin/env python
from math import ceil

from src.model.coin import Coin
from src.model.measure import Measure
from src.converters.coin_converter import CoinConverter


class CoinInventory(object):

    def __init__(self, change, coin_converter: CoinConverter):
        self.__change = change
        self.__coin_converter = coin_converter

    def get_value(self, coins):
        return self.__coin_converter.get_value(coins)

    def is_valid(self, coin: Coin):
        return self.__coin_converter.is_valid(coin)

    # Simplistic solution.
    def has_enough_change(self, amount):
        return self.__coin_converter.get_value(self.__change) >= amount

    # Simplistic solution.
    def get_change(self, amount):
        coin = Coin(Measure(1, 1))
        quantity = int(ceil(amount / self.__coin_converter.to_money(coin).get_value()))
        return [Coin(Measure(1, 1)) for _ in range(quantity)]
