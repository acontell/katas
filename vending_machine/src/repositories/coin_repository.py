#!/usr/bin/env python
from functools import reduce
from math import ceil

from src.model.coin import Coin
from src.model.measure import Measure


class CoinRepository(object):

    def __init__(self, money_map):
        self.__money_map = money_map

    def get_value(self, coins):
        return reduce(lambda x, m: x + m.get_value(), self.__get_valid_money(coins), 0)

    def __get_valid_money(self, coins):
        return list(filter(lambda m: m.is_valid(), self.__get_money(coins)))

    def __get_money(self, coins):
        return list(map(lambda coin: self.__to_money(coin), coins))

    def __to_money(self, coin: Coin):
        return self.__money_map[coin.get_measure()]

    def is_valid(self, coin: Coin):
        return self.__to_money(coin).is_valid()

    def get_change(self, amount):
        coin = Coin(Measure(1, 1))
        quantity = int(ceil(amount / self.__to_money(coin).get_value()))
        return [Coin(Measure(1, 1)) for _ in range(quantity)]
