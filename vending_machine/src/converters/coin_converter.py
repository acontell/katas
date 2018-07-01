#!/usr/bin/env python
from functools import reduce

from src.model.coin import Coin


class CoinConverter(object):

    def __init__(self, money_map):
        self.__money_map = money_map

    def get_value(self, coins):
        return reduce(lambda x, m: x + m.get_value(), self.__get_valid_money(coins), 0)

    def __get_valid_money(self, coins):
        return list(filter(lambda m: m.is_valid(), self.__get_money(coins)))

    def __get_money(self, coins):
        return list(map(lambda coin: self.to_money(coin), coins))

    def to_money(self, coin: Coin):
        return self.__money_map[coin.get_measure()]

    def is_valid(self, coin: Coin):
        return self.to_money(coin).is_valid()
