#!/usr/bin/env python
from math import ceil

from coin import Coin


class CoinManager(object):

    def __init__(self, money_map):
        self.__money_map = money_map

    def is_valid(self, coin: Coin):
        return self.__to_money(coin).is_valid()

    def __to_money(self, coin: Coin):
        return self.__money_map[coin.get_measure()]

    def get_value(self, coin: Coin):
        return self.__to_money(coin).get_value()

    def get_name(self, coin: Coin):
        return self.__to_money(coin).get_name()

    def get_change(self, amount):
        smallest = self.__get_smallest()
        return {smallest.get_name(): ceil(amount / smallest.get_value())}

    def __get_smallest(self):
        smallest_money = None
        current_value = float('inf')
        for money in self.__money_map.values():
            if 0 < money.get_value() < current_value:
                smallest_money = money
                current_value = money.get_value()
        return smallest_money
