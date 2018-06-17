#!/usr/bin/env python
from coin import Coin


class CoinManager(object):

    def __init__(self, money_map):
        self.__money_map = money_map

    def is_valid(self, coin: Coin):
        return self.__coin_to_money(coin).is_valid()

    def __coin_to_money(self, coin: Coin):
        return self.__money_map[coin.get_measure()]

    def get_value(self, coin: Coin):
        return self.__coin_to_money(coin).get_value()

    def get_name(self, coin: Coin):
        return self.__coin_to_money(coin).get_name()
