#!/usr/bin/env python


class CoinManager(object):

    def __init__(self, valid_coins, invalid_coins, coin_values):
        self.__valid_coins = valid_coins
        self.__invalid_coins = invalid_coins
        self.__coin_values = coin_values

    def is_valid(self, coin_type):
        return coin_type in self.__valid_coins

    def get_value(self, coin_type):
        return self.__coin_values.get(coin_type, 0)
