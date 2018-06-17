#!/usr/bin/env python
from coin_manager import CoinManager


class AmountManager(object):

    def __init__(self, coin_manager: CoinManager):
        self.__coin_manager = coin_manager
        self.__current_amount = 0
        self.__coin_return = {}

    def insert_coin(self, coin_type):
        if self.__coin_manager.is_valid(coin_type):
            self.__current_amount += self.__coin_manager.get_value(coin_type)
        else:
            self.__coin_return[coin_type] = self.__coin_return.get(coin_type, 0) + 1

    def get_amount(self):
        return self.__current_amount

    def get_return_map(self):
        return self.__coin_return
