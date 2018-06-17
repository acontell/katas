#!/usr/bin/env python
from coin_manager import CoinManager
from coin import Coin


class AmountManager(object):

    def __init__(self, coin_manager: CoinManager):
        self.__coin_manager = coin_manager
        self.__current_amount = 0
        self.__coin_return = {}

    def insert_coin(self, coin: Coin):
        self.__update_amount(coin) if self.__coin_manager.is_valid(coin) else self.__update_return(coin)

    def __update_amount(self, coin: Coin):
        self.__current_amount += self.__coin_manager.get_value(coin)

    def __update_return(self, coin: Coin):
        name = self.__coin_manager.get_name(coin)
        self.__coin_return[name] = self.__coin_return.get(name, 0) + 1

    def get_amount(self):
        return self.__current_amount

    def get_return_map(self):
        return self.__coin_return
