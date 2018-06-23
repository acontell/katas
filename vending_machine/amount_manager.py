#!/usr/bin/env python
from coin_manager import CoinManager
from coin import Coin


class AmountManager(object):

    def __init__(self, coin_manager: CoinManager):
        self.__coin_manager = coin_manager
        self.__current_amount = 0
        self.__coin_return = {}

    def insert_coin(self, coin: Coin):
        coin_name = self.__coin_manager.get_name(coin)
        self.__update_amount(coin) if self.__coin_manager.is_valid(coin) else self.__update_return(coin_name)

    def __update_amount(self, coin: Coin):
        self.__current_amount += self.__coin_manager.get_value(coin)

    def __update_return(self, name, amount=1):
        self.__coin_return[name] = self.__coin_return.get(name, 0) + amount

    def get_amount(self):
        return self.__current_amount

    def get_return(self):
        return self.__coin_return

    def spend(self, amount):
        return self.__return_change_and_reset_amount(amount) if self.__is_enough_money(amount) else False

    def __is_enough_money(self, amount):
        return amount <= self.get_amount()

    def __return_change_and_reset_amount(self, amount):
        self.__add_change_to_return(self.__coin_manager.get_change(self.__current_amount - amount))
        self.__current_amount = 0
        return True

    def __add_change_to_return(self, change):
        for name, amount in change.items():
            self.__update_return(name, amount)
