#!/usr/bin/env python
from coin_manager import CoinManager
from coin import Coin


class AmountManager(object):

    def __init__(self, coin_manager: CoinManager):
        self.__coin_manager = coin_manager
        self.__coins = []
        self.__return = []

    def insert_coin(self, coin: Coin):
        self.__coins.append(coin) if self.__coin_manager.is_valid(coin) else self.__return.append(coin)

    def get_amount(self):
        return self.__coin_manager.get_value(self.__coins)

    def get_return(self):
        return self.__return

    def try_to_spend(self, amount):
        return self.__give_change_and_reset(amount) if self.__is_enough_money(amount) else False

    def __is_enough_money(self, amount):
        return amount <= self.get_amount()

    def __give_change_and_reset(self, amount):
        self.__update_return_and_empty_coins(self.__coin_manager.get_change(self.get_amount() - amount))
        return True

    def __update_return_and_empty_coins(self, coins):
        self.__update_return(coins)
        self.__empty_coins()

    def __update_return(self, coins):
        self.__return += coins

    def __empty_coins(self):
        self.__coins = []

    def flush_coins(self):
        self.__update_return_and_empty_coins(self.__coins)
