#!/usr/bin/env python
from coin_repository import CoinRepository
from coin import Coin


class ReturnManager(object):

    def __init__(self, coin_repository: CoinRepository):
        self.__coin_repository = coin_repository
        self.__return = []

    def insert_coin(self, coin: Coin):
        self.__coins.append(coin) if self.__coin_repository.is_valid(coin) else self.__return.append(coin)

    def get_amount(self):
        return self.__coin_repository.get_value(self.__coins)

    def get_return(self):
        return self.__return

    def spend_if_possible(self, amount):
        return self.__give_change_and_reset(amount) if self.check_enough_amount(amount) else False

    def check_enough_amount(self, amount):
        return amount <= self.get_amount()

    def __give_change_and_reset(self, amount):
        self.__update_return_and_empty_coins(self.__coin_repository.get_change(self.get_amount() - amount))
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
