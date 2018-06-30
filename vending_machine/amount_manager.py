#!/usr/bin/env python
from coin_repository import CoinRepository
from coin import Coin


class AmountManager(object):

    def __init__(self, coin_repository: CoinRepository):
        self.__coin_repository = coin_repository
        self.__coins = []

    def get_coins(self):
        return self.__coins

    def insert_coin(self, coin: Coin):
        self.__coins.append(coin)

    def is_valid(self, coin):
        return self.__coin_repository.is_valid(coin)

    def get_amount(self):
        return self.__coin_repository.get_value(self.__coins)

    def has_enough_amount(self, amount):
        return amount <= self.get_amount()

    def spend(self, amount):
        return self.__change_and_empty(amount)

    def __change_and_empty(self, amount):
        change = self.__coin_repository.get_change(self.get_amount() - amount)
        self.empty_coins()
        return change

    def empty_coins(self):
        self.__coins = []
