#!/usr/bin/env python
from src.inventories.coin_inventory import CoinInventory
from src.model.coin import Coin


class MoneyManager(object):

    def __init__(self, coin_inventory: CoinInventory):
        self.__coin_inventory = coin_inventory
        self.__coins = []

    def get_coins(self):
        return self.__coins

    def insert_coin(self, coin: Coin):
        self.__coins.append(coin)

    def is_valid(self, coin):
        return self.__coin_inventory.is_valid(coin)

    def get_money(self):
        return self.__coin_inventory.get_value(self.__coins)

    def has_enough_money(self, amount):
        return amount <= self.get_money()

    def has_enough_change(self, amount):
        return self.__coin_inventory.has_enough_change(amount)

    def spend(self, amount):
        self.__coins = self.__coin_inventory.get_change(self.get_money() - amount)

    def empty_coins(self):
        self.__coins = []
