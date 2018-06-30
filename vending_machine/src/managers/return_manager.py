#!/usr/bin/env python
from src.model.coin import Coin


class ReturnManager(object):

    def __init__(self):
        self.__return = []

    def add(self, coin: Coin):
        self.__return.append(coin)

    def add_coins(self, coins):
        self.__return += coins

    def check_return(self):
        return self.__return
