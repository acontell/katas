#!/usr/bin/env python

import unittest
from coin_enum import CoinEnum
from coin import Coin
from amount_manager import AmountManager
from coin_manager import CoinManager


class AmountManagerTests(unittest.TestCase):
    __COIN_1 = CoinEnum.NICKELS
    __COIN_2 = CoinEnum.DIMES
    __COIN_3 = CoinEnum.QUARTERS
    __COIN_VALUE_1 = 0.05
    __COIN_VALUE_2 = 0.1
    __COIN_VALUE_3 = 0.25
    __VALID_COINS = [__COIN_1, __COIN_2, __COIN_3]
    __INVALID_COIN = Coin(15, 20)
    __INVALID_COINS = [__INVALID_COIN]
    __COIN_VALUES = {CoinEnum.NICKELS: __COIN_VALUE_1, CoinEnum.DIMES: __COIN_VALUE_2,
                     CoinEnum.QUARTERS: __COIN_VALUE_3}

    def setUp(self):
        self.coin_manager = CoinManager(self.__VALID_COINS, self.__COIN_VALUES)
        self.amount_manager = AmountManager(self.coin_manager)
        self.expected_amount_of_money = 0

    def test_should_add_correct_amount_for_one_coin(self):
        for valid_coin in self.__VALID_COINS:
            amount_manager = AmountManager(self.coin_manager)
            amount_manager.insert_coin(valid_coin)
            self.assertEqual(amount_manager.get_amount(), self.coin_manager.get_value(valid_coin))

    def test_should_not_add_anything_when_incorrect_coin_inserted(self):
        for invalid_coin in self.__INVALID_COINS:
            amount_manager = AmountManager(self.coin_manager)
            amount_manager.insert_coin(invalid_coin)
            self.assertEqual(amount_manager.get_amount(), 0)

    def test_should_add_amount_of_inserted_coins(self):
        self.given_amount_manager_with_coins()
        self.assertEqual(self.amount_manager.get_amount(), self.expected_amount_of_money)

    def given_amount_manager_with_coins(self):
        self.amount_manager.insert_coin(self.__COIN_1)
        self.amount_manager.insert_coin(self.__COIN_1)
        self.amount_manager.insert_coin(self.__COIN_2)
        self.amount_manager.insert_coin(self.__COIN_3)
        self.amount_manager.insert_coin(self.__COIN_3)
        self.expected_amount_of_money = 2 * self.__COIN_VALUE_1 + self.__COIN_VALUE_2 + 2 * self.__COIN_VALUE_3
        pass

    def test_should_return_map_with_coins_to_return(self):
        self.given_amount_manager_with_coins()
        self.amount_manager.insert_coin(self.__INVALID_COIN)
        self.amount_manager.insert_coin(self.__INVALID_COIN)
        self.assertEqual(self.amount_manager.get_return_map(), {self.__INVALID_COIN: 2})

        if __name__ == '__main__':
            unittest.main()
