#!/usr/bin/env python

import unittest
from coin_enum import CoinEnum
from coin import Coin
from coin_manager import CoinManager


class CoinManagerTests(unittest.TestCase):
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
        self.coin_validator = CoinManager(self.__VALID_COINS, self.__COIN_VALUES)

    def test_should_return_true_when_coin_is_valid(self):
        for valid_coin in self.__VALID_COINS:
            self.assertTrue(self.coin_validator.is_valid(valid_coin))

    def test_should_return_false_when_coin_is_invalid(self):
        for invalid_coin in self.__INVALID_COINS:
            self.assertFalse(self.coin_validator.is_valid(invalid_coin))

    def test_should_return_coin_value(self):
        for valid_coin in self.__VALID_COINS:
            self.assertEqual(self.coin_validator.get_value(valid_coin), self.__COIN_VALUES[valid_coin])

    def test_should_return_zero_when_get_value_of_invalid_coin(self):
        for invalid_coin in self.__INVALID_COINS:
            self.assertEqual(self.coin_validator.get_value(invalid_coin), 0)

        if __name__ == '__main__':
            unittest.main()
