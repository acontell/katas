#!/usr/bin/env python

import unittest
from coin_enum import CoinEnum
from coin import Coin
from vending_machine import VendingMachine
from display import Display
from amount_manager import AmountManager
from coin_manager import CoinManager


class VendingMachineTests(unittest.TestCase):
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
    __NO_COINS_MSG = 'INSERT_COIN'
    __AMOUNT_TPL = 'Current amount: %.2f$'
    __RETURN_TPL = "{}:\t{}"
    __DISPLAY = Display(__NO_COINS_MSG, __AMOUNT_TPL, __RETURN_TPL)
    __COIN_MANAGER = CoinManager(__VALID_COINS, __COIN_VALUES)

    def setUp(self):
        self.vending_machine = VendingMachine(self.__DISPLAY, AmountManager(self.__COIN_MANAGER))
        self.expected_amount_of_money = 0

    def test_should_add_correct_amount_for_one_coin(self):
        for valid_coin in self.__VALID_COINS:
            vending_machine = VendingMachine(self.__DISPLAY, AmountManager(self.__COIN_MANAGER))
            vending_machine.insert_coin(valid_coin)
            self.assertEqual(vending_machine.get_amount(), self.__COIN_VALUES[valid_coin])

    def test_should_not_add_anything_when_incorrect_coin_inserted(self):
        for invalid_coin in self.__INVALID_COINS:
            vending_machine = VendingMachine(self.__DISPLAY, AmountManager(self.__COIN_MANAGER))
            vending_machine.insert_coin(invalid_coin)
            self.assertEqual(vending_machine.get_amount(), 0)

    def test_should_add_amount_of_inserted_coins(self):
        self.given_vending_machine_with_coins()
        self.assertEqual(self.vending_machine.get_amount(), self.expected_amount_of_money)

    def given_vending_machine_with_coins(self):
        self.vending_machine.insert_coin(self.__COIN_1)
        self.vending_machine.insert_coin(self.__COIN_1)
        self.vending_machine.insert_coin(self.__COIN_2)
        self.vending_machine.insert_coin(self.__COIN_3)
        self.vending_machine.insert_coin(self.__COIN_3)
        self.expected_amount_of_money = 2 * self.__COIN_VALUE_1 + self.__COIN_VALUE_2 + 2 * self.__COIN_VALUE_3

    def test_should_display_amount_when_valid_coins_inserted(self):
        self.vending_machine.insert_coin(self.__COIN_1)
        self.assertEqual(self.vending_machine.display_message(), self.__AMOUNT_TPL % self.__COIN_VALUE_1)

    def test_should_display_amount_when_more_than_one_valid_coins_inserted(self):
        self.given_vending_machine_with_coins()
        self.assertEqual(self.vending_machine.display_message(), self.__AMOUNT_TPL % self.expected_amount_of_money)

    def test_should_display_insert_coin_msg_when_no_coins_inserted(self):
        self.assertEqual(self.vending_machine.display_message(), self.__NO_COINS_MSG)

    def test_should_display_insert_coin_msg_when_no_valid_coins_inserted(self):
        self.vending_machine.insert_coin(self.__INVALID_COIN)
        self.assertEqual(self.vending_machine.display_message(), self.__NO_COINS_MSG)

    def test_should_place_rejected_coins_in_coin_return(self):
        self.given_vending_machine_with_coins()
        self.vending_machine.insert_coin(self.__INVALID_COIN)
        self.vending_machine.insert_coin(self.__INVALID_COIN)
        self.assertEqual(self.vending_machine.display_return(), self.__RETURN_TPL.format(self.__INVALID_COIN, 2))

        if __name__ == '__main__':
            unittest.main()
