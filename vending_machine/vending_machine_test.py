#!/usr/bin/env python

import unittest
from vending_machine import VendingMachine
from amount_manager import AmountManager
from tests_fixture import TestsFixture


class VendingMachineTests(unittest.TestCase):

    def setUp(self):
        self.vending_machine = VendingMachine(TestsFixture.DISPLAY, AmountManager(TestsFixture.COIN_MANAGER))
        self.expected_amount_of_money = 0

    def test_should_add_correct_amount_for_one_coin(self):
        for valid_coin in TestsFixture.VALID_COINS:
            vending_machine = VendingMachine(TestsFixture.DISPLAY, AmountManager(TestsFixture.COIN_MANAGER))
            vending_machine.insert_coin(valid_coin)
            self.assertEqual(vending_machine.get_amount(), TestsFixture.COIN_VALUES[valid_coin])

    def test_should_not_add_anything_when_incorrect_coin_inserted(self):
        for invalid_coin in TestsFixture.INVALID_COINS:
            vending_machine = VendingMachine(TestsFixture.DISPLAY, AmountManager(TestsFixture.COIN_MANAGER))
            vending_machine.insert_coin(invalid_coin)
            self.assertEqual(vending_machine.get_amount(), 0)

    def test_should_add_amount_of_inserted_coins(self):
        self.given_vending_machine_with_coins()
        self.assertEqual(self.vending_machine.get_amount(), self.expected_amount_of_money)

    def given_vending_machine_with_coins(self):
        self.vending_machine.insert_coin(TestsFixture.COIN_1)
        self.vending_machine.insert_coin(TestsFixture.COIN_1)
        self.vending_machine.insert_coin(TestsFixture.COIN_2)
        self.vending_machine.insert_coin(TestsFixture.COIN_3)
        self.vending_machine.insert_coin(TestsFixture.COIN_3)
        self.expected_amount_of_money = 2 * TestsFixture.COIN_VALUE_1 + TestsFixture.COIN_VALUE_2 + 2 * TestsFixture.COIN_VALUE_3

    def test_should_display_amount_when_valid_coins_inserted(self):
        self.vending_machine.insert_coin(TestsFixture.COIN_1)
        self.assertEqual(self.vending_machine.display_message(), TestsFixture.AMOUNT_TPL % TestsFixture.COIN_VALUE_1)

    def test_should_display_amount_when_more_than_one_valid_coins_inserted(self):
        self.given_vending_machine_with_coins()
        self.assertEqual(self.vending_machine.display_message(), TestsFixture.AMOUNT_TPL % self.expected_amount_of_money)

    def test_should_display_insert_coin_msg_when_no_coins_inserted(self):
        self.assertEqual(self.vending_machine.display_message(), TestsFixture.NO_COINS_MSG)

    def test_should_display_insert_coin_msg_when_no_valid_coins_inserted(self):
        self.vending_machine.insert_coin(TestsFixture.INVALID_COIN)
        self.assertEqual(self.vending_machine.display_message(), TestsFixture.NO_COINS_MSG)

    def test_should_place_rejected_coins_in_coin_return(self):
        self.given_vending_machine_with_coins()
        self.vending_machine.insert_coin(TestsFixture.INVALID_COIN)
        self.vending_machine.insert_coin(TestsFixture.INVALID_COIN)
        self.assertEqual(self.vending_machine.display_return(), TestsFixture.RETURN_TPL.format(TestsFixture.INVALID_COIN, 2))

        if __name__ == '__main__':
            unittest.main()
