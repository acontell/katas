#!/usr/bin/env python

import unittest
from src.managers.money_manager import MoneyManager
from test.tests_fixture import TestsFixture


class AmountManagerTests(unittest.TestCase):

    def setUp(self):
        self.amount_manager = MoneyManager(TestsFixture.COIN_INVENTORY)
        self.expected_amount_of_money = 0

    def test_should_add_correct_amount_for_one_coin(self):
        for valid_coin in TestsFixture.VALID_COINS:
            amount_manager = MoneyManager(TestsFixture.COIN_INVENTORY)
            amount_manager.insert_coin(valid_coin)
            self.assertEqual(amount_manager.get_money(), TestsFixture.COIN_INVENTORY.get_value([valid_coin]))

    def test_should_add_amount_of_inserted_coins(self):
        self.given_amount_manager_with_coins()
        self.assertEqual(self.amount_manager.get_money(), self.expected_amount_of_money)

    def given_amount_manager_with_coins(self):
        self.amount_manager.insert_coin(TestsFixture.COIN_1)
        self.amount_manager.insert_coin(TestsFixture.COIN_1)
        self.amount_manager.insert_coin(TestsFixture.COIN_2)
        self.amount_manager.insert_coin(TestsFixture.COIN_3)
        self.amount_manager.insert_coin(TestsFixture.COIN_3)
        self.expected_amount_of_money = 2 * TestsFixture.MONEY_VALUE_1 + TestsFixture.MONEY_VALUE_2 + 2 * TestsFixture.MONEY_VALUE_3
        pass

    def test_should_have_change_in_coins_when_enough_to_spend(self):
        self.given_amount_manager_with_coins()
        self.amount_manager.spend(0.65)
        self.assertEqual(self.amount_manager.get_coins(), [TestsFixture.COIN_1])

    def test_should_have_change_when_spend_is_successful(self):
        self.given_amount_manager_with_coins()
        self.amount_manager.spend(0.1)
        self.assertEqual(self.amount_manager.get_money(), 0.6)

    def test_should_return_true_when_enough_money(self):
        self.given_amount_manager_with_coins()
        self.assertTrue(self.amount_manager.has_enough_money(0.1))

    def test_should_return_false_when_not_enough_money(self):
        self.given_amount_manager_with_coins()
        self.assertFalse(self.amount_manager.has_enough_money(100))

    def test_should_empty_coins(self):
        self.amount_manager.empty_coins()
        self.assertEqual(self.amount_manager.get_coins(), [])

    def test_should_return_true_when_enough_change(self):
        self.assertTrue(TestsFixture.COIN_INVENTORY.has_enough_change(0.3))

    def test_should_return_false_when_not_enough_change(self):
        self.assertFalse(TestsFixture.COIN_INVENTORY.has_enough_change(30))

        if __name__ == '__main__':
            unittest.main()
