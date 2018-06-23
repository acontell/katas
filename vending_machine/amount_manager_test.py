#!/usr/bin/env python

import unittest
from amount_manager import AmountManager
from coin_manager import CoinManager
from tests_fixture import TestsFixture


class AmountManagerTests(unittest.TestCase):

    def setUp(self):
        self.coin_manager = CoinManager(TestsFixture.MONEY_MAP)
        self.amount_manager = AmountManager(self.coin_manager)
        self.expected_amount_of_money = 0

    def test_should_add_correct_amount_for_one_coin(self):
        for valid_coin in TestsFixture.VALID_COINS:
            amount_manager = AmountManager(self.coin_manager)
            amount_manager.insert_coin(valid_coin)
            self.assertEqual(amount_manager.get_amount(), self.coin_manager.get_value(valid_coin))

    def test_should_not_add_anything_when_incorrect_coin_inserted(self):
        for invalid_coin in TestsFixture.INVALID_COINS:
            amount_manager = AmountManager(self.coin_manager)
            amount_manager.insert_coin(invalid_coin)
            self.assertEqual(amount_manager.get_amount(), 0)

    def test_should_add_amount_of_inserted_coins(self):
        self.given_amount_manager_with_coins()
        self.assertEqual(self.amount_manager.get_amount(), self.expected_amount_of_money)

    def given_amount_manager_with_coins(self):
        self.amount_manager.insert_coin(TestsFixture.COIN_1)
        self.amount_manager.insert_coin(TestsFixture.COIN_1)
        self.amount_manager.insert_coin(TestsFixture.COIN_2)
        self.amount_manager.insert_coin(TestsFixture.COIN_3)
        self.amount_manager.insert_coin(TestsFixture.COIN_3)
        self.expected_amount_of_money = 2 * TestsFixture.MONEY_VALUE_1 + TestsFixture.MONEY_VALUE_2 + 2 * TestsFixture.MONEY_VALUE_3
        pass

    def test_should_return_map_with_coins_to_return(self):
        self.given_amount_manager_with_coins()
        self.amount_manager.insert_coin(TestsFixture.INVALID_COIN)
        self.amount_manager.insert_coin(TestsFixture.INVALID_COIN)
        self.assertEqual(self.amount_manager.get_return_map(), {TestsFixture.INVALID_MONEY_NAME: 2})

    def test_should_return_false_when_not_enough_amount_to_spend(self):
        self.assertFalse(self.amount_manager.spend(100))

    def test_should_return_true_when_enough_to_spend(self):
        self.given_amount_manager_with_coins()
        self.assertTrue(self.amount_manager.spend(0.1))

    def test_should_reset_amount_when_spend_is_successful(self):
        self.given_amount_manager_with_coins()
        self.amount_manager.spend(0.1)
        self.assertEqual(self.amount_manager.get_amount(), 0)

        if __name__ == '__main__':
            unittest.main()
