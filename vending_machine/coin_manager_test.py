#!/usr/bin/env python

import unittest
from coin_manager import CoinManager
from tests_fixture import TestsFixture


class CoinManagerTests(unittest.TestCase):

    def setUp(self):
        self.coin_manager = CoinManager(TestsFixture.MONEY_MAP)

    def test_should_return_true_when_coin_is_valid(self):
        for valid_coin in TestsFixture.VALID_COINS:
            self.assertTrue(self.coin_manager.is_valid(valid_coin))

    def test_should_return_false_when_coin_is_invalid(self):
        for invalid_coin in TestsFixture.INVALID_COINS:
            self.assertFalse(self.coin_manager.is_valid(invalid_coin))

    def test_should_return_coin_value(self):
        for valid_coin in TestsFixture.VALID_COINS:
            value = TestsFixture.MONEY_MAP[valid_coin.get_measure()].get_value()
            self.assertEqual(self.coin_manager.get_value(valid_coin), value)

    def test_should_return_zero_when_get_value_of_invalid_coin(self):
        for invalid_coin in TestsFixture.INVALID_COINS:
            self.assertEqual(self.coin_manager.get_value(invalid_coin), 0)

    def test_should_return_change(self):
        self.assertEqual(self.coin_manager.get_change(0.7), {TestsFixture.MONEY_NAME_1: 14})

        if __name__ == '__main__':
            unittest.main()
