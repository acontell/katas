#!/usr/bin/env python

import unittest
from test.tests_fixture import TestsFixture


class CoinConverterTests(unittest.TestCase):

    def test_should_return_zero_when_coins_is_empty(self):
        self.assertEqual(TestsFixture.COIN_CONVERTER.get_value([]), 0)

    def test_should_return_amount_when_coins_is_not_empty(self):
        total = TestsFixture.MONEY_VALUE_1 + TestsFixture.MONEY_VALUE_2 + TestsFixture.MONEY_VALUE_3
        self.assertEqual(TestsFixture.COIN_CONVERTER.get_value(TestsFixture.VALID_COINS), total)

    def test_should_return_amount_and_invalid_not_taken_into_account_when_coins_is_not_empty(self):
        total = TestsFixture.MONEY_VALUE_1 + TestsFixture.MONEY_VALUE_2 + TestsFixture.MONEY_VALUE_3
        self.assertEqual(TestsFixture.COIN_CONVERTER.get_value(TestsFixture.VALID_COINS + TestsFixture.INVALID_COINS), total)

    def test_should_return_true_when_coin_is_valid(self):
        for valid_coin in TestsFixture.VALID_COINS:
            self.assertTrue(TestsFixture.COIN_CONVERTER.is_valid(valid_coin))

    def test_should_return_false_when_coin_is_not_valid(self):
        for invalid_coin in TestsFixture.INVALID_COINS:
            self.assertFalse(TestsFixture.COIN_CONVERTER.is_valid(invalid_coin))

    def test_should_return_money_from_coin(self):
        self.assertEqual(TestsFixture.COIN_CONVERTER.to_money(TestsFixture.COIN_2), TestsFixture.MONEY_2)

        if __name__ == '__main__':
            unittest.main()
