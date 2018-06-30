#!/usr/bin/env python

import unittest
from test.tests_fixture import TestsFixture


class CoinTests(unittest.TestCase):

    def test_should_return_true_when_value_is_more_than_zero(self):
        self.assertTrue(TestsFixture.MONEY_1.is_valid())

    def test_should_return_false_when_value_is_less_or_equal_to_zero(self):
        self.assertFalse(TestsFixture.INVALID_MONEY.is_valid())

        if __name__ == '__main__':
            unittest.main()
