#!/usr/bin/env python

import unittest
from coin import Coin
from measure import Measure
from tests_fixture import TestsFixture


class CoinTests(unittest.TestCase):

    def test_should_return_equals_when_same_measure(self):
        self.assertTrue(TestsFixture.COIN_1.__eq__(Coin(Measure(1, 1))))

    def test_should_return_not_equal_when_different_type_of_object(self):
        self.assertFalse(TestsFixture.COIN_1.__eq__(1))

    def test_should_return_same_hash_for_same_object(self):
        self.assertEqual(TestsFixture.COIN_1.__hash__(), Coin(Measure(1, 1)).__hash__())

        if __name__ == '__main__':
            unittest.main()
