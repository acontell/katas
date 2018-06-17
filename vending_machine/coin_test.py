#!/usr/bin/env python

import unittest
from coin import Coin


class CoinTests(unittest.TestCase):

    def test_should_return_equals_when_same_coin(self):
        self.assertTrue(Coin(1, 1).__eq__(Coin(1, 1)))

    def test_should_return_not_equal_when_different_weight(self):
        self.assertFalse(Coin(1, 1).__eq__(Coin(2, 1)))

    def test_should_return_not_equal_when_different_size(self):
        self.assertFalse(Coin(1, 1).__eq__(Coin(1, 2)))

    def test_should_return_not_equal_when_different_type_of_object(self):
        self.assertFalse(Coin(1, 1).__eq__(1))

    def test_should_return_same_hash_for_same_object(self):
        self.assertEqual(Coin(1, 1).__hash__(), Coin(1, 1).__hash__())

        if __name__ == '__main__':
            unittest.main()
