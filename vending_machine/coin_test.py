#!/usr/bin/env python

import unittest
from coin import Coin


class CoinTests(unittest.TestCase):
    __COIN_NAME = "a"
    __COIN = Coin(__COIN_NAME, 1, 1)

    def test_should_return_equals_when_same_coin(self):
        self.assertTrue(self.__COIN.__eq__(Coin(self.__COIN_NAME, 1, 1)))

    def test_should_return_equals_when_same_size_and_weight_but_different_name(self):
        self.assertTrue(self.__COIN.__eq__(Coin("b", 1, 1)))

    def test_should_return_not_equal_when_different_weight(self):
        self.assertFalse(self.__COIN.__eq__(Coin(self.__COIN_NAME, 2, 1)))

    def test_should_return_not_equal_when_different_size(self):
        self.assertFalse(self.__COIN.__eq__(Coin(self.__COIN_NAME, 1, 2)))

    def test_should_return_not_equal_when_different_type_of_object(self):
        self.assertFalse(self.__COIN.__eq__(1))

    def test_should_return_same_hash_for_same_object(self):
        self.assertEqual(self.__COIN.__hash__(), Coin(self.__COIN_NAME, 1, 1).__hash__())

    def test_should_return_same_hash_for_same_weight_size_but_different_name(self):
        self.assertEqual(self.__COIN.__hash__(), Coin("b", 1, 1).__hash__())

    def test_should_return_name_when__str__(self):
        self.assertEqual(self.__COIN.__str__(), self.__COIN_NAME)

        if __name__ == '__main__':
            unittest.main()
