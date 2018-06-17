#!/usr/bin/env python

import unittest
from display import Display


class DisplayTests(unittest.TestCase):
    __NO_COINS_MSG = 'INSERT_COIN'
    __AMOUNT_TPL = 'Current amount: %.2f$'
    __RETURN_TPL = "{}:\t{}"

    def setUp(self):
        self.display = Display(self.__NO_COINS_MSG, self.__AMOUNT_TPL, self.__RETURN_TPL)

    def test_should_display_amount(self):
        self.assertEqual(self.display.display_amount(5), self.__AMOUNT_TPL % 5)

    def test_should_display_insert_coin_msg_when_amount_is_zero(self):
        self.assertEqual(self.display.display_amount(0), self.__NO_COINS_MSG)

    def test_should_display_return_coins_msg(self):
        self.assertEqual(self.display.display_return({"DIMES": 2}), "DIMES:\t2")

    def test_should_display_empty_string_when_return_coins_map_is_empty(self):
        self.assertEqual(self.display.display_return({}), "")

        if __name__ == '__main__':
            unittest.main()
