#!/usr/bin/env python

import unittest
from tests_fixture import TestsFixture


class DisplayTests(unittest.TestCase):

    def setUp(self):
        self.display = TestsFixture.DISPLAY

    def test_should_display_amount(self):
        self.assertEqual(self.display.display_amount(5), TestsFixture.AMOUNT_TPL % 5)

    def test_should_display_insert_coin_msg_when_amount_is_zero(self):
        self.assertEqual(self.display.display_amount(0), TestsFixture.NO_COINS_MSG)

    def test_should_display_return_coins_msg(self):
        self.assertEqual(self.display.display_return({"PENNY": 2}), "PENNY:\t2")

    def test_should_display_empty_string_when_return_coins_map_is_empty(self):
        self.assertEqual(self.display.display_return({}), "")

    def test_should_display_price_msg(self):
        self.assertEqual(self.display.display_price(), TestsFixture.PRICE_MSG)

    def test_should_display_thanks_msg(self):
        self.assertEqual(self.display.display_thanks(), TestsFixture.THANKS_MSG)

        if __name__ == '__main__':
            unittest.main()
