#!/usr/bin/env python

import unittest
from tests_fixture import TestsFixture


class DisplayTests(unittest.TestCase):

    def test_should_display_amount(self):
        self.assertEqual(TestsFixture.DISPLAY.amount(5), TestsFixture.AMOUNT_TPL % 5)

    def test_should_display_insert_coin_msg_when_amount_is_zero(self):
        self.assertEqual(TestsFixture.DISPLAY.amount(0), TestsFixture.NO_COINS_MSG)

    def test_should_display_price_msg(self):
        self.assertEqual(TestsFixture.DISPLAY.price(), TestsFixture.PRICE_MSG)

    def test_should_display_thanks_msg(self):
        self.assertEqual(TestsFixture.DISPLAY.thanks(), TestsFixture.THANKS_MSG)

    def test_should_display_sold_out_msg(self):
        self.assertEqual(TestsFixture.DISPLAY.sold_out(), TestsFixture.SOLD_OUT_MSG)

        if __name__ == '__main__':
            unittest.main()
