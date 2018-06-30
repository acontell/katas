#!/usr/bin/env python

import unittest
from src.vending_machine import VendingMachine
from src.managers.amount_manager import AmountManager
from src.managers.dispenser_manager import DispenserManager
from src.managers.return_manager import ReturnManager
from test.tests_fixture import TestsFixture


class VendingMachineTests(unittest.TestCase):

    def setUp(self):
        self.given_vending_machine()
        self.expected_money = 0

    def given_vending_machine(self):
        self.vending_machine = VendingMachine(TestsFixture.DISPLAY,
                                              AmountManager(TestsFixture.COIN_MANAGER),
                                              DispenserManager(TestsFixture.PRODUCT_MANAGER),
                                              ReturnManager())

    def test_should_display_correct_amount_for_one_valid_coin_inserted(self):
        for valid_coin in TestsFixture.VALID_COINS:
            self.given_vending_machine()
            value = TestsFixture.MONEY_MAP[valid_coin.get_measure()].get_value()
            self.assertEqual(self.vending_machine.insert_coin(valid_coin), TestsFixture.AMOUNT_TPL % value)

    def test_should_display_no_coin_msg_when_invalid_coin_inserted(self):
        for invalid_coin in TestsFixture.INVALID_COINS:
            self.given_vending_machine()
            self.assertEqual(self.vending_machine.insert_coin(invalid_coin), TestsFixture.NO_COINS_MSG)

    def test_should_display_added_amount_of_inserted_coins(self):
        self.given_vending_machine_with_coins()
        self.expected_money += TestsFixture.MONEY_VALUE_1
        self.assertEqual(self.vending_machine.insert_coin(TestsFixture.COIN_1), self.get_display_of_expected_money())

    def given_vending_machine_with_coins(self):
        self.given_vending_machine()
        self.vending_machine.insert_coin(TestsFixture.COIN_1)
        self.vending_machine.insert_coin(TestsFixture.COIN_1)
        self.vending_machine.insert_coin(TestsFixture.COIN_2)
        self.vending_machine.insert_coin(TestsFixture.COIN_3)
        self.vending_machine.insert_coin(TestsFixture.COIN_3)
        self.expected_money = 2 * TestsFixture.MONEY_VALUE_1 + TestsFixture.MONEY_VALUE_2 + 2 * TestsFixture.MONEY_VALUE_3

    def test_should_display_amount_when_valid_coins_inserted(self):
        self.vending_machine.insert_coin(TestsFixture.COIN_1)
        self.assertEqual(self.vending_machine.display(), TestsFixture.AMOUNT_TPL % TestsFixture.MONEY_VALUE_1)

    def test_should_display_amount_when_more_than_one_valid_coins_inserted(self):
        self.given_vending_machine_with_coins()
        self.assertEqual(self.vending_machine.display(), self.get_display_of_expected_money())

    def get_display_of_expected_money(self):
        return TestsFixture.AMOUNT_TPL % self.expected_money

    def test_should_display_insert_coin_msg_when_no_coins_inserted(self):
        self.assertEqual(self.vending_machine.display(), TestsFixture.NO_COINS_MSG)

    def test_should_display_insert_coin_msg_when_no_valid_coins_inserted(self):
        self.vending_machine.insert_coin(TestsFixture.INVALID_COIN)
        self.assertEqual(self.vending_machine.display(), TestsFixture.NO_COINS_MSG)

    def test_should_place_rejected_coins_in_coin_return(self):
        self.given_vending_machine_with_coins()
        self.vending_machine.insert_coin(TestsFixture.INVALID_COIN)
        self.vending_machine.insert_coin(TestsFixture.INVALID_COIN)
        self.assertEqual(self.vending_machine.check_return(), [TestsFixture.INVALID_COIN, TestsFixture.INVALID_COIN])

    def test_should_display_thanks_msg_when_button_is_pressed_and_enough_money(self):
        self.given_vending_machine_with_coins()
        self.assertEqual(self.vending_machine.push_product_button(TestsFixture.PRODUCT_NAME_3), TestsFixture.THANKS_MSG)

    def test_should_display_price_msg_when_button_is_pressed_and_not_enough_money(self):
        self.given_vending_machine_with_coins()
        self.assertEqual(self.vending_machine.push_product_button(TestsFixture.PRODUCT_NAME_1), TestsFixture.PRICE_MSG)

    def test_should_display_insert_coin_when_button_is_pressed_and_enough_money_and_display_checked_again(self):
        self.given_vending_machine_with_coins_and_user_pushes_button(TestsFixture.PRODUCT_NAME_3)
        self.assertEqual(self.vending_machine.display(), TestsFixture.NO_COINS_MSG)

    def given_vending_machine_with_coins_and_user_pushes_button(self, product_name):
        self.given_vending_machine_with_coins()
        self.vending_machine.push_product_button(product_name)

    def test_should_display_amount_when_button_is_pressed_and_not_enough_money_and_there_is_amount(self):
        self.given_vending_machine_with_coins_and_user_pushes_button(TestsFixture.PRODUCT_NAME_1)
        self.assertEqual(self.vending_machine.display(), self.get_display_of_expected_money())

    def test_should_display_insert_coin_when_button_is_pressed_and_not_enough_money_and_there_is_nothing(self):
        self.vending_machine.push_product_button(TestsFixture.PRODUCT_NAME_1)
        self.assertEqual(self.vending_machine.display(), TestsFixture.NO_COINS_MSG)

    def test_should_show_empty_dispenser_when_button_is_pressed_and_not_enough_money(self):
        self.given_vending_machine()
        self.vending_machine.push_product_button(TestsFixture.PRODUCT_NAME_1)
        self.assertEqual(self.vending_machine.check_dispenser(), [])

    def test_should_display_return_with_remaining_amount_when_product_is_bought(self):
        self.given_vending_machine_with_coins()
        self.vending_machine.push_product_button(TestsFixture.PRODUCT_NAME_3)
        self.assertEqual(self.vending_machine.check_return(), [TestsFixture.COIN_1])

    def test_should_show_product_in_dispenser_when_product_is_bought(self):
        self.given_vending_machine_with_coins()
        self.vending_machine.push_product_button(TestsFixture.PRODUCT_NAME_3)
        self.assertEqual(self.vending_machine.check_dispenser(), [TestsFixture.PRODUCT_3])

    def test_should_display_insert_coin_when_return_button_is_pressed(self):
        self.vending_machine.insert_coin(TestsFixture.COIN_1)
        self.assertEqual(self.vending_machine.push_return_button(), TestsFixture.NO_COINS_MSG)

    def test_should_return_inserted_money_when_return_button_is_pressed(self):
        self.vending_machine.insert_coin(TestsFixture.COIN_1)
        self.vending_machine.push_return_button()
        self.assertEqual(self.vending_machine.check_return(), [TestsFixture.COIN_1])

    def test_should_say_sold_out_when_stock_of_selected_product_is_zero(self):
        self.given_vending_machine_with_coins()
        actual = self.vending_machine.push_product_button(TestsFixture.PRODUCT_NAME_2)
        self.assertEqual(actual, TestsFixture.SOLD_OUT_MSG)

    def test_should_say_insert_coin_when_stock_and_amount_is_zero_and_display_is_checked_again(self):
        self.given_vending_machine()
        self.vending_machine.push_product_button(TestsFixture.PRODUCT_NAME_2)
        self.assertEqual(self.vending_machine.display(), TestsFixture.NO_COINS_MSG)

    def test_should_say_amount_when_stock_is_zero_and_amount_not_zero_and_display_is_checked_again(self):
        self.vending_machine.insert_coin(TestsFixture.COIN_1)
        self.vending_machine.push_product_button(TestsFixture.PRODUCT_NAME_2)
        self.assertEqual(self.vending_machine.display(), TestsFixture.AMOUNT_TPL % TestsFixture.MONEY_VALUE_1)

        if __name__ == '__main__':
            unittest.main()
