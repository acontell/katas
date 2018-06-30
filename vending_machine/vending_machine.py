#!/usr/bin/env python
from display import Display
from amount_manager import AmountManager
from coin import Coin
from dispenser_manager import DispenserManager


class VendingMachine(object):
    def __init__(self, display: Display, amount_manager: AmountManager, dispenser_manager: DispenserManager):
        self.__display = display
        self.__amount_manager = amount_manager
        self.__dispenser_manager = dispenser_manager

    def insert_coin(self, coin: Coin):
        self.__amount_manager.insert_coin(coin)
        return self.display()

    def display(self):
        return self.__display.amount(self.__amount_manager.get_amount())

    def check_return(self):
        return self.__amount_manager.get_return()

    def push_product_button(self, b_name):
        has_enough_stock = self.__dispenser_manager.has_stock(b_name)
        product_price = self.__dispenser_manager.get_product_price(b_name)
        has_enough_amount = self.__amount_manager.check_enough_amount(product_price)
        if has_enough_stock:
            if has_enough_amount:
                self.__amount_manager.spend_if_possible(product_price)
                self.__dispenser_manager.dispense_if_possible(b_name)
                return self.__display.thanks()
            return self.__display.price()
        return self.__display.sold_out()

    def __dispense_product(self, name):
        self.__dispenser_manager.dispense_if_possible(name)

    def check_dispenser(self):
        return self.__dispenser_manager.get_dispenser()

    def push_return_button(self):
        self.__amount_manager.flush_coins()
        return self.display()
