#!/usr/bin/env python
from display import Display
from amount_manager import AmountManager
from coin import Coin
from product_manager import ProductManager


class VendingMachine(object):
    def __init__(self, display: Display, amount_manager: AmountManager, product_manager: ProductManager):
        self.__display = display
        self.__amount_manager = amount_manager
        self.__product_manager = product_manager

    def insert_coin(self, coin: Coin):
        self.__amount_manager.insert_coin(coin)
        return self.display()

    def display(self):
        return self.__display.amount(self.__amount_manager.get_amount())

    def check_return(self):
        return self.__amount_manager.get_return()

    def push_product_button(self, b_name):
        purchase_ok = self.__amount_manager.spend_if_possible(self.__product_manager.get_product_price_by_name(b_name))
        return self.__dispense_product(b_name) if purchase_ok else self.__display.price()

    def __dispense_product(self, name):
        self.__product_manager.dispense(name)
        return self.__display.thanks()

    def check_dispenser(self):
        return self.__product_manager.get_dispenser()

    def push_return_button(self):
        self.__amount_manager.flush_coins()
        return self.display()
