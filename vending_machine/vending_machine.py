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

    def push_product_button(self, product_name):
        return self.__display.thanks() if self.__is_buying_ok(product_name) else self.__display.price()

    def __is_buying_ok(self, product_name):
        return self.__amount_manager.try_to_spend(self.__product_manager.get_product_price_by_name(product_name))

    def push_return_button(self):
        self.__amount_manager.flush_coins()
        return self.display()
