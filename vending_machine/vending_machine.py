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
        return self.check_display()

    def check_display(self):
        return self.__display.display_amount(self.__amount_manager.get_amount())

    def check_return(self):
        return self.__display.display_return(self.__amount_manager.get_return())

    def push_button(self, product_name):
        return self.__display.display_thanks() if self.__is_buying_ok(product_name) else self.__display.display_price()

    def __is_buying_ok(self, product_name):
        return self.__amount_manager.spend(self.__product_manager.get_product_price_by_name(product_name))
