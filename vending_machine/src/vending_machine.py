#!/usr/bin/env python
from src.displays.display import Display
from src.managers.amount_manager import AmountManager
from src.model.coin import Coin
from src.managers.dispenser_manager import DispenserManager
from src.managers.return_manager import ReturnManager


class VendingMachine(object):
    def __init__(self,
                 display: Display,
                 amount_manager: AmountManager,
                 dispenser_manager: DispenserManager,
                 return_manager: ReturnManager):
        self.__display = display
        self.__amount = amount_manager
        self.__dispenser = dispenser_manager
        self.__return = return_manager

    def insert_coin(self, coin: Coin):
        self.__amount.insert_coin(coin) if self.__amount.is_valid(coin) else self.__return.add(coin)
        return self.display()

    def display(self):
        return self.__display.amount(self.__amount.get_amount())

    def check_return(self):
        return self.__return.check_return()

    def push_product_button(self, b_name):
        has_enough_stock = self.__dispenser.has_stock(b_name)
        return self.__dispense_if_enough_amount(b_name) if has_enough_stock else self.__display.sold_out()

    def __dispense_if_enough_amount(self, b_name):
        product_price = self.__dispenser.get_product_price(b_name)
        has_enough_amount = self.__amount.has_enough_amount(product_price)
        return self.__dispense(b_name, product_price) if has_enough_amount else self.__display.price()

    def __dispense(self, b_name, product_price):
        self.__amount.spend(product_price)
        self.__coins_to_return()
        self.__dispenser.dispense(b_name)
        return self.__display.thanks()

    def __coins_to_return(self):
        self.__return.add_coins(self.__amount.get_coins())
        self.__amount.empty_coins()

    def check_dispenser(self):
        return self.__dispenser.get_dispenser()

    def push_return_button(self):
        self.__coins_to_return()
        return self.display()
