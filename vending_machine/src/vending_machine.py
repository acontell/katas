#!/usr/bin/env python
from src.displays.display import Display
from src.managers.money_manager import MoneyManager
from src.model.coin import Coin
from src.managers.product_manager import ProductManager
from src.managers.return_manager import ReturnManager


class VendingMachine(object):
    def __init__(self,
                 display: Display,
                 money: MoneyManager,
                 product: ProductManager,
                 return_: ReturnManager):
        self.__display = display
        self.__money = money
        self.__product = product
        self.__return = return_

    def insert_coin(self, coin: Coin):
        self.__money.insert_coin(coin) if self.__money.is_valid(coin) else self.__return.add(coin)
        return self.display()

    def display(self):
        return self.__display_money() if self.__has_enough_change() else self.__display.exact_change()

    def __display_money(self):
        return self.__display.money(self.__money.get_money()) if self.__has_money() else self.__display.no_money()

    def __has_money(self):
        return self.__money.get_money() > 0

    def __has_enough_change(self):
        return all(list(map(lambda price: self.__money.has_enough_change(price), self.__product.get_product_prices())))

    def check_return(self):
        return self.__return.check_return()

    def push_product_button(self, b_name):
        has_enough_stock = self.__product.has_stock(b_name)
        return self.__dispense_if_enough_money(b_name) if has_enough_stock else self.__display.sold_out()

    def __dispense_if_enough_money(self, b_name):
        product_price = self.__product.get_product_price(b_name)
        has_enough_money = self.__money.has_enough_money(product_price)
        return self.__dispense(b_name, product_price) if has_enough_money else self.__display.price()

    def __dispense(self, b_name, product_price):
        self.__money.spend(product_price)
        self.__move_coins_to_return()
        self.__product.dispense(b_name)
        return self.__display.thanks()

    def __move_coins_to_return(self):
        self.__return.add_coins(self.__money.get_coins())
        self.__money.empty_coins()

    def check_dispenser(self):
        return self.__product.get_dispenser()

    def push_return_button(self):
        self.__move_coins_to_return()
        return self.display()
