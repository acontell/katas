#!/usr/bin/env python
from product_repository import ProductRepository


class DispenserManager(object):

    def __init__(self, product_manager: ProductRepository):
        self.__product_manager = product_manager
        self.__dispenser = []

    def get_products(self):
        return self.__product_manager.get_products()

    def get_dispenser(self):
        return self.__dispenser

    def get_product_price(self, name):
        return self.__product_manager.get_product(name).get_price()

    def dispense(self, name):
        self.__dispenser.append(self.__product_manager.get_product(name))

    def has_stock(self, name):
        return self.__product_manager.has_stock(name)
