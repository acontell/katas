#!/usr/bin/env python
from src.inventories.product_inventory import ProductInventory


class ProductManager(object):

    def __init__(self, product_inventory: ProductInventory):
        self.__product_manager = product_inventory
        self.__dispenser = []

    def get_dispenser(self):
        return self.__dispenser

    def get_product_price(self, name):
        return self.__product_manager.get_product(name).get_price()

    def dispense(self, name):
        self.__dispenser.append(self.__product_manager.get_product(name))

    def has_stock(self, name):
        return self.__product_manager.has_stock(name)
