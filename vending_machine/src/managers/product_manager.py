#!/usr/bin/env python
from src.inventories.product_inventory import ProductInventory


class ProductManager(object):

    def __init__(self, product_inventory: ProductInventory):
        self.__product_inventory = product_inventory
        self.__dispenser = []

    def get_dispenser(self):
        return self.__dispenser

    def get_product_price(self, name):
        return self.__product_inventory.get_product(name).get_price()

    def dispense(self, name):
        self.__dispenser.append(self.__product_inventory.get_product(name))

    def has_stock(self, name):
        return self.__product_inventory.has_stock(name)

    def get_product_prices(self):
        return list(map(lambda product: product.get_price(), self.__product_inventory.get_products()))
