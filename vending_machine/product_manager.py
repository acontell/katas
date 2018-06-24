#!/usr/bin/env python


class ProductManager(object):

    def __init__(self, products):
        self.__products = products
        self.__dispenser = []

    def get_products(self):
        return self.__products

    def get_dispenser(self):
        return self.__dispenser

    def get_product_price_by_name(self, name):
        return self.__get_product_by_name(name).get_price()

    def __get_product_by_name(self, name):
        return next(x for x in self.__products if x.get_name() == name)

    def dispense(self, name):
        self.__dispenser.append(self.__get_product_by_name(name))
