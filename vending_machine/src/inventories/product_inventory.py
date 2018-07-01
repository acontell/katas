#!/usr/bin/env python


class ProductInventory(object):

    def __init__(self, products):
        self.__products = products

    def get_product(self, name):
        return next(x for x in self.__products if x.get_name() == name)

    def has_stock(self, name):
        return self.get_product(name).get_stock() > 0

    def get_products(self):
        return self.__products
