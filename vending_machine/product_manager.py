#!/usr/bin/env python


class ProductManager(object):

    def __init__(self, products):
        self.__products = products

    def get_products(self):
        return self.__products

    def get_product_price_by_name(self, name):
        return next(x for x in self.__products if x.get_name() == name).get_price()
