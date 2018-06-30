#!/usr/bin/env python


class Product(object):

    def __init__(self, name, price, stock):
        self.__name = name
        self.__price = price
        self.__stock = stock

    def get_name(self):
        return self.__name

    def get_price(self):
        return self.__price

    def get_stock(self):
        return self.__stock

    def __eq__(self, other):
        return isinstance(other, Product) and self.__name == other.__name and self.__price == other.__price

    def __hash__(self):
        return hash(self.__name) ^ hash(self.__price)
