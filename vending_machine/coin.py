#!/usr/bin/env python


class Coin(object):

    def __init__(self, name, weight, size):
        self.__name = name
        self.__weight = weight
        self.__size = size

    def get_weight(self):
        return self.__weight

    def get_size(self):
        return self.__size

    def __eq__(self, other):
        return isinstance(other, Coin) and self.__weight == other.__weight and self.__size == other.__size

    def __hash__(self):
        return hash(self.__weight) ^ hash(self.__size)

    def __str__(self):
        return self.__name
