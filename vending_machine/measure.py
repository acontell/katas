#!/usr/bin/env python


class Measure(object):

    def __init__(self, weight, size):
        self.__weight = weight
        self.__size = size

    def __eq__(self, other):
        return isinstance(other, Measure) and self.__weight == other.__weight and self.__size == other.__size

    def __hash__(self):
        return hash(self.__size) ^ hash(self.__weight)
