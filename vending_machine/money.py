#!/usr/bin/env python
from measure import Measure


class Money(object):

    def __init__(self, name, value, measure: Measure):
        self.__name = name
        self.__value = value
        self.__measure = measure

    def get_name(self):
        return self.__name

    def get_value(self):
        return self.__value

    def is_valid(self):
        return self.get_value() > 0

    def get_measure(self):
        return self.__measure
