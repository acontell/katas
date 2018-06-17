#!/usr/bin/env python
from measure import Measure


class Coin(object):

    def __init__(self, measure: Measure):
        self.__measure = measure

    def get_measure(self):
        return self.__measure
