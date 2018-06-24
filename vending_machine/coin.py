#!/usr/bin/env python
from measure import Measure


class Coin(object):

    def __init__(self, measure: Measure):
        self.__measure = measure

    def get_measure(self):
        return self.__measure

    def __eq__(self, other):
        return isinstance(other, Coin) and self.__measure.__eq__(other.__measure)

    def __hash__(self):
        return self.__measure.__hash__()
