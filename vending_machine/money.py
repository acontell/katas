#!/usr/bin/env python


class Money(object):

    def __init__(self, value):
        self.__value = value

    def get_value(self):
        return self.__value
