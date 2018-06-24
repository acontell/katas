#!/usr/bin/env python


class Display(object):

    def __init__(self, no_coins_msg, current_amount_msg_tpl, price_msg, thanks_msg):
        self.__no_coins_msg = no_coins_msg
        self.__current_amount_msg_tpl = current_amount_msg_tpl
        self.__price_msg = price_msg
        self.__thanks_msg = thanks_msg

    def amount(self, amount):
        return self.__no_coins_msg if amount == 0 else self.__get_amount_msg(amount)

    def __get_amount_msg(self, amount):
        return self.__current_amount_msg_tpl % amount

    def thanks(self):
        return self.__thanks_msg

    def price(self):
        return self.__price_msg
