#!/usr/bin/env python


class Display(object):

    def __init__(self, no_coins_msg, money_msg_tpl, price_msg, thanks_msg, sold_out_msg, exact_change_msg):
        self.__no_coins_msg = no_coins_msg
        self.__money_msg_tpl = money_msg_tpl
        self.__price_msg = price_msg
        self.__thanks_msg = thanks_msg
        self.__sold_out_msg = sold_out_msg
        self.__exact_change_msg = exact_change_msg

    def money(self, amount):
        return self.__money_msg_tpl % amount

    def no_money(self):
        return self.__no_coins_msg

    def thanks(self):
        return self.__thanks_msg

    def price(self):
        return self.__price_msg

    def sold_out(self):
        return self.__sold_out_msg

    def exact_change(self):
        return self.__exact_change_msg
