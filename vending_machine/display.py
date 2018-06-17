#!/usr/bin/env python


class Display(object):

    def __init__(self, no_coins_msg, current_amount_msg_tpl, return_coins_msg_tpl):
        self.__no_coins_msg = no_coins_msg
        self.__current_amount_msg_tpl = current_amount_msg_tpl
        self.__return_coins_msg_tpl = return_coins_msg_tpl

    def display_amount(self, amount):
        return self.__no_coins_msg if amount == 0 else self.__get_amount_msg(amount)

    def __get_amount_msg(self, amount):
        return self.__current_amount_msg_tpl % amount

    def display_return(self, coins_to_return_map):
        return "\n".join(self.__get_return_msg_list(coins_to_return_map))

    def __get_return_msg_list(self, coins_to_return_map):
        return [self.__get_coin_msg(k, v) for k, v in coins_to_return_map.items()]

    def __get_coin_msg(self, coin_name, amount):
        return self.__return_coins_msg_tpl.format(coin_name, amount)
