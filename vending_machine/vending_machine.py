#!/usr/bin/env python
from display import Display
from amount_manager import AmountManager


class VendingMachine(object):
    def __init__(self, display: Display, amount_manager: AmountManager):
        self.display = display
        self.amount_manager = amount_manager

    def insert_coin(self, coin_type):
        return self.amount_manager.insert_coin(coin_type)

    def get_amount(self):
        return self.amount_manager.get_amount()

    def display_message(self):
        return self.display.display_amount(self.amount_manager.get_amount())

    def display_return(self):
        return self.display.display_return(self.amount_manager.get_return_map())
