#!/usr/bin/env python

from src.model.coin import Coin
from src.displays.display import Display
from src.inventories.coin_inventory import CoinInventory
from src.model.money import Money
from src.model.measure import Measure
from src.model.product import Product
from src.inventories.product_inventory import ProductInventory
from src.managers.product_manager import ProductManager
from src.converters.coin_converter import CoinConverter


class TestsFixture(object):
    MEASURE_1 = Measure(1, 1)
    MEASURE_2 = Measure(1, 2)
    MEASURE_3 = Measure(2, 1)
    INVALID_COIN_MEASURE = Measure(1, 4)

    COIN_1 = Coin(MEASURE_1)
    COIN_2 = Coin(MEASURE_2)
    COIN_3 = Coin(MEASURE_3)
    INVALID_COIN = Coin(INVALID_COIN_MEASURE)

    MONEY_VALUE_1 = 0.05
    MONEY_VALUE_2 = 0.1
    MONEY_VALUE_3 = 0.25

    MONEY_NAME_1 = "NICKEL"
    MONEY_NAME_2 = "DIME"
    MONEY_NAME_3 = "QUARTER"
    INVALID_MONEY_NAME = "PENNY"

    MONEY_1 = Money(MONEY_NAME_1, MONEY_VALUE_1, MEASURE_1)
    MONEY_2 = Money(MONEY_NAME_2, MONEY_VALUE_2, MEASURE_2)
    MONEY_3 = Money(MONEY_NAME_3, MONEY_VALUE_3, MEASURE_3)
    INVALID_MONEY = Money(INVALID_MONEY_NAME, 0, INVALID_COIN_MEASURE)

    MONEY_MAP = {MEASURE_1: MONEY_1, MEASURE_2: MONEY_2, MEASURE_3: MONEY_3, INVALID_COIN_MEASURE: INVALID_MONEY}
    VALID_COINS = [COIN_1, COIN_2, COIN_3]
    INVALID_COINS = [INVALID_COIN]
    CURRENT_CHANGE = [COIN_3, COIN_3, COIN_3, COIN_3, COIN_3, COIN_3]

    NO_COINS_MSG = 'INSERT_COIN'
    AMOUNT_TPL = 'Current amount: %.2f$'
    THANKS_MSG = 'THANK YOU'
    PRICE_MSG = 'PRICE'
    SOLD_OUT_MSG = 'SOLD OUT'
    EXACT_CHANGE_MSG = 'EXACT CHANGE ONLY'

    DISPLAY = Display(NO_COINS_MSG, AMOUNT_TPL, PRICE_MSG, THANKS_MSG, SOLD_OUT_MSG, EXACT_CHANGE_MSG)
    COIN_CONVERTER = CoinConverter(MONEY_MAP)
    COIN_INVENTORY = CoinInventory(CURRENT_CHANGE, COIN_CONVERTER)

    PRODUCT_NAME_1 = 'Cola'
    PRODUCT_NAME_2 = 'Chips'
    PRODUCT_NAME_3 = 'Candy'
    PRODUCT_PRICE_1 = 1
    PRODUCT_PRICE_2 = 0.5
    PRODUCT_PRICE_3 = 0.65
    PRODUCT_STOCK_1 = 3
    PRODUCT_STOCK_2 = 0
    PRODUCT_STOCK_3 = 1
    PRODUCT_1 = Product(PRODUCT_NAME_1, PRODUCT_PRICE_1, PRODUCT_STOCK_1)
    PRODUCT_2 = Product(PRODUCT_NAME_2, PRODUCT_PRICE_2, PRODUCT_STOCK_2)
    PRODUCT_3 = Product(PRODUCT_NAME_3, PRODUCT_PRICE_3, PRODUCT_STOCK_3)
    PRODUCTS = [PRODUCT_1, PRODUCT_2, PRODUCT_3]
    PRODUCT_INVENTORY = ProductInventory(PRODUCTS)
    PRODUCT_MANAGER = ProductManager(PRODUCT_INVENTORY)
