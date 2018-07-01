#!/usr/bin/env python

import unittest
from test.tests_fixture import TestsFixture
from src.managers.product_manager import ProductManager


class ProductManagerTests(unittest.TestCase):

    def test_should_return_empty_dispenser_when_no_product_sold(self):
        self.assertEqual(TestsFixture.PRODUCT_MANAGER.get_dispenser(), [])

    def test_should_return_dispenser_when_product_sold(self):
        dispenser_manager = ProductManager(TestsFixture.PRODUCT_INVENTORY)
        dispenser_manager.dispense(TestsFixture.PRODUCT_NAME_1)
        dispenser_manager.dispense(TestsFixture.PRODUCT_NAME_3)
        self.assertEqual(dispenser_manager.get_dispenser(), [TestsFixture.PRODUCT_1, TestsFixture.PRODUCT_3])

    def test_should_return_price_when_asked_by_product_name(self):
        price = TestsFixture.PRODUCT_MANAGER.get_product_price(TestsFixture.PRODUCT_NAME_1)
        self.assertEqual(price, TestsFixture.PRODUCT_PRICE_1)

    def test_should_return_true_when_enough_stock(self):
        self.assertTrue(TestsFixture.PRODUCT_MANAGER.has_stock(TestsFixture.PRODUCT_NAME_3))

    def test_should_return_false_when_not_enough_stock(self):
        self.assertFalse(TestsFixture.PRODUCT_MANAGER.has_stock(TestsFixture.PRODUCT_NAME_2))

    def test_should_return_array_of_prices(self):
        prices = [TestsFixture.PRODUCT_PRICE_1, TestsFixture.PRODUCT_PRICE_2, TestsFixture.PRODUCT_PRICE_3]
        self.assertEqual(TestsFixture.PRODUCT_MANAGER.get_product_prices(), prices)

        if __name__ == '__main__':
            unittest.main()
