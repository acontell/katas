#!/usr/bin/env python

import unittest
from tests_fixture import TestsFixture
from product_manager import ProductManager


class ProductManagerTests(unittest.TestCase):

    def test_should_return_products(self):
        self.assertEqual(TestsFixture.PRODUCT_MANAGER.get_products(), TestsFixture.PRODUCTS)

    def test_should_return_empty_dispenser_when_no_product_sold(self):
        self.assertEqual(TestsFixture.PRODUCT_MANAGER.get_dispenser(), [])

    def test_should_return_dispenser_when_product_sold(self):
        product_manager = ProductManager(TestsFixture.PRODUCTS)
        product_manager.dispense(TestsFixture.PRODUCT_NAME_2)
        product_manager.dispense(TestsFixture.PRODUCT_NAME_3)
        self.assertEqual(product_manager.get_dispenser(), [TestsFixture.PRODUCT_2, TestsFixture.PRODUCT_3])

    def test_should_return_price_when_asked_by_product_name(self):
        price = TestsFixture.PRODUCT_MANAGER.get_product_price_by_name(TestsFixture.PRODUCT_NAME_1)
        self.assertEqual(price, TestsFixture.PRODUCT_PRICE_1)

        if __name__ == '__main__':
            unittest.main()
