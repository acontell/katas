#!/usr/bin/env python

import unittest
from tests_fixture import TestsFixture


class ProductManagerTests(unittest.TestCase):

    def test_should_return_products(self):
        self.assertEqual(TestsFixture.PRODUCT_MANAGER.get_products(), TestsFixture.PRODUCTS)

    def test_should_return_price_when_asked_by_product_name(self):
        price = TestsFixture.PRODUCT_MANAGER.get_product_price_by_name(TestsFixture.PRODUCT_NAME_1)
        self.assertEqual(price, TestsFixture.PRODUCT_PRICE_1)

        if __name__ == '__main__':
            unittest.main()
