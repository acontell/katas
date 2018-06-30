#!/usr/bin/env python

import unittest
from test.tests_fixture import TestsFixture


class ProductRepositoryTests(unittest.TestCase):

    def test_should_return_products(self):
        self.assertEqual(TestsFixture.PRODUCT_MANAGER.get_products(), TestsFixture.PRODUCTS)

    def test_should_return_product_when_asked_by_product_name(self):
        product = TestsFixture.PRODUCT_MANAGER.get_product(TestsFixture.PRODUCT_NAME_1)
        self.assertEqual(product, TestsFixture.PRODUCT_1)

    def test_should_return_true_when_enough_stock(self):
        self.assertTrue(TestsFixture.PRODUCT_MANAGER.has_stock(TestsFixture.PRODUCT_NAME_3))

    def test_should_return_false_when_not_enough_stock(self):
        self.assertFalse(TestsFixture.PRODUCT_MANAGER.has_stock(TestsFixture.PRODUCT_NAME_2))

        if __name__ == '__main__':
            unittest.main()
