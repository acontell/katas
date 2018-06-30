#!/usr/bin/env python

import unittest
from test.tests_fixture import TestsFixture
from src.managers.dispenser_manager import DispenserManager


class DispenserManagerTests(unittest.TestCase):

    def test_should_return_products(self):
        self.assertEqual(TestsFixture.DISPENSER_MANAGER.get_products(), TestsFixture.PRODUCTS)

    def test_should_return_empty_dispenser_when_no_product_sold(self):
        self.assertEqual(TestsFixture.DISPENSER_MANAGER.get_dispenser(), [])

    def test_should_return_dispenser_when_product_sold(self):
        dispenser_manager = DispenserManager(TestsFixture.PRODUCT_MANAGER)
        dispenser_manager.dispense(TestsFixture.PRODUCT_NAME_1)
        dispenser_manager.dispense(TestsFixture.PRODUCT_NAME_3)
        self.assertEqual(dispenser_manager.get_dispenser(), [TestsFixture.PRODUCT_1, TestsFixture.PRODUCT_3])

    def test_should_return_price_when_asked_by_product_name(self):
        price = TestsFixture.DISPENSER_MANAGER.get_product_price(TestsFixture.PRODUCT_NAME_1)
        self.assertEqual(price, TestsFixture.PRODUCT_PRICE_1)

    def test_should_return_true_when_enough_stock(self):
        self.assertTrue(TestsFixture.DISPENSER_MANAGER.has_stock(TestsFixture.PRODUCT_NAME_3))

    def test_should_return_false_when_not_enough_stock(self):
        self.assertFalse(TestsFixture.DISPENSER_MANAGER.has_stock(TestsFixture.PRODUCT_NAME_2))

        if __name__ == '__main__':
            unittest.main()
