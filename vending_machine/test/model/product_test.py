#!/usr/bin/env python

import unittest
from src.model.product import Product
from test.tests_fixture import TestsFixture


class ProductTests(unittest.TestCase):

    def test_should_return_equals_when_same_name_and_price(self):
        self.assertTrue(TestsFixture.PRODUCT_1.__eq__(Product("Cola", 1, 0)))

    def test_should_return_not_equal_when_different_price(self):
        self.assertFalse(TestsFixture.PRODUCT_1.__eq__(Product("Cola", 2, 0)))

    def test_should_return_not_equal_when_different_name(self):
        self.assertFalse(TestsFixture.PRODUCT_1.__eq__(Product("Candy", 1, 0)))

    def test_should_return_not_equal_when_different_type_of_object(self):
        self.assertFalse(TestsFixture.PRODUCT_1.__eq__(1))

    def test_should_return_same_hash_for_same_object(self):
        self.assertEqual(TestsFixture.PRODUCT_1.__hash__(), Product("Cola", 1, 0).__hash__())

        if __name__ == '__main__':
            unittest.main()
