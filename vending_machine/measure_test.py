#!/usr/bin/env python

import unittest
from measure import Measure


class MeasureTests(unittest.TestCase):
    __MEASURE = Measure(1, 1)

    def test_should_return_equals_when_same_measure(self):
        self.assertTrue(self.__MEASURE.__eq__(Measure(1, 1)))

    def test_should_return_not_equal_when_different_weight(self):
        self.assertFalse(self.__MEASURE.__eq__(Measure(2, 1)))

    def test_should_return_not_equal_when_different_size(self):
        self.assertFalse(self.__MEASURE.__eq__(Measure(1, 2)))

    def test_should_return_not_equal_when_different_type_of_object(self):
        self.assertFalse(self.__MEASURE.__eq__(1))

    def test_should_return_same_hash_for_same_object(self):
        self.assertEqual(self.__MEASURE.__hash__(), Measure(1, 1).__hash__())

        if __name__ == '__main__':
            unittest.main()
