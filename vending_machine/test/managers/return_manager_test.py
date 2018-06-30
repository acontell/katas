#!/usr/bin/env python

import unittest
from src.managers.return_manager import ReturnManager
from test.tests_fixture import TestsFixture


class ReturnManagerTests(unittest.TestCase):

    def setUp(self):
        self.return_manager = ReturnManager()

    def test_should_return_list_with_coins_to_return(self):
        self.return_manager.add(TestsFixture.INVALID_COIN)
        self.assertEqual(self.return_manager.check_return(), [TestsFixture.INVALID_COIN])

    def test_should_return_list_with_coins_to_return_when_list_of_coins_added(self):
        self.return_manager.add_coins([TestsFixture.INVALID_COIN])
        self.assertEqual(self.return_manager.check_return(), [TestsFixture.INVALID_COIN])

        if __name__ == '__main__':
            unittest.main()
