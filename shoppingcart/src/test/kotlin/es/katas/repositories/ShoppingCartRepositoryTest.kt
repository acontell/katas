package es.katas.repositories

import es.katas.domain.Amount
import es.katas.domain.Item
import org.hamcrest.CoreMatchers.`is`
import org.junit.Test

import org.junit.Assert.*
import java.time.LocalDateTime

class ShoppingCartRepositoryTest {

    private val now = LocalDateTime.of(2018, 1, 10, 14, 0, 0)
    private val item = Item(1, Amount(1))
    private val shoppingCartRepository = ShoppingCartRepository()

    @Test
    fun `isEmpty should return true when there are no items stored`() {
        assertThat(shoppingCartRepository.isEmpty(), `is`(true))
    }

    @Test
    fun `isEmpty should return false when there are items stored`() {
        shoppingCartRepository.addItem(item)
        assertThat(shoppingCartRepository.isEmpty(), `is`(false))
    }

    @Test
    fun `addItem should add item to list of items`() {
        shoppingCartRepository.addItem(item)
        assertThat(shoppingCartRepository.items[0], `is`(item))
    }

    @Test
    fun `initShoppingCart should set given date`() {
        shoppingCartRepository.initShoppingCart(now)
        assertThat(shoppingCartRepository.localDateTime, `is`(now))
    }

    @Test
    fun `initShoppingCart should empty list of items`() {
        shoppingCartRepository.addItem(item)
        shoppingCartRepository.initShoppingCart(now)
        assertThat(shoppingCartRepository.isEmpty(), `is`(true))
    }
}