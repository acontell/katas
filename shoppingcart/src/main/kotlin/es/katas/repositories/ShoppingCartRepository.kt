package es.katas.repositories

import es.katas.domain.Item
import java.time.LocalDateTime

class ShoppingCartRepository {
    lateinit var localDateTime: LocalDateTime
    val items: MutableList<Item> = mutableListOf()

    fun isEmpty() = items.isEmpty()
    fun addItem(item: Item) = items.add(item)
    fun initShoppingCart(now: LocalDateTime) {
        localDateTime = now
        items.clear()
    }

}
