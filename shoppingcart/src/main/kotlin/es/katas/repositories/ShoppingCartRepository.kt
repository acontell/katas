package es.katas.repositories

import es.katas.domain.Item
import java.time.LocalDateTime

class ShoppingCartRepository {
    lateinit var localDateTime: LocalDateTime
    val items: MutableMap<Int, Item> = mutableMapOf()

    fun isEmpty() = items.isEmpty()
    fun addItem(item: Item) = items.merge(item.id, item, { x, y -> Item(x.id, x.quantity + y.quantity) })
    fun initShoppingCart(now: LocalDateTime) {
        localDateTime = now
        items.clear()
    }

}
