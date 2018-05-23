package es.katas

import es.katas.builders.TicketBuilder
import es.katas.domain.Item
import es.katas.repositories.ShoppingCartRepository
import es.katas.resources.Clock
import es.katas.printers.ShoppingCartPrinter

class ShoppingCartService(private val shoppingCartRepository: ShoppingCartRepository,
                          private val clock: Clock,
                          private val shoppingCartPrinter: ShoppingCartPrinter,
                          private val ticketBuilder: TicketBuilder) {
    fun addItem(id: Int, quantity: Int) {
        if (shoppingCartRepository.isEmpty()) {
            shoppingCartRepository.initShoppingCart(clock.now())
        }
        shoppingCartRepository.addItem(Item(id, quantity))
    }

    fun printTicket() = shoppingCartPrinter.print(buildTicket())

    private fun buildTicket() = ticketBuilder
            .withDate(shoppingCartRepository.localDateTime)
            .withItems(shoppingCartRepository.items.toList())
            .build()
}
