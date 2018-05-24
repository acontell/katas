package es.katas.builders

import es.katas.calculators.Calculator
import es.katas.domain.Amount
import es.katas.domain.Item
import es.katas.domain.Product
import es.katas.domain.ticket.Ticket
import es.katas.domain.ticket.TicketDiscount
import es.katas.domain.ticket.TicketLine
import es.katas.repositories.ProductRepository
import java.time.LocalDateTime

class TicketBuilder(private val productRepository: ProductRepository, private val calculator: Calculator) {
    private lateinit var localDateTime: LocalDateTime
    private lateinit var lines: List<TicketLine>
    private lateinit var discounts: List<TicketDiscount>
    private lateinit var total: Amount

    fun withDate(now: LocalDateTime): TicketBuilder {
        localDateTime = now
        return this
    }

    fun withItems(items: List<Item>): TicketBuilder {
        lines = getLines(items)
        discounts = getDiscounts(items)
        total = calculator.getTotal(lines, discounts)
        return this
    }

    private fun getLines(items: List<Item>) = items.map { item -> buildTicketLine(getProduct(item.id), item) }

    private fun getDiscounts(items: List<Item>) = items
            .map { item -> buildTicketDiscount(getProduct(item.id), item) }
            .filter { ticketDiscount -> ticketDiscount.totalDiscount.isBiggerThanZero() }

    private fun buildTicketDiscount(product: Product, item: Item) =
            TicketDiscount(product.productName, calculator.getDiscount(item, product))

    private fun buildTicketLine(product: Product, item: Item) =
            TicketLine(product.productName, item.quantity, product.price, calculator.getProductTotalPrice(item, product))

    private fun getProduct(id: Long) = productRepository.getProduct(id)!!

    fun build() = Ticket(localDateTime, lines, discounts, total)
}
