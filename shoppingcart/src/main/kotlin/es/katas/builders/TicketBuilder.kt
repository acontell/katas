package es.katas.builders

import es.katas.calculators.Calculator
import es.katas.domain.Item
import es.katas.domain.Ticket.Ticket
import es.katas.domain.Ticket.TicketDiscount
import es.katas.domain.Ticket.TicketLine
import es.katas.repositories.ProductRepository
import java.math.BigDecimal
import java.time.LocalDateTime

class TicketBuilder(private val productRepository: ProductRepository, private val calculator: Calculator) {
    private lateinit var localDateTime: LocalDateTime
    private lateinit var lines: List<TicketLine>
    private lateinit var discounts: List<TicketDiscount>
    private lateinit var total: BigDecimal

    fun withDate(now: LocalDateTime): TicketBuilder {
        localDateTime = now
        return this
    }

    fun withItems(items: MutableList<Item>) : TicketBuilder {
        lines = calculator.calculateLines(productRepository, items)
        discounts = calculator.calculateDiscounts(productRepository, items)
        total = calculator.calculateTotal(productRepository, items)
        return this
    }
    fun build() = Ticket(localDateTime, lines, discounts, total)
}
