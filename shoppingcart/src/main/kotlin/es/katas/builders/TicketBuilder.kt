package es.katas.builders

import es.katas.calculators.Calculator
import es.katas.domain.Item
import es.katas.domain.Ticket
import es.katas.repositories.ProductRepository
import java.time.LocalDateTime

class TicketBuilder(val productRepository: ProductRepository, val calculator: Calculator) {
    fun withDate(now: LocalDateTime) = this
    fun withItems(list: MutableMap<Int, Item>) = this
    fun build() = Ticket()
}
