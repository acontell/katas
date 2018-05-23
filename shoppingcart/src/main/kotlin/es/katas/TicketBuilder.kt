package es.katas

import es.katas.calculators.Calculator
import es.katas.domain.Item
import es.katas.domain.Ticket
import es.katas.repositories.ProductRepository
import java.time.LocalDateTime

class TicketBuilder(val productRepository: ProductRepository, val calculator: Calculator) {
    fun withDate(now: LocalDateTime) = this
    fun withItems(list: List<Item>) = this
    fun build() = Ticket()
}
