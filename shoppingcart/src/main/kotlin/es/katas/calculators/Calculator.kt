package es.katas.calculators

import es.katas.domain.Item
import es.katas.domain.Ticket.TicketDiscount
import es.katas.domain.Ticket.TicketLine
import es.katas.repositories.ProductRepository
import java.math.BigDecimal

class Calculator {
    fun calculateLines(productRepository: ProductRepository, items: MutableList<Item>): List<TicketLine> {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    fun calculateDiscounts(productRepository: ProductRepository, items: MutableList<Item>): List<TicketDiscount> {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    fun calculateTotal(productRepository: ProductRepository, items: MutableList<Item>): BigDecimal {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

}
