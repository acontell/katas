package es.katas.builders

import es.katas.*
import es.katas.calculators.Calculator
import es.katas.domain.Amount
import es.katas.domain.Item
import es.katas.domain.ticket.TicketDiscount
import es.katas.domain.ticket.TicketLine
import org.hamcrest.CoreMatchers.`is`
import org.hamcrest.MatcherAssert.assertThat
import org.junit.Test

class TicketBuilderTest {
    private val items = listOf(Item(1, Amount(5)), Item(2, Amount(5)))
    private val chocolateLine = TicketLine(CHOCOLATE_BAR, Amount(5), Amount(2), Amount(10))
    private val baguetteLine = TicketLine(BAGUETTE, Amount(5), Amount(0.8), Amount(4))
    private val lines = listOf(chocolateLine, baguetteLine)
    private val discounts = listOf(TicketDiscount(BAGUETTE, Amount(0.48)))
    private val total = Amount(13.52)

    @Test
    fun `build should return a ticket with date, lines, discounts and total`() {
        val ticket = TicketBuilder(productRepository, Calculator())
                .withItems(items)
                .withDate(now)
                .build()

        assertThat(ticket.localDateTime, `is`(now))
        assertThat(ticket.lines, `is`(lines))
        assertThat(ticket.discounts, `is`(discounts))
        assertThat(ticket.total, `is`(total))
    }
}