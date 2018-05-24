package es.katas.builders

import es.katas.calculators.Calculator
import es.katas.domain.Amount
import es.katas.domain.Discount
import es.katas.domain.Item
import es.katas.domain.Product
import es.katas.domain.ticket.Ticket
import es.katas.domain.ticket.TicketDiscount
import es.katas.domain.ticket.TicketLine
import es.katas.repositories.ProductRepository
import org.hamcrest.CoreMatchers.`is`
import org.hamcrest.MatcherAssert.assertThat
import org.junit.Before
import org.junit.Test
import java.time.LocalDateTime

class TicketBuilderTest {
    companion object {
        private const val CHOCOLATE_BAR = "Chocolate bar"
        private const val BAGUETTE = "Baguette"
        private const val CHOCOLATE_BAR_ID = 1L
        private const val BAGUETTE_ID = 2L
    }

    private val calculator = Calculator()
    private val now = LocalDateTime.of(2018, 1, 10, 14, 0, 0)
    private val items = listOf(Item(1, Amount(5)), Item(2, Amount(5)))
    private val chocolateLine = TicketLine(CHOCOLATE_BAR, Amount(5), Amount(2), Amount(10))
    private val baguetteLine = TicketLine(BAGUETTE, Amount(5), Amount(0.8), Amount(4))
    private val lines = listOf(chocolateLine, baguetteLine)
    private val discounts = listOf(TicketDiscount(BAGUETTE, Amount(0.48)))
    private val total = Amount(13.52)
    private val productRepository = ProductRepository()
            .addProduct(CHOCOLATE_BAR_ID, Product(CHOCOLATE_BAR, Amount(2.0), Discount()))
            .addProduct(BAGUETTE_ID, Product(BAGUETTE, Amount(0.8), Discount(Amount(3), Amount(20.0))))

    private val ticketBuilder = TicketBuilder(productRepository, calculator)
    private lateinit var ticket: Ticket

    @Before
    fun setUp() {
        ticket = ticketBuilder
                .withItems(items)
                .withDate(now)
                .build()
    }

    @Test
    fun `build should return a ticket with date, lines, discounts and total`() {
        assertThat(ticket.localDateTime, `is`(now))
        assertThat(ticket.lines, `is`(lines))
        assertThat(ticket.discounts, `is`(discounts))
        assertThat(ticket.total, `is`(total))
    }
}