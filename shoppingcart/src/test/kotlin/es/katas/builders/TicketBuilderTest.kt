package es.katas.builders

import es.katas.calculators.Calculator
import es.katas.domain.Item
import es.katas.domain.Ticket.Ticket
import es.katas.domain.Ticket.TicketDiscount
import es.katas.domain.Ticket.TicketLine
import es.katas.repositories.ProductRepository
import org.hamcrest.CoreMatchers.`is`
import org.hamcrest.MatcherAssert.assertThat
import org.junit.Before
import org.junit.Test
import org.mockito.Mock
import org.mockito.Mockito
import org.mockito.Mockito.`when`
import java.math.BigDecimal
import java.time.LocalDateTime

class TicketBuilderTest {
    private val now = LocalDateTime.of(2018, 1, 10, 14, 0, 0)
    private val items = mutableListOf(Item(1, 5))
    private val lines = listOf(TicketLine())
    private val discounts = listOf(TicketDiscount())
    private val total: BigDecimal = BigDecimal(5.0)
    private val productRepository = ProductRepository()

    @Mock
    private val calculator = Mockito.mock(Calculator::class.java)

    private val ticketBuilder = TicketBuilder(productRepository, calculator)
    private lateinit var ticket: Ticket

    @Before
    fun setUp() {
        `when`(calculator.calculateLines(productRepository, items)).thenReturn(lines)
        `when`(calculator.calculateDiscounts(productRepository, items)).thenReturn(discounts)
        `when`(calculator.calculateTotal(productRepository, items)).thenReturn(total)
        ticket = ticketBuilder
                .withItems(items)
                .withDate(now)
                .build()
    }

    @Test
    fun `withDate should return a ticket with previously set date`() {
        assertThat(ticket.localDateTime, `is`(now))
    }

    @Test
    fun `withItems should return a ticket with lines, discounts and total`() {
        assertThat(ticket.lines, `is`(lines))
        assertThat(ticket.discounts, `is`(discounts))
        assertThat(ticket.total, `is`(total))
    }
}