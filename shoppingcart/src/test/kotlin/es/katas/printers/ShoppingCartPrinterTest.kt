package es.katas.printers

import es.katas.calculators.Calculator
import es.katas.domain.Amount
import es.katas.domain.ticket.Ticket
import es.katas.domain.ticket.TicketDiscount
import es.katas.domain.ticket.TicketLine
import es.katas.resources.Console
import org.junit.Test
import org.mockito.InOrder
import org.mockito.Mock
import org.mockito.Mockito
import java.time.LocalDateTime

class ShoppingCartPrinterTest {
    companion object {
        private const val CHOCOLATE_BAR = "Chocolate bar"
        private const val BAGUETTE = "Baguette"
    }

    private val now = LocalDateTime.of(2018, 1, 10, 14, 0, 0)

    @Mock
    private val console = Mockito.mock(Console::class.java)

    private val inOrder: InOrder = Mockito.inOrder(console)

    private val shoppingCartPrinter = ShoppingCartPrinter(console)

    @Test
    fun `should print a ticket without multiple items and without discount`() {
        val chocolateLine = TicketLine(CHOCOLATE_BAR, Amount(1), Amount(2), Amount(2))
        val baguetteLine = TicketLine(BAGUETTE, Amount(1), Amount(0.8), Amount(0.80))
        val lines = listOf(chocolateLine, baguetteLine)

        shoppingCartPrinter.print(Ticket(now, lines, listOf(), Amount(2.80)))

        inOrder.verify(console).print("10/01/2018 14:00:00")
        inOrder.verify(console).print("PRODUCTS:")
        inOrder.verify(console).print(" Chocolate bar       2.00")
        inOrder.verify(console).print(" Baguette            0.80")
        inOrder.verify(console).print("TOTAL: 2.80")
    }

    @Test
    fun `should print a ticket with multiple items and without discount`() {
        val chocolateLine = TicketLine(CHOCOLATE_BAR, Amount(2), Amount(2), Amount(4))
        val baguetteLine = TicketLine(BAGUETTE, Amount(1), Amount(0.8), Amount(0.8))
        val lines = listOf(chocolateLine, baguetteLine)

        shoppingCartPrinter.print(Ticket(now, lines, listOf(), Amount(4.80)))

        inOrder.verify(console).print("10/01/2018 14:00:00")
        inOrder.verify(console).print("PRODUCTS:")
        inOrder.verify(console).print(" Chocolate bar       4.00")
        inOrder.verify(console).print(" 2 x 2.00")
        inOrder.verify(console).print(" Baguette            0.80")
        inOrder.verify(console).print("TOTAL: 4.80")
    }

    @Test
    fun `should print a ticket with multiple items and discount`() {
        val chocolateLine = TicketLine(CHOCOLATE_BAR, Amount(1), Amount(2), Amount(2))
        val baguetteLine = TicketLine(BAGUETTE, Amount(5), Amount(0.8), Amount(4))
        val lines = listOf(chocolateLine, baguetteLine)
        val discounts = listOf(TicketDiscount(BAGUETTE, Amount(0.48)))

        shoppingCartPrinter.print(Ticket(now, lines, discounts, Amount(5.52)))

        inOrder.verify(console).print("10/01/2018 14:00:00")
        inOrder.verify(console).print("PRODUCTS:")
        inOrder.verify(console).print(" Chocolate bar       2.00")
        inOrder.verify(console).print(" Baguette            4.00")
        inOrder.verify(console).print(" 5 x 0.80")
        inOrder.verify(console).print(" Baguette discount  -0.48")
        inOrder.verify(console).print("TOTAL: 5.52")
    }
}