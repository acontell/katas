package es.katas

import es.katas.builders.TicketBuilder
import es.katas.calculators.Calculator
import es.katas.domain.Discount
import es.katas.domain.Product
import es.katas.printers.ShoppingCartPrinter
import es.katas.repositories.ProductRepository
import es.katas.repositories.ShoppingCartRepository
import es.katas.resources.Clock
import es.katas.resources.Console
import org.junit.Test
import java.time.LocalDateTime
import org.junit.Before
import org.mockito.BDDMockito.given
import org.mockito.BDDMockito.reset
import org.mockito.InOrder
import org.mockito.Mock
import org.mockito.Mockito
import org.mockito.Mockito.mock
import java.math.BigDecimal
import java.time.LocalDateTime.of


class ShoppingCartServiceFeature {
    companion object {
        private const val CHOCOLATE_BAR = "Chocolate bar"
        private const val BAGUETTE = "Baguette"
        private const val CHOCOLATE_BAR_ID = 1
        private const val BAGUETTE_ID = 2
    }

    @Mock
    private val console: Console = mock(Console::class.java)
    @Mock
    private val clock: Clock = mock(Clock::class.java)

    private val inOrder: InOrder = Mockito.inOrder(console)
    private val now: LocalDateTime = of(2018, 1, 10, 14, 0, 0)
    private val shoppingCartPrinter: ShoppingCartPrinter = ShoppingCartPrinter(console)
    private val ticketBuilder: TicketBuilder = TicketBuilder(buildProductsRepository(), Calculator())
    private fun buildProductsRepository() = ProductRepository()
            .addProduct(CHOCOLATE_BAR_ID, Product(CHOCOLATE_BAR, BigDecimal(2.0), Discount()))
            .addProduct(BAGUETTE_ID, Product(BAGUETTE, BigDecimal(0.8), Discount(3, 20.0)))

    private lateinit var shoppingCartService: ShoppingCartService

    @Before
    fun setUp() {
        reset(console)
        given(clock.now()).willReturn(now)
        shoppingCartService = ShoppingCartService(ShoppingCartRepository(), clock, shoppingCartPrinter, ticketBuilder)
    }

    @Test
    fun `should print a ticket without multiple items and without discount`() {
        givenShoppingCartServiceWithQuantities(1, 1)

        shoppingCartService.printTicket()

        inOrder.verify(console).print("10/01/2018 14:00:00")
        inOrder.verify(console).print("PRODUCTS:")
        inOrder.verify(console).print(" Chocolate bar       2.00")
        inOrder.verify(console).print(" Baguette            0.80")
        inOrder.verify(console).print("TOTAL: 2.80")
    }

    @Test
    fun `should print a ticket with multiple items and without discount`() {
        givenShoppingCartServiceWithQuantities(2, 1)

        shoppingCartService.printTicket()

        inOrder.verify(console).print("10/01/2018 14:00:00")
        inOrder.verify(console).print("PRODUCTS:")
        inOrder.verify(console).print(" Chocolate bar       4.00")
        inOrder.verify(console).print(" 2 x 2.00")
        inOrder.verify(console).print(" Baguette            0.80")
        inOrder.verify(console).print("TOTAL: 4.80")
    }

    @Test
    fun `should print a ticket with multiple items and discount`() {
        givenShoppingCartServiceWithQuantities(1, 5)

        shoppingCartService.printTicket()

        inOrder.verify(console).print("10/01/2018 14:00:00")
        inOrder.verify(console).print("PRODUCTS:")
        inOrder.verify(console).print(" Chocolate bar       2.00")
        inOrder.verify(console).print(" Baguette            4.00")
        inOrder.verify(console).print(" 5 x 0.80")
        inOrder.verify(console).print(" Baguette discount  -0.48")
        inOrder.verify(console).print("TOTAL: 5.52")
    }

    private fun givenShoppingCartServiceWithQuantities(chocolateQuantity: Int, baguetteQuantity: Int) {
        shoppingCartService.addItem(CHOCOLATE_BAR_ID, chocolateQuantity)
        shoppingCartService.addItem(BAGUETTE_ID, baguetteQuantity)
    }
}
