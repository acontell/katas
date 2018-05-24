package es.katas

import es.katas.repositories.ShoppingCartRepository
import org.junit.Test
import org.junit.Before
import org.mockito.BDDMockito.given
import org.mockito.BDDMockito.reset

class ShoppingCartServiceFeature {
    private val shoppingCartService = ShoppingCartService(ShoppingCartRepository(), clock, shoppingCartPrinter, ticketBuilder)

    @Before
    fun setUp() {
        reset(console)
        given(clock.now()).willReturn(now)
    }

    @Test
    fun `should print a ticket without multiple items and without discount`() {
        givenShoppingCartServiceWithQuantities(shoppingCartService, 1, 1)

        shoppingCartService.printTicket()

        inOrder.verify(console).print("10/01/2018 14:00:00")
        inOrder.verify(console).print("PRODUCTS:")
        inOrder.verify(console).print(" Chocolate bar       2.00")
        inOrder.verify(console).print(" Baguette            0.80")
        inOrder.verify(console).print("TOTAL: 2.80")
    }

    @Test
    fun `should print a ticket with multiple items and without discount`() {
        givenShoppingCartServiceWithQuantities(shoppingCartService, 2, 1)

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
        givenShoppingCartServiceWithQuantities(shoppingCartService, 1, 5)

        shoppingCartService.printTicket()

        inOrder.verify(console).print("10/01/2018 14:00:00")
        inOrder.verify(console).print("PRODUCTS:")
        inOrder.verify(console).print(" Chocolate bar       2.00")
        inOrder.verify(console).print(" Baguette            4.00")
        inOrder.verify(console).print(" 5 x 0.80")
        inOrder.verify(console).print(" Baguette discount  -0.48")
        inOrder.verify(console).print("TOTAL: 5.52")
    }
}
