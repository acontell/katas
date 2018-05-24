package es.katas

import es.katas.builders.TicketBuilder
import es.katas.calculators.Calculator
import es.katas.domain.Amount
import es.katas.domain.Discount
import es.katas.domain.Item
import es.katas.domain.Product
import es.katas.printers.ShoppingCartPrinter
import es.katas.repositories.ProductRepository
import es.katas.resources.Clock
import es.katas.resources.Console
import org.mockito.InOrder
import org.mockito.Mock
import org.mockito.Mockito
import java.time.LocalDateTime

const val CHOCOLATE_BAR = "Chocolate bar"
const val BAGUETTE = "Baguette"
const val CHOCOLATE_BAR_ID = 1L
const val BAGUETTE_ID = 2L
const val PRODUCT_QUANTITY = 1

@Mock
val console: Console = Mockito.mock(Console::class.java)
@Mock
val clock: Clock = Mockito.mock(Clock::class.java)

val inOrder: InOrder = Mockito.inOrder(console)
val now: LocalDateTime = LocalDateTime.of(2018, 1, 10, 14, 0, 0)
val shoppingCartPrinter: ShoppingCartPrinter = ShoppingCartPrinter(console)
val productRepository = ProductRepository()
        .addProduct(CHOCOLATE_BAR_ID, Product(CHOCOLATE_BAR, Amount(2.0), Discount()))
        .addProduct(BAGUETTE_ID, Product(BAGUETTE, Amount(0.8), Discount(Amount(3), Amount(20.0))))
val ticketBuilder: TicketBuilder = TicketBuilder(productRepository, Calculator())
val item = Item(CHOCOLATE_BAR_ID, Amount(PRODUCT_QUANTITY))
val product = Product(CHOCOLATE_BAR, Amount(PRODUCT_QUANTITY), Discount())

fun givenShoppingCartServiceWithQuantities(shoppingCartService: ShoppingCartService, chocolateQuantity: Int, baguetteQuantity: Int) {
    shoppingCartService.addItem(CHOCOLATE_BAR_ID, chocolateQuantity)
    shoppingCartService.addItem(BAGUETTE_ID, baguetteQuantity)
}