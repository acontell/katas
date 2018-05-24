package es.katas

import es.katas.builders.TicketBuilder
import es.katas.domain.Amount
import es.katas.domain.Item
import es.katas.domain.ticket.Ticket
import es.katas.printers.ShoppingCartPrinter
import es.katas.repositories.ShoppingCartRepository
import es.katas.resources.Clock
import org.junit.Before
import org.junit.Test
import org.mockito.Mock
import org.mockito.Mockito.*
import java.time.LocalDateTime

class ShoppingCartServiceTest {

    companion object {
        private const val PRODUCT_ID = 1L
        private const val PRODUCT_QUANTITY = 1
    }

    private val now = LocalDateTime.of(2018, 1, 10, 14, 0, 0)
    private val item = Item(PRODUCT_ID, Amount(PRODUCT_QUANTITY))

    @Mock
    private val shoppingCartRepository = mock(ShoppingCartRepository::class.java)
    @Mock
    private val clock = mock(Clock::class.java)
    @Mock
    private val shoppingCartPrinter = mock(ShoppingCartPrinter::class.java)
    @Mock
    private val ticketBuilder = mock(TicketBuilder::class.java)
    @Mock
    private val ticket = mock(Ticket::class.java)

    private val shoppingCartService = ShoppingCartService(shoppingCartRepository, clock, shoppingCartPrinter, ticketBuilder)

    @Before
    fun setUp() {
        `when`(clock.now()).thenReturn(now)
        `when`(shoppingCartRepository.items).thenReturn(mutableListOf())
        `when`(shoppingCartRepository.localDateTime).thenReturn(now)
        `when`(ticketBuilder.withDate(now)).thenReturn(ticketBuilder)
        `when`(ticketBuilder.withItems(shoppingCartRepository.items)).thenReturn(ticketBuilder)
        `when`(ticketBuilder.build()).thenReturn(ticket)
    }

    @Test
    fun `addItem should initialize Shopping Cart when empty`() {
        `when`(shoppingCartRepository.isEmpty()).thenReturn(true)
        shoppingCartService.addItem(PRODUCT_ID, PRODUCT_QUANTITY)
        verify(shoppingCartRepository).initShoppingCart(now)
    }

    @Test
    fun `addItem should add to Shopping Cart when empty`() {
        `when`(shoppingCartRepository.isEmpty()).thenReturn(true)
        shoppingCartService.addItem(PRODUCT_ID, PRODUCT_QUANTITY)
        verify(shoppingCartRepository).addItem(item)
    }

    @Test
    fun `addItem should not initialize Shopping Cart when not empty`() {
        shoppingCartService.addItem(PRODUCT_ID, PRODUCT_QUANTITY)
        verify(shoppingCartRepository, times(0)).initShoppingCart(now)
    }

    @Test
    fun `addItem should add to Shopping Cart when not empty`() {
        shoppingCartService.addItem(PRODUCT_ID, PRODUCT_QUANTITY)
        verify(shoppingCartRepository).addItem(item)
    }

    @Test
    fun `printTicket should create ticket`() {
        shoppingCartService.printTicket()
        verify(ticketBuilder).withDate(now)
        verify(ticketBuilder).withItems(mutableListOf())
        verify(ticketBuilder).build()
    }

    @Test
    fun `printTicket should delegate to shoppingCartPrinter`() {
        shoppingCartService.printTicket()
        verify(shoppingCartPrinter).print(ticket)
    }
}