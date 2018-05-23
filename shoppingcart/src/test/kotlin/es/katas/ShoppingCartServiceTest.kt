package es.katas

import es.katas.builders.TicketBuilder
import es.katas.domain.Item
import es.katas.domain.Ticket
import es.katas.printers.ShoppingCartPrinter
import es.katas.repositories.ShoppingCartRepository
import es.katas.resources.Clock
import org.junit.Before
import org.junit.Test
import org.mockito.Mock
import org.mockito.Mockito.*
import org.mockito.MockitoAnnotations
import java.time.LocalDateTime

class ShoppingCartServiceTest {

    companion object {
        private const val PRODUCT_ID = 1
        private const val PRODUCT_QUANTITY = 1
    }

    private val now: LocalDateTime = LocalDateTime.of(2018, 1, 10, 14, 0, 0)
    private val item: Item = Item(PRODUCT_ID, PRODUCT_QUANTITY)
    private val ticket: Ticket = Ticket()

    @Mock
    private lateinit var shoppingCartRepository: ShoppingCartRepository
    @Mock
    private lateinit var clock: Clock
    @Mock
    private lateinit var shoppingCartPrinter: ShoppingCartPrinter
    @Mock
    private lateinit var ticketBuilder: TicketBuilder

    private lateinit var shoppingCartService: ShoppingCartService

    @Before
    fun setUp() {
        MockitoAnnotations.initMocks(this)
        `when`(clock.now()).thenReturn(now)
        `when`(shoppingCartRepository.items).thenReturn(mutableListOf())
        `when`(shoppingCartRepository.localDateTime).thenReturn(now)
        `when`(ticketBuilder.withDate(now)).thenReturn(ticketBuilder)
        `when`(ticketBuilder.withItems(shoppingCartRepository.items)).thenReturn(ticketBuilder)
        `when`(ticketBuilder.build()).thenReturn(ticket)
        shoppingCartService = ShoppingCartService(shoppingCartRepository, clock, shoppingCartPrinter, ticketBuilder)
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