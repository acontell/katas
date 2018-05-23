package es.katas.calculators

import es.katas.domain.Discount
import es.katas.domain.Item
import es.katas.domain.Product
import es.katas.domain.Ticket.TicketDiscount
import es.katas.domain.Ticket.TicketLine
import org.hamcrest.core.Is.`is`
import org.junit.Assert.assertThat
import org.junit.Test
import java.math.BigDecimal

class CalculatorTest {
    private val productName = "chocolate"
    private val item = Item(1, 5)
    private val product = Product(productName, BigDecimal(0.8), Discount(3, 20.0))
    private val calculator = Calculator()
    private val totalPrice = calculator.scale(BigDecimal(4))
    private val discountPrice = calculator.scale(BigDecimal(0.48))
    private val lines = listOf(TicketLine(productName, 3, BigDecimal(5), BigDecimal(15)))
    private val discounts = listOf(TicketDiscount(productName, BigDecimal(2)))
    private val total = calculator.scale(BigDecimal(13))

    @Test
    fun `getProductTotalPrice should return the total price of the product according to the quantity`() {
        assertThat(calculator.getProductTotalPrice(item, product), `is`(totalPrice))
    }

    @Test
    fun `getDiscount should return the discount applied to a certain quantity of product`() {
        assertThat(calculator.getDiscount(item, product), `is`(discountPrice))
    }

    @Test
    fun `getTotal should return the total price of lines minus discounts`() {
        assertThat(calculator.getTotal(lines, discounts), `is`(total))
    }

    @Test
    fun `isBiggerThanZero should return true when bigger than 0, false otherwise`() {
        assertThat(calculator.isBiggerThanZero(BigDecimal.TEN), `is`(true))
        assertThat(calculator.isBiggerThanZero(BigDecimal.ZERO), `is`(false))
    }
}