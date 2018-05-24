package es.katas.calculators

import es.katas.domain.Amount
import es.katas.domain.Discount
import es.katas.domain.Item
import es.katas.domain.Product
import es.katas.domain.ticket.TicketDiscount
import es.katas.domain.ticket.TicketLine
import org.hamcrest.core.Is.`is`
import org.junit.Assert.assertThat
import org.junit.Test
import java.math.BigDecimal

class CalculatorTest {
    private val productName = "chocolate"
    private val item = Item(1, Amount(5))
    private val product = Product(productName, Amount(0.8), Discount(Amount(3), Amount(20.0)))
    private val calculator = Calculator()
    private val totalPrice = Amount(4)
    private val discountPrice = Amount(0.48)
    private val lines = listOf(TicketLine(productName, Amount(3), Amount(5), Amount(15)))
    private val discounts = listOf(TicketDiscount(productName, Amount(2)))
    private val total = Amount(13)

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
        assertThat(Amount(10).isBiggerThanZero(), `is`(true))
        assertThat(Amount(0).isBiggerThanZero(), `is`(false))
    }

    @Test
    fun `isBiggerThan should return true when bigger than x, false otherwise`() {
        assertThat(Amount(10).isBiggerThan(Amount(1)), `is`(true))
        assertThat(Amount(10).isBiggerThan(Amount(100)), `is`(false))
    }
}