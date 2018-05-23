package es.katas.calculators

import es.katas.domain.Item
import es.katas.domain.Product
import es.katas.domain.Ticket.TicketDiscount
import es.katas.domain.Ticket.TicketLine
import java.math.BigDecimal
import java.math.RoundingMode

class Calculator {

    private fun subs(p1: BigDecimal, p2: BigDecimal) = scale(p1.subtract(p2))

    private fun add(p1: BigDecimal, p2: BigDecimal) = scale(p1.add(p2))

    private fun add(list: List<BigDecimal>) = list.fold(BigDecimal.ZERO, { acc, x -> add(acc, x) })

    private fun mult(p1: BigDecimal, p2: BigDecimal) = scale(p1.multiply(p2))

    fun scale(p: BigDecimal) = p.setScale(2, RoundingMode.HALF_UP)

    fun getProductTotalPrice(item: Item, product: Product) = mult(BigDecimal(item.quantity), product.price)

    fun getDiscount(item: Item, product: Product): BigDecimal {
        val packPrice = mult(product.price, BigDecimal(product.discount.quantity))
        val discountedPricePerPack = mult(packPrice, BigDecimal(product.discount.percentage / 100))
        val numberOfDiscounts = item.quantity / product.discount.quantity
        return mult(discountedPricePerPack, BigDecimal(numberOfDiscounts))
    }

    fun getTotal(lines: List<TicketLine>, discounts: List<TicketDiscount>) = subs(linesTotal(lines), discountsTotal(discounts))

    fun isBiggerThanZero(p : BigDecimal) = p > BigDecimal.ZERO

    private fun linesTotal(lines: List<TicketLine>) = add(lines.map { x -> x.totalPrice })

    private fun discountsTotal(discounts: List<TicketDiscount>) = add(discounts.map { x -> x.totalDiscount })

}
