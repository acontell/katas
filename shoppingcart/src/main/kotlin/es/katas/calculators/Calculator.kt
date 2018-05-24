package es.katas.calculators

import es.katas.domain.Amount
import es.katas.domain.Item
import es.katas.domain.Product
import es.katas.domain.ticket.TicketDiscount
import es.katas.domain.ticket.TicketLine

class Calculator {

    private fun add(list: List<Amount>) = list.fold(Amount(), { acc, x -> x.add(acc) })

    fun getProductTotalPrice(item: Item, product: Product) = item.quantity.mult(product.price)

    fun getDiscount(item: Item, product: Product): Amount {
        val packPrice = product.price.mult(product.discount.quantity)
        val percent = product.discount.percentage.div(Amount(100))
        val discountedPricePerPack = packPrice.mult(percent)
        val numberOfDiscounts = item.quantity.div(product.discount.quantity)
        return discountedPricePerPack.mult(numberOfDiscounts)
    }

    fun getTotal(lines: List<TicketLine>, discounts: List<TicketDiscount>) = linesTotal(lines).subs(discountsTotal(discounts))

    private fun linesTotal(lines: List<TicketLine>) = add(lines.map { x -> x.totalPrice })

    private fun discountsTotal(discounts: List<TicketDiscount>) = add(discounts.map { x -> x.totalDiscount })
}
