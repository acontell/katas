package es.katas.domain.ticket

import java.math.BigDecimal

class TicketLine(val productName: String, val quantity: Int, val unitPrice: BigDecimal, val totalPrice: BigDecimal) {
    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (javaClass != other?.javaClass) return false

        other as TicketLine

        if (productName != other.productName) return false
        if (quantity != other.quantity) return false
        if (unitPrice != other.unitPrice) return false
        if (totalPrice != other.totalPrice) return false

        return true
    }

    override fun hashCode(): Int {
        var result = productName.hashCode()
        result = 31 * result + quantity
        result = 31 * result + unitPrice.hashCode()
        result = 31 * result + totalPrice.hashCode()
        return result
    }
}