package es.katas.domain.ticket

import es.katas.domain.Amount

class TicketDiscount(val productName: String, val totalDiscount: Amount) {
    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (javaClass != other?.javaClass) return false

        other as TicketDiscount

        if (productName != other.productName) return false
        if (totalDiscount != other.totalDiscount) return false

        return true
    }

    override fun hashCode(): Int {
        var result = productName.hashCode()
        result = 31 * result + totalDiscount.hashCode()
        return result
    }
}