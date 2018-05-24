package es.katas.domain

class Discount(val quantity: Amount, val percentage: Amount) {
    constructor() : this(Amount(1), Amount(0.0))

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (javaClass != other?.javaClass) return false

        other as Discount

        if (quantity != other.quantity) return false
        if (percentage != other.percentage) return false

        return true
    }

    override fun hashCode(): Int {
        var result = quantity.hashCode()
        result = 31 * result + percentage.hashCode()
        return result
    }
}
