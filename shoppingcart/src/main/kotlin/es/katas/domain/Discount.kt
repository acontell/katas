package es.katas.domain

class Discount(val quantity: Int, val percentage: Double) {
    constructor() : this(1, 0.0)

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (javaClass != other?.javaClass) return false

        other as Discount

        if (quantity != other.quantity) return false
        if (percentage != other.percentage) return false

        return true
    }

    override fun hashCode(): Int {
        var result = quantity
        result = (31 * result + percentage).toInt()
        return result
    }
}
