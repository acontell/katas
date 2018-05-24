package es.katas.domain

import java.math.BigDecimal
import java.math.RoundingMode

class Amount(private val amount_: BigDecimal) {
    constructor() : this(BigDecimal.ZERO)
    constructor(amount_: Int) : this(BigDecimal(amount_))
    constructor(amount_: Double) : this(BigDecimal(amount_))

    val amount: BigDecimal
        get() = amount_.setScale(2, RoundingMode.HALF_UP)

    fun subs(p: Amount) = Amount(amount.subtract(p.amount))

    fun add(p: Amount) = Amount(amount.add(p.amount))

    fun mult(p: Amount) = Amount(amount.multiply(p.amount))

    fun div(p: Amount) = Amount(amount.divide(p.amount))

    fun isBiggerThan(p: Amount) = amount > p.amount

    fun isBiggerThanZero() = amount > BigDecimal.ZERO

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (javaClass != other?.javaClass) return false

        other as Amount

        if (amount_ != other.amount_) return false

        return true
    }

    override fun hashCode(): Int {
        return amount_.hashCode()
    }

    override fun toString(): String {
        return amount.toString()
    }
}