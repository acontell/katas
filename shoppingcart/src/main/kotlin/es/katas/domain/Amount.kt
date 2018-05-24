package es.katas.domain

import java.math.BigDecimal
import java.math.RoundingMode

class Amount(private val amount_: BigDecimal) {
    constructor() : this(BigDecimal.ZERO)
    constructor(amount_: Int) : this(BigDecimal(amount_))
    constructor(amount_: Double) : this(BigDecimal(amount_))

    private val amount = amount_.setScale(2, RoundingMode.HALF_EVEN)

    fun subs(p: Amount) = Amount(amount_.subtract(p.amount_))

    fun add(p: Amount) = Amount(amount_.add(p.amount_))

    fun mult(p: Amount) = Amount(amount_.multiply(p.amount_))

    fun div(p: Amount) = Amount(amount_.divide(p.amount_, 2, RoundingMode.HALF_EVEN))

    fun isBiggerThan(p: Amount) = amount_ > p.amount_

    fun isBiggerThanZero() = amount_ > BigDecimal.ZERO

    fun floor() = Amount(amount_.setScale(0, RoundingMode.DOWN))

    fun toInt() = amount.toBigInteger()

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (javaClass != other?.javaClass) return false

        other as Amount

        if (amount != other.amount) return false

        return true
    }

    override fun hashCode(): Int {
        return amount.hashCode()
    }

    override fun toString(): String {
        return amount.toString()
    }
}