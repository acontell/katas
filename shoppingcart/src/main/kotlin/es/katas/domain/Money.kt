package es.katas.domain

import java.math.BigDecimal
import java.math.RoundingMode

class Money(amount_: Double) {
    val amount = scale(BigDecimal(amount_))

    private fun subs(p: BigDecimal) = scale(amount.subtract(p))

    private fun add(p: BigDecimal) = scale(amount.add(p))

    private fun mult(p: BigDecimal) = scale(amount.multiply(p))

    private fun scale(p: BigDecimal) = p.setScale(2, RoundingMode.HALF_UP)
}