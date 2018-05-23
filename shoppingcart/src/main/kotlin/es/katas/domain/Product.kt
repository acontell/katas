package es.katas.domain

import java.math.BigDecimal

class Product(val productName: String, val price: BigDecimal, val discount: Discount) {
    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (javaClass != other?.javaClass) return false

        other as Product

        if (productName != other.productName) return false
        if (price != other.price) return false
        if (discount != other.discount) return false

        return true
    }

    override fun hashCode(): Int {
        var result = productName.hashCode()
        result = 31 * result + price.hashCode()
        result = 31 * result + discount.hashCode()
        return result
    }
}
