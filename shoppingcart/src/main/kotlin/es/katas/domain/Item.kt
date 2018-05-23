package es.katas.domain

class Item(val id: Int, val quantity: Int) {
    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (javaClass != other?.javaClass) return false

        other as Item

        if (id != other.id) return false
        if (quantity != other.quantity) return false

        return true
    }

    override fun hashCode(): Int {
        var result = id
        result = 31 * result + quantity
        return result
    }
}
