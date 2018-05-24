package es.katas.repositories

import es.katas.domain.Product

class ProductRepository {
    private val products = mutableMapOf<Long, Product>()

    fun addProduct(productId: Long, product: Product): ProductRepository {
        products[productId] = product
        return this
    }

    fun getProduct(productId: Long) = products[productId]
}
