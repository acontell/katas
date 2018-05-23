package es.katas.repositories

import es.katas.domain.Product

class ProductRepository {
    private val products = mutableMapOf<Int, Product>()

    fun addProduct(productId: Int, product: Product): ProductRepository {
        products[productId] = product
        return this
    }

    fun getProduct(productId: Int) = products[productId]
}
