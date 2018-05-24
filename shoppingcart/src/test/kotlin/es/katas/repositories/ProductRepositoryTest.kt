package es.katas.repositories

import es.katas.CHOCOLATE_BAR_ID
import es.katas.product
import org.hamcrest.CoreMatchers.`is`
import org.junit.Assert.*
import org.junit.Test

class ProductRepositoryTest {
    private val productRepository = ProductRepository()

    @Test
    fun `addProduct should store product in the repository`() {
        productRepository.addProduct(CHOCOLATE_BAR_ID, product)
    }

    @Test
    fun `getProduct should return product stored in the repository`() {
        productRepository.addProduct(CHOCOLATE_BAR_ID, product)
        assertThat(productRepository.getProduct(CHOCOLATE_BAR_ID), `is`(product))
    }
}