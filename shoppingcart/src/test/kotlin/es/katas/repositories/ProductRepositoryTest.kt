package es.katas.repositories

import es.katas.domain.Discount
import es.katas.domain.Product
import org.hamcrest.CoreMatchers.`is`
import org.junit.Assert.*
import org.junit.Test

class ProductRepositoryTest {
    private val product = Product("Example", 5.0, Discount())
    private val productId = 1
    private val productRepository = ProductRepository()

    @Test
    fun `addProduct should store product in the repository`() {
        productRepository.addProduct(productId, product)
    }

    @Test
    fun `getProduct should return product stored in the repository`() {
        productRepository.addProduct(productId, product)
        assertThat(productRepository.getProduct(productId), `is`(product))
    }
}