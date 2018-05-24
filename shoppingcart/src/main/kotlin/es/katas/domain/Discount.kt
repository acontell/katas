package es.katas.domain

data class Discount(val quantity: Amount = Amount(1),
                    val percentage: Amount = Amount(0.0))