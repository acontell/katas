package es.katas.domain.ticket

import es.katas.domain.Amount

data class TicketLine(val productName: String,
                      val quantity: Amount,
                      val unitPrice: Amount,
                      val totalPrice: Amount)