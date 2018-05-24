package es.katas.domain.ticket

import java.math.BigDecimal
import java.time.LocalDateTime

class Ticket(val localDateTime: LocalDateTime,
             val lines: List<TicketLine>,
             val discounts: List<TicketDiscount>,
             val total: BigDecimal)