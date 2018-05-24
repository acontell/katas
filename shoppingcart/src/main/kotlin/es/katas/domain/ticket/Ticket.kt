package es.katas.domain.ticket

import es.katas.domain.Amount
import java.time.LocalDateTime

class Ticket(val localDateTime: LocalDateTime,
             val lines: List<TicketLine>,
             val discounts: List<TicketDiscount>,
             val total: Amount)