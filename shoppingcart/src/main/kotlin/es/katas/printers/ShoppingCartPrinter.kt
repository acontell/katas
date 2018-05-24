package es.katas.printers

import es.katas.domain.Ticket.Ticket
import es.katas.domain.Ticket.TicketDiscount
import es.katas.domain.Ticket.TicketLine
import es.katas.resources.Console
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter.ofPattern

class ShoppingCartPrinter(private val console: Console) {
    fun print(ticket: Ticket) {
        printDate(ticket.localDateTime)
        console.print("PRODUCTS:")
        printTicketLines(ticket.lines)
        printTicketDiscounts(ticket.discounts)
        console.print("TOTAL: ${ticket.total}")
    }

    private fun printDate(date: LocalDateTime) {
        console.print(date.format(ofPattern("dd/MM/yyyy HH:mm:ss")))
    }

    private fun printTicketLines(lines: List<TicketLine>) {
        lines.forEach({ ticketLine -> printTicketLine(ticketLine) })
    }

    private fun printTicketDiscounts(discounts: List<TicketDiscount>) {
        discounts.forEach({ ticketDiscount -> printTicketDiscount(ticketDiscount) })
    }

    private fun printTicketLine(ticketLine: TicketLine) {
        console.print(" ${ticketLine.productName.padEnd(19)} ${ticketLine.totalPrice}")
        if (ticketLine.quantity > 1) {
            console.print(" ${ticketLine.quantity} x ${ticketLine.unitPrice}")
        }
    }

    private fun printTicketDiscount(ticketDiscount: TicketDiscount) {
        val discountStr = "${ticketDiscount.productName} discount".padEnd(18)
        console.print(" $discountStr -${ticketDiscount.totalDiscount}")
    }
}
