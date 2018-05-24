package es.katas.printers

import es.katas.domain.Amount
import es.katas.domain.ticket.Ticket
import es.katas.domain.ticket.TicketDiscount
import es.katas.domain.ticket.TicketLine
import es.katas.resources.Console
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter.ofPattern

class ShoppingCartPrinter(private val console: Console) {
    fun print(ticket: Ticket) {
        printDate(ticket.localDateTime)
        printProductsHeader("PRODUCTS:")
        printTicketLines(ticket.lines)
        printTicketDiscounts(ticket.discounts)
        printTotal(ticket.total)
    }

    private fun printProductsHeader(header: String) {
        console.print(header)
    }

    private fun printTotal(total: Amount) {
        console.print("TOTAL: $total")
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
        printTicketLineBreakdown(ticketLine)
    }

    private fun printTicketLineBreakdown(ticketLine: TicketLine) {
        if (ticketLine.quantity.isBiggerThan(Amount(1))) {
            console.print(" ${ticketLine.quantity} x ${ticketLine.unitPrice}")
        }
    }

    private fun printTicketDiscount(ticketDiscount: TicketDiscount) {
        val discountStr = "${ticketDiscount.productName} discount".padEnd(18)
        console.print(" $discountStr -${ticketDiscount.totalDiscount}")
    }
}
