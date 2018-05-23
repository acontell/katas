package es.katas.resources

import java.time.LocalDateTime

class Clock {
    fun now() = LocalDateTime.now()!!
}