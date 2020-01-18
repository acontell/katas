package training.weather.time;

import java.time.LocalDate;
import java.util.Date;

import static java.time.ZoneId.systemDefault;
import static java.util.Objects.isNull;

public class Clock {

    public LocalDate getLocalDateOrNow(final Date date) {
        return isNull(date)
                ? now()
                : this.dateToLocalDate(date);
    }

    LocalDate now() {
        return LocalDate.now();
    }

    public boolean isSameDate(final LocalDate date, final LocalDate otherDate) {
        return date.compareTo(otherDate) == 0;
    }

    private LocalDate dateToLocalDate(final Date date) {
        return date.toInstant()
                .atZone(systemDefault())
                .toLocalDate();
    }

    public boolean isDateBetweenRange(final LocalDate localDate, final int offsetInDays) {
        return localDate.isBefore(this.now().plusDays(offsetInDays));
    }
}
