package infrastructure;

import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Date;

import static java.util.Optional.ofNullable;

public class Clock {

    public LocalDate localDateOrDefault(final Date date) {
        return ofNullable(date)
                .map(this::from)
                .orElse(LocalDate.now());
    }

    private LocalDate from(final Date date) {
        return date.toInstant().atZone(ZoneId.systemDefault()).toLocalDate();
    }
}
