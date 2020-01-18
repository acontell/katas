package training.weather.time;

import org.junit.Before;
import org.junit.Test;

import java.time.LocalDate;
import java.util.Date;

import static java.time.ZoneId.systemDefault;
import static org.hamcrest.core.IsInstanceOf.instanceOf;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.spy;
import static training.weather.WeatherForecastFixture.getDate;
import static training.weather.time.Clock.DATE_FORMAT;

public class ClockTest {
    private static final LocalDate LOCAL_DATE = LocalDate.now();
    private static final Date DATE = new Date();
    private static final String DATE_STRING = "12-01-2020";
    private Clock clock;

    @Before
    public void setUp() {
        this.clock = spy(new Clock());
        doReturn(LOCAL_DATE).when(this.clock).now();
    }

    @Test
    public void shouldReturnLocalDateTimeWhenDateIsNotNull() {
        assertEquals(this.clock.getLocalDateOrNow(DATE), dateToLocalDate());
    }

    private LocalDate dateToLocalDate() {
        return ClockTest.DATE.toInstant()
                .atZone(systemDefault())
                .toLocalDate();
    }

    @Test
    public void shouldReturnNowWhenDateIsNull() {
        assertEquals(this.clock.getLocalDateOrNow(null), LOCAL_DATE);
    }

    @Test
    public void shouldReturnLocalDateTime() {
        assertThat(new Clock().now(), instanceOf(LocalDate.class));
    }

    @Test
    public void shouldReturnFalseWhenDateMoreThanSixDays() {
        assertFalse(this.clock.isDateBetweenRange(LOCAL_DATE.plusDays(7), 6));
    }

    @Test
    public void shouldReturnTrueWhenDateLessIsSixDaysOrLess() {
        assertFalse(this.clock.isDateBetweenRange(LOCAL_DATE.plusDays(6), 6));
    }

    @Test
    public void shouldReturnTrueWhenSameDate() {
        assertTrue(this.clock.isSameDate(LOCAL_DATE, LOCAL_DATE));
    }

    @Test
    public void shouldReturnFalseWhenDateIsBigger() {
        assertFalse(this.clock.isSameDate(LOCAL_DATE, LOCAL_DATE.plusDays(1)));
    }

    @Test
    public void shouldReturnTrueWhenDateIsSmaller() {
        assertFalse(this.clock.isSameDate(LOCAL_DATE, LOCAL_DATE.minusDays(1)));
    }

    @Test
    public void shouldParseDateFromString() {
        assertEquals(this.clock.stringToDate(DATE_STRING), getDate(DATE_STRING, DATE_FORMAT));
    }

    @Test(expected = IllegalStateException.class)
    public void shouldThrowExceptionWhenDateIsIncorrect() {
        this.clock.stringToDate("asdf");
    }
}