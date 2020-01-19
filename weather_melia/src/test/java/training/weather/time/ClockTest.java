package training.weather.time;

import org.junit.Before;
import org.junit.Test;

import java.time.LocalDate;
import java.util.Date;

import static org.hamcrest.core.IsInstanceOf.instanceOf;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.spy;
import static training.weather.WeatherForecastFixture.DATE_NOW;
import static training.weather.WeatherForecastFixture.DATE_NOW_LOCAL_DATE;
import static training.weather.WeatherForecastFixture.DATE_STRING_FROM_TERMINAL;
import static training.weather.WeatherForecastFixture.LOCAL_DATE;
import static training.weather.WeatherForecastFixture.getDate;
import static training.weather.time.Clock.DATE_FORMAT;

public class ClockTest {
    private static final int OFFSET_IN_DAYS = 6;
    private Clock clock;

    @Before
    public void setUp() {
        this.clock = spy(new Clock());
        doReturn(LOCAL_DATE).when(this.clock).now();
    }

    @Test
    public void should_return_local_date_when_date_is_not_null() {
        final LocalDate actual = this.clock.getLocalDateOrNow(DATE_NOW);
        assertEquals(DATE_NOW_LOCAL_DATE, actual);
    }

    @Test
    public void should_return_now_when_date_is_null() {
        final LocalDate actual = this.clock.getLocalDateOrNow(null);
        assertEquals(LOCAL_DATE, actual);
    }

    @Test
    public void should_return_local_date_object() {
        final LocalDate actual = new Clock().now();
        assertThat(actual, instanceOf(LocalDate.class));
    }

    @Test
    public void should_return_false_when_date_is_ahead_offset() {
        final boolean actual = this.clock.isDateBetweenRange(LOCAL_DATE.plusDays(OFFSET_IN_DAYS + 1), OFFSET_IN_DAYS);
        assertFalse(actual);
    }

    @Test
    public void should_return_false_when_date_is_equal_to_offset() {
        final boolean actual = this.clock.isDateBetweenRange(LOCAL_DATE.plusDays(OFFSET_IN_DAYS), OFFSET_IN_DAYS);
        assertFalse(actual);
    }

    @Test
    public void should_return_true_when_date_is_before_to_offset() {
        final boolean actual = this.clock.isDateBetweenRange(LOCAL_DATE.plusDays(OFFSET_IN_DAYS - 1), OFFSET_IN_DAYS);
        assertTrue(actual);
    }

    @Test
    public void should_parse_date_from_string() {
        final Date actual = this.clock.stringToDate(DATE_STRING_FROM_TERMINAL);
        assertEquals(getDate(DATE_STRING_FROM_TERMINAL, DATE_FORMAT), actual);
    }

    @Test(expected = IllegalStateException.class)
    public void should_throw_exception_when_date_is_malformed() {
        this.clock.stringToDate("asdf");
    }
}