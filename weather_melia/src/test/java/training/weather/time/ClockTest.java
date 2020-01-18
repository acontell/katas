package training.weather.time;

import org.junit.Before;
import org.junit.Test;

import java.time.LocalDate;

import static org.hamcrest.core.IsInstanceOf.instanceOf;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.spy;
import static training.weather.WeatherForecastFixture.DATE_NOW;
import static training.weather.WeatherForecastFixture.DATE_NOW_LOCAL_DATE;
import static training.weather.WeatherForecastFixture.DATE_STRING;
import static training.weather.WeatherForecastFixture.LOCAL_DATE_NOW;
import static training.weather.WeatherForecastFixture.getDate;
import static training.weather.time.Clock.DATE_FORMAT;

public class ClockTest {
    private Clock clock;

    @Before
    public void setUp() {
        this.clock = spy(new Clock());
        doReturn(LOCAL_DATE_NOW).when(this.clock).now();
    }

    @Test
    public void shouldReturnLocalDateTimeWhenDateIsNotNull() {
        assertEquals(this.clock.getLocalDateOrNow(DATE_NOW), DATE_NOW_LOCAL_DATE);
    }

    @Test
    public void shouldReturnNowWhenDateIsNull() {
        assertEquals(this.clock.getLocalDateOrNow(null), LOCAL_DATE_NOW);
    }

    @Test
    public void shouldReturnLocalDateTime() {
        assertThat(new Clock().now(), instanceOf(LocalDate.class));
    }

    @Test
    public void shouldReturnFalseWhenDateMoreThanSixDays() {
        assertFalse(this.clock.isDateBetweenRange(LOCAL_DATE_NOW.plusDays(7), 6));
    }

    @Test
    public void shouldReturnTrueWhenDateLessIsSixDaysOrLess() {
        assertFalse(this.clock.isDateBetweenRange(LOCAL_DATE_NOW.plusDays(6), 6));
    }

    @Test
    public void shouldReturnTrueWhenSameDate() {
        assertTrue(this.clock.isSameDate(LOCAL_DATE_NOW, LOCAL_DATE_NOW));
    }

    @Test
    public void shouldReturnFalseWhenDateIsBigger() {
        assertFalse(this.clock.isSameDate(LOCAL_DATE_NOW, LOCAL_DATE_NOW.plusDays(1)));
    }

    @Test
    public void shouldReturnTrueWhenDateIsSmaller() {
        assertFalse(this.clock.isSameDate(LOCAL_DATE_NOW, LOCAL_DATE_NOW.minusDays(1)));
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