package training.weather;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import training.weather.models.City;
import training.weather.models.ConsolidatedWeather;
import training.weather.models.Prediction;
import training.weather.services.CityService;
import training.weather.services.ConsolidatedWeatherService;
import training.weather.time.Clock;

import java.io.IOException;
import java.time.LocalDate;
import java.util.Date;
import java.util.List;

import static java.time.ZoneId.systemDefault;
import static java.time.format.DateTimeFormatter.ISO_DATE;
import static java.util.Arrays.asList;
import static org.junit.Assert.assertEquals;
import static org.mockito.BDDMockito.given;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.doCallRealMethod;
import static training.weather.WeatherForecast.NO_WEATHER_STATE;
import static training.weather.WeatherForecast.OFFSET_IN_DAYS;
import static training.weather.WeatherForecastFixture.getDate;

@RunWith(MockitoJUnitRunner.class)
public class WeatherForecastTest {
    private static final String DATE_STRING = "2020-01-17";
    private static final String DATE_STRING_1 = "2020-01-19";
    private static final Date DATE = getDate(DATE_STRING_1);
    private static final Date DATE_NOT_IN_RANGE = getDate("2021-01-19");
    private static final Date DATE_NOT_IN_RESULTS_BUT_IN_RANGE = getDate("2020-01-21");
    private static final String PREDICTION = "Clear";
    private static final String PREDICTION_1 = "Clear";
    private static final List<Prediction> PREDICTIONS = asList(new Prediction(PREDICTION, LocalDate.parse(DATE_STRING, ISO_DATE)), new Prediction(PREDICTION_1, LocalDate.parse(DATE_STRING_1, ISO_DATE)));
    private final static ConsolidatedWeather CONSOLIDATED_WEATHER = new ConsolidatedWeather(PREDICTIONS);
    private static final City CITY = new City("123");
    private static final String CITY_NAME = "Madrid";

    @Mock
    private CityService cityService;
    @Mock
    private ConsolidatedWeatherService consolidatedWeatherService;
    @Mock
    private Clock clock;

    private WeatherForecast weatherForecast;

    @Before
    public void setUp() throws Exception {
        this.weatherForecast = new WeatherForecast(this.cityService, this.consolidatedWeatherService, this.clock);
        given(this.cityService.getCity(CITY_NAME)).willReturn(CITY);
        given(this.consolidatedWeatherService.getConsolidatedWeather(CITY)).willReturn(CONSOLIDATED_WEATHER);
        given(this.clock.getLocalDateOrNow(DATE)).willReturn(this.dateToLocalDate(DATE));
        doCallRealMethod().when(this.clock).isSameDate(any(), any());
    }

    private LocalDate dateToLocalDate(final Date dateTime) {
        return dateTime.toInstant()
                .atZone(systemDefault())
                .toLocalDate();
    }

    @Test
    public void shouldCreateDateNowWhenDateIsNull() throws IOException {
        final String forecastToday = weatherForecast.getCityWeather(CITY_NAME, DATE);
        final String expectedForecast = weatherForecast.getCityWeather(CITY_NAME, null);
        assertEquals(forecastToday, expectedForecast);
    }

    @Test
    public void shouldReturnEmptyStringWhenDateIsNotInRange() throws IOException {
        given(this.clock.isDateBetweenRange(this.dateToLocalDate(DATE_NOT_IN_RANGE), OFFSET_IN_DAYS)).willReturn(false);
        final String forecastToday = weatherForecast.getCityWeather(CITY_NAME, DATE_NOT_IN_RANGE);
        assertEquals(forecastToday, NO_WEATHER_STATE);
    }

    @Test
    public void shouldReturnPredictionWhenIsInRange() throws IOException {
        given(this.clock.isDateBetweenRange(this.dateToLocalDate(DATE), OFFSET_IN_DAYS)).willReturn(true);
        final String forecastToday = this.weatherForecast.getCityWeather(CITY_NAME, DATE);
        assertEquals(forecastToday, PREDICTION_1);
    }

    @Test
    public void shouldReturnEmptyStringWhenDateIsInRangeButNotInResponse() throws IOException {
        given(this.clock.isDateBetweenRange(this.dateToLocalDate(DATE_NOT_IN_RESULTS_BUT_IN_RANGE), OFFSET_IN_DAYS)).willReturn(true);
        final String forecastToday = weatherForecast.getCityWeather(CITY_NAME, DATE_NOT_IN_RESULTS_BUT_IN_RANGE);
        assertEquals(forecastToday, NO_WEATHER_STATE);
    }
}