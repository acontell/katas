package training.weather.services;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import training.weather.time.Clock;

import java.io.IOException;

import static org.junit.Assert.assertEquals;
import static org.mockito.BDDMockito.given;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.doCallRealMethod;
import static training.weather.WeatherForecastFixture.CITY;
import static training.weather.WeatherForecastFixture.CITY_NAME;
import static training.weather.WeatherForecastFixture.CONSOLIDATED_WEATHER;
import static training.weather.WeatherForecastFixture.DATE_IN_RANGE;
import static training.weather.WeatherForecastFixture.DATE_IN_RANGE_NOT_FOUND;
import static training.weather.WeatherForecastFixture.DATE_IN_RANGE_NOT_FOUND_TO_LOCAL_DATE;
import static training.weather.WeatherForecastFixture.DATE_IN_RANGE_TO_LOCAL_DATE;
import static training.weather.WeatherForecastFixture.DATE_NOT_IN_RANGE;
import static training.weather.WeatherForecastFixture.DATE_NOT_IN_RANGE_TO_LOCAL_DATE;
import static training.weather.WeatherForecastFixture.PREDICTION_1;
import static training.weather.WeatherForecastFixture.PREDICTION_FOR_DATE_IN_RANGE_1;
import static training.weather.services.WeatherForecast.NO_WEATHER_STATE;
import static training.weather.services.WeatherForecast.OFFSET_IN_DAYS;

@RunWith(MockitoJUnitRunner.class)
public class WeatherForecastTest {
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
        given(this.clock.getLocalDateOrNow(DATE_IN_RANGE)).willReturn(DATE_IN_RANGE_TO_LOCAL_DATE);
        given(this.clock.getLocalDateOrNow(null)).willReturn(DATE_IN_RANGE_TO_LOCAL_DATE);
        doCallRealMethod().when(this.clock).isSameDate(any(), any());
    }

    @Test
    public void should_create_now_when_date_is_null() throws IOException {
        given(this.clock.isDateBetweenRange(DATE_IN_RANGE_TO_LOCAL_DATE, OFFSET_IN_DAYS)).willReturn(true);
        final String actual = this.weatherForecast.getCityWeather(CITY_NAME, null);
        assertEquals(PREDICTION_FOR_DATE_IN_RANGE_1, actual);
    }

    @Test
    public void should_return_no_results_when_date_is_not_in_range() throws IOException {
        given(this.clock.isDateBetweenRange(DATE_NOT_IN_RANGE_TO_LOCAL_DATE, OFFSET_IN_DAYS)).willReturn(false);
        final String forecastToday = weatherForecast.getCityWeather(CITY_NAME, DATE_NOT_IN_RANGE);
        assertEquals(forecastToday, NO_WEATHER_STATE);
    }

    @Test
    public void should_return_prediction_when_date_is_in_range() throws IOException {
        given(this.clock.isDateBetweenRange(DATE_IN_RANGE_TO_LOCAL_DATE, OFFSET_IN_DAYS)).willReturn(true);
        final String actual = this.weatherForecast.getCityWeather(CITY_NAME, DATE_IN_RANGE);
        assertEquals(PREDICTION_1.getWeatherStateName(), actual);
    }

    @Test
    public void should_return_no_results_when_date_in_range_but_not_in_results() throws IOException {
        given(this.clock.isDateBetweenRange(DATE_IN_RANGE_NOT_FOUND_TO_LOCAL_DATE, OFFSET_IN_DAYS)).willReturn(true);
        final String forecastToday = weatherForecast.getCityWeather(CITY_NAME, DATE_IN_RANGE_NOT_FOUND);
        assertEquals(forecastToday, NO_WEATHER_STATE);
    }
}