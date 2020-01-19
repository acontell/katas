package training.weather.services;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import training.weather.time.Clock;

import java.io.IOException;

import static java.util.Optional.of;
import static org.junit.Assert.assertEquals;
import static org.mockito.BDDMockito.given;
import static training.weather.WeatherForecastFixture.CITY_NAME;
import static training.weather.WeatherForecastFixture.DATE_IN_RANGE;
import static training.weather.WeatherForecastFixture.DATE_IN_RANGE_LOCAL_DATE;
import static training.weather.WeatherForecastFixture.DATE_NOT_IN_RANGE;
import static training.weather.WeatherForecastFixture.DATE_NOT_IN_RANGE_LOCAL_DATE;
import static training.weather.WeatherForecastFixture.PREDICTION;
import static training.weather.WeatherForecastFixture.PREDICTION_FOR_DATE_IN_RANGE;
import static training.weather.services.WeatherForecast.NO_WEATHER_STATE;
import static training.weather.services.WeatherForecast.OFFSET_IN_DAYS;

@RunWith(MockitoJUnitRunner.class)
public class WeatherForecastTest {
    @Mock
    private PredictionService predictionService;
    @Mock
    private Clock clock;

    private WeatherForecast weatherForecast;

    @Before
    public void setUp() {
        this.weatherForecast = new WeatherForecast(this.predictionService, this.clock);
    }

    @Test
    public void should_create_now_when_date_is_null() throws IOException {
        given_has_prediction();
        given_clock_works_with_null();
        final String actual = this.weatherForecast.getCityWeather(CITY_NAME, null);
        assertEquals(PREDICTION_FOR_DATE_IN_RANGE, actual);
    }

    private void given_has_prediction() throws IOException {
        given(this.predictionService.getPrediction(CITY_NAME, DATE_IN_RANGE_LOCAL_DATE)).willReturn(of(PREDICTION));
        given(this.clock.getLocalDateOrNow(DATE_IN_RANGE)).willReturn(DATE_IN_RANGE_LOCAL_DATE);
        given(this.clock.isDateBetweenRange(DATE_IN_RANGE_LOCAL_DATE, OFFSET_IN_DAYS)).willReturn(true);
    }

    private void given_clock_works_with_null() {
        given(this.clock.getLocalDateOrNow(null)).willReturn(DATE_IN_RANGE_LOCAL_DATE);
        given(this.clock.isDateBetweenRange(DATE_IN_RANGE_LOCAL_DATE, OFFSET_IN_DAYS)).willReturn(true);
    }

    @Test
    public void should_return_no_results_when_date_is_not_in_range() throws IOException {
        given(this.clock.isDateBetweenRange(DATE_NOT_IN_RANGE_LOCAL_DATE, OFFSET_IN_DAYS)).willReturn(false);
        final String actual = this.weatherForecast.getCityWeather(CITY_NAME, DATE_NOT_IN_RANGE);
        assertEquals(NO_WEATHER_STATE, actual);
    }

    @Test
    public void should_return_prediction_when_date_is_in_range() throws IOException {
        given_has_prediction();
        final String actual = this.weatherForecast.getCityWeather(CITY_NAME, DATE_IN_RANGE);
        assertEquals(PREDICTION_FOR_DATE_IN_RANGE, actual);
    }
}