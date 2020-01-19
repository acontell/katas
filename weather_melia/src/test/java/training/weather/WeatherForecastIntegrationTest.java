package training.weather;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.fasterxml.jackson.module.paramnames.ParameterNamesModule;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import training.weather.io.HttpClient;
import training.weather.io.RestTemplate;
import training.weather.services.CityService;
import training.weather.services.ConsolidatedWeatherService;
import training.weather.services.PredictionService;
import training.weather.services.WeatherForecast;
import training.weather.time.Clock;

import java.io.IOException;

import static java.lang.String.format;
import static org.junit.Assert.assertEquals;
import static org.mockito.BDDMockito.given;
import static training.weather.WeatherForecastFixture.CITY_NAME;
import static training.weather.WeatherForecastFixture.CITY_WOEID;
import static training.weather.WeatherForecastFixture.DATE_IN_RANGE;
import static training.weather.WeatherForecastFixture.DATE_IN_RANGE_NOT_FOUND;
import static training.weather.WeatherForecastFixture.DATE_NOT_IN_RANGE;
import static training.weather.WeatherForecastFixture.PREDICTION_FOR_DATE_IN_RANGE;
import static training.weather.WeatherForecastFixture.getResourceAsString;

@RunWith(MockitoJUnitRunner.class)
public class WeatherForecastIntegrationTest {
    private static final String NO_WEATHER_STATE = "";
    private static final String CITY_URL = format("https://www.metaweather.com/api/location/search/?query=%s", CITY_NAME);
    private static final String WEATHER_URL = format("https://www.metaweather.com/api/location/%s", CITY_WOEID);
    private static final ObjectMapper MAPPER = new ObjectMapper()
            .registerModule(new ParameterNamesModule())
            .registerModule(new Jdk8Module())
            .registerModule(new JavaTimeModule());
    private static final String CITIES = getResourceAsString("cities.json");
    private static final String WEATHER = getResourceAsString("weather.json");

    @Mock
    private HttpClient httpClient;

    private WeatherForecast weatherForecast;

    @Before
    public void setUp() {
        final RestTemplate restTemplate = new RestTemplate(this.httpClient, MAPPER);
        final CityService cityService = new CityService(restTemplate);
        final ConsolidatedWeatherService consolidatedWeatherService = new ConsolidatedWeatherService(restTemplate);
        final PredictionService predictionService = new PredictionService(cityService, consolidatedWeatherService);
        final Clock clock = new Clock();
        this.weatherForecast = new WeatherForecast(predictionService, clock);
    }

    @Test
    public void should_return_weather_prediction_for_city_in_date() throws IOException {
        given_http_client_return_results();
        final String actual = this.weatherForecast.getCityWeather(CITY_NAME, DATE_IN_RANGE);
        assertEquals(PREDICTION_FOR_DATE_IN_RANGE, actual);
    }

    private void given_http_client_return_results() throws IOException {
        given(this.httpClient.fetchGet(CITY_URL)).willReturn(CITIES);
        given(this.httpClient.fetchGet(WEATHER_URL)).willReturn(WEATHER);
    }

    @Test
    public void should_return_no_result_when_date_not_in_range() throws IOException {
        given_http_client_return_results();
        final String actual = this.weatherForecast.getCityWeather(CITY_NAME, DATE_IN_RANGE_NOT_FOUND);
        assertEquals(NO_WEATHER_STATE, actual);

    }

    @Test
    public void should_return_no_result_when_is_more_than_six_days_away() throws IOException {
        given_http_client_return_results();
        final String actual = this.weatherForecast.getCityWeather(CITY_NAME, DATE_NOT_IN_RANGE);
        assertEquals(NO_WEATHER_STATE, actual);
    }

    @Test(expected = IOException.class)
    public void should_propagate_io_exception() throws IOException {
        given(this.httpClient.fetchGet(CITY_URL)).willThrow(new IOException());
        this.weatherForecast.getCityWeather(CITY_NAME, DATE_IN_RANGE);
    }
}
