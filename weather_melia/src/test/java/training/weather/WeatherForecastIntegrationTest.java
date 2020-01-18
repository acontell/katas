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
import training.weather.time.Clock;

import java.io.IOException;
import java.util.Date;

import static java.lang.String.format;
import static org.junit.Assert.assertEquals;
import static org.mockito.BDDMockito.given;
import static training.weather.WeatherForecastFixture.getDate;
import static training.weather.WeatherForecastFixture.getResourceAsString;

@RunWith(MockitoJUnitRunner.class)
public class WeatherForecastIntegrationTest {
    private static final String NO_WEATHER_STATE = "";
    private static final String CITY_NAME = "Madrid";
    private static final String CITY_WOEID = "766273";
    private static final String CITY_URL = format("https://www.metaweather.com/api/location/search/?query=%s", CITY_NAME);
    private static final String WEATHER_URL = format("https://www.metaweather.com/api/location/%s", CITY_WOEID);
    private static final Date DATE_IN_RANGE = getDate("2020-01-20");
    private static final Date DATE_NOT_IN_RANGE = getDate("2020-02-20");
    private static final Date DATE_IN_RANGE_NOT_FOUND = getDate("2020-01-21");
    private static final String PREDICTION_FOR_DATE_IN_RANGE = "Thunder";
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
    public void setUp() throws IOException {
        final RestTemplate restTemplate = new RestTemplate(this.httpClient, MAPPER);
        final CityService cityService = new CityService(restTemplate);
        final ConsolidatedWeatherService consolidatedWeatherService = new ConsolidatedWeatherService(restTemplate);
        final Clock clock = new Clock();
        this.weatherForecast = new WeatherForecast(cityService, consolidatedWeatherService, clock);
        given(this.httpClient.fetchGet(CITY_URL)).willReturn(CITIES);
        given(this.httpClient.fetchGet(WEATHER_URL)).willReturn(WEATHER);
    }

    @Test
    public void shouldReturnValue() throws IOException {
        final String weatherState = this.weatherForecast.getCityWeather(CITY_NAME, DATE_IN_RANGE);
        assertEquals(weatherState, PREDICTION_FOR_DATE_IN_RANGE);
    }

    @Test
    public void shouldReturnEmptyWhenDateNotFound() throws IOException {
        final String weatherState = this.weatherForecast.getCityWeather(CITY_NAME, DATE_IN_RANGE_NOT_FOUND);
        assertEquals(weatherState, NO_WEATHER_STATE);

    }

    @Test
    public void shouldReturnEmptyWhenDateIsMoreThanSixDaysAways() throws IOException {
        final String weatherState = this.weatherForecast.getCityWeather(CITY_NAME, DATE_NOT_IN_RANGE);
        assertEquals(weatherState, NO_WEATHER_STATE);
    }

    @Test(expected = IOException.class)
    public void shouldPropagateIoException() throws IOException {
        given(this.httpClient.fetchGet(CITY_URL)).willThrow(new IOException());
        this.weatherForecast.getCityWeather(CITY_NAME, DATE_IN_RANGE);
    }
}
