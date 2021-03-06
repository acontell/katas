package training.weather.services;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import training.weather.io.RestTemplate;
import training.weather.models.ConsolidatedWeather;

import java.io.IOException;

import static java.lang.String.format;
import static org.junit.Assert.assertEquals;
import static org.mockito.BDDMockito.given;
import static training.weather.WeatherForecastFixture.CITY;
import static training.weather.WeatherForecastFixture.CITY_WOEID;
import static training.weather.WeatherForecastFixture.CONSOLIDATED_WEATHER;
import static training.weather.services.ConsolidatedWeatherService.URL_TEMPLATE;

@RunWith(MockitoJUnitRunner.class)
public class ConsolidatedWeatherServiceTest {
    private static final String CONSOLIDATED_WEATHER_FETCH_URL = format(URL_TEMPLATE, CITY_WOEID);

    @Mock
    private RestTemplate restTemplate;

    @Test
    public void should_return_consolidated_weather_from_city() throws IOException {
        given(this.restTemplate.getForObject(CONSOLIDATED_WEATHER_FETCH_URL, ConsolidatedWeather.class)).willReturn(CONSOLIDATED_WEATHER);
        final ConsolidatedWeather actual = new ConsolidatedWeatherService(this.restTemplate).getConsolidatedWeather(CITY);
        assertEquals(CONSOLIDATED_WEATHER, actual);
    }
}