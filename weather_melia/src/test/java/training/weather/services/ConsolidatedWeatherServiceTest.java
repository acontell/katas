package training.weather.services;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import training.weather.io.RestTemplate;
import training.weather.models.City;
import training.weather.models.ConsolidatedWeather;

import java.io.IOException;

import static java.lang.String.format;
import static java.util.Collections.emptyList;
import static org.junit.Assert.assertEquals;
import static org.mockito.BDDMockito.given;
import static training.weather.services.ConsolidatedWeatherService.URL_TEMPLATE;

@RunWith(MockitoJUnitRunner.class)
public class ConsolidatedWeatherServiceTest {
    private static final String CITY_WOEID = "123";
    private static final String URL = format(URL_TEMPLATE, CITY_WOEID);
    private static final ConsolidatedWeather CONSOLIDATED_WEATHER = new ConsolidatedWeather(emptyList());
    private static final City CITY = new City(CITY_WOEID);

    @Mock
    private RestTemplate restTemplate;

    @Test
    public void shouldReturnInformationFromCity() throws IOException {
        final ConsolidatedWeatherService consolidatedWeatherService = new ConsolidatedWeatherService(this.restTemplate);
        given(this.restTemplate.getForObject(URL, ConsolidatedWeather.class)).willReturn(CONSOLIDATED_WEATHER);
        assertEquals(consolidatedWeatherService.getConsolidatedWeather(CITY), CONSOLIDATED_WEATHER);
    }
}