package training.weather.services;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import training.weather.io.RestTemplate;
import training.weather.models.City;

import java.io.IOException;

import static java.lang.String.format;
import static org.junit.Assert.assertEquals;
import static org.mockito.BDDMockito.given;
import static training.weather.WeatherForecastFixture.CITY;
import static training.weather.WeatherForecastFixture.CITY_1;
import static training.weather.WeatherForecastFixture.CITY_NAME;
import static training.weather.services.CityService.URL_TEMPLATE;

@RunWith(MockitoJUnitRunner.class)
public class CityServiceTest {
    private static final String CITY_FETCH_URL = format(URL_TEMPLATE, CITY_NAME);

    @Mock
    private RestTemplate restTemplate;

    @Test
    public void shouldReturnInformationFromFirstCity() throws IOException {
        final CityService cityService = new CityService(this.restTemplate);
        given(this.restTemplate.getForObject(CITY_FETCH_URL, City[].class)).willReturn(new City[]{CITY, CITY_1});
        assertEquals(cityService.getCity(CITY_NAME), CITY);
    }
}