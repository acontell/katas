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
import static training.weather.services.CityService.URL_TEMPLATE;

@RunWith(MockitoJUnitRunner.class)
public class CityServiceTest {
    private static final String CITY_NAME = "asdf";
    private static final String URL = format(URL_TEMPLATE, CITY_NAME);
    private static final City CITY = new City("123");
    private static final City CITY_1 = new City("321");

    @Mock
    private RestTemplate restTemplate;

    @Test
    public void shouldReturnInformationFromFirstCity() throws IOException {
        final CityService cityService = new CityService(this.restTemplate);
        given(this.restTemplate.getForObject(URL, City[].class)).willReturn(new City[]{CITY, CITY_1});
        assertEquals(cityService.getCity(CITY_NAME), CITY);
    }
}