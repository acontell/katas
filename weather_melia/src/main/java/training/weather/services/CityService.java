package training.weather.services;

import training.weather.io.RestTemplate;
import training.weather.models.City;

import java.io.IOException;

import static java.lang.String.format;

public class CityService {
    static final String URL_TEMPLATE = "https://www.metaweather.com/api/location/search/?query=%s";
    private final RestTemplate restTemplate;

    public CityService(final RestTemplate restTemplate) {
        this.restTemplate = restTemplate;
    }

    City getCity(final String cityName) throws IOException {
        return this.restTemplate.getForObject(format(URL_TEMPLATE, cityName), City[].class)[0];
    }
}
