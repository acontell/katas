package training.weather.services;

import training.weather.io.RestTemplate;
import training.weather.models.City;
import training.weather.models.ConsolidatedWeather;

import java.io.IOException;

import static java.lang.String.format;

public class ConsolidatedWeatherService {
    static final String URL_TEMPLATE = "https://www.metaweather.com/api/location/%s";
    private final RestTemplate restTemplate;

    public ConsolidatedWeatherService(final RestTemplate restTemplate) {
        this.restTemplate = restTemplate;
    }

    public ConsolidatedWeather getConsolidatedWeather(final City city) throws IOException {
        return this.restTemplate.getForObject(format(URL_TEMPLATE, city.getWoeid()), ConsolidatedWeather.class);
    }
}
