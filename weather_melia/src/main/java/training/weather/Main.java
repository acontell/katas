package training.weather;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.fasterxml.jackson.module.paramnames.ParameterNamesModule;
import training.weather.io.HttpClient;
import training.weather.io.RestTemplate;
import training.weather.services.CityService;
import training.weather.services.ConsolidatedWeatherService;
import training.weather.time.Clock;

import java.io.IOException;

public class Main {
    private static final ObjectMapper MAPPER = new ObjectMapper()
            .registerModule(new ParameterNamesModule())
            .registerModule(new Jdk8Module())
            .registerModule(new JavaTimeModule());
    private static final HttpClient HTTP_CLIENT = new HttpClient();
    private static final RestTemplate REST_TEMPLATE = new RestTemplate(HTTP_CLIENT, MAPPER);
    private static final CityService CITY_SERVICE = new CityService(REST_TEMPLATE);
    private static final ConsolidatedWeatherService CONSOLIDATED_WEATHER_SERVICE = new ConsolidatedWeatherService(REST_TEMPLATE);
    private static final Clock CLOCK = new Clock();

    public static void main(final String[] args) throws IOException {
        final String cityWeather = new WeatherForecast(CITY_SERVICE, CONSOLIDATED_WEATHER_SERVICE, CLOCK)
                .getCityWeather(args[0], CLOCK.stringToDate(args[1]));
        System.out.println(cityWeather);
    }
}
