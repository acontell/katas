package training.weather;

import training.weather.models.City;
import training.weather.models.Prediction;
import training.weather.services.CityService;
import training.weather.services.ConsolidatedWeatherService;
import training.weather.time.Clock;

import java.io.IOException;
import java.time.LocalDate;
import java.util.Date;
import java.util.List;
import java.util.Optional;

public class WeatherForecast {
    static final int OFFSET_IN_DAYS = 6;
    static final String NO_WEATHER_STATE = "";

    private final CityService cityService;
    private final ConsolidatedWeatherService consolidatedWeatherService;
    private final Clock clock;

    public WeatherForecast(final CityService cityService,
                           final ConsolidatedWeatherService consolidatedWeatherService,
                           final Clock clock) {
        this.cityService = cityService;
        this.consolidatedWeatherService = consolidatedWeatherService;
        this.clock = clock;
    }

    public String getCityWeather(final String cityName, final Date dateTime) throws IOException {
        return getCityWeather(cityName, this.clock.getLocalDateOrNow(dateTime));
    }

    private String getCityWeather(final String cityName, final LocalDate localDate) throws IOException {
        return this.clock.isDateBetweenRange(localDate, OFFSET_IN_DAYS)
                ? getWeatherStateName(this.cityService.getCity(cityName), localDate)
                : NO_WEATHER_STATE;
    }

    private String getWeatherStateName(final City city, final LocalDate localDate) throws IOException {
        return this.getPrediction(city, localDate)
                .map(Prediction::getWeatherStateName)
                .orElse(NO_WEATHER_STATE);
    }


    private Optional<Prediction> getPrediction(final City city, final LocalDate localDate) throws IOException {
        return getPredictions(city).stream()
                .filter(prediction -> this.clock.isSameDate(prediction.getLocalDate(), localDate))
                .findAny();
    }

    private List<Prediction> getPredictions(final City city) throws IOException {
        return this.consolidatedWeatherService.getConsolidatedWeather(city).getPredictions();
    }
}
