package training.weather.services;

import training.weather.models.Prediction;
import training.weather.time.Clock;

import java.io.IOException;
import java.time.LocalDate;
import java.util.Date;

public class WeatherForecast {
    static final int OFFSET_IN_DAYS = 6;
    static final String NO_WEATHER_STATE = "";

    private final PredictionService predictionService;
    private final Clock clock;

    public WeatherForecast(final PredictionService predictionService,
                           final Clock clock) {
        this.predictionService = predictionService;
        this.clock = clock;
    }

    public String getCityWeather(final String cityName, final Date dateTime) throws IOException {
        return getCityWeather(cityName, this.clock.getLocalDateOrNow(dateTime));
    }

    private String getCityWeather(final String cityName, final LocalDate localDate) throws IOException {
        return this.clock.isDateBetweenRange(localDate, OFFSET_IN_DAYS)
                ? getPrediction(cityName, localDate)
                : NO_WEATHER_STATE;
    }

    private String getPrediction(final String cityName, final LocalDate localDate) throws IOException {
        return this.predictionService.getPrediction(cityName, localDate)
                .map(Prediction::getWeatherStateName)
                .orElse(NO_WEATHER_STATE);
    }
}
