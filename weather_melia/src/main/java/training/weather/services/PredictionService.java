package training.weather.services;

import training.weather.models.City;
import training.weather.models.Prediction;

import java.io.IOException;
import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

public class PredictionService {
    private final CityService cityService;
    private final ConsolidatedWeatherService consolidatedWeatherService;

    public PredictionService(final CityService cityService,
                             final ConsolidatedWeatherService consolidatedWeatherService) {
        this.cityService = cityService;
        this.consolidatedWeatherService = consolidatedWeatherService;
    }

    Optional<Prediction> getPrediction(final String cityName, final LocalDate localDate) throws IOException {
        return this.getPrediction(this.cityService.getCity(cityName), localDate);
    }

    private Optional<Prediction> getPrediction(final City city, final LocalDate localDate) throws IOException {
        return getPredictions(city).stream()
                .filter(prediction -> isSameDate(localDate, prediction.getLocalDate()))
                .findAny();
    }

    private boolean isSameDate(final LocalDate localDate, final LocalDate otherLocalDate) {
        return localDate.compareTo(otherLocalDate) == 0;
    }

    private List<Prediction> getPredictions(final City city) throws IOException {
        return this.consolidatedWeatherService.getConsolidatedWeather(city).getPredictions();
    }
}
