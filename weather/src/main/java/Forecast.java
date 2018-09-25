import httpclients.GoogleHttpClient;
import infrastructure.Clock;
import models.Prediction;
import providers.PredictionProvider;
import providers.metaweather.MetaWeatherPredictionProvider;

import java.io.IOException;
import java.time.LocalDate;
import java.util.Date;
import java.util.Optional;

class Forecast {

    private static final String EMPTY = "";
    private final PredictionProvider predictionProvider;
    private final Clock clock;

    Forecast() {
        this(new MetaWeatherPredictionProvider(new GoogleHttpClient()), new Clock());
    }

    private Forecast(final PredictionProvider predictionProvider, final Clock clock) {
        this.predictionProvider = predictionProvider;
        this.clock = clock;
    }

    String predict(final String city, final Date datetime, final boolean wind) throws IOException {
        return getPrediction(city, this.clock.localDateOrDefault(datetime))
                .map(p -> wind ? p.getWindSpeed() : p.getWeatherState())
                .orElse(EMPTY);
    }

    private Optional<Prediction> getPrediction(final String city, final LocalDate date) throws IOException {
        return this.predictionProvider.getPredictions(city)
                .stream()
                .filter(p -> date.equals(p.getWhen()))
                .findAny();
    }
}
