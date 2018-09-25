package providers;

import models.Prediction;

import java.io.IOException;
import java.util.List;

public interface PredictionProvider {
    List<Prediction> getPredictions(final String cityName) throws IOException;
}
