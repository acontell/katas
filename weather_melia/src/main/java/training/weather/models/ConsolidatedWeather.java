package training.weather.models;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.util.List;

@Data
@JsonIgnoreProperties(ignoreUnknown = true)
public final class ConsolidatedWeather {
    private final List<Prediction> predictions;

    @JsonCreator
    public ConsolidatedWeather(@JsonProperty("consolidated_weather") final List<Prediction> predictions) {
        this.predictions = predictions;
    }
}
