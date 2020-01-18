package training.weather.models;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.time.LocalDate;

@Data
@JsonIgnoreProperties(ignoreUnknown = true)
public final class Prediction {
    private final String weatherStateName;

    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd")
    private final LocalDate localDate;

    @JsonCreator
    public Prediction(@JsonProperty("weather_state_name") final String weatherStateName,
                      @JsonProperty("applicable_date") final LocalDate localDate) {
        this.weatherStateName = weatherStateName;
        this.localDate = localDate;
    }
}
