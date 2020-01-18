package training.weather.models;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

@Data
@JsonIgnoreProperties(ignoreUnknown = true)
public final class City {
    private final String woeid;

    @JsonCreator
    public City(@JsonProperty("woeid") final String woeid) {
        this.woeid = woeid;
    }
}