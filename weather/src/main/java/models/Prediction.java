package models;

import java.time.LocalDate;

public class Prediction {
    private final LocalDate when;
    private final String windSpeed;
    private final String weatherState;

    private Prediction(final LocalDate when, final String windSpeed, final String weatherState) {
        this.when = when;
        this.windSpeed = windSpeed;
        this.weatherState = weatherState;
    }

    public static Prediction of(final LocalDate when, final String windSpeed, final String weatherState) {
        return new Prediction(when, windSpeed, weatherState);
    }

    public LocalDate getWhen() {
        return this.when;
    }

    public String getWindSpeed() {
        return this.windSpeed;
    }

    public String getWeatherState() {
        return this.weatherState;
    }
}
