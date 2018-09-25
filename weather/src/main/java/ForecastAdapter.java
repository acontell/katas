import java.io.IOException;
import java.util.Date;

public class ForecastAdapter implements ForecastService {
    private final Forecast forecast;

    public ForecastAdapter(Forecast forecast) {
        this.forecast = forecast;
    }

    @Override
    public String predictWeather(final String cityName, final Date when) {
        try {
            return this.forecast.predict(cityName, when, false);
        } catch (final IOException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public String predictWind(final String cityName, final Date when) {
        try {
            return this.forecast.predict(cityName, when, true);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}
