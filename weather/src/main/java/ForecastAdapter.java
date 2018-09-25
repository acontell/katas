import java.io.IOException;
import java.util.Date;

public class ForecastAdapter implements ForecastService {
    private final Forecast forecast;

    public ForecastAdapter(final Forecast forecast) {
        this.forecast = forecast;
    }

    @Override
    public String predictWeather(final String cityName, final Date when) {
        return predict(cityName, when, false);
    }

    @Override
    public String predictWind(final String cityName, final Date when) {
        return predict(cityName, when, true);
    }

    private String predict(final String cityName, final Date when, final boolean wind) {
        try {
            return this.forecast.predict(cityName, when, wind);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}
