import java.util.Date;

public interface ForecastService {
    String predictWeather(final String cityName, final Date when);

    String predictWind(final String cityName, final Date when);
}
