package training.weather;

import org.apache.commons.io.IOUtils;
import training.weather.models.City;
import training.weather.models.ConsolidatedWeather;
import training.weather.models.Prediction;

import java.io.IOException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.util.Date;
import java.util.List;

import static java.time.ZoneId.systemDefault;
import static java.util.Collections.singletonList;

public class WeatherForecastFixture {
    public static final LocalDate LOCAL_DATE_NOW = LocalDate.now();
    public static final Date DATE_NOW = new Date();
    public static final LocalDate DATE_NOW_LOCAL_DATE = DATE_NOW.toInstant()
            .atZone(systemDefault())
            .toLocalDate();
    public static final String DATE_STRING = "12-01-2020";
    public static final String FETCH_GET_URL = "http://www.google.es/";
    public static final String FETCH_GET_RESULT = "hola";
    public static final String CITY_WOEID = "766273";
    public static final String CITY_WOEID_1 = "321";
    public static final City CITY = new City(CITY_WOEID);
    public static final City CITY_1 = new City(CITY_WOEID_1);
    public static final String CITY_NAME = "Madrid";
    public static final String WEATHER_STATE_NAME = "123";
    public static final LocalDate LOCAL_DATE = LocalDate.now();
    public static final Prediction PREDICTION = new Prediction(WEATHER_STATE_NAME, LOCAL_DATE);
    public static final List<Prediction> PREDICTIONS = singletonList(PREDICTION);
    public static final ConsolidatedWeather CONSOLIDATED_WEATHER = new ConsolidatedWeather(PREDICTIONS);
    public static final String DATE_FORMAT = "yyyy-MM-dd";

    public static Date getDate(final String date, final String format) {
        try {
            return new SimpleDateFormat(format).parse(date);
        } catch (final ParseException e) {
            throw new IllegalStateException();
        }
    }

    static Date getDate(final String date) {
        try {
            return new SimpleDateFormat(DATE_FORMAT).parse(date);
        } catch (final ParseException e) {
            throw new IllegalStateException();
        }
    }

    static String getResourceAsString(final String file) {
        try {
            return IOUtils.toString(
                    WeatherForecastIntegrationTest.class.getResourceAsStream(file),
                    "UTF-8"
            );
        } catch (final IOException e) {
            throw new IllegalStateException();
        }
    }
}
