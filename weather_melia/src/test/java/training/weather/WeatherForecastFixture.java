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
import static java.util.Arrays.asList;

public class WeatherForecastFixture {
    public static final Date DATE_NOW = getDate("2020-01-19");
    public static final LocalDate DATE_NOW_LOCAL_DATE = DATE_NOW.toInstant()
            .atZone(systemDefault())
            .toLocalDate();
    public static final String DATE_STRING_FROM_TERMINAL = "12-01-2020";
    public static final Date DATE_IN_RANGE = getDate("2020-01-19");
    public static final LocalDate DATE_IN_RANGE_TO_LOCAL_DATE = toLocalDate(DATE_IN_RANGE);
    public static final Date DATE_NOT_IN_RANGE = getDate("2020-02-20");
    public static final LocalDate DATE_NOT_IN_RANGE_TO_LOCAL_DATE = toLocalDate(DATE_NOT_IN_RANGE);
    public static final Date DATE_IN_RANGE_NOT_FOUND = getDate("2020-01-21");
    public static final LocalDate DATE_IN_RANGE_NOT_FOUND_TO_LOCAL_DATE = toLocalDate(DATE_IN_RANGE_NOT_FOUND);
    public static final String PREDICTION_FOR_DATE_IN_RANGE = "Light Cloud";
    public static final String PREDICTION_FOR_DATE_IN_RANGE_1 = "Heavy Rain";
    public static final String FETCH_GET_URL = "http://www.google.es/";
    public static final String FETCH_GET_RESULT = "hola";
    public static final String CITY_WOEID = "766273";
    private static final String CITY_WOEID_1 = "321";
    public static final City CITY = new City(CITY_WOEID);
    public static final City CITY_1 = new City(CITY_WOEID_1);
    public static final String CITY_NAME = "Madrid";
    public static final LocalDate LOCAL_DATE = LocalDate.of(2020, 1, 18);
    public static final Prediction PREDICTION = new Prediction(PREDICTION_FOR_DATE_IN_RANGE, LOCAL_DATE);
    public static final Prediction PREDICTION_1 = new Prediction(PREDICTION_FOR_DATE_IN_RANGE_1, LOCAL_DATE.plusDays(1));
    public static final List<Prediction> PREDICTIONS = asList(PREDICTION, PREDICTION_1);
    public static final ConsolidatedWeather CONSOLIDATED_WEATHER = new ConsolidatedWeather(PREDICTIONS);
    private static final String DATE_FORMAT = "yyyy-MM-dd";

    private static LocalDate toLocalDate(final Date date) {
        return date.toInstant()
                .atZone(systemDefault())
                .toLocalDate();
    }

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
