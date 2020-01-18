package training.weather;

import org.apache.commons.io.IOUtils;

import java.io.IOException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

class WeatherForecastFixture {
    private static final String DATE_FORMAT = "yyyy-MM-dd";

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
