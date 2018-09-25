package providers.metaweather;

import models.Prediction;
import org.json.JSONArray;
import org.json.JSONObject;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;

import static models.Prediction.of;

class MetaWeatherPredictionParser {
    private static final String DATE_PATTERN = "yyyy-MM-dd";
    private static final String APPLICABLE_DATE_KEY = "applicable_date";
    private static final String WIND_SPEED_KEY = "wind_speed";
    private static final String WEATHER_STATE_NAME_KEY = "weather_state_name";

    static List<Prediction> parse(final JSONArray predictions) {
        final List<Prediction> result = new ArrayList<>();
        for (int i = 0; i < predictions.length(); i++) {
            result.add(parse(predictions.getJSONObject(i)));
        }
        return result;
    }

    private static Prediction parse(final JSONObject prediction) {
        return of(getDate(prediction), getWindSpeed(prediction), getWeatherState(prediction));
    }

    private static LocalDate getDate(final JSONObject prediction) {
        return toLocalDate(prediction.get(APPLICABLE_DATE_KEY).toString());
    }

    private static LocalDate toLocalDate(final String strDate) {
        return LocalDate.parse(strDate, DateTimeFormatter.ofPattern(DATE_PATTERN));
    }

    private static String getWindSpeed(final JSONObject prediction) {
        return prediction.get(WIND_SPEED_KEY).toString();
    }

    private static String getWeatherState(final JSONObject prediction) {
        return prediction.get(WEATHER_STATE_NAME_KEY).toString();
    }
}
