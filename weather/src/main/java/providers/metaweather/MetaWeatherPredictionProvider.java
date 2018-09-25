package providers.metaweather;

import httpclients.HttpClient;
import models.Prediction;
import org.json.JSONArray;
import org.json.JSONObject;
import providers.PredictionProvider;

import java.io.IOException;
import java.util.List;

import static providers.metaweather.MetaWeatherPredictionParser.parse;

public class MetaWeatherPredictionProvider implements PredictionProvider {
    private static final String CITY_URL = "https://www.metaweather.com/api/location/search/?query=";
    private static final String PREDICTION_URL = "https://www.metaweather.com/api/location/";
    private static final String WOEID_KEY = "woeid";
    private static final String CONSOLIDATED_WEATHER_KEY = "consolidated_weather";

    private final HttpClient client;

    public MetaWeatherPredictionProvider(final HttpClient client) {
        this.client = client;
    }

    @Override
    public List<Prediction> getPredictions(final String cityName) throws IOException {
        return parse(getPredictionsByWoeid(getWoeid(cityName)));
    }

    private JSONArray getPredictionsByWoeid(final String woeid) throws IOException {
        return new JSONObject(this.client.getUrl(PREDICTION_URL + woeid))
                .getJSONArray(CONSOLIDATED_WEATHER_KEY);
    }

    private String getWoeid(final String cityName) throws IOException {
        return new JSONArray(this.client.getUrl(CITY_URL + cityName))
                .getJSONObject(0)
                .get(WOEID_KEY)
                .toString();
    }
}
