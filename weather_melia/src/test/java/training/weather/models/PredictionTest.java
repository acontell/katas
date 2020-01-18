package training.weather.models;

import org.junit.Test;

import static nl.jqno.equalsverifier.EqualsVerifier.forClass;
import static org.junit.Assert.assertEquals;
import static training.weather.WeatherForecastFixture.LOCAL_DATE;
import static training.weather.WeatherForecastFixture.PREDICTION;
import static training.weather.WeatherForecastFixture.WEATHER_STATE_NAME;

public class PredictionTest {
    @Test
    public void beanTest() {
        assertEquals(PREDICTION.getWeatherStateName(), WEATHER_STATE_NAME);
        assertEquals(PREDICTION.getLocalDate(), LOCAL_DATE);
    }

    @Test
    public void equalsVerifierTest() {
        forClass(Prediction.class).verify();
    }
}