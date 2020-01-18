package training.weather.models;

import org.junit.Test;

import static nl.jqno.equalsverifier.EqualsVerifier.forClass;
import static org.junit.Assert.assertEquals;
import static training.weather.WeatherForecastFixture.LOCAL_DATE;
import static training.weather.WeatherForecastFixture.PREDICTION;
import static training.weather.WeatherForecastFixture.PREDICTION_FOR_DATE_IN_RANGE;

public class PredictionTest {
    @Test
    public void bean_test() {
        assertEquals(PREDICTION.getWeatherStateName(), PREDICTION_FOR_DATE_IN_RANGE);
        assertEquals(PREDICTION.getLocalDate(), LOCAL_DATE);
    }

    @Test
    public void equals_verifier_tests() {
        forClass(Prediction.class).verify();
    }
}