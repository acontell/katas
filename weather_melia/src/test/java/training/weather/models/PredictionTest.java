package training.weather.models;

import nl.jqno.equalsverifier.EqualsVerifier;
import org.junit.Test;

import java.time.LocalDate;

import static org.junit.Assert.assertEquals;

public class PredictionTest {
    private static final String WEATHER_STATE_NAME = "123";
    private static final LocalDate LOCAL_DATE = LocalDate.now();

    @Test
    public void beanTest() {
        final Prediction prediction = new Prediction(WEATHER_STATE_NAME, LOCAL_DATE);
        assertEquals(prediction.getWeatherStateName(), WEATHER_STATE_NAME);
        assertEquals(prediction.getLocalDate(), LOCAL_DATE);
    }

    @Test
    public void equalsVerifierTest() {
        EqualsVerifier.forClass(Prediction.class).verify();
    }
}