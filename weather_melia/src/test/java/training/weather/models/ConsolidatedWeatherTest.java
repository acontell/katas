package training.weather.models;

import nl.jqno.equalsverifier.EqualsVerifier;
import org.junit.Test;

import java.time.LocalDate;
import java.util.List;

import static java.util.Collections.singletonList;
import static org.junit.Assert.assertEquals;

public class ConsolidatedWeatherTest {
    private static final List<Prediction> PREDICTIONS = singletonList(new Prediction("", LocalDate.now()));

    @Test
    public void beanTest() {
        final ConsolidatedWeather consolidatedWeather = new ConsolidatedWeather(PREDICTIONS);
        assertEquals(consolidatedWeather.getPredictions(), PREDICTIONS);
    }

    @Test
    public void equalsVerifierTest() {
        EqualsVerifier.forClass(ConsolidatedWeather.class).verify();
    }
}