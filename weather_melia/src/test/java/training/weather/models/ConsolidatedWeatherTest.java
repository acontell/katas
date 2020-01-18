package training.weather.models;

import org.junit.Test;

import static nl.jqno.equalsverifier.EqualsVerifier.forClass;
import static org.junit.Assert.assertEquals;
import static training.weather.WeatherForecastFixture.CONSOLIDATED_WEATHER;
import static training.weather.WeatherForecastFixture.PREDICTIONS;

public class ConsolidatedWeatherTest {

    @Test
    public void beanTest() {
        assertEquals(CONSOLIDATED_WEATHER.getPredictions(), PREDICTIONS);
    }

    @Test
    public void equalsVerifierTest() {
        forClass(ConsolidatedWeather.class).verify();
    }
}