package training.weather.models;

import org.junit.Test;

import static nl.jqno.equalsverifier.EqualsVerifier.forClass;
import static org.junit.Assert.assertEquals;
import static training.weather.WeatherForecastFixture.CONSOLIDATED_WEATHER;
import static training.weather.WeatherForecastFixture.PREDICTIONS;

public class ConsolidatedWeatherTest {

    @Test
    public void bean_test() {
        assertEquals(CONSOLIDATED_WEATHER.getPredictions(), PREDICTIONS);
    }

    @Test
    public void equals_verifier_tests() {
        forClass(ConsolidatedWeather.class).verify();
    }
}