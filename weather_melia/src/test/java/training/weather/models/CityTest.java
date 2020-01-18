package training.weather.models;

import nl.jqno.equalsverifier.EqualsVerifier;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class CityTest {
    private static final String WOEID = "123";

    @Test
    public void beanTest() {
        final City city = new City(WOEID);
        assertEquals(city.getWoeid(), WOEID);
    }

    @Test
    public void equalsVerifierTest() {
        EqualsVerifier.forClass(City.class).verify();
    }
}