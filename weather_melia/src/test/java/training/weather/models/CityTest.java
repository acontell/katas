package training.weather.models;

import org.junit.Test;

import static nl.jqno.equalsverifier.EqualsVerifier.forClass;
import static org.junit.Assert.assertEquals;
import static training.weather.WeatherForecastFixture.CITY;
import static training.weather.WeatherForecastFixture.CITY_WOEID;

public class CityTest {
    @Test
    public void bean_test() {
        assertEquals(CITY.getWoeid(), CITY_WOEID);
    }

    @Test
    public void equals_verifier_tests() {
        forClass(City.class).verify();
    }
}