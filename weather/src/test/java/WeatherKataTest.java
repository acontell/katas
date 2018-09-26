import org.junit.Before;
import org.junit.Test;

import java.util.Date;

import static junit.framework.TestCase.assertTrue;
import static org.junit.Assert.assertEquals;

public class WeatherKataTest {

    private static final int ONE_DAY = 1000 * 60 * 60 * 24 * 1;

    private ForecastService forecastService;

    /*
    Integration tests provided by Codium team: in order to test, check prediction gets printed on sout.
     */

    @Before
    public void setUp() {
        this.forecastService = new ForecastAdapter(new Forecast());
    }

    // https://www.metaweather.com/api/location/766273/
    @Test
    public void find_the_weather_of_today() {
        String prediction = this.forecastService.predictWeather("Madrid", null);

        System.out.println("Today: " + prediction);
        assertTrue("I don't know how to test it", true);
    }

    @Test
    public void find_the_weather_of_any_day() {
        Date tomorrow = new Date(new Date().getTime() + ONE_DAY);

        String prediction = this.forecastService.predictWeather("Madrid", tomorrow);
        System.out.println("Tomorrow: " + prediction);
        assertTrue("I don't know how to test it", true);
    }

    @Test
    public void find_the_wind_of_any_day() {
        String prediction = this.forecastService.predictWind("Madrid", null);

        System.out.println("Wind: " + prediction);
        assertTrue("I don't know how to test it", true);
    }

    @Test
    public void there_is_no_prediction_for_more_than_5_days() {
        Date tomorrow = new Date(new Date().getTime() + (ONE_DAY * 6));

        String prediction = this.forecastService.predictWeather("Madrid", tomorrow);
        assertEquals("", prediction);
    }
}
