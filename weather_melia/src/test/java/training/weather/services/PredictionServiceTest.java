package training.weather.services;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import training.weather.models.Prediction;

import java.io.IOException;
import java.util.Optional;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.mockito.BDDMockito.given;
import static training.weather.WeatherForecastFixture.CITY;
import static training.weather.WeatherForecastFixture.CITY_NAME;
import static training.weather.WeatherForecastFixture.CONSOLIDATED_WEATHER;
import static training.weather.WeatherForecastFixture.DATE_NOT_IN_RANGE_TO_LOCAL_DATE;
import static training.weather.WeatherForecastFixture.LOCAL_DATE;
import static training.weather.WeatherForecastFixture.PREDICTION;

@RunWith(MockitoJUnitRunner.class)
public class PredictionServiceTest {
    @Mock
    private CityService cityService;
    @Mock
    private ConsolidatedWeatherService consolidatedWeatherService;

    private PredictionService predictionService;

    @Before
    public void setUp() {
        this.predictionService = new PredictionService(this.cityService, this.consolidatedWeatherService);
    }

    @Test
    public void should_return_result_when_date_in_results() throws IOException {
        given_city_has_consolidated_weather();
        final Optional<Prediction> actual = this.predictionService.getPrediction(CITY_NAME, LOCAL_DATE);
        assertEquals(PREDICTION, actual.get());
    }

    private void given_city_has_consolidated_weather() throws IOException {
        given(this.cityService.getCity(CITY_NAME)).willReturn(CITY);
        given(this.consolidatedWeatherService.getConsolidatedWeather(CITY)).willReturn(CONSOLIDATED_WEATHER);
    }

    @Test
    public void should_return_empty_when_date_not_in_results() throws IOException {
        given_city_has_consolidated_weather();
        final Optional<Prediction> actual = this.predictionService.getPrediction(CITY_NAME, DATE_NOT_IN_RANGE_TO_LOCAL_DATE);
        assertFalse(actual.isPresent());
    }
}