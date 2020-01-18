package training.weather.io;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import training.weather.models.City;

import java.io.IOException;

import static org.junit.Assert.assertEquals;
import static org.mockito.BDDMockito.given;
import static training.weather.WeatherForecastFixture.CITY;
import static training.weather.WeatherForecastFixture.FETCH_GET_RESULT;
import static training.weather.WeatherForecastFixture.FETCH_GET_URL;

@RunWith(MockitoJUnitRunner.class)
public class RestTemplateTest {
    @Mock
    private ObjectMapper objectMapper;
    @Mock
    private HttpClient httpClient;

    @Test
    public void should_fetch_and_convert_object() throws IOException {
        given_fetch_returns_string_and_mapper_converts();
        final City actual = new RestTemplate(this.httpClient, this.objectMapper).getForObject(FETCH_GET_URL, City.class);
        assertEquals(CITY, actual);
    }

    private void given_fetch_returns_string_and_mapper_converts() throws IOException {
        given(this.httpClient.fetchGet(FETCH_GET_URL)).willReturn(FETCH_GET_RESULT);
        given(this.objectMapper.readValue(FETCH_GET_RESULT, City.class)).willReturn(CITY);
    }
}