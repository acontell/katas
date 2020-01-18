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

@RunWith(MockitoJUnitRunner.class)
public class RestTemplateTest {
    private static final String SAMPLE = "asdf";
    private static final String URL = "111asdf";
    private static final City CITY = new City("123");

    @Mock
    private ObjectMapper objectMapper;
    @Mock
    private HttpClient httpClient;

    @Test
    public void shouldFetchAndConvertObject() throws IOException {
        given(this.httpClient.fetchGet(URL)).willReturn(SAMPLE);
        given(this.objectMapper.readValue(SAMPLE, City.class)).willReturn(CITY);
        assertEquals(new RestTemplate(this.httpClient, this.objectMapper).getForObject(URL, City.class), CITY);
    }
}