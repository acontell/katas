package training.weather.io;

import org.apache.http.client.fluent.Content;
import org.apache.http.client.fluent.Request;
import org.apache.http.client.fluent.Response;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;

import java.io.IOException;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.spy;
import static training.weather.WeatherForecastFixture.FETCH_GET_RESULT;
import static training.weather.WeatherForecastFixture.FETCH_GET_URL;

@RunWith(MockitoJUnitRunner.class)
public class HttpClientTest {
    @Mock
    private Request request;
    @Mock
    private Response response;
    @Mock
    private Content content;

    private HttpClient httpClient;

    @Test
    public void should_fetch_data_from_url() throws IOException {
        given_spied_client_with_mocked_methods();
        final String actual = this.httpClient.fetchGet(FETCH_GET_URL);
        assertEquals(FETCH_GET_RESULT, actual);
    }

    private void given_spied_client_with_mocked_methods() throws IOException {
        this.httpClient = spy(new HttpClient());
        given(this.httpClient.getRequest(FETCH_GET_URL)).willReturn(this.request);
        given(this.request.execute()).willReturn(this.response);
        given(this.response.returnContent()).willReturn(this.content);
        given(this.content.asString()).willReturn(FETCH_GET_RESULT);
    }

    @Test
    public void should_create_request() {
        final Request actual = new HttpClient().getRequest(FETCH_GET_URL);
        assertNotNull(actual);
    }
}