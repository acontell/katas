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

@RunWith(MockitoJUnitRunner.class)
public class HttpClientTest {
    private static final String URL = "http://www.google.es/";
    private static final String FETCH_RESULT = "hola";

    @Mock
    private Request request;
    @Mock
    private Response response;
    @Mock
    private Content content;

    private HttpClient httpClient;

    @Test
    public void shouldFetchDataFromUrl() throws IOException {
        givenSpiedClientWithMocks();
        assertEquals(this.httpClient.fetchGet(URL), FETCH_RESULT);
    }

    private void givenSpiedClientWithMocks() throws IOException {
        this.httpClient = spy(new HttpClient());
        given(this.httpClient.getRequest(URL)).willReturn(this.request);
        given(this.request.execute()).willReturn(this.response);
        given(this.response.returnContent()).willReturn(this.content);
        given(this.content.asString()).willReturn(FETCH_RESULT);
    }

    @Test
    public void shouldCreateRequest() {
        assertNotNull(new HttpClient().getRequest(URL));
    }
}