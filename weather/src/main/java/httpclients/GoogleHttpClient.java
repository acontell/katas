package httpclients;

import com.google.api.client.http.GenericUrl;
import com.google.api.client.http.HttpRequestFactory;
import com.google.api.client.http.javanet.NetHttpTransport;

import java.io.IOException;

public class GoogleHttpClient implements HttpClient {

    private static final HttpRequestFactory REQUEST_FACTORY = new NetHttpTransport().createRequestFactory();

    @Override
    public String getUrl(final String url) throws IOException {
        return REQUEST_FACTORY
                .buildGetRequest(getGenericUrl(url))
                .execute()
                .parseAsString();
    }

    private GenericUrl getGenericUrl(final String url) {
        return new GenericUrl(url);
    }
}
