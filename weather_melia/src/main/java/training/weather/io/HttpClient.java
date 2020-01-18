package training.weather.io;

import org.apache.http.client.fluent.Request;

import java.io.IOException;

public class HttpClient {

    public String fetch(final String url) throws IOException {
        return getRequest(url)
                .execute()
                .returnContent()
                .asString();
    }

    Request getRequest(final String url) {
        return Request.Get(url);
    }
}
