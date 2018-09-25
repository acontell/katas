package httpclients;

import java.io.IOException;

public interface HttpClient {
    String getUrl(final String url) throws IOException;
}
