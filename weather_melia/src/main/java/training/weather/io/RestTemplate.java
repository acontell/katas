package training.weather.io;

import com.fasterxml.jackson.databind.ObjectMapper;

import java.io.IOException;

public class RestTemplate {
    private final HttpClient httpClient;
    private final ObjectMapper objectMapper;

    public RestTemplate(final HttpClient httpClient, final ObjectMapper objectMapper) {
        this.httpClient = httpClient;
        this.objectMapper = objectMapper;
    }

    public <T> T getForObject(final String url, final Class<T> clazz) throws IOException {
        return this.objectMapper.readValue(this.httpClient.fetchGet(url), clazz);
    }
}
