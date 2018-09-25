# Weather kata
The goal of this kata is to identify the code smells and fix them.

Exercise:
- Use ForecastService interface.
- Abstract clients.
- Make use of collaborators.

Refactor

Step 1:

1. Created ForecastService interface.
2. Created ForecastAdapter.
3. Adapt tests.

Step 2:

1. Create Prediction (domain object).
2. Create interface PredictionProvider (data source abstraction).
3. Create MetaWeatherPredictionProvider (implementation of PredictionProvider: data source is MetaWeather).

Step 3:

1. Abstract HttpClient.
2. Create GoogleHttpClient (an implementation of HttpClient).
3. MetaWeatherPredictionProvider makes use of HttpClient.

Step 4:

1. Clean up.
2. Remove Date.
3. Remove unnecessary code.

Step 5:

1. Organize into packages.
2. New collaborators: time delegation to Clock, json object parsing delegated to MetaWeatherParser.
3. Factory method in Prediction.
