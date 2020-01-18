# Introducción

**WeatherForecast** es un ejercicio similar a la [Weather kata](https://github.com/CodiumTeam/weather-kata) de [Codium Team](https://www.codium.team).

Se trata de una clase que devuelve la previsión del tiempo de una ciudad en una fecha concreta.

Para ello, esta clase utiliza una API externa (requiere conexión a internet): [www.metaweather.com](https://www.metaweather.com) 

Ejemplo:

```java
WeatherForecast weatherForecast = new WeatherForecast();
weatherForecast.getCityWeather("Madrid", new Date());
```


# Ejercicio

El ejercicio consiste en **refactorizar** el código haciéndolo más **mantenible**, ya que el código existente es muy difícil de entender.  
  
Para ello se pueden realizar múltiples modificaciones siempre que no cambie la funcionalidad: incluir tests automáticos, extraer métodos, renombrar variables, modificar tipos de datos, crear nuevas clases, añadir librerías, etc.

Finalmente se debería incluir un fichero README explicando cómo ejecutar el ejercicio incluyendo algunos ejemplos.


# Solución

La solución se puede subir a un repositorio de código público como [github](https://github.com/) ó se puede comprimir en un fichero .zip y enviar por email.
