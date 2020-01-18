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

# Mi Solución

- En primer lugar, ya había realizado esta Kata cuando Codium visitó Palma y presentó el ejercicio al grupo Software Crafters de Mallorca. Mi primera solución (la hice el año pasado) la podeis consultar aquí [Weather kata](https://github.com/acontell/katas/tree/master/weather)
- **PARA EJECUTAR**: Desde raíz ```mvn clean install``` 
- Existe un test de integración que permite realizar pruebas rápidas.
    - Tener en cuenta que ```HttpClient``` ha sido mockeado para evitar realizar llamadas externas en test.
- Se puede ejecutar a través de terminal: ```java -jar target/MeliaWeather-jar-with-dependencies.jar city_name string_date``` donde:
    - ```city_name``` se corresponde con el nombre de la ciudad
    - ```string_date``` es la fecha de la que se quiere obtener la predicción en formato ```dd-mm-YYY```
    - Ejemplo: ```java -jar target/MeliaWeather-jar-with-dependencies.jar Madrid 18-01-2020```
- Conforme está montado, sería sencillo hacer una aplicación Web o gráfica que hiciera uso de ```WeatherForecast```.
- He tomado la decisión de no manejar la ```IOException``` y propagarla a los consumidores de la clase: la razón es que, de hacerlo, cambiaría la firma del método público de ```WeatherForecast``` rompiendo así el contrato. Aparte, considero que es más acertado que sean los propios clientes los que decidan qué hacer cuando se produzca un error de tipo IO.
- Tampoco controlo que el array de ciudades contenga al menos un elemento, por lo que, al igual que sucede con el código Legacy, se lanzará una excepción si esto sucede.
- Se podrían realizar varias mejoras:
    - Actualizar a jUnit5
    - Añadir Logs
    - Añadir Caches a llamadas remotas
    - Añadir capa Web o gráfica que haga uso de ```WeatherForecast```
    - Seguir puliendo nombres de variables y métodos (tarea realmente infinita en función del nivel de exigencia de cada uno)
- Metodología de trabajo
    - Hacer tests
    - do
        - Refactorizar
        - Pasar tests
        - Commitear
    - until finish
    - Como nota decir que tenía pensado enviar un zip y tenía un repo en local. Tras leer el README y ver que se podía pasar un enlace a repo, inicialicé repo en Github subiendo todo el código de golpe, con lo que se perdió histórico de commits.