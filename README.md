Resumen:

En este proyecto realizo la búsqueda de las películas más taquilleras en España desde 2002. Una vez halladas, se relaciona su recaudación con el Pib por cápita del año correspondiente. Con esta relación establecemos el valor que los espectadores han atribuido a cada film. A continuación buscamos la puntuación que la comunidad internacional ha otorgado a cada título, y con esta información y la relación calculada previamente lo que hacemos es ordenar las películas según la sobreacogida/infraacogida que tuvo en nuestros cines.

A continuación, y como añadido, se realizan dos tareas más. Por un lado recuperamos las coordenadas del país que produjo la película. Por otro lado lanzamos una búsqueda en Twitter de los títulos de las primeras 5 películas (las que mejor acogida tuvieron) y a estos tweets ls aplicamos wordcloud y visualizamos las imágenes.

Para ejecutar este proyecto es necesario editar las 4 variables al principio de mainScript.R, informándolas con vuestras credenciales de la API de Twitter.

También hay que editar la variable path para adaptarla al entorno de trabajo donde se va a ejecutar.

Recomiendo ir con cuidado a la hora de abrir el mainScript, ya que puede abrirse con un encoding erróneo, y lo mejor es copiar el código y pegarlo en un documento nuevo, que no es necesario guardar.

Las imágenes wordcloud varían según la ejecución, ya que no siempre se encuentran los mismos tweets.
