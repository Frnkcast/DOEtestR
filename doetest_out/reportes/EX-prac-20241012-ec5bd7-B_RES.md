------------Respuestas del Examen----------------
Generada para el examen:::  EX-prac-20241012-ec5bd7-B 
- Alias::  unpleasant_cathartic_lotad 
Problemas recopilados::: 04B04t03, 08B03t03, 07B02t02, 06B04t06, 02B04t05 
- Usando las bases de datos::  AB-ques-20241012-813611 
----------------------------------
Pregunta de Anova 1F + Bloque:  04B04t03 
-----------------------------------------------------------
 Entrada  1 B --   04B04t03 
-----------------------------------------------------------

   # Niveles de A:  4 
   # Niveles de B (Bloque):  5 
 
  Enunciado:::  
(4) Un chef quiere determinar el efecto que tienen diferentes metodos de cocción sobre la textura de la carne. Se eligieron %a% metodos de cocción a probar, y para controlar la variación que hay en la calidad de la carne, se establecieron %b% bloques para el corte de carne utilizado. Cada metodo de cocción fue asignado aleatoreamente a los cortes de cada bloque, y la textura fue determinada con un panel de degustadores. Las variables involucradas en el experimento: %V%  A: Metodos de cocción %O% B (Bloque): Corte de la carne %O% Y: Textura de la carne

 </li> <p> A continuación se presentan la tabla con los datos del exerimento: </p> 
 %T% 
 A continuación, se presenta parcialmente la tabla de ANOVA generada con los datos. 
 %A% 

  Datos::  
|B  |    A1|    A2|    A3|    A4|
|:--|-----:|-----:|-----:|-----:|
|B1 | 39.22|  9.81| 88.07| 37.41|
|B2 | 43.39| 17.21| 35.89| 33.36|
|B3 | 11.73|  5.52| 21.09| 28.57|
|B4 | 65.00| 44.88| 52.37| 48.54|
|B5 |  0.16| 34.71| 50.81| 32.72| 

  Preguntas y sus respuestas:: 

- A1 (2 pts) Reporta la suma de cuadrados de los tratamientos (SSt), del bloque (SSb) y del error (SSE), redondeando a 3 cifras significativas. Importante: indica en tu respuesta cual es cual 
- A2 (2 pts) Reporta la media de cuadrados de los tratamientos (MSt), del bloque (MSb) y del error (MSE) a 3 cifras significativas. Importante: indica en tu respuesta cual es cual. 
- A3 (1 pts) Reporta tu estadistico de prueba, F0 
Res: 
|Fuente    |      SS_| Df|     MS_|    F0|  Pval|
|:---------|--------:|--:|-------:|-----:|-----:|
|X         | 1917.365|  3| 639.122| 2.181| 0.143|
|B         | 3028.463|  4| 757.116| 2.584| 0.091|
|Residuals | 3516.220| 12| 293.018|    NA|    NA|
|Total     | 8462.048| 19| 445.371|    NA|    NA| 

- A4 (1 pts) A partir del F0 calculado, y el Fcritico (revisa el enunciado del problema) para una confianza del 95%, considerando una hipotesis nula donde ninguno de los niveles del factor afectan a la variable de respuesta, y una hipotesis alternativa donde al menos uno de los niveles tiene un efecto significativo. ¿Puedes concluir en que la hipotesis nula se rechaza o no? Escribe el enunciado completo de la conclusión, usando la definición de las variables involucradas.  
Res: 
     No se rechaza H0. El factor A no es significativo. 

- A5 (2 pts) Realiza una Prueba de Tukey de Comparaciones Múltiples para identificar cual (o cuales) de los niveles del factor A es (o son) significativamente diferente del resto. (En el enunciado del problema está el valor del estadistico q de Tukey a utilizar para la prueba) 
Res: 
|  MSerror| Df|   Mean|       CV| StudRange|      HSD|
|--------:|--:|------:|--------:|---------:|--------:|
| 293.0183| 12| 35.023| 48.87582|   4.19866| 32.14202| 

|X   |      Y|groups |
|:---|------:|:------|
|X   | 31.900|a      |
|X.1 | 22.426|a      |
|X.2 | 49.646|a      |
|X.3 | 36.120|a      | 

|   |A1 |A2    |A3    |A4    |
|:--|:--|:-----|:-----|:-----|
|A1 |NA |FALSE |FALSE |FALSE |
|A2 |NA |NA    |FALSE |FALSE |
|A3 |NA |NA    |NA    |FALSE |
|A4 |NA |NA    |NA    |NA    |

- A6 (2 pts)  Vuelve a realizar tu analisis ANOVA, pero ahora sin considerar la variable Bloque, recordando ajustar las sumas de cuadrados y grados de libertad apropiados. El valor de F critico a utilizar se encuentra en el enunciado de este problema. ¿Identificas alguna diferencia con respecto a tu conclusión anterior? ¿Qué puedes concluir con respecto al bloque incorporado en este experimento? 
Res:
|Fuente    |      SS_| Df|     MS_|    F0|  Pval|
|:---------|--------:|--:|-------:|-----:|-----:|
|X         | 1917.365|  3| 639.122| 1.562| 0.237|
|Residuals | 6544.683| 16| 409.043|    NA|    NA|
|Total     | 8462.048| 19| 445.371|    NA|    NA| 
     No se rechaza H0. El factor A no es significativo 
     Quitar el bloque no cambia la conclusion sobre H0 


-----------------------------------------------------------


Pregunta de Anova 1F + Bloque:  08B03t03 
-----------------------------------------------------------
 Entrada  1 B --   08B03t03 
-----------------------------------------------------------

   # Niveles de A:  4 
   # Niveles de B (Bloque):  5 
 
  Enunciado:::  
(3) Un investigador quiere conocer el efecto de %a% diferentes fertilizantes sobre el crecimiento de plantas de tomate. Dentro de un invernadero, se divide el espacio disponible en %b% diferentes bloques segun el tipo de tierra a utilizar, cada uno con cuatro plantas de tomate. Al final de cuatro semanas, se evalua la altura de las plantas. Las variables involucradas en el experimento: %V%  A: Fertilizante empleado %O% B (Bloque): Tipo de tierra %O% Y: Altura de la planta

 </li> <p> A continuación se presentan la tabla con los datos del exerimento: </p> 
 %T% 
 A continuación, se presenta parcialmente la tabla de ANOVA generada con los datos. 
 %A% 

  Datos::  
|B  |    A1|    A2|    A3|    A4|
|:--|-----:|-----:|-----:|-----:|
|B1 | 39.22|  9.81| 88.07| 37.41|
|B2 | 43.39| 17.21| 35.89| 33.36|
|B3 | 11.73|  5.52| 21.09| 28.57|
|B4 | 65.00| 44.88| 52.37| 48.54|
|B5 |  0.16| 34.71| 50.81| 32.72| 

  Preguntas y sus respuestas:: 

- A1 (2 pts) Reporta la suma de cuadrados de los tratamientos (SSt), del bloque (SSb) y del error (SSE), redondeando a 3 cifras significativas. Importante: indica en tu respuesta cual es cual 
- A2 (2 pts) Reporta la media de cuadrados de los tratamientos (MSt), del bloque (MSb) y del error (MSE) a 3 cifras significativas. Importante: indica en tu respuesta cual es cual. 
- A3 (1 pts) Reporta tu estadistico de prueba, F0 
Res: 
|Fuente    |      SS_| Df|     MS_|    F0|  Pval|
|:---------|--------:|--:|-------:|-----:|-----:|
|X         | 1917.365|  3| 639.122| 2.181| 0.143|
|B         | 3028.463|  4| 757.116| 2.584| 0.091|
|Residuals | 3516.220| 12| 293.018|    NA|    NA|
|Total     | 8462.048| 19| 445.371|    NA|    NA| 

- A4 (1 pts) A partir del F0 calculado, y el Fcritico (revisa el enunciado del problema) para una confianza del 95%, considerando una hipotesis nula donde ninguno de los niveles del factor afectan a la variable de respuesta, y una hipotesis alternativa donde al menos uno de los niveles tiene un efecto significativo. ¿Puedes concluir en que la hipotesis nula se rechaza o no? Escribe el enunciado completo de la conclusión, usando la definición de las variables involucradas.  
Res: 
     No se rechaza H0. El factor A no es significativo. 

- A5 (2 pts) Realiza una Prueba de Tukey de Comparaciones Múltiples para identificar cual (o cuales) de los niveles del factor A es (o son) significativamente diferente del resto. (En el enunciado del problema está el valor del estadistico q de Tukey a utilizar para la prueba) 
Res: 
|  MSerror| Df|   Mean|       CV| StudRange|      HSD|
|--------:|--:|------:|--------:|---------:|--------:|
| 293.0183| 12| 35.023| 48.87582|   4.19866| 32.14202| 

|X   |      Y|groups |
|:---|------:|:------|
|X   | 31.900|a      |
|X.1 | 22.426|a      |
|X.2 | 49.646|a      |
|X.3 | 36.120|a      | 

|   |A1 |A2    |A3    |A4    |
|:--|:--|:-----|:-----|:-----|
|A1 |NA |FALSE |FALSE |FALSE |
|A2 |NA |NA    |FALSE |FALSE |
|A3 |NA |NA    |NA    |FALSE |
|A4 |NA |NA    |NA    |NA    |

- A6 (2 pts)  Vuelve a realizar tu analisis ANOVA, pero ahora sin considerar la variable Bloque, recordando ajustar las sumas de cuadrados y grados de libertad apropiados. El valor de F critico a utilizar se encuentra en el enunciado de este problema. ¿Identificas alguna diferencia con respecto a tu conclusión anterior? ¿Qué puedes concluir con respecto al bloque incorporado en este experimento? 
Res:
|Fuente    |      SS_| Df|     MS_|    F0|  Pval|
|:---------|--------:|--:|-------:|-----:|-----:|
|X         | 1917.365|  3| 639.122| 1.562| 0.237|
|Residuals | 6544.683| 16| 409.043|    NA|    NA|
|Total     | 8462.048| 19| 445.371|    NA|    NA| 
     No se rechaza H0. El factor A no es significativo 
     Quitar el bloque no cambia la conclusion sobre H0 


-----------------------------------------------------------


Pregunta de Anova 1F + Bloque:  07B02t02 
-----------------------------------------------------------
 Entrada  1 B --   07B02t02 
-----------------------------------------------------------

   # Niveles de A:  4 
   # Niveles de B (Bloque):  3 
 
  Enunciado:::  
(2) Una universidad quiere evaluar la efecitividad de %a% diferentes metodos de enseñanza para incrementar los resultados de un examen. Se aprovecha la división de alumnos en %b% salones diferentes como la variable de bloqueo, y dentro de cada salón a cada alumno se le asigna un metodo diferente de enseñanza. Despues de un mes, se evaluó la mejora de cada estudiante mediante un examen. Las variables involucradas en el experimento: %V%  A: Metodo de enseñanza %O% B (Bloque): Salón %O% Y: Incremento en el resultado del examen

 </li> <p> A continuación se presentan la tabla con los datos del exerimento: </p> 
 %T% 
 A continuación, se presenta parcialmente la tabla de ANOVA generada con los datos. 
 %A% 

  Datos::  
|B  |    A1|    A2|    A3|    A4|
|:--|-----:|-----:|-----:|-----:|
|B1 | 61.07| 61.22| 61.49| 62.30|
|B2 | 61.51| 61.01| 61.74| 61.88|
|B3 | 61.71| 61.27| 61.63| 61.50| 

  Preguntas y sus respuestas:: 

- A1 (2 pts) Reporta la suma de cuadrados de los tratamientos (SSt), del bloque (SSb) y del error (SSE), redondeando a 3 cifras significativas. Importante: indica en tu respuesta cual es cual 
- A2 (2 pts) Reporta la media de cuadrados de los tratamientos (MSt), del bloque (MSb) y del error (MSE) a 3 cifras significativas. Importante: indica en tu respuesta cual es cual. 
- A3 (1 pts) Reporta tu estadistico de prueba, F0 
Res: 
|Fuente    |   SS_| Df|   MS_|    F0|  Pval|
|:---------|-----:|--:|-----:|-----:|-----:|
|X         | 0.846|  3| 0.282| 2.804| 0.131|
|B         | 0.000|  2| 0.000| 0.002| 0.998|
|Residuals | 0.604|  6| 0.101|    NA|    NA|
|Total     | 1.450| 11| 0.132|    NA|    NA| 

- A4 (1 pts) A partir del F0 calculado, y el Fcritico (revisa el enunciado del problema) para una confianza del 95%, considerando una hipotesis nula donde ninguno de los niveles del factor afectan a la variable de respuesta, y una hipotesis alternativa donde al menos uno de los niveles tiene un efecto significativo. ¿Puedes concluir en que la hipotesis nula se rechaza o no? Escribe el enunciado completo de la conclusión, usando la definición de las variables involucradas.  
Res: 
     No se rechaza H0. El factor A no es significativo. 

- A5 (2 pts) Realiza una Prueba de Tukey de Comparaciones Múltiples para identificar cual (o cuales) de los niveles del factor A es (o son) significativamente diferente del resto. (En el enunciado del problema está el valor del estadistico q de Tukey a utilizar para la prueba) 
Res: 
|   MSerror| Df|    Mean|        CV| StudRange|       HSD|
|---------:|--:|-------:|---------:|---------:|---------:|
| 0.1006139|  6| 61.5275| 0.5155368|  4.895599| 0.8965493| 

|X   |        Y|groups |
|:---|--------:|:------|
|X   | 61.43000|a      |
|X.1 | 61.16667|a      |
|X.2 | 61.62000|a      |
|X.3 | 61.89333|a      | 

|   |A1 |A2    |A3    |A4    |
|:--|:--|:-----|:-----|:-----|
|A1 |NA |FALSE |FALSE |FALSE |
|A2 |NA |NA    |FALSE |FALSE |
|A3 |NA |NA    |NA    |FALSE |
|A4 |NA |NA    |NA    |NA    |

- A6 (2 pts)  Vuelve a realizar tu analisis ANOVA, pero ahora sin considerar la variable Bloque, recordando ajustar las sumas de cuadrados y grados de libertad apropiados. El valor de F critico a utilizar se encuentra en el enunciado de este problema. ¿Identificas alguna diferencia con respecto a tu conclusión anterior? ¿Qué puedes concluir con respecto al bloque incorporado en este experimento? 
Res:
|Fuente    |   SS_| Df|   MS_|    F0| Pval|
|:---------|-----:|--:|-----:|-----:|----:|
|X         | 0.846|  3| 0.282| 3.736| 0.06|
|Residuals | 0.604|  8| 0.076|    NA|   NA|
|Total     | 1.450| 11| 0.132|    NA|   NA| 
     No se rechaza H0. El factor A no es significativo 
     Quitar el bloque no cambia la conclusion sobre H0 


-----------------------------------------------------------


Pregunta de Anova 1F + Bloque:  06B04t06 
-----------------------------------------------------------
 Entrada  1 B --   06B04t06 
-----------------------------------------------------------

   # Niveles de A:  3 
   # Niveles de B (Bloque):  4 
 
  Enunciado:::  
(4) Un chef quiere determinar el efecto que tienen diferentes metodos de cocción sobre la textura de la carne. Se eligieron %a% metodos de cocción a probar, y para controlar la variación que hay en la calidad de la carne, se establecieron %b% bloques para el corte de carne utilizado. Cada metodo de cocción fue asignado aleatoreamente a los cortes de cada bloque, y la textura fue determinada con un panel de degustadores. Las variables involucradas en el experimento: %V%  A: Metodos de cocción %O% B (Bloque): Corte de la carne %O% Y: Textura de la carne

 </li> <p> A continuación se presentan la tabla con los datos del exerimento: </p> 
 %T% 
 A continuación, se presenta parcialmente la tabla de ANOVA generada con los datos. 
 %A% 

  Datos::  
|B  |    A1|    A2|    A3|
|:--|-----:|-----:|-----:|
|B1 | 37.85| 38.19| 38.12|
|B2 | 38.17| 38.26| 37.91|
|B3 | 38.20| 38.26| 37.83|
|B4 | 37.93| 38.37| 38.05| 

  Preguntas y sus respuestas:: 

- A1 (2 pts) Reporta la suma de cuadrados de los tratamientos (SSt), del bloque (SSb) y del error (SSE), redondeando a 3 cifras significativas. Importante: indica en tu respuesta cual es cual 
- A2 (2 pts) Reporta la media de cuadrados de los tratamientos (MSt), del bloque (MSb) y del error (MSE) a 3 cifras significativas. Importante: indica en tu respuesta cual es cual. 
- A3 (1 pts) Reporta tu estadistico de prueba, F0 
Res: 
|Fuente    |   SS_| Df|   MS_|    F0|  Pval|
|:---------|-----:|--:|-----:|-----:|-----:|
|X         | 0.191|  2| 0.095| 3.781| 0.087|
|B         | 0.008|  3| 0.003| 0.101| 0.957|
|Residuals | 0.152|  6| 0.025|    NA|    NA|
|Total     | 0.350| 11| 0.032|    NA|    NA| 

- A4 (1 pts) A partir del F0 calculado, y el Fcritico (revisa el enunciado del problema) para una confianza del 95%, considerando una hipotesis nula donde ninguno de los niveles del factor afectan a la variable de respuesta, y una hipotesis alternativa donde al menos uno de los niveles tiene un efecto significativo. ¿Puedes concluir en que la hipotesis nula se rechaza o no? Escribe el enunciado completo de la conclusión, usando la definición de las variables involucradas.  
Res: 
     No se rechaza H0. El factor A no es significativo. 

- A5 (2 pts) Realiza una Prueba de Tukey de Comparaciones Múltiples para identificar cual (o cuales) de los niveles del factor A es (o son) significativamente diferente del resto. (En el enunciado del problema está el valor del estadistico q de Tukey a utilizar para la prueba) 
Res: 
|   MSerror| Df|   Mean|        CV| StudRange|       HSD|
|---------:|--:|------:|---------:|---------:|---------:|
| 0.0252528|  6| 38.095| 0.4171446|  4.339195| 0.3447734| 

|X   |       Y|groups |
|:---|-------:|:------|
|X   | 38.0375|a      |
|X.1 | 38.2700|a      |
|X.2 | 37.9775|a      | 

|   |A1 |A2    |A3    |
|:--|:--|:-----|:-----|
|A1 |NA |FALSE |FALSE |
|A2 |NA |NA    |FALSE |
|A3 |NA |NA    |NA    |

- A6 (2 pts)  Vuelve a realizar tu analisis ANOVA, pero ahora sin considerar la variable Bloque, recordando ajustar las sumas de cuadrados y grados de libertad apropiados. El valor de F critico a utilizar se encuentra en el enunciado de este problema. ¿Identificas alguna diferencia con respecto a tu conclusión anterior? ¿Qué puedes concluir con respecto al bloque incorporado en este experimento? 
Res:
|Fuente    |   SS_| Df|   MS_|    F0|  Pval|
|:---------|-----:|--:|-----:|-----:|-----:|
|X         | 0.191|  2| 0.095| 5.399| 0.029|
|Residuals | 0.159|  9| 0.018|    NA|    NA|
|Total     | 0.350| 11| 0.032|    NA|    NA| 
     Se rechaza H0. El factor A es significativo 
     Quitar el bloque cambia la conclusión sobre H0 


-----------------------------------------------------------


Pregunta de Anova 1F + Bloque:  02B04t05 
-----------------------------------------------------------
 Entrada  1 B --   02B04t05 
-----------------------------------------------------------

   # Niveles de A:  3 
   # Niveles de B (Bloque):  3 
 
  Enunciado:::  
(4) Un chef quiere determinar el efecto que tienen diferentes metodos de cocción sobre la textura de la carne. Se eligieron %a% metodos de cocción a probar, y para controlar la variación que hay en la calidad de la carne, se establecieron %b% bloques para el corte de carne utilizado. Cada metodo de cocción fue asignado aleatoreamente a los cortes de cada bloque, y la textura fue determinada con un panel de degustadores. Las variables involucradas en el experimento: %V%  A: Metodos de cocción %O% B (Bloque): Corte de la carne %O% Y: Textura de la carne

 </li> <p> A continuación se presentan la tabla con los datos del exerimento: </p> 
 %T% 
 A continuación, se presenta parcialmente la tabla de ANOVA generada con los datos. 
 %A% 

  Datos::  
|B  |    A1|    A2|    A3|
|:--|-----:|-----:|-----:|
|B1 | 29.70| 29.35| 27.25|
|B2 | 28.68| 29.91| 29.44|
|B3 | 31.63| 28.24| 29.49| 

  Preguntas y sus respuestas:: 

- A1 (2 pts) Reporta la suma de cuadrados de los tratamientos (SSt), del bloque (SSb) y del error (SSE), redondeando a 3 cifras significativas. Importante: indica en tu respuesta cual es cual 
- A2 (2 pts) Reporta la media de cuadrados de los tratamientos (MSt), del bloque (MSb) y del error (MSE) a 3 cifras significativas. Importante: indica en tu respuesta cual es cual. 
- A3 (1 pts) Reporta tu estadistico de prueba, F0 
Res: 
|Fuente    |    SS_| Df|   MS_|    F0|  Pval|
|:---------|------:|--:|-----:|-----:|-----:|
|X         |  2.523|  2| 1.262| 0.661| 0.565|
|B         |  1.569|  2| 0.785| 0.411| 0.688|
|Residuals |  7.637|  4| 1.909|    NA|    NA|
|Total     | 11.730|  8| 1.466|    NA|    NA| 

- A4 (1 pts) A partir del F0 calculado, y el Fcritico (revisa el enunciado del problema) para una confianza del 95%, considerando una hipotesis nula donde ninguno de los niveles del factor afectan a la variable de respuesta, y una hipotesis alternativa donde al menos uno de los niveles tiene un efecto significativo. ¿Puedes concluir en que la hipotesis nula se rechaza o no? Escribe el enunciado completo de la conclusión, usando la definición de las variables involucradas.  
Res: 
     No se rechaza H0. El factor A no es significativo. 

- A5 (2 pts) Realiza una Prueba de Tukey de Comparaciones Múltiples para identificar cual (o cuales) de los niveles del factor A es (o son) significativamente diferente del resto. (En el enunciado del problema está el valor del estadistico q de Tukey a utilizar para la prueba) 
Res: 
|  MSerror| Df|     Mean|       CV| StudRange|      HSD|
|--------:|--:|--------:|--------:|---------:|--------:|
| 1.909178|  4| 29.29889| 4.715981|  5.040241| 4.020813| 

|X   |        Y|groups |
|:---|--------:|:------|
|X   | 30.00333|a      |
|X.1 | 29.16667|a      |
|X.2 | 28.72667|a      | 

|   |A1 |A2    |A3    |
|:--|:--|:-----|:-----|
|A1 |NA |FALSE |FALSE |
|A2 |NA |NA    |FALSE |
|A3 |NA |NA    |NA    |

- A6 (2 pts)  Vuelve a realizar tu analisis ANOVA, pero ahora sin considerar la variable Bloque, recordando ajustar las sumas de cuadrados y grados de libertad apropiados. El valor de F critico a utilizar se encuentra en el enunciado de este problema. ¿Identificas alguna diferencia con respecto a tu conclusión anterior? ¿Qué puedes concluir con respecto al bloque incorporado en este experimento? 
Res:
|Fuente    |    SS_| Df|   MS_|    F0|  Pval|
|:---------|------:|--:|-----:|-----:|-----:|
|X         |  2.523|  2| 1.262| 0.822| 0.483|
|Residuals |  9.206|  6| 1.534|    NA|    NA|
|Total     | 11.730|  8| 1.466|    NA|    NA| 
     No se rechaza H0. El factor A no es significativo 
     Quitar el bloque no cambia la conclusion sobre H0 


-----------------------------------------------------------


