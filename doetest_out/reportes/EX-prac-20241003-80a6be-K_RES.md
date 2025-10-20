------------Respuestas del Examen----------------
Generada para el examen:::  EX-prac-20241003-80a6be-K 
- Alias::  scarlet_tectonic_horsea 
Problemas recopilados::: 03k07t02, 01k07t02, 02k06t04, 04k01t04 
- Usando las bases de datos::  2k-ques-20241003-e6362b 
----------------------------------
Pregunta de Diseño 2^k:  03k07t02 
-----------------------------------------------------------
 Entrada  1 #: --  03k07t02 
-----------------------------------------------------------
   # Factores:  3 
   Replicas:  3 
 
  Enunciado:::  
(7) Un laboratorio de desarrollo vegetal está estudiando el impapcto de diferentes fertilizantes sobre el crecimiento de las plantas cultivadas. Para esto, deciden investigar como 3 variables pueden influenciar la velocidad de crecimiento. El experimento se diseñó con las siguientes variables: %V%  A: Tipo de fertilizante (Sintetico, Bioestimulante) %O% B: Exposición a la luz (Bajo, Alto) %O% C: Frecuencia de riego (Infrecuente, Frecuente) %O% Y: Altura de las plantas

 </li> <p> A continuación se presentan la matriz de diseño (en unidades codificadas) y los resultados de %n% réplicas: </p> 
 %T%  

  Datos::  
|  A|  B|  C|     Y|   Y.1|   Y.2|
|--:|--:|--:|-----:|-----:|-----:|
| -1| -1| -1| 29.63| 26.13| 34.47|
|  1| -1| -1| 31.21| 33.64| 31.54|
| -1|  1| -1| 30.16| 30.35| 26.10|
|  1|  1| -1| 34.68| 33.33| 33.91|
| -1| -1|  1| 32.46| 33.80| 30.95|
|  1| -1|  1| 30.56| 35.78| 32.04|
| -1|  1|  1| 31.30| 32.23| 30.94|
|  1|  1|  1| 34.84| 37.76| 33.36| 

  Preguntas y sus respuestas:: 

- B1 (2 pts) Calcula y reporta los efectos de los factores principales y las interacciones. 
Res: 
|Factor      | Efecto|   Coef|
|:-----------|------:|------:|
|(Intercept) | 64.264| 32.132|
|A           |  2.844|  1.422|
|B           |  0.562|  0.281|
|C           |  1.739|  0.870|
|A:B         |  1.623|  0.811|
|A:C         | -0.734| -0.367|
|B:C         |  0.244|  0.122|
|A:B:C       |  0.097|  0.049|

- B2 (2 pts) En tu hoja de respuestas, dibuja las gráficas de Efectos Principales para los 3 factores. En el espacio a continuación (aqui en Canvas) explica si, a partir de las gráficas, pudiera identificar si uno de los factores pareciera tener un efecto significativo, y explica por qué. 
Res: NA 
 

- B3 (3 pts) Realiza un analisis de varianza Anova y reporta cuales factores e interacciones son significativos. Utiliza un alfa de 0.05 (el valor de F critico a utilizar en las comparaciones está dado en el enunciado del problema) 
Res: 
|Fuente    |     SS_| Df|    MS_|    F0|  Pval|
|:---------|-------:|--:|------:|-----:|-----:|
|A         |  48.536|  1| 48.536| 9.653| 0.007|
|B         |   1.898|  1|  1.898| 0.378| 0.548|
|C         |  18.148|  1| 18.148| 3.609| 0.076|
|A:B       |  15.795|  1| 15.795| 3.141| 0.095|
|A:C       |   3.234|  1|  3.234| 0.643| 0.434|
|B:C       |   0.358|  1|  0.358| 0.071| 0.793|
|A:B:C     |   0.057|  1|  0.057| 0.011| 0.917|
|Residuals |  80.450| 16|  5.028|    NA|    NA|
|Total     | 168.476| 23|  7.325|    NA|    NA| 
Significativos:  A

- B4 (1 pts) A partir de los factores e interacciones que resultaron significativos, da el Modelo de Regresión Abreviado (unicamente con los coeficientes de los  factores e interacciones que resultaron significativos) 
Res: 
Ecuacion de regresion abreviada  y = 32.132 + 1.422 A

Predicción de la respuesta con el mejor modelo: 
|  A|  B|  C|     Y|    Y_fit|
|--:|--:|--:|-----:|--------:|
| -1| -1| -1| 29.63| 30.71000|
|  1| -1| -1| 31.21| 33.55417|
| -1|  1| -1| 30.16| 30.71000|
|  1|  1| -1| 34.68| 33.55417|
| -1| -1|  1| 32.46| 30.71000|
|  1| -1|  1| 30.56| 33.55417|
| -1|  1|  1| 31.30| 30.71000|
|  1|  1|  1| 34.84| 33.55417|
| -1| -1| -1| 26.13| 30.71000|
|  1| -1| -1| 33.64| 33.55417|
| -1|  1| -1| 30.35| 30.71000|
|  1|  1| -1| 33.33| 33.55417|
| -1| -1|  1| 33.80| 30.71000|
|  1| -1|  1| 35.78| 33.55417|
| -1|  1|  1| 32.23| 30.71000|
|  1|  1|  1| 37.76| 33.55417|
| -1| -1| -1| 34.47| 30.71000|
|  1| -1| -1| 31.54| 33.55417|
| -1|  1| -1| 26.10| 30.71000|
|  1|  1| -1| 33.91| 33.55417|
| -1| -1|  1| 30.95| 30.71000|
|  1| -1|  1| 32.04| 33.55417|
| -1|  1|  1| 30.94| 30.71000|
|  1|  1|  1| 33.36| 33.55417| 

- B5 (2 pts) Utilizando la formula del modelo de regresión abreviada, de la pregunta anterior, contesta:
        a) ¿Cuál es el valor minimo de la variable respuesta predicho por el modelo abreviado
        b) ¿Qué combinación de niveles en unidades originales (es decir, NO como -1 y +1) de las variables sugerirías para minimizar la respuesta? 
Res: 
   - Minimizar: 
|  A|  B|  C|     Y|    Y_fit|
|--:|--:|--:|-----:|--------:|
|  1| -1| -1| 31.21| 33.55417| 

- B6 (2 pts) Utilizando la formula del modelo de regresión abreviada, de la pregunta anterior, contesta:
        a) ¿Cuál es el valor maximo de la variable respuesta predicho por el modelo abreviado
        b) ¿Qué combinación de niveles en unidades originales (es decir, NO como -1 y +1) de las variables sugerirías para maximizar la respuesta? 
Res: 
   - Maximizar: 
|  A|  B|  C|     Y| Y_fit|
|--:|--:|--:|-----:|-----:|
| -1| -1| -1| 29.63| 30.71|
 
-----------------------------------------------------------


Pregunta de Diseño 2^k:  01k07t02 
-----------------------------------------------------------
 Entrada  1 #: --  01k07t02 
-----------------------------------------------------------
   # Factores:  3 
   Replicas:  3 
 
  Enunciado:::  
(7) Un laboratorio de desarrollo vegetal está estudiando el impapcto de diferentes fertilizantes sobre el crecimiento de las plantas cultivadas. Para esto, deciden investigar como 3 variables pueden influenciar la velocidad de crecimiento. El experimento se diseñó con las siguientes variables: %V%  A: Tipo de fertilizante (Sintetico, Bioestimulante) %O% B: Exposición a la luz (Bajo, Alto) %O% C: Frecuencia de riego (Infrecuente, Frecuente) %O% Y: Altura de las plantas

 </li> <p> A continuación se presentan la matriz de diseño (en unidades codificadas) y los resultados de %n% réplicas: </p> 
 %T%  

  Datos::  
|  A|  B|  C|     Y|   Y.1|   Y.2|
|--:|--:|--:|-----:|-----:|-----:|
| -1| -1| -1| 29.63| 26.13| 34.47|
|  1| -1| -1| 31.21| 33.64| 31.54|
| -1|  1| -1| 30.16| 30.35| 26.10|
|  1|  1| -1| 34.68| 33.33| 33.91|
| -1| -1|  1| 32.46| 33.80| 30.95|
|  1| -1|  1| 30.56| 35.78| 32.04|
| -1|  1|  1| 31.30| 32.23| 30.94|
|  1|  1|  1| 34.84| 37.76| 33.36| 

  Preguntas y sus respuestas:: 

- B1 (2 pts) Calcula y reporta los efectos de los factores principales y las interacciones. 
Res: 
|Factor      | Efecto|   Coef|
|:-----------|------:|------:|
|(Intercept) | 64.264| 32.132|
|A           |  2.844|  1.422|
|B           |  0.562|  0.281|
|C           |  1.739|  0.870|
|A:B         |  1.623|  0.811|
|A:C         | -0.734| -0.367|
|B:C         |  0.244|  0.122|
|A:B:C       |  0.097|  0.049|

- B2 (2 pts) En tu hoja de respuestas, dibuja las gráficas de Efectos Principales para los 3 factores. En el espacio a continuación (aqui en Canvas) explica si, a partir de las gráficas, pudiera identificar si uno de los factores pareciera tener un efecto significativo, y explica por qué. 
Res: NA 
 

- B3 (3 pts) Realiza un analisis de varianza Anova y reporta cuales factores e interacciones son significativos. Utiliza un alfa de 0.05 (el valor de F critico a utilizar en las comparaciones está dado en el enunciado del problema) 
Res: 
|Fuente    |     SS_| Df|    MS_|    F0|  Pval|
|:---------|-------:|--:|------:|-----:|-----:|
|A         |  48.536|  1| 48.536| 9.653| 0.007|
|B         |   1.898|  1|  1.898| 0.378| 0.548|
|C         |  18.148|  1| 18.148| 3.609| 0.076|
|A:B       |  15.795|  1| 15.795| 3.141| 0.095|
|A:C       |   3.234|  1|  3.234| 0.643| 0.434|
|B:C       |   0.358|  1|  0.358| 0.071| 0.793|
|A:B:C     |   0.057|  1|  0.057| 0.011| 0.917|
|Residuals |  80.450| 16|  5.028|    NA|    NA|
|Total     | 168.476| 23|  7.325|    NA|    NA| 
Significativos:  A

- B4 (1 pts) A partir de los factores e interacciones que resultaron significativos, da el Modelo de Regresión Abreviado (unicamente con los coeficientes de los  factores e interacciones que resultaron significativos) 
Res: 
Ecuacion de regresion abreviada  y = 32.132 + 1.422 A

Predicción de la respuesta con el mejor modelo: 
|  A|  B|  C|     Y|    Y_fit|
|--:|--:|--:|-----:|--------:|
| -1| -1| -1| 29.63| 30.71000|
|  1| -1| -1| 31.21| 33.55417|
| -1|  1| -1| 30.16| 30.71000|
|  1|  1| -1| 34.68| 33.55417|
| -1| -1|  1| 32.46| 30.71000|
|  1| -1|  1| 30.56| 33.55417|
| -1|  1|  1| 31.30| 30.71000|
|  1|  1|  1| 34.84| 33.55417|
| -1| -1| -1| 26.13| 30.71000|
|  1| -1| -1| 33.64| 33.55417|
| -1|  1| -1| 30.35| 30.71000|
|  1|  1| -1| 33.33| 33.55417|
| -1| -1|  1| 33.80| 30.71000|
|  1| -1|  1| 35.78| 33.55417|
| -1|  1|  1| 32.23| 30.71000|
|  1|  1|  1| 37.76| 33.55417|
| -1| -1| -1| 34.47| 30.71000|
|  1| -1| -1| 31.54| 33.55417|
| -1|  1| -1| 26.10| 30.71000|
|  1|  1| -1| 33.91| 33.55417|
| -1| -1|  1| 30.95| 30.71000|
|  1| -1|  1| 32.04| 33.55417|
| -1|  1|  1| 30.94| 30.71000|
|  1|  1|  1| 33.36| 33.55417| 

- B5 (2 pts) Utilizando la formula del modelo de regresión abreviada, de la pregunta anterior, contesta:
        a) ¿Cuál es el valor minimo de la variable respuesta predicho por el modelo abreviado
        b) ¿Qué combinación de niveles en unidades originales (es decir, NO como -1 y +1) de las variables sugerirías para minimizar la respuesta? 
Res: 
   - Minimizar: 
|  A|  B|  C|     Y|    Y_fit|
|--:|--:|--:|-----:|--------:|
|  1| -1| -1| 31.21| 33.55417| 

- B6 (2 pts) Utilizando la formula del modelo de regresión abreviada, de la pregunta anterior, contesta:
        a) ¿Cuál es el valor maximo de la variable respuesta predicho por el modelo abreviado
        b) ¿Qué combinación de niveles en unidades originales (es decir, NO como -1 y +1) de las variables sugerirías para maximizar la respuesta? 
Res: 
   - Maximizar: 
|  A|  B|  C|     Y| Y_fit|
|--:|--:|--:|-----:|-----:|
| -1| -1| -1| 29.63| 30.71|
 
-----------------------------------------------------------


Pregunta de Diseño 2^k:  02k06t04 
-----------------------------------------------------------
 Entrada  1 #: --  02k06t04 
-----------------------------------------------------------
   # Factores:  3 
   Replicas:  2 
 
  Enunciado:::  
(6) Los alumnos de la clase de Soluciones Integrales tienen como objetivo mejorar la remoción de metales pesados en muestras de agua. Para ello, deciden utilizar una estrategia basada en adsorción con zeolitos. El experimento se diseñó con las siguientes variables: %V%  A: Tipo de zeolito (CLTA, MLTA) %O% B: pH de la solución (8, 10) %O% C: Temperatura (293K, 313K) %O% Y: Metales pesados en agua tras tratamiento

 </li> <p> A continuación se presentan la matriz de diseño (en unidades codificadas) y los resultados de %n% réplicas: </p> 
 %T%  

  Datos::  
|  A|  B|  C|     Y|   Y.1|
|--:|--:|--:|-----:|-----:|
| -1| -1| -1| 94.40| 94.50|
|  1| -1| -1| 94.30| 94.24|
| -1|  1| -1| 94.27| 94.28|
|  1|  1| -1| 94.28| 94.32|
| -1| -1|  1| 94.22| 94.21|
|  1| -1|  1| 94.24| 94.25|
| -1|  1|  1| 94.30| 94.41|
|  1|  1|  1| 94.27| 94.30| 

  Preguntas y sus respuestas:: 

- B1 (2 pts) Calcula y reporta los efectos de los factores principales y las interacciones. 
Res: 
|Factor      |  Efecto|   Coef|
|:-----------|-------:|------:|
|(Intercept) | 188.599| 94.299|
|A           |  -0.049| -0.024|
|B           |   0.009|  0.004|
|C           |  -0.049| -0.024|
|A:B         |   0.026|  0.013|
|A:C         |   0.029|  0.014|
|B:C         |   0.081|  0.041|
|A:B:C       |  -0.076| -0.038|

- B2 (2 pts) En tu hoja de respuestas, dibuja las gráficas de Efectos Principales para los 3 factores. En el espacio a continuación (aqui en Canvas) explica si, a partir de las gráficas, pudiera identificar si uno de los factores pareciera tener un efecto significativo, y explica por qué. 
Res: NA 
 

- B3 (3 pts) Realiza un analisis de varianza Anova y reporta cuales factores e interacciones son significativos. Utiliza un alfa de 0.05 (el valor de F critico a utilizar en las comparaciones está dado en el enunciado del problema) 
Res: 
|Fuente    |   SS_| Df|   MS_|     F0|  Pval|
|:---------|-----:|--:|-----:|------:|-----:|
|A         | 0.010|  1| 0.010|  5.337| 0.050|
|B         | 0.000|  1| 0.000|  0.172| 0.689|
|C         | 0.010|  1| 0.010|  5.337| 0.050|
|A:B       | 0.003|  1| 0.003|  1.547| 0.249|
|A:C       | 0.003|  1| 0.003|  1.856| 0.210|
|B:C       | 0.026|  1| 0.026| 14.825| 0.005|
|A:B:C     | 0.023|  1| 0.023| 13.056| 0.007|
|Residuals | 0.014|  8| 0.002|     NA|    NA|
|Total     | 0.089| 15| 0.006|     NA|    NA| 
Significativos:  A

- B4 (1 pts) A partir de los factores e interacciones que resultaron significativos, da el Modelo de Regresión Abreviado (unicamente con los coeficientes de los  factores e interacciones que resultaron significativos) 
Res: 
Ecuacion de regresion abreviada  y = 94.299 + -0.024 A + -0.024 C + 0.041 B:C + -0.038 A:B:C

Predicción de la respuesta con el mejor modelo: 
|  A|  B|  C|     Y|    Y_fit|
|--:|--:|--:|-----:|--------:|
| -1| -1| -1| 94.40| 94.42687|
|  1| -1| -1| 94.30| 94.30187|
| -1|  1| -1| 94.27| 94.26937|
|  1|  1| -1| 94.28| 94.29688|
| -1| -1|  1| 94.22| 94.22062|
|  1| -1|  1| 94.24| 94.24813|
| -1|  1|  1| 94.30| 94.37812|
|  1|  1|  1| 94.27| 94.25312|
| -1| -1| -1| 94.50| 94.42687|
|  1| -1| -1| 94.24| 94.30187|
| -1|  1| -1| 94.28| 94.26937|
|  1|  1| -1| 94.32| 94.29688|
| -1| -1|  1| 94.21| 94.22062|
|  1| -1|  1| 94.25| 94.24813|
| -1|  1|  1| 94.41| 94.37812|
|  1|  1|  1| 94.30| 94.25312| 

- B5 (2 pts) Utilizando la formula del modelo de regresión abreviada, de la pregunta anterior, contesta:
        a) ¿Cuál es el valor minimo de la variable respuesta predicho por el modelo abreviado
        b) ¿Qué combinación de niveles en unidades originales (es decir, NO como -1 y +1) de las variables sugerirías para minimizar la respuesta? 
Res: 
   - Minimizar: 
|  A|  B|  C|    Y|    Y_fit|
|--:|--:|--:|----:|--------:|
| -1| -1| -1| 94.4| 94.42687| 

- B6 (2 pts) Utilizando la formula del modelo de regresión abreviada, de la pregunta anterior, contesta:
        a) ¿Cuál es el valor maximo de la variable respuesta predicho por el modelo abreviado
        b) ¿Qué combinación de niveles en unidades originales (es decir, NO como -1 y +1) de las variables sugerirías para maximizar la respuesta? 
Res: 
   - Maximizar: 
|  A|  B|  C|     Y|    Y_fit|
|--:|--:|--:|-----:|--------:|
| -1| -1|  1| 94.22| 94.22062|
 
-----------------------------------------------------------


Pregunta de Diseño 2^k:  04k01t04 
-----------------------------------------------------------
 Entrada  1 #: --  04k01t04 
-----------------------------------------------------------
   # Factores:  3 
   Replicas:  2 
 
  Enunciado:::  
(1) Se tiene una maquina de inyección de plástico que produce botellas de plastico. Sin embargo, se desconoce que parametros de operación tienen un efecto sobre la resistencia mecanica de las botellas producidas. Para identificar esto y establecer limites de control, se diseña el siguiente experimento 2^k: %V% A: Temperatura del barril (160, 180) °C %O% B: Presión de inyección (30, 50) lb/pulg2 %O% C: Tiempo de inyección (7, 8) segundos %O% Y: Resistencia mecanica

 </li> <p> A continuación se presentan la matriz de diseño (en unidades codificadas) y los resultados de %n% réplicas: </p> 
 %T%  

  Datos::  
|  A|  B|  C|     Y|   Y.1|
|--:|--:|--:|-----:|-----:|
| -1| -1| -1| 94.40| 94.50|
|  1| -1| -1| 94.30| 94.24|
| -1|  1| -1| 94.27| 94.28|
|  1|  1| -1| 94.28| 94.32|
| -1| -1|  1| 94.22| 94.21|
|  1| -1|  1| 94.24| 94.25|
| -1|  1|  1| 94.30| 94.41|
|  1|  1|  1| 94.27| 94.30| 

  Preguntas y sus respuestas:: 

- B1 (2 pts) Calcula y reporta los efectos de los factores principales y las interacciones. 
Res: 
|Factor      |  Efecto|   Coef|
|:-----------|-------:|------:|
|(Intercept) | 188.599| 94.299|
|A           |  -0.049| -0.024|
|B           |   0.009|  0.004|
|C           |  -0.049| -0.024|
|A:B         |   0.026|  0.013|
|A:C         |   0.029|  0.014|
|B:C         |   0.081|  0.041|
|A:B:C       |  -0.076| -0.038|

- B2 (2 pts) En tu hoja de respuestas, dibuja las gráficas de Efectos Principales para los 3 factores. En el espacio a continuación (aqui en Canvas) explica si, a partir de las gráficas, pudiera identificar si uno de los factores pareciera tener un efecto significativo, y explica por qué. 
Res: NA 
 

- B3 (3 pts) Realiza un analisis de varianza Anova y reporta cuales factores e interacciones son significativos. Utiliza un alfa de 0.05 (el valor de F critico a utilizar en las comparaciones está dado en el enunciado del problema) 
Res: 
|Fuente    |   SS_| Df|   MS_|     F0|  Pval|
|:---------|-----:|--:|-----:|------:|-----:|
|A         | 0.010|  1| 0.010|  5.337| 0.050|
|B         | 0.000|  1| 0.000|  0.172| 0.689|
|C         | 0.010|  1| 0.010|  5.337| 0.050|
|A:B       | 0.003|  1| 0.003|  1.547| 0.249|
|A:C       | 0.003|  1| 0.003|  1.856| 0.210|
|B:C       | 0.026|  1| 0.026| 14.825| 0.005|
|A:B:C     | 0.023|  1| 0.023| 13.056| 0.007|
|Residuals | 0.014|  8| 0.002|     NA|    NA|
|Total     | 0.089| 15| 0.006|     NA|    NA| 
Significativos:  A

- B4 (1 pts) A partir de los factores e interacciones que resultaron significativos, da el Modelo de Regresión Abreviado (unicamente con los coeficientes de los  factores e interacciones que resultaron significativos) 
Res: 
Ecuacion de regresion abreviada  y = 94.299 + -0.024 A + -0.024 C + 0.041 B:C + -0.038 A:B:C

Predicción de la respuesta con el mejor modelo: 
|  A|  B|  C|     Y|    Y_fit|
|--:|--:|--:|-----:|--------:|
| -1| -1| -1| 94.40| 94.42687|
|  1| -1| -1| 94.30| 94.30187|
| -1|  1| -1| 94.27| 94.26937|
|  1|  1| -1| 94.28| 94.29688|
| -1| -1|  1| 94.22| 94.22062|
|  1| -1|  1| 94.24| 94.24813|
| -1|  1|  1| 94.30| 94.37812|
|  1|  1|  1| 94.27| 94.25312|
| -1| -1| -1| 94.50| 94.42687|
|  1| -1| -1| 94.24| 94.30187|
| -1|  1| -1| 94.28| 94.26937|
|  1|  1| -1| 94.32| 94.29688|
| -1| -1|  1| 94.21| 94.22062|
|  1| -1|  1| 94.25| 94.24813|
| -1|  1|  1| 94.41| 94.37812|
|  1|  1|  1| 94.30| 94.25312| 

- B5 (2 pts) Utilizando la formula del modelo de regresión abreviada, de la pregunta anterior, contesta:
        a) ¿Cuál es el valor minimo de la variable respuesta predicho por el modelo abreviado
        b) ¿Qué combinación de niveles en unidades originales (es decir, NO como -1 y +1) de las variables sugerirías para minimizar la respuesta? 
Res: 
   - Minimizar: 
|  A|  B|  C|    Y|    Y_fit|
|--:|--:|--:|----:|--------:|
| -1| -1| -1| 94.4| 94.42687| 

- B6 (2 pts) Utilizando la formula del modelo de regresión abreviada, de la pregunta anterior, contesta:
        a) ¿Cuál es el valor maximo de la variable respuesta predicho por el modelo abreviado
        b) ¿Qué combinación de niveles en unidades originales (es decir, NO como -1 y +1) de las variables sugerirías para maximizar la respuesta? 
Res: 
   - Maximizar: 
|  A|  B|  C|     Y|    Y_fit|
|--:|--:|--:|-----:|--------:|
| -1| -1|  1| 94.22| 94.22062|
 
-----------------------------------------------------------


