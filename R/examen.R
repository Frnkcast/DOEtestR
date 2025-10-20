### Generador de Examen ----------------------------------------
#' Generador de Examen
#'
#' Version 2.9.0 - Se incorpora la opcion de generar el reporte de resultados en markdown -- word. Se incorpora tambien una solucion cruda para el problema de tener preguntas con datos repetidos. Falta una solucion mas elegante que permita incorporar de diferentes tablas.
#' Version 2.8.2 - Se corrige un error para cuando el directorio no tiene una tabla de un tema en particular.
#' Version 2.8.1 - Cambios menores para Quality of Life. Se añaden pausas en la interfaz para el usuario.
#' Version 2.8.0 - Se incorporan el framework para trabajar con los temas B y K
#' Version 2.7.5 - Correccion del idioma y los problemas con el orden de los enunciados
#' Version 2.7.4 - Se separa el ensamblador para facilitar el trabajar con la tabla y poder regenerar el formato sin tener que regenerar la tabla
#' Version 2.7.3 - Correccion para que no se repitan los problemas de ANOVA.
#' Version 2.7.2 - Correcciones menores de formato para cli
#' Version 2.7.1 - Se cambian los ´on.exit(sink())´ y ´sink()´ por  ´closeAllConnections()´
#' Version 2.7 - usando el paquete cli para explicarle al usuario todo lo que se esta haciendo en la función (eg. llamar a las tablas, etc.)
#' Version 2.6.9 - ajuste en el query y argumentos para acomodar a la interfaz doeExam()
#' Version 2.6.8 - usar los paquetes here y folders para los paths relativos
#' Version 2.6.7 - Se llama a la generacion del reporte de resultados inmediatamente que se genera el examen
#' Version 2.6.6 - Ajuste para funcionar dentro del paquete, tablas externas se guardan como internas y se añaden dependencia de paquetes
#' Version 2.6.5
#'
#' @param parcial 1 o 2, según el parcial que se vaya a utilizar
#' @param format HTML o LaTeX, formato en que se generara el archivo de salida del examen
#' @param lang Esp o Eng. Idioma en que se armara el archivo de salida
#'
#' @return Genera una tabla con los enunciados del examen, un archivo de salida de texto (HTML o LaTeX), y actualiza el directorio
#' @export
#'
#' @examples
genExamen <- function(parcial = NULL, format = "HTML", lang = "Esp", rformat = "txt"){
  #query <- "M3-H3-A3"
  wd <- here::here("doetest_out", "tablas")
  avisoA1 <- FALSE
  #wd0 <- "D:/Documents/ITESM/IBT21/BT2004B/003 - Actividades/Examen/"

  #### Estilos de cli() ##########;
  s.error <- cli::combine_ansi_styles("red", "bold")
  s.warn <- cli::combine_ansi_styles("yellow", "underline")
  s.succ <- cli::combine_ansi_styles("green", "italic")
  s.note <- cli::col_cyan
  s.dir <- cli::col_magenta
  s.inst <- cli::combine_ansi_styles("blue", "underline")
  ################################;

  ###------------- Tabla Examen
  Exam <- tibble::tibble(
    id = numeric(),
    Code = character(),
    Topic = character(),
    #z = character(), ##! Ver 2.8.0 Se cambio a lista, para el cambio en la implementacion del idioma
    z = list(),
    Origin = character()
  )

  #query <- "M3-H3-A3"
  ###---------- QUERY
  ## No se dio un parcial como argumento, eg. cuando se llama sola a la función sin la interfaz de doeExam(). Preguntarle al usuario
  if(is.null(parcial)){
    parcial <- menu(c("1er Parcial", "2do Parcial"), title="Que examen se desea generar?")
  } #No hay else, porque lo contrario es que se dio un parcial como argumento.

  if (parcial == 1){
    M <- readline("Cuantos problemas de Media Muestral? ")
    H <- readline("Cuantos problemas de Prueba de Hipotesis? ")
    A <- readline("Cuantos problemas de Anova de 1 Factor? ")

    query <- paste0("M",M,"-H",H,"-A",A)
    
  }else if (parcial == 2){
    #cli::cli_alert_warning(s.warn("Paquete {.pkg doe.testR} Ver 2.0.0, No tienen preparadas funciones para armar un examen de 2do Parcial"))
    #cli::cli_alert_danger(s.error("FIN: Se aborta la generación del examen. Intente otra opción o espere una versión futura."))
    #stop(cli::cli_alert_danger(s.error("FIN: Se aborta la generación del examen. Intente otra opción o espere una versión futura.")), call. = FALSE)
  
    M <- readline("Cuantos problemas de Media Muestral? ")
    H <- readline("Cuantos problemas de Prueba de Hipotesis? ")
    B <- readline("Cuantos de 1F Anova con Bloque? ")
    K <- readline("Cuantos de Diseños 2^k? ")
    query <- paste0("M",M,"-H",H,"-B",B,"-K",K)
  }

  query_split <- stringr::str_split_1(query,"-") ### ESTA MARCANDO ERROR AQUI!!!
    ##Esto esta muy rebuscado... mejor simplemente una lista.

  ###-------------- Generador del Examen
  for (i in 1:length(query_split)){
    x <- query_split[i]
    query_tema <- stringr::str_sub(x,1,1) #Busca la Letra - indica el Tema del query
    query_n <- as.numeric(gsub("[^0-9.-]", "", x)) #Indica el número de elementos a generar

    ##--- Selecciona una tabla por tema del Directorio
    if (query_tema == "M"){
      ## Buscar la tabla para media muestral
      query_selec <- Directorio %>%
        dplyr::filter(Tema == "Media Muestral" &
                        Cont == "Preguntas formuladas") %>%
        dplyr::slice_sample(n = 1) #Regresa un registro del directorio

    }else if (query_tema == "H"){
      ## Buscar la tabla para prueba de hipotesis
      query_selec <- Directorio %>%
        dplyr::filter(Tema == "Prueba de Hipotesis" &
                        Cont == "Preguntas formuladas") %>%
        dplyr::slice_sample(n = 1) #Regresa un registro del directorio

    }else if (query_tema == "A"){
      ## Buscar la tabla para anova 1F
      avisoA1 <- TRUE
      query_selec <- Directorio %>%
        dplyr::filter(Tema == "Anova de 1 Factor" &
                        Cont == "Preguntas formuladas") %>%
        dplyr::slice_sample(n = 1) #Regresa un registro del directorio
      query_selec_Anova <- query_selec
    }else if (query_tema == "B"){
      ## Buscar la tabla para anova 1F+Bloque
      query_selec <- Directorio %>%
        dplyr::filter(Tema == "Anova 1F + Bloque" &
                        Cont == "Preguntas formuladas") %>%
        dplyr::slice_sample(n = 1) #Regresa un registro del directorio
      query_selec_Anova <- query_selec
    }else if (query_tema == "K"){
      ## Buscar la tabla para anova 1F+Bloque
      query_selec <- Directorio %>%
        dplyr::filter(Tema == "Diseño factorial 2^k" &
                        Cont == "Preguntas formuladas") %>%
        dplyr::slice_sample(n = 1) #Regresa un registro del directorio
      query_selec_Anova <- query_selec
      }
  
    ##--- Obtener la tabla
        ##Aqui se rompe el codigo si no hay un codigo correcto. Si no existen tablas de un tema, query_select estara vacio (nrow = 0). Por tanto, al llamar a importDB, quien llamara a parsearQuery, quien no sabra que hacer.
        #se necesita una forma de forzar a armar la tabla!
            ## Ya quedó?
    ### Caso de Error: No se encontró la tabla del tema en el directorio -------------------
    if(nrow(query_selec)==0){
      cli::cli_alert_warning(" {s.warn('Error!')} No se tiene una tabla en el Directorio para este tema. Se procederá a generar antes de continuar con el ensamblado del examen. \n")
      query_tema2 <- dplyr::case_when(query_tema == "M" ~ 1, query_tema == "H" ~ 2,
                                      query_tema == "A" ~ 3, query_tema == "B" ~ 4,
                                      query_tema == "K" ~ 5)
      poblarDirectorio(tema = query_tema2,aislado = TRUE) #Con aislado == TRUE, se reinicia temporalmente el Directorio. Solo tendrá 2 tablas: La de Datos y la de Preguntas del tema en cuestion.
      query_selec <- Directorio %>%
        dplyr::filter(Cont == "Preguntas formuladas") #Regresa del directorio truncado la fila perteneciente a la tabla de preguntas.
      if(query_tema == "A" | query_tema == "B"  | query_tema == "K" ){
        query_selec_Anova <- query_selec
        }
      resetDirectorio() #Reinicia el directorio, se cierra esta interaccion.
      cli::cli_alert_success(" {s.warn('Error corregido con éxito!')} Se procede con la generacion del examen")
    }  
    
    query_path <- paste0(query_selec$DBname,".RDS") #Path del archivo de la tabla
    query_out <- importDB(query_path) ##Importa desde nuevo la tabla desde el archivo. Evita el problema de los nombres de las tablas como variable.

    ## v2.7 - Mensaje de la tabla que se esta usando.
    c_tema <- dplyr::case_when(query_tema == "A" ~ "Anova de 1 Factor",
                        query_tema == "M" ~ "Media Muestral",
                        query_tema == "H" ~ "Prueba de Hipotesis",
                        query_tema == "B" ~ "Anova 1F + Bloque",
                        query_tema == "K" ~ "Diseñño factorial 2^k")
    cli::cli_h3("Del tema {c_tema}")
    cli::cli_alert_success("Se cargó la tabla: {.val {query_selec$DBname}} para la construcción del examen.")
    cli::cli_alert_info("Eligiendo preguntas provenientes de la tabla.")

    
    #####--- Obtener las preguntas
       ###TODO: Se necesita una forma de revisar que no se esten repitiendo los datos. Por mientras, se hara manual. 
    aprov <- F
    while(aprov == F){
      
      ## Seleccion de las preguntas por tema de forma aleatorea.
      if(query_tema == "A"){ ##Para ANOVAS, cada 3ero es de Comp.Multiples, DEBE ser significativo
        query_code_AN <-  query_out$Code #Los codigos de la tabla, disponibles. De ANOVA
        query_code <- c() ##La lista de codigos se genera uno por uno
        k <- 1 ##Variable contador
  
        ## Para los postHoc, busamos solo IDs de sets de datos donde el resultado sea significativo.
        query_code_AN_data_si <- importDB(attr(query_out, "DBorigin")) %>%
          dplyr::filter(signif == "Signif") ##Tabla de menos filas
  
        #query_code_AN_data_si$id ##ids de sets de datos significativos
        ## Ahora, busca los ids significativos en la tabla de codigos
        query_code_AN_ques_si <- query_out %>%
          dplyr::filter(id %in% query_code_AN_data_si$id) ##Tabla de problemas que usan datos significativos
  
        ## Para sacar la lista de codigos, todo depende del tamaño de query_n
        for (n in 1:query_n){
          if(k == 3){ ##Ya es el 3ero, le toca Comp.Multiple
            ## Sample de 1 elemento de la lista de codigos significativos
            query_code[n] <- sample(query_code_AN_ques_si$Code, 1)
  
            ## El elemento añadido, se borra de ambas listas.
            query_code_AN <- query_code_AN[! query_code_AN %in% query_code[n]]
            query_code_AN_ques_si <- query_code_AN_ques_si[!query_code_AN_ques_si %in% query_code[n]]
  
            k <- 1 ##Se reinicia el contador
          }else {
            ## Sample de 1 codigo de la lista query_code_AN
            query_code[n] <- sample(query_code_AN, 1)
  
            ## El elemento añadido, se borra de ambas listas.
            query_code_AN <- query_code_AN[! query_code_AN %in% query_code[n]]
            query_code_AN_ques_si <- query_code_AN_ques_si[!query_code_AN_ques_si %in% query_code[n]]
  
            k <- k + 1 ##Se añade 1 al contador
          }
        }
      }else { #Para todos los demas casos
        query_code <- sample(query_out$Code, query_n, replace = FALSE) #Da n codigos de preguntas - sin repetir. Es un VECTOR
      }
      
      
      ## v2.7 - Mensaje de la tabla que se esta usando.
      cli::cli_alert_info("De la tabla {.val {query_selec$DBname}}, se seleccionaron al azar las preguntas: ")
      cli::cli_ul() ##Abriendo contenedor para la lista
      cli::cli_li(query_code)
      cli::cli_end() ##Cerrando contenedor para la lista
      #cli::cat_line()
      
      ## -- Revision para determinar si se queda o se van las 
      apr <- menu(c("Si, continuar con las preguntas", "No, regenerar la selección"), title = "¿Se desea continuar con estas preguntas, o se desea regenerar la selección? (Nota, revisar que no coincidan los ultimos dos digitos del código, para no repetir los datos muestrales usados para los problemas)")
      aprov <- dplyr::case_when(apr == 1 ~ T,  ## Se aprueban las preguntas, se continua con el siguiente tema
                                apr == 2 ~ F)  ## Se tiene que volver a regenerar.      
      
      if(aprov == F){
        cli::cli_alert_warning("Se volveran a seleccionar preguntas de la tabla.")
      }else {
        cli::cli_alert_success("Se continua con las preguntas seleccionada para el tema.")
        }
    }
    
    
    
    

    ######-------- Implementacion de Idioma: Cambiar el enunciado segun el idioma elegido
    #if (lang == "Esp") {
    #  query_out$z <- query_out$z_Esp
    #}else if (lang == "Eng"){
    #  query_out$z <- query_out$z_Eng
    #} ##Ver 2.8.0 -- Mejor una lista que contenga AMBOS [1] = español, [2] = ingles


    ##Enunciados guardados en la variable z de las tablas.
    #query_ques <- query_out$z[query_out$Code %in% query_code] #!CHANGED
        ##No usar! Puesto que la busqueda se hace respetando el orden de la tabla de preguntas, no la de query_code



    ######-------------- Armando la tabla del examen.
    for (j in 1:query_n){
      Exam <- Exam %>% dplyr::add_row(
        id = j, # Indice del set de datos
        Code = query_code[j],
        Topic = query_tema,
        ### Ver 2.7.4 - Ahora se añade
        #z = query_out$z[query_out$Code %in% query_code[j]],
        ### Ver 2.8.0 - Se cambio la implementación del idioma. Ahora z se guarda en una lista, la tabla del Examen contiene ambos y ya no esta limitada a un solo idioma.
        z = list(tibble::tibble(Esp = query_out$z_Esp[query_out$Code %in% query_code[j]],
                                Eng = query_out$z_Eng[query_out$Code %in% query_code[j]])),
        Origin = query_selec$DBname)
    }
    cli::cli_alert_success("Preguntas añadidas correctamente a la tabla de Examen.")
  } #Pasa al siguiente tema

  ###----- Despues de armar la tabla, actualizar con metadatos
  Exam <- metadataDB(Exam, "EX", "exam")

  DBorigin <- paste(unique(Exam$Origin), collapse = ", ")
  attr(Exam,"DBorigin") <- DBorigin

  #cat(attr(Exam, "Elementos"))

  attr(Exam,"DBname") <-  paste0(attr(Exam,"DBname"),"-",lang)
  attr(Exam, "Idioma") <- lang

  ## AQUI YA ESTA SELECCIONADO EL EXAMEN
  cli::cat_line()
  cli::cli_alert_info("Actualizando {s.dir('Directorio')} con la nueva tabla del Examen.")
  exportDB(Exam)
  #actualizarDirectorio()

  cli::cli_alert_success("{s.dir('Directorio')} actualizado")

  ##--- Ensamblar los enunciados del examen
  cli::cat_line()

  ensamblarExamen(Exam, format, lang) ##Separar el ensamblador de la función de generación de tabla, de tal forma que se puedan regenerar los enunciados a partir de la misma tabla de examen.

  Exam_pkm <- stringr::str_split_1(attr(Exam,'id_pokemon'), pattern = "_")[3]
  Exam_loc <- paste0(attr(Exam,'DBname'),".txt")
  Exam_rep_loc <- paste0(attr(Exam,'DBname'),"_RES.txt")
  
  cli::cli_alert_success("El examen {.val {attr(Exam,'DBname')}}, con apodo {.val {Exam_pkm}}, ha sido generado con éxito y se guardó en: \n")
  cli::cli_text("{.path {here::here('doetest_out', Exam_loc)}} \n\n")
  cli::cat_line()

  if(avisoA1 == TRUE){
    #TODO: Que solo aparezca cuando se usan Anovas de 1 Factor.
    ### Warning! Se deben hacer ajustes!
    cli::cli_alert_warning(" {s.warn('NOTA!')} Los problemas del tema ANOVA 1F estan configurados para ser de tres tipos (recursivos). {.emph Se deben hacer ajustes manuales a cada tipo de problema.}")
    cli::cli_ul() ##Abre contenedor para la lista
    cli::cli_li("{cli::style_bold('Primer ejercicio:')} Se da al alumno una tabla ANOVA incompleta para ser llenada. {s.inst('Se debe borrar la tabla de datos y borrar las celdas necesarias')}")
    cli::cli_li("{cli::style_bold('Segundo ejercicio:')} Se da al alumno una tabla de datos para analizar mediante un ANOVA. {s.inst('Se debe borrar la tabla del ANOVA precargada, dejando solo la tabla de datos.')}")
    cli::cli_li("{cli::style_bold('Tercer ejercicio:')} Se da al alumno una tabla de datos para realizar una prueba Post Hoc. {s.inst('Se debe borrar la tabla del ANOVA precargada')}")
    cli::cli_end() ##Cierra contenedor
  
    readline(prompt="Si ya leiste el aviso anterior, presiona <enter> para continuar.")
  }
    
  cli::cat_line()
  cli::cli_alert_info("Se procede a generar el reporte de respuestas del examen:")

  rep_Exam(Exam, format = rformat) ## Genera una hoja de resultados del examen. Como se da la tabla directa, no deberia haber problema con el orden de los problemas.

  
  cli::cli_alert_success("El reporte de respuestas {.val {attr(Exam,'DBname')}_RES} ha sido generado con éxito, y se encuentra ubicado en: \n")
  cli::cli_text("{.path {here::here('doetest_out', 'reportes',Exam_loc)}} \n\n")
  
  return(Exam)
}



### Generador de Ejercicios de Practica ----------------------------------------
#' Generador de Ejercicios de practica por tema
#'
#' Ver 0.2.0 - Se incorporan el framework para trabajar con los temas B y K
#' Version 0.1.0 - Version inicial
#'
#' @param tema Segun el tema: 1 (Media muestral), 2 (Pruebas de hipotesis), 3 (Anova 1F), 4 (Anova 1F+B), 5 (Diseño 2^k)
#' @param format HTML o LaTeX, formato en que se generara el archivo de salida del examen
#' @param lang Esp o Eng. Idioma en que se armara el archivo de salida
#'
#' @return Genera una tabla con los enunciados del ejercicio, un archivo de salida de texto (HTML o LaTeX), y actualiza el directorio
#' @export
#'
#' @examples
genPractica <- function(tema = NULL, format = "HTML", lang = "Esp"){
  #query <- "M3-H3-A3"
  wd <- here::here("doetest_out", "tablas")
  #wd0 <- "D:/Documents/ITESM/IBT21/BT2004B/003 - Actividades/Examen/"
  
  #### Estilos de cli() ##########;
  s.error <- cli::combine_ansi_styles("red", "bold")
  s.warn <- cli::combine_ansi_styles("yellow", "underline")
  s.succ <- cli::combine_ansi_styles("green", "italic")
  s.note <- cli::col_cyan
  s.dir <- cli::col_magenta
  s.inst <- cli::combine_ansi_styles("blue", "underline")
  ################################;
  
  ###------------- Tabla Examen
  Exam <- tibble::tibble(
    id = numeric(),
    Code = character(),
    Topic = character(),
    z = list(),
    Origin = character()
  )
  
  #query <- "M3-H3-A3"
  ###---------- QUERY
  ## No se dio un tema como argumento, eg. cuando se llama sola a la función sin la interfaz de doeExam(). Preguntarle al usuario
  if(is.null(tema)){
    tema <- menu(c("Media Muestral", #1
                   "Pruebas de Hipotesis", #2
                   "Anova de 1 Factor", #3
                   "Anova 1Factor + Bloque", #4 
                   "Diseños 2^k"), #5
         title = s.note("¿Para qué tema se generarán las preguntas?"))
  } #No hay else, porque lo contrario es que se dio un parcial como argumento.
  
  ### TODO: Esto esta fallando. Ver como corregir
  #P <- dplyr::case_when(tema == 1 ~ readline("Cuantos problemas de Media Muestral? "),
  #                      tema == 2 ~ readline("Cuantos problemas de Prueba de Hipotesis? "),
  #                      tema == 3 ~ readline("Cuantos problemas de Anova de 1 Factor? "),
  #                      tema == 4 ~ readline("Cuantos problemas de Anova 1F + Bloque? "),
  #                      tema == 5 ~ readline("Cuantos problemas de Diseño factorial 2^k? "))
  if(tema == 1){
    P <- readline("Cuantos problemas de Media Muestral? ")
  }else if(tema == 2){
    P <-readline("Cuantos problemas de Prueba de Hipotesis? ")
  }else if(tema == 3){
    P <-readline("Cuantos problemas de Anova de 1 Factor? ")
  }else if(tema == 4){
    P <-readline("Cuantos problemas de Anova 1F + Bloque? ")
  }else if(tema == 5){
    P <-readline("Cuantos problemas de Diseño factorial 2^k? ")
  }
  

  ###-------------- Generador del Examen
  #for (i in 1:length(query_split)){
  #x <- query_split[i]
  query_tema <-  dplyr::case_when(tema == 1 ~ "M",
                                  tema == 2 ~ "H",
                                  tema == 3 ~ "A",
                                  tema == 4 ~ "B",
                                  tema == 5 ~ "K")
  query_n <- as.numeric(P) 
    
  ##--- Selecciona una tabla por tema del Directorio
  if (query_tema == "M"){
      ## Buscar la tabla para media muestral
      query_selec <- Directorio %>%
        dplyr::filter(Tema == "Media Muestral" &
                        Cont == "Preguntas formuladas") %>%
        dplyr::slice_sample(n = 1) #Regresa un registro del directorio
      
  }else if (query_tema == "H"){
      ## Buscar la tabla para prueba de hipotesis
      query_selec <- Directorio %>%
        dplyr::filter(Tema == "Prueba de Hipotesis" &
                        Cont == "Preguntas formuladas") %>%
        dplyr::slice_sample(n = 1) #Regresa un registro del directorio
      
  }else if (query_tema == "A"){
      ## Buscar la tabla para anova 1F
      query_selec <- Directorio %>%
        dplyr::filter(Tema == "Anova de 1 Factor" &
                        Cont == "Preguntas formuladas") %>%
        dplyr::slice_sample(n = 1) #Regresa un registro del directorio
      query_selec_Anova <- query_selec
      
  }else if (query_tema == "B"){
    ## Buscar la tabla para anova 1F+Bloque
    query_selec <- Directorio %>%
      dplyr::filter(Tema == "Anova 1F + Bloque" &
                      Cont == "Preguntas formuladas") %>%
      dplyr::slice_sample(n = 1) #Regresa un registro del directorio
    query_selec_Anova <- query_selec
    
  }else if (query_tema == "K"){
    ## Buscar la tabla para anova 1F+Bloque
    query_selec <- Directorio %>%
      dplyr::filter(Tema == "Diseño factorial 2^k" &
                      Cont == "Preguntas formuladas") %>%
      dplyr::slice_sample(n = 1) #Regresa un registro del directorio
    query_selec_Anova <- query_selec
  }
    
  # 1 Solo tema por Tabla de Ejercicios.
  ##--- Obtener la tabla.
  query_path <- paste0(query_selec$DBname,".RDS") #Path del archivo de la tabla
  query_out <- importDB(query_path) ##Importa desde nuevo la tabla desde el archivo. Evita el problema de los nombres de las tablas como variable.
    
  ## v2.7 - Mensaje de la tabla que se esta usando.
  c_tema <- dplyr::case_when(query_tema == "A" ~ "Anova de 1 Factor",
                             query_tema == "M" ~ "Media Muestral",
                             query_tema == "H" ~ "Prueba de Hipotesis",
                             query_tema == "B" ~ "Anova 1F + Bloque",
                             query_tema == "K" ~ "Diseño factorial 2^k")
    cli::cli_h3("Del tema {c_tema}")
    cli::cli_alert_success("Se cargó la tabla: {.val {query_selec$DBname}} para la construcción del ejercicio.")
    cli::cli_alert_info("Eligiendo preguntas provenientes de la tabla.")
    
    #####--- Obtener las preguntas
    if(query_tema == "A"){ ##Para ANOVAS, cada 3ero es de Comp.Multiples, DEBE ser significativo
      
      query_code_AN <-  query_out$Code #Los codigos de la tabla, disponibles. De ANOVA
      query_code <- c() ##La lista de codigos se genera uno por uno
      k <- 1 ##Variable contador
      
      ## Para los postHoc, busamos solo IDs de sets de datos donde el resultado sea significativo.
      query_code_AN_data_si <- importDB(attr(query_out, "DBorigin")) %>%
        dplyr::filter(signif == "Signif") ##Tabla de menos filas
      
      #query_code_AN_data_si$id ##ids de sets de datos significativos
      ## Ahora, busca los ids significativos en la tabla de codigos
      query_code_AN_ques_si <- query_out %>%
        dplyr::filter(id %in% query_code_AN_data_si$id) ##Tabla de problemas que usan datos significativos
      
      ## Para sacar la lista de codigos, todo depende del tamaño de query_n
      for (n in 1:query_n){
        if(k == 3){ ##Ya es el 3ero, le toca Comp.Multiple
          ## Sample de 1 elemento de la lista de codigos significativos
          query_code[n] <- sample(query_code_AN_ques_si$Code, 1)
          
          ## El elemento añadido, se borra de ambas listas.
          query_code_AN <- query_code_AN[! query_code_AN %in% query_code[n]]
          query_code_AN_ques_si <- query_code_AN_ques_si[!query_code_AN_ques_si %in% query_code[n]]
          
          k <- 1 ##Se reinicia el contador
        }else {
          ## Sample de 1 codigo de la lista query_code_AN
          query_code[n] <- sample(query_code_AN, 1)
          
          ## El elemento añadido, se borra de ambas listas.
          query_code_AN <- query_code_AN[! query_code_AN %in% query_code[n]]
          query_code_AN_ques_si <- query_code_AN_ques_si[!query_code_AN_ques_si %in% query_code[n]]
          
          k <- k + 1 ##Se añade 1 al contador
        }
      }
    }else { #Para todos los demas casos
      query_code <- sample(query_out$Code, query_n, replace = FALSE) #Da n codigos de preguntas - sin repetir. Es un VECTOR
    }
    
    ## v2.7 - Mensaje de la tabla que se esta usando.
    cli::cli_alert_info("De la tabla {.val {query_selec$DBname}}, se eligieron las preguntas: ")
    cli::cli_ul() ##Abriendo contenedor para la lista
    cli::cli_li(query_code)
    cli::cli_end() ##Cerrando contenedor para la lista
    #cli::cat_line()
    
    ######-------------- Armando la tabla del examen.
    for (j in 1:query_n){
      Exam <- Exam %>% dplyr::add_row(
        id = j, # Indice del set de datos
        Code = query_code[j],
        Topic = query_tema,
        ### Ver 2.7.4 - Ahora se añade
        #z = query_out$z[query_out$Code %in% query_code[j]],
        ### Ver 2.8.0 - Se cambio la implementación del idioma. Ahora z se guarda en una lista, la tabla del Examen contiene ambos y ya no esta limitada a un solo idioma.
        z = list(tibble::tibble(Esp = query_out$z_Esp[query_out$Code %in% query_code[j]],
                                Eng = query_out$z_Eng[query_out$Code %in% query_code[j]])),
        Origin = query_selec$DBname)
    }
    cli::cli_alert_success("Preguntas añadidas correctamente a la tabla de Examen.")
  
  ###----- Despues de armar la tabla, actualizar con metadatos
  Exam <- metadataDB(Exam, "EX", "prac")
  
  DBorigin <- paste(unique(Exam$Origin), collapse = ", ")
  attr(Exam,"DBorigin") <- DBorigin
  
  #cat(attr(Exam, "Elementos"))
  
  attr(Exam,"DBname") <-  paste0(attr(Exam,"DBname"),"-",query_tema)
  attr(Exam, "Idioma") <- lang
  
  ## AQUI YA ESTA SELECCIONADO EL EXAMEN
  cli::cat_line()
  cli::cli_alert_info("Actualizando {s.dir('Directorio')} con la nueva tabla del Examen.")
  exportDB(Exam)
  #actualizarDirectorio()
  
  cli::cli_alert_success("{s.dir('Directorio')} actualizado")
  
  ##--- Ensamblar los enunciados del examen
  cli::cat_line()
  
  ensamblarExamen(Exam, format, lang) #TODO Para distinguir examenes de practicas, añadir "prac"
  
  Exam_pkm <- stringr::str_split_1(attr(Exam,'id_pokemon'), pattern = "_")[3]
  Exam_loc <- paste0(attr(Exam,'DBname'),".txt")
  Exam_rep_loc <- paste0(attr(Exam,'DBname'),"_RES.txt")
  
  cli::cli_alert_success("El ejercicio de practica {.val {attr(Exam,'DBname')}}, con apodo {.val {Exam_pkm}}, ha sido generado con éxito, y se encuentra ubicado en: \n")
  cli::cli_text("{.path {here::here('doetest_out', Exam_loc)}} \n\n")
  cli::cat_line()
  
  if(query_tema == "A"){  
    ### Warning! Se deben hacer ajustes manuales al resultado!
  cli::cli_alert_warning(" {s.warn('NOTA!')} Los problemas del tema ANOVA 1F estan configurados para ser de tres tipos (recursivos). {.emph Se deben hacer ajustes manuales a cada tipo de problema.}")
  cli::cli_ul() ##Abre contenedor para la lista
  cli::cli_li("{cli::style_bold('Primer ejercicio:')} Se da al alumno una tabla ANOVA incompleta para ser llenada. {s.inst('Se debe borrar la tabla de datos y borrar las celdas necesarias')}")
  cli::cli_li("{cli::style_bold('Segundo ejercicio:')} Se da al alumno una tabla de datos para analizar mediante un ANOVA. {s.inst('Se debe borrar la tabla del ANOVA precargada, dejando solo la tabla de datos.')}")
  cli::cli_li("{cli::style_bold('Tercer ejercicio:')} Se da al alumno una tabla de datos para realizar una prueba Post Hoc. {s.inst('Se debe borrar la tabla del ANOVA precargada')}")
  cli::cli_end() ##Cierra contenedor
  
  readline(prompt="Si ya leiste el aviso anterior, presiona <enter> para continuar.")
  }
  
  cli::cat_line()
  cli::cli_alert_info("Se procede a generar el reporte de respuestas del examen:")
  
  ## TODO - 03Oct Por alguna razon esta fallando! No logra usar stringr::str_split_1(). Dice que no encuentra los elementos de la tabla
  rep_Exam(Exam) ## Genera una hoja de resultados del examen. Como se da la tabla directa, no deberia haber problema con el orden de los problemas.
  
  cli::cli_alert_success("El reporte de respuestas {.val {attr(Exam,'DBname')}_RES} ha sido generado con éxito, y se encuentra ubicado en: \n")
  cli::cli_text("{.path {here::here('doetest_out', 'reportes',Exam_loc)}} \n\n")
  
  return(Exam)
}


### Ensamblador del examen --------------------------------------------------
#' Ensamblador del examen
#'
#' TODO: Ajustar para cuando se hagan ejercicios de practica (input: modo = "exam" o "prac")
#' Ver 2.2.0 - Se incorporan el framework para trabajar con los temas B y K
#' Ver 2.1.0 - Se ajusta la implementación del idioma
#' Ver 2.0.0 - Se separa de la función genExam() para facilitar el manejo de tablas de examen, y dar mas flexibilidad
#'
#' @param Data Una tabla de Examen. Si no se proporciona, se pregunta por un id para buscar en el Directorio
#' @param format Formato para el archivo de texto de salida. HTML o LaTeX
#' @param lang Idioma para los enunciados. "Eng" o "Esp"
#'
#' @return Un archivo de salida, .txt, con el formato especificado
#' @export
#'
#' @examples
ensamblarExamen <- function(Data = NULL, format = "HTML", lang = "Esp"){
  ## Si no se da un query como argumento, pedirlo al usuario.
  if (is.null(Data)){
    query <- readline("Da el codigo del examen a ensamblar: ")
    #query_nombre <- parsearQuery(exam_query)
    Exam <- importDB(query) ## Busca en el Directorio una tabla que coincida con el ID del query
  } else {
    Exam <- Data ##Se dio una tabla como argumento, se renombra para trabajar con ella internamene.
  }
  DBorigin <- paste(unique(Exam$Origin), collapse = ", ")

  cli::cli_alert_info("Ensamblando los enunciados del examen.")

  ###---------- SETUP: Enunciados de ANOVAS
  ## Ver 2.6.6 (genExamen) - Anov_quest0 y MComp_statement ya estan precargados en sysdata.rda
  #Anov_quest0 <- readr::read_delim(paste0(wd0,"ANOVA_EnunProb.csv"),delim ="\t")
  Anov_quest <- Anov_quest0 %>% dplyr::filter(Lang == lang)
  #A1FBloq_quest0 <- readr::read_delim(paste0(wd0,"1FBloque_EnunProb.csv"),delim ="\t")
  A1FBloq_quest <- A1FBloq_quest0 %>% dplyr::filter(Lang == lang)
  #D2k_quest0 <- readr::read_delim(paste0(wd0,"2k_EnunProb.csv"),delim ="\t")
  D2k_quest <- D2k_quest0 %>% dplyr::filter(Lang == lang)
  #MComp_statement0 <- readr::read_delim(paste0(wd0,"ANOVA_MultiComp.csv"),delim ="\t")
  MComp_statement <- MComp_statement0 %>% dplyr::filter(Lang == lang)
  #[1] es Fisher, [2] es Tukey


  #####--------------- Archivo de Salida: ENSAMBLADOR
  out <- attr(Exam,"DBname")
  #wd2 <- "D:/Documents/ITESM/IBT21/BT2004B/003 - Actividades/Examen/DB/Output/"
  wd <- here::here("doetest_out") ##! EL EXAMEN (texto) SE GUARDA EN LA CARPETA ARRIBA DE LAS TABLAS Y LOS REPORTES. Es el output "principal" de la función

  outfile <- here::here(wd, paste0(out,".txt")) ##Mejor guardarlo en formato txt

  ##Instrucciones HTML
  ## Ver 2.6.6 - Anov_quest0 y MComp_statement ya estan precargados en sysdata.rda
  #Exam_INST0 <- readr::read_delim(paste0(wd0,"Examen_INST.csv"),delim = "\t")
  Exam_INST <- Exam_INST0 %>% dplyr::filter(Lang == lang)
  Exam_INST <- Exam_INST$Enunciado[1]
  Exam_INST <- stringr::str_replace(Exam_INST,"%CODIGO%",attr(Exam,"id_pokemon"))

  ##NOTA!!! La implementación del idioma:
      ## Ahora z es una lista que contiene los enunciados z_Esp y z_Eng en una tibble
      ## Exam$z[[j]][lang][[1]] Se usa para llamar a un enunciado particular.

  j <- 1 ##Contador para ANOVA
  sink(outfile)
  cat("------------Examen--------------------------\n")
  cat("- Alias:: ", attr(Exam, "id_pokemon"), "\n")
  cat("Problemas recopilados:::", attr(Exam, "Elementos"),"\n")
  cat("- Usando las bases de datos:: ", DBorigin, "\n")
  cat("--------------------------------------------\n")
  z <- c()
  if (format == "LaTeX"){ ### Formato LATEX
    for(i in 1:nrow(Exam)){
      if(Exam$Topic[i] == "M"){ ##Problemas de Media Muestral
        cat("\\question[2] \n")
        #cat(Exam$z[i], "\\droppoints \\ansline \n\n")
        z <- Exam$z[[i]][lang][[1]]
        #z <- stringr::str_replace(z,stringr::fixed("X &sim; N( &mu;  ="), "$X \\sim N \\left( {\\mu  =")
        #z <- stringr::str_replace(z,stringr::fixed(") , se obtuvo"), "} \\right)$, se obtuvo")
        #z <- stringr::str_replace(z,stringr::fixed(" S = "), " S = $")
        #z <- stringr::str_replace(z,stringr::fixed(". Dado que P(X&#772;"),"$. Dado que $P\\left( {\\overline{X}")
        #z <- stringr::str_replace(z,stringr::fixed(") = "), " } \\right) = ")
        #z <- stringr::str_replace(z,stringr::fixed(". \n"), "$. \n")
        #z <- stringr::str_replace(z,stringr::fixed("&mu;"),"$\\mu$")
        #cat(Exam$z[[i]][lang][[1]], "\\droppoints \\ansline \n\n")
        cat(z, "\\droppoints \\ansline \n\n")

      }else if(Exam$Topic[i] == "H"){ ##Problemas de Prueba de Hipotesis
        cat("\\question[6] \n")
        #cat(Exam$z[i], "\\droppoints \\ansline \n \n")
        z <- Exam$z[[i]][lang][[1]]
        #z <- stringr::str_replace(z,stringr::fixed("que tenía una desviación de &sigma;<sub>1</sub>"),"que tenía una desviación de ${{\\sigma}_1}")
        #z <- stringr::str_replace(z,stringr::fixed(", se obtuvo una muestra de tamaño n<sub>1</sub> ="),"$, se obtuvo una muestra de tamaño ${n_1} =")
        #z <- stringr::str_replace(z,stringr::fixed("Del primer grupo,  se obtuvo una muestra de tamaño n<sub>1</sub> ="),"Del primer grupoo,  se obtuvo una muestra de tamaño ${n_1} =")
        #z <- stringr::str_replace(z,stringr::fixed(", que tuvo un promedio de X&#772;<sub>1</sub> ="), "$, que tuvo un promedio de ${{\\overline X}_1} =")
        #z <- stringr::str_replace(z,stringr::fixed(", y una desviación S<sub>1</sub> "), "$, y una desviación ${S_1}")
        #z <- stringr::str_replace(z,stringr::fixed(" \n"), "$ \n")
        
        #z <- stringr::str_replace(z,stringr::fixed("que tenía una desviación de &sigma;<sub>2</sub>"),"que tenía una desviación de ${{\\sigma}_2}")
        #z <- stringr::str_replace(z,stringr::fixed(", se obtuvo una muestra de tamaño n<sub>2</sub> ="),"$, se obtuvo una muestra de tamaño ${n_2} =")
        #z <- stringr::str_replace(z,stringr::fixed("Del segundo grupo,  se obtuvo una muestra de tamaño n<sub>2</sub> ="),"Del segundo grupo, se obtuvo una muestra de tamaño ${n_2} =")
        #z <- stringr::str_replace(z,stringr::fixed(", que tuvo un promedio de X&#772;<sub>2</sub> ="), "$, que tuvo un promedio de ${{\\overline X}_2} =")
        #z <- stringr::str_replace(z,stringr::fixed(", y una desviación S<sub>2</sub> "), "$, y una desviación ${S_2}")
        #z <- stringr::str_replace(z,stringr::fixed("un valor de &alpha;"), "un valor de $ \\alpha")
        #z <- stringr::str_replace(z,stringr::fixed(", prueba las siguientes hipotesis"), "$, prueba las siguientes hipotesis")
        #z <- stringr::str_replace(z,stringr::fixed(" H0: &mu;<sub>1</sub> - &mu;<sub>2</sub> = 0  vs HA: &mu;<sub>1</sub> - &mu;<sub>2</sub>"), " ${H_0}:{\\mu _1} - {\\mu _2} = 0$  vs ${H_A}:{\\mu _1} - {\\mu _2}")
        #z <- stringr::str_replace(z,stringr::fixed("&ne;"), "\\ne")
        #z <- stringr::str_replace(z,stringr::fixed(" , y da tu conclusión."), "$ , y da tu conclusión.")
        
        #cat(Exam$z[[i]][lang][[1]], "\\droppoints \\ansline \n\n")
        cat(z, "\\droppoints \\ansline \n\n")
        

      }else if(Exam$Topic[i] == "A"){ ##Problemas de ANOVA
        cat("\\question[10] \n")
        #j <- sample(c(1,3),1)
        ##-- 1era parte del Enunciado
        #cat(Exam$z[i], "\n")
        cat(Exam$z[[i]][lang][[1]], "\n\n")

        if (j == 3){
          ###-- Prepara la 2da parte del enunciado de ANOVA/Multi Comp
          SD <- sample(c(1,2),1) ##1 = Fisher; 2 = Tukey
          MComp_stat <- MComp_statement$Enunciado[SD]

          ###-- Obten las tablas del problema y de los datos
          DB_A1MComp_prob <- importDB(Exam$Origin[i])
          DB_A1MComp_data <- importDB(attr(DB_A1MComp_prob,"DBorigin"))

          ##Fila, donde coinciden codigo en tablas de Examen y Prob.
          A1MComp_p_fila <- grep(Exam$Code[i],
                                 DB_A1MComp_prob$Code)
          A1MComp_p <- DB_A1MComp_prob[A1MComp_p_fila, ] #Con el numero de fila, regresa la fila entera de la tabla de problemas del tema
          A1MComp_d <- DB_A1MComp_data[A1MComp_p$id[1], ] #Con el id del set de datos, regresa la fila entera de la tabla de datos.

          ###--- Reemplazo de datos de LSD/Tukey para el enunciado
          if (SD == 1){ ## LSD
            MComp_stat <- stringr::str_replace(
              MComp_stat,"%MSE%", as.character(
                A1MComp_d$lsd[[1]][[1]]))
            MComp_stat <- stringr::str_replace(
              MComp_stat,"%df%", as.character(
                A1MComp_d$lsd[[1]][[2]]))
            MComp_stat <- stringr::str_replace(
              MComp_stat,"%ts%", as.character(
                A1MComp_d$lsd[[1]][[5]]))
          }else if (SD == 2){ ## Tukey
            MComp_stat <- stringr::str_replace(
              MComp_stat,"%MSE%", as.character(
                A1MComp_d$tukey[[1]][[1]]))
            MComp_stat <- stringr::str_replace(
              MComp_stat,"%df%", as.character(
                A1MComp_d$tukey[[1]][[2]]))
            MComp_stat <- stringr::str_replace(
              MComp_stat,"%ts%", as.character(
                A1MComp_d$tukey[[1]][[5]]))
          }
          ##--- 2da parte del Enunciado
          cat(MComp_stat, "\n")
          ##--- 3era parte del Enunciado
          cat(dplyr::filter(Anov_quest,
                            Anov_quest$Format == "LaTeX")$Enunciado[j])
          cat("\n\n\n")
          j <- 1 ## Reinicia el contador para que no pase de 3

        }else {
          ##---- Resto del enunciado para problemas normales
          cat(dplyr::filter(Anov_quest,
                            Anov_quest$Format == "LaTeX")$Enunciado[j])
          cat("\n\n\n")
          j <- j + 1}
      }else if(Exam$Topic[i] == "B"){ ## Ejercios de Anova 1F + Bloque
        cat(Exam$z[[i]][lang][[1]], "[10pt] \n\n")
        cat(dplyr::filter(A1FBloq_quest,
                          A1FBloq_quest$Format == "LaTeX")$Enunciado[1])
        cat("\n\n\n")
        
      }else if(Exam$Topic[i] == "K"){ ## Ejercicios de Diseño 2k
        cat(Exam$z[[i]][lang][[1]], "[12pt] \n\n\n")
        cat(dplyr::filter(D2k_quest,
                          D2k_quest$Format == "LaTeX")$Enunciado[1])
        cat("\n\n\n")
      }
      
    }
  }
  else if(format == "HTML"){
    ## Instrucciones

    cat(Exam_INST, "\n\n")

    cat('<ol style="list-style-type: decimal;">', "\n")
    for(i in 1:nrow(Exam)){
      cat('<li>')
      if(Exam$Topic[i] == "M"){ ## Ejercicio de Media Muestral
        #cat(Exam$z[i], "[2pt] \n\n\n")
        cat(Exam$z[[i]][lang][[1]], "[2pt] \n\n\n")

      }else if(Exam$Topic[i] == "H"){ ## Ejercicio de Pr.de Hipotesis
        #cat(Exam$z[i], "[5pt] \n\n\n")
        cat(Exam$z[[i]][lang][[1]], "[5pt] \n\n\n")

      }else if(Exam$Topic[i] == "A"){ ## Ejercicio de ANOVA
        #j <- sample(c(1,3),1)
        ##--Carga la 1era parte del enunciado de ANOVA
        #cat(Exam$z[i], "\n\n")
        cat(Exam$z[[i]][lang][[1]], " \n\n\n")

        if (j == 3){ ## Ejercicio de LSD/HSD
          ###-- Prepara la 2da parte del enunciado de ANOVA/Multi Comp.
          SD <- sample(c(1,2),1) ##1 = Fisher; 2 = Tukey
          MComp_stat <- MComp_statement$Enunciado[SD]

          ###-- Obten las tablas del problema y de los datos
          DB_A1MComp_prob <- importDB(Exam$Origin[i])
          DB_A1MComp_data <- importDB(attr(DB_A1MComp_prob,"DBorigin"))

          ##Fila, donde coinciden codigo en tablas de Examen y Prob.
          A1MComp_p_fila <- grep(Exam$Code[i],
                                 DB_A1MComp_prob$Code)
          A1MComp_p <- DB_A1MComp_prob[A1MComp_p_fila, ] #Con el numero de fila, regresa la fila entera de la tabla de problemas del tema.
          A1MComp_d <- DB_A1MComp_data[A1MComp_p$id[1], ] #Con el id del set de datos, regresa la fila entera de la tabla de datos.

          ###--- Reemplazo de datos de LSD/Tukey para el enunciado
          if (SD == 1){ ## LSD
            MComp_stat <- stringr::str_replace(
              MComp_stat,"%MSE%", as.character(round(
                A1MComp_d$lsd[[1]][[1]],3)))
            MComp_stat <- stringr::str_replace(
              MComp_stat,"%df%", as.character(round(
                A1MComp_d$lsd[[1]][[2]],3)))
            MComp_stat <- stringr::str_replace(
              MComp_stat,"%ts%", as.character(round(
                A1MComp_d$lsd[[1]][[5]],3)))
          }else if (SD == 2){ ## Tukey
            MComp_stat <- stringr::str_replace(
              MComp_stat,"%MSE%", as.character(round(
                A1MComp_d$tukey[[1]][[1]],3)))
            MComp_stat <- stringr::str_replace(
              MComp_stat,"%df%", as.character(round(
                A1MComp_d$tukey[[1]][[2]],3)))
            MComp_stat <- stringr::str_replace(
              MComp_stat,"%ts%", as.character(round(
                A1MComp_d$tukey[[1]][[5]],3)))
          }

          ## 2da parte del Enunciado
          cat(MComp_stat, "\n\n")
          ## 3era parte del Enunciado
          cat(dplyr::filter(Anov_quest,
                            Anov_quest$Format == "HTML")$Enunciado[j])
          cat("\n\n\n")
          j <- 1 ## Reinicia el contador para que no pase de 3

        }else { ## Ejercicios normales de Anova
          cat(dplyr::filter(Anov_quest,
                            Anov_quest$Format == "HTML")$Enunciado[j])
          cat("\n\n\n")
          j <- j + 1}

      }else if(Exam$Topic[i] == "B"){ ## Ejercios de Anova 1F + Bloque
        cat(Exam$z[[i]][lang][[1]], "[10pt] \n\n")
        cat(dplyr::filter(A1FBloq_quest,
                          A1FBloq_quest$Format == "HTML")$Enunciado[1])
        cat("\n\n\n")
        
      }else if(Exam$Topic[i] == "K"){ ## Ejercicios de Diseño 2k
        cat(Exam$z[[i]][lang][[1]], "[12pt] \n\n\n")
        cat(dplyr::filter(D2k_quest,
                          D2k_quest$Format == "HTML")$Enunciado[1])
        cat("\n\n\n")
      }

      cat('<br /><br /></li>', "\n")
    }
    cat('</ol>')
  }

  #sink()
  closeAllConnections()   # .........................
  file.show(outfile)

}




### Reporte de respuestas del examen ----------------------------------------
#' Reporte de respuestas del examen
#'
#' Ver 2.5.0 - Cambio importante. Ahora se puede generar el reporte usando un template en  Markdown.
#' Ver 2.4.0 - Se incorporan el framework para trabajar con los temas B y K
#' Ver 2.3.1  - Se cambian los ´on.exit(sink())´ y ´sink()´ por  ´closeAllConnections()´
#' Ver 2.3 - usar los paquetes here y folders para los paths relativos
#' Ver 2.2 - Se altera el input, de tal forma que se pueda usar la función justo despues de crear un examen.
#' Ver 2.1
#'
#' @param Data Pide una tabla de enunciados de examen, en caso de no darla, pide un id para buscar en el directorio.
#'
#' @return Genera un archivo de texto con las preguntas del examen.
#' @export
#'
#' @examples
rep_Exam <- function(Data = NULL, format = "txt"){
  ## Ver 2.2 - Si no se da un query como argumento, pedirlo al usuario.
  if (is.null(Data)){
    query <- readline("Da el codigo del examen a buscar: ")
    #query_nombre <- parsearQuery(exam_query)
    Exam <- importDB(query) ## Busca en el Directorio una tabla que coincida con el ID del query
    #Nota - importDB() ya incluye una llamada a la función parsearQuery(), no es necesario hacerlo dos veces.
  } else {
    Exam <- Data ##Se dio una tabla como argumento, se renombra para trabajar con ella internamene.
  }
  
  ##Opcion A: De la lista proveniente del examen
  
  #wd <- "D:/Documents/ITESM/IBT21/BT2004B/003 - Actividades/Examen/DB/Output/"
  wd <- here::here("doetest_out", "reportes")
  template <- system.file("rmd", "Template_ExamRes.Rmd", package = "DOEtestR") 
  
  
  #TODO: Esto genera error! - No se generaron los 
  Exam_elementos <- stringr::str_split_1(attr(Exam, "Elementos"), ", ") #Codigos de preguntas
  Exam_origen <- stringr::str_split_1(attr(Exam, "DBorigin"), ", ") #Nombre de tablas de donde sacaron los codigos de preguntas
  Examen_idioma <- attr(Exam, "Idioma")
  
  ## Buscar en el elemento segun la tabla de origen?? O buscar la tabla de origen según el elemento??
  
  if (format == "txt"){
    #------------ Formato:: Archivo de texto
    out <- attr(Exam, "DBname")
    outfile <- here::here(wd,paste0(out,"_RES.txt"))
    
    sink(outfile)
    cat("------------Respuestas del Examen----------------\n")
    cat("Generada para el examen::: ", attr(Exam, "DBname"), "\n")
    cat("- Alias:: ", attr(Exam, "id_pokemon"), "\n")
    cat("Problemas recopilados:::", attr(Exam, "Elementos"),"\n")
    cat("- Usando las bases de datos:: ", attr(Exam,"DBorigin"), "\n")
    cat("-------------------------------------------------\n")
    
    ###----- Busqueda de preguntas del examen
    for (i in 1:length(Exam_origen)){ #Vamos tabla por tabla
      query_tabla <- importDB(Exam_origen[i]) # Importa la tabla de problemas de un tema
      Exam_elementos_sub <- Exam %>%
        dplyr::filter(Origin == Exam_origen[i]) # Subset con las preguntas que surgen de una determinada tabla
      query_p_origin <- attr(query_tabla, "DBorigin") # Nombre de la tabla de origen de los datos de las preguntas
      query_p_datos <- importDB(query_p_origin) # Importa la tabla de origen de los datos de las preguntas.
      
      for (j in 1:nrow(Exam_elementos_sub)){ #Vamos pregunta por pregunta
        query_p_fila <- grep(Exam_elementos_sub$Code[j],
                             query_tabla$Code) #Fila, en la tabla de problemas en la que esta el j-esimo codigo del examen
        query_pregunta <- query_tabla[query_p_fila,] #Con el numero de fila, regresa la fila entera de la tabla de problemas del tema.
        
        ###---- Ahora, a generar el archivo de salida, poco a poco, segun el tema...
        if (stringr::str_detect(Exam_origen[i], "MM-") == T){ #Son de MediaMues
          cat("Pregunta de Media Muestral: ", query_pregunta$Code, "\n")
          rep_Q_MM(query_p_datos,query_pregunta,1)
          cat("\n")
        }else if (stringr::str_detect(Exam_origen[i], "PH-") == T){ #Son de Prueb.Hip
          cat("Pregunta de Prueba de Hipotesis: ", query_pregunta$Code, "\n")
          rep_Q_PH(query_p_datos,query_pregunta,1)
          cat("\n")
        }else if (stringr::str_detect(Exam_origen[i], "A1-") == T){ #Son de Anova
          cat("Pregunta de Anova de 1 Factor: ", query_pregunta$Code, "\n")
          rep_Q_A1(query_p_datos,query_pregunta,1)
          cat("\n")
        }else if (stringr::str_detect(Exam_origen[i], "AB-") == T){ #Son de Anova1f+B
          cat("Pregunta de Anova 1F + Bloque: ", query_pregunta$Code, "\n")
          rep_Q_AB(query_p_datos,query_pregunta,1)
          cat("\n")
        }else if (stringr::str_detect(Exam_origen[i], "2k-") == T){ #Son de Diseño 2k
          cat("Pregunta de Diseño 2^k: ", query_pregunta$Code, "\n")
          rep_Q_2k(query_p_datos,query_pregunta,1)
          cat("\n")
        }
      }#Pregunta por pregunta
    }#Tabla por tabla
    #sink()
    closeAllConnections()   # .........................
    file.show(outfile)
  } else if (format == "Rmd"){ ### Esto es lo nuevo 09/01/2025
    
    ## El markdown se puede knittea una sola vez, ¿Como entonces lograr que se vaya armando con las tablas necesarias?
    ## Una lista por tema?? O una lista general, considerando que pueden haber temas distintos...
    
    #Exam_origen <- stringr::str_split_1(attr(Exam, "DBorigin"), ", ") 
    Exam_ques <- list()
    for (i in 1:length(Exam_origen)){
      query_tabla <- importDB(Exam_origen[i]) #Importar la tabla de preguntas del tema
      Exam_ques[[i]] <- query_tabla[query_tabla$Code %in% Exam$Code,] #Filas de la tabla de preguntas que fueron usadas para el examen.
    }
    Exam_ques ## Lista con todas las preguntas usadas en el examen
    
    rmarkdown::render(template, params = list(E = Exam, Q=Exam_ques),
                      output_file=paste0(here::here(wd,attr(Exam,"DBname")),"_RES", ".docx"))
    
  }
}
