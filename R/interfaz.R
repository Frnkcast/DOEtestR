#### INTERFAZ DE USUARIO PARA GENERAR EXAMENES ---------------------------
#' Interfaz de usuario: Generador de Examenes de Diseño de Experimentos
#'
#' Ver 1.4.1 - Se pueden generar ls reportes de respuesta en formato word (con un archivo markdown intermedio, y un template ya incluido en el paquete)
#' Ver 1.4.0 - Se empieza el retrabajo para el formato de respuestas. Hay una nueva pregunta en la interfaz, y nuevas funciones internas a llamar.
#' Ver 1.3.1 - Ajustado, ahora examen 2 incluye preguntas de M, H, B y K.
#' Ver 1.3.0 - Se incorporan el framework para trabajar con los temas B y K, falta probarlo
#' Ver 1.2.0 - poblarDirectorio cambio, ahora se debe llamar cada vez que se use un tema nuevo. Los temas son definidos por el parcial
#' Ver 1.1.0 - Correccion para los casos donde el directorio existe, pero esta vacio
#' Ver 1.0.2 
#'
#' @return
#' @export
#'
#' @examples
doeExam <- function(){

  #### Estilos de cli() ##########;
  s.error <- cli::combine_ansi_styles("red", "bold")
  s.warn <- cli::combine_ansi_styles("yellow", "underline")
  s.succ <- cli::combine_ansi_styles("green", "italic")
  s.note <- cli::col_cyan
  s.dir <- cli::col_magenta
  s.inst <- cli::combine_ansi_styles("blue", "underline")
  ################################;

  ###---------------- PRIMERA PARTE: SET-UP DE LAS CARPETAS
  cli::cli_h1(paste0("Generador de Examenes: ", cli::col_green("Diseño de Experimentos")))
  cli::cli_text(s.note("Paquete creado por: Fco.C-E. ", "{{zzz}}°°°( -_-)>c[_]"))
  cli::cli_text(s.note("Version:  1.4.0"))
  ## Preguntar por la carpeta:
  cli::cli_h2("PARTE 1: Set-Up")
  cli::cli_text("La carpeta de trabajo de esta aplicación esta ubicada en: \n {.path {here::here('doetest_out')}}\n")
  ## Cargar directorio
  ## Revisar si existe la carpeta, si no - las crea con folder::folder. 3 Escenarios
  if(dir.exists(here::here("doetest_out","tablas")) == FALSE){
    ## Escenario 1: No existe el Directorio ni las carpetas. Crear uno nuevo.
    cli::cli_alert_warning(s.warn("No se encontró una carpeta de trabajo, se procederá a generar."))
    ## Genera los folders: doetest_out, tablas y reportes
    folders::create_folders(here::here("doetest_out", c("tablas", "reportes")))
    cli::cli_alert_success(s.succ("Se generaron las carpetas:"))
    cli::cat_bullet(s.succ(c("~/doetest_out/tablas", "~/doetest_out/reportes")))

    ## Despues de crear las carpetas, inicializa un Directorio.
    cli::cli_alert_info("Se procede a generar un nuevo {s.dir('Directorio')}:")
    clearDirectorio()
    cli::cat_line()
    cli::cli_alert_success("Se estableció un nuevo {s.dir('Directorio')}")

    Dir_vacio <- TRUE #FLAG: Directorio esta vacio!

  }else if(length(list.files(here::here("doetest_out", "tablas"), pattern =  ".RDS")) ==  0){
    ##Escenario 2. La carpeta existe, pero  el Directorio esta VACIO
    cli::cli_alert_warning(s.warn("El {s.dir('Directorio')} esa vacio. Se debe proseguir con cuidado."))
    ## Genera los folders: doetest_out, tablas y reportes
    folders::create_folders(here::here("doetest_out", c("tablas", "reportes")))

    ## Despues de crear las carpetas, inicializa un Directorio.
    cli::cli_alert_info("Se procede a generar un nuevo {s.dir('Directorio')}: \n")
    clearDirectorio()
    cli::cli_alert_success("Se estableció un nuevo {s.dir('Directorio')}")
    Dir_vacio <- TRUE #FLAG: Directorio esta vacio!

  }else{
    ## Escenario 3. Carpetas existen, resetear el Directorio con los archivos que ahi se encuentran
    cli::cli_alert_success(s.succ("Se encontró la carpeta ~/doetest_out/"))
    cli::cli_alert_info("Se procede a cargar el {s.dir('Directorio')} \n")
    resetDirectorio()
    cli::cli_alert_success("{s.dir('Directorio')} cargado.")

    Dir_vacio <- FALSE #FLAG: Directorio no esta vacio
    }

  cli::cat_rule()

  ###----------- SEGUNDA PARTE: SET-UP DE LAS CARPETAS
  cli::cli_h2("PARTE 2: Tablas")
  ## Presentar al usuario una descripción del Directorio. #CHANGED Ya no!  No es necesario presentarlo dos veces
  #cat("Información del directorio actual: \n")
  #describirDirectorio()

  cat("\n")
  ## Preguntar: ¿Se hara un examen desde 0, o se desea usar las tablas del directorio?
  optablas <- menu(c("Generar nuevas tablas", "Usar tablas del Directorio actual"),
                   title = s.note("¿Qué tablas se usaran para generar el examen?"))

  if(Dir_vacio == TRUE){
    ### AQUI UN RIESGO! Si el directorio esta vacio,
    cli::cli_alert_warning(s.warn("Nota! El {s.dir('Directorio')} esta vacio. Se forzará la opción 1 (Generar nuevas tablas)"))
    optablas <- 1 #Se fuerza la opción 1. Protección contra posibles errores
  }

  ## TODO: Incorporar la opcion de elegir los temas del examen libremente.
  cli::cat_line()
  opparcial <- menu(c("Primer parcial", "Segundo parcial"),
                    title = s.note("¿Qué examen se generará?"))
  
  ### Segun el parcial son los temas a usar 
  if(opparcial == 1){
    if(optablas == 1){
        # Parcial 1 llema los temas: 
          # - Media Muestral, 
          # - Prueba de Hipotesis, 
          # - Anova de 1 Factor
      cli::cli_alert_info("Se procederá a generar tablas para todos los temas.")
      #poblarDirectorio(opparcial,aislado = TRUE)
      poblarDirectorio(tema = 1,aislado = TRUE) #Se generan tablas para el tema 1 - Dist. Muestral
      poblarDirectorio(tema = 2,aislado = FALSE) #Se generan tablas para el tema 2 - Prueb.Hipotesis
      poblarDirectorio(tema = 3,aislado = FALSE) #Se generan tablas para el tema 3 - Anova 1F
      # Solo el primero recibe aislado = TRUE, para iniciar un Directorio temporal. 
      
      describirDirectorio() #Describir el nuevo directorio
    }else if(optablas == 2){
      cli::cli_alert_info("Se procederá con las tablas ya existentes.")
      #resetDirectorio() #! Ya se reinicio arriba, no volver a correr la función innecesariamente.
      ## Hay que llamar a la función: genExamen(). Darle un query desde aqui!
    }

  
  }else if(opparcial == 2){
    if(optablas == 1){
      #cli::cli_alert_warning(s.warn("Paquete {.pkg doe.testR Version 2.0.0}, no tiene preparadas funciones para armar un examen de 2do Parcial."))
    #cli::cli_alert_danger(s.error("FIN: Se aborta la generación del examen. Intente otra opción o espere una versión futura."))
      #stop(cli::cli_alert_danger(s.error("FIN: Se aborta la generación del examen. Intente otra opción o espere una versión futura.")), call. = FALSE)
      
      poblarDirectorio(tema = 1,aislado = TRUE) #Se generan tablas para el tema 1 - Dist. Muestral
      poblarDirectorio(tema = 2,aislado = FALSE) #Se generan tablas para el tema 2 - Prueb.Hipotesis
      poblarDirectorio(tema = 4,aislado = FALSE) #Se generan tablas para el tema 4 - Anova 1F+Bloque
      poblarDirectorio(tema = 5,aislado = FALSE) #Se generan tablas para el tema 5 - Diseños 2k
      #describirDirectorio() #Describir el nuevo directorio
      # Solo el primero recibe aislado = TRUE, para iniciar un Directorio temporal. 
      
    }else if(optablas == 2){
      cli::cli_alert_info("Se procederá con las tablas ya existentes.")
      #resetDirectorio() #! Ya se reinicio arriba, no volver a correr la función innecesariamente.
      ## Hay que llamar a la función: genExamen(). Darle un query desde aqui!
    }
  }

  cli::cat_line()#Newline
  cli::cat_rule()

  ###---------------------- PARTE 3: GENERAR EXAMEN

  cli::cli_h2("PARTE 3: Examen")

  oplang <- menu(c("Español", "Ingles"), title = s.note("¿Idioma?"))
  opformat <- menu(c("HTML", "LaTeX"), title = s.note("¿Formato de salida?"))
  oprformat <- menu(c("txt", "Word"), title = s.note("¿Formato para el reporte de respuestas/solucionario?"))

  lang <- dplyr::case_when(oplang == 1 ~ "Esp", oplang == 2 ~ "Eng")
  format <- dplyr::case_when(opformat == 1 ~ "HTML", opformat == 2 ~ "LaTeX")
  rformat <- dplyr::case_when(oprformat == 1 ~ "txt", oprformat == 2 ~ "Rmd")

  Examen <- genExamen(parcial = opparcial, lang = lang, format = format, rformat = rformat)
  cli::cat_line()
  cli::cli_alert_success(s.succ("Se generó el examen con éxito. Saliendo de la applicación."))

  cli::cli_alert_info("
∩――--------―-∩
 ||     ∧ ﾍ　 ||
 ||    (* ´ ｰ`) ZZzz
 |ﾉ^⌒⌒づ`￣   ＼
 (　ノ　　⌒ ヽ   ＼
  ＼　　||￣￣￣￣￣||
 　 ＼,ﾉ|         ||
                    ")

  #cli::cli_text(s.note("∠( ᐛ 」∠)＿"))

  return(Examen)
  }



### FUNCION PARA ARMAR NUEVAS TABLAS PARA UN DIRECTORIO ---------------
#' Crear tablas para un Directorio.
#'
#' Ver 2.1.0 - Implementando tameas 4 y 5
#' Ver 2.0.0 - Ajuste mayor: Ahora se genera tabla tema por tema. Se debe llamar a la función otra vez si se desea un tema distinto. 
#' Ver 1.0.0 - Inicial, se generan todas las tablas de temas del parcial
#'
#' @param tema Numero del tema (1-5), donde 1 = Media Muestral, 2 = Prueba de Hipotesis, 3 = Anova 1Factor, 4 = Anova 1F+Bloque, 5 = Diseño 2^k
#' @param aislado Flag para saber si se genera un Directorio temporal o no. 
#'
#' @return
#' @export
#'
#' @examples
poblarDirectorio <- function(tema = NULL, aislado = FALSE){
  wd <- here::here("doetest_out", "tablas")

  #### Estilos de cli() ##########;
  s.error <- cli::combine_ansi_styles("red", "bold")
  s.warn <- cli::combine_ansi_styles("yellow", "underline")
  s.succ <- cli::combine_ansi_styles("green", "italic")
  s.note <- cli::col_cyan
  s.dir <- cli::col_magenta()
  s.inst <- cli::combine_ansi_styles("blue", "underline")
  ################################;

  if(aislado == TRUE){
    clearDirectorio() ##Inicializa el directorio desde 0.
  } #else if{resetDirectorio()} ##NO SE RECONSTRUYE EL DIRECTORIO!

  ## Ver 1.0.0 Se tenia parcial como parametro
  #if(is.null(parcial)){
  #  parcial <- menu(c("1er Parcial", "2do Parcial"), title="Que examen se desea generar?")
  #} #No hay else, porque lo contrario es que se dio un parcial como argumento.

    
  if(is.null(tema)){
    tema <- menu(c("Media Muestral", #1
           "Pruebas de Hipotesis", #2 
           "Anova de 1 Factor", #3
           "Anova 1Factor + Bloque", #4 
           "Diseños 2^k"), #5
         title = s.note("¿Para qué tema se generarán las tablas?"))
    }

  ## Se preguntará al usuario con readline cuantos sets de datos y preguntas se desean generar

  #if (parcial == 1){ #Ver1.0.0
  if(tema == 1){
    cli::cli_h3("Del tema Media Muestral")
    M1 <- readline("Cuantos sets de datos muestrales de desea generar? ")
    M1_tabla <- genMediaMuestral(M1) ## Generación de sets de datos
      cli::cli_alert_success("Se construyó la tabla {.val {attr(M1_tabla,'DBname')}}")
    M2 <- readline("Cuantas preguntas de desea generar? ")
    M2_tabla <- questionMediaMuestral(M1_tabla, M2) ## Generación de preguntas
      cli::cli_alert_success("Se construyó la tabla {.val {attr(M2_tabla,'DBname')}}")
      
    }else if(tema == 2){
    cli::cli_h3("Del tema Pruebas de Hipotesis")
    H1 <- readline("Cuantos sets de datos muestrales de desea generar? ")
    H1_tabla <- genPruebaHipotesis(H1) ## Generación de sets de datos
      cli::cli_alert_success("Se construyó la tabla {.val {attr(H1_tabla,'DBname')}}")
    H2 <- readline("Cuantos sets de preguntas de desea generar? ")
    H2_tabla <- questionPruebaHipotesis(H1_tabla, H2) ## Generación de preguntas
      cli::cli_alert_success("Se construyó la tabla {.val {attr(H2_tabla,'DBname')}}")
      
    }else if(tema == 3){
    cli::cli_h3("Del tema Anova de 1 Factor")
    A1 <- readline("Cuantos sets de datos muestrales de desea generar? ")
    A1_tabla <- gen1Anova(A1) ## Generación de sets de datos
      cli::cli_alert_success("Se construyó la tabla {.val {attr(A1_tabla,'DBname')}}")
    A2 <- readline("Cuantos sets de preguntas de desea generar? ")
    A2_tabla <- question1Anova(A1_tabla, A2) ## Generación de preguntas
      cli::cli_alert_success("Se construyó la tabla {.val {attr(A2_tabla,'DBname')}}")
      
    ## Despues de cada función, se actualiza el directorio con la nueva tabla
    ## SOLAMENTE CON LAS TABLAS GENERADAS AQUI! NO SE RECONSTRUYE!
    }else if(tema == 4){
      cli::cli_h3("Del tema Anova 1F + Bloque")
      B1 <- readline("Cuantos sets de datos muestrales de desea generar? ")
      B1_tabla <- gen1FBlock(B1) ## Generación de sets de datos
      cli::cli_alert_success("Se construyó la tabla {.val {attr(B1_tabla,'DBname')}}")
      B2 <- readline("Cuantos sets de preguntas de desea generar? ")
      B2_tabla <- question1FBlock(B1_tabla, B2) ## Generación de preguntas
      cli::cli_alert_success("Se construyó la tabla {.val {attr(B2_tabla,'DBname')}}")
      
    }else if(tema == 5){
      cli::cli_h3("Del tema Diseño 2^k")
      K1 <- readline("Cuantos sets de datos muestrales de desea generar? ")
      K1_tabla <- gen2k(K1) ## Generación de sets de datos
      cli::cli_alert_success("Se construyó la tabla {.val {attr(K1_tabla,'DBname')}}")
      K2 <- readline("Cuantos sets de preguntas de desea generar? ")
      K2_tabla <- question2k(K1_tabla, K2) ## Generación de preguntas
      cli::cli_alert_success("Se construyó la tabla {.val {attr(K2_tabla,'DBname')}}")
      
    }
      
      #}else if(tema == 4 | tema == 5){
      #B <- readline("Cuantos de 1F Anova con Bloque?")
    #K <- readline("Cuantos de Diseños 2^k?")

    ##---- Cuano no se tenian los temas 4 y 5, se entraba en panico
      #cli::cli_alert_warning(s.warn("Paquete {.pkg doe.testR} Ver 2.0.0, No tienen preparadas funciones para armar un examen de 2do Parcial"))
     # stop(cli::cli_alert_danger(s.error("FIN: Se aborta la generación del examen. Intente otra opción o espere una versión futura.")), call. = FALSE)
  #}

  cli::cat_line()
  if (aislado == TRUE){
  cli::cli_alert_success("Se construyó un {cli::col_magenta('Directorio')} (temp). Solo estas tablas serán usadas para generar el examen.")
  cli::cli_alert_warning(s.warn("Estas tablas se incorporarán al {cli::col_magenta('Directorio')} canonico después que acabe esta instancia."))
  }
  #describirDirectorio()

}



#### INTERFAZ DE USUARIO PARA GENERAR EJERCICIOS DE PRACTICA ---------------------------
#' Interfaz de usuario: Generador de Ejercicios de Diseño de Experimentos
#'
#' Ver 0.2.0 - Incorporando temas del Parcial 2
#' Ver 0.1.0 - Versión inicial.
#'
#' @return
#' @export
#'
#' @examples
doePractice <- function(){
  #### Estilos de cli() ##########;
  s.error <- cli::combine_ansi_styles("red", "bold")
  s.warn <- cli::combine_ansi_styles("yellow", "underline")
  s.succ <- cli::combine_ansi_styles("green", "italic")
  s.note <- cli::col_cyan
  s.dir <- cli::col_magenta
  s.inst <- cli::combine_ansi_styles("blue", "underline")
  ################################;
  
  ### PRIMERA PARTE: SET-UP DE LAS CARPETAS
  cli::cli_h1(paste0("Generador de Ejercicios de Practica: ", cli::col_green("Diseño de Experimentos")))
  cli::cli_text(s.note("Paquete creado por: Fco.C-E. ", "{{zzz}}°°°( -_-)>c[_]"))
  cli::cli_text(s.note("Version:  0.1.2"))
  ## Preguntar por la carpeta:
  cli::cli_h2("PARTE 1: Set-Up")
  cli::cli_text("La carpeta de trabajo de esta aplicación esta ubicada en: \n {.path {here::here('doetest_out')}}\n")
  ## Cargar directorio
  ## Revisar si existe la carpeta, si no - las crea con folder::folder. 3 Escenarios
  if(dir.exists(here::here("doetest_out","tablas")) == FALSE){
    ## Escenario 1: No existe el Directorio ni las carpetas. Crear uno nuevo.
    cli::cli_alert_warning(s.warn("No se encontró una carpeta de trabajo, se procederá a generar."))
    ## Genera los folders: doetest_out, tablas y reportes
    folders::create_folders(here::here("doetest_out", c("tablas", "reportes")))
    cli::cli_alert_success(s.succ("Se generaron las carpetas:"))
    cli::cat_bullet(s.succ(c("~/doetest_out/tablas", "~/doetest_out/reportes")))
    
    ## Despues de crear las carpetas, inicializa un Directorio.
    cli::cli_alert_info("Se procede a generar un nuevo {s.dir('Directorio')}:")
    clearDirectorio()
    cli::cat_line()
    cli::cli_alert_success("Se estableció un nuevo {s.dir('Directorio')}")
    
    Dir_vacio <- TRUE #FLAG: Directorio esta vacio!
    
  }else if(length(list.files(here::here("doetest_out", "tablas"), pattern =  ".RDS")) ==  0){
    ##Escenario 2. La carpeta existe, pero  el Directorio esta VACIO
    cli::cli_alert_warning(s.warn("El {s.dir('Directorio')} esa vacio. Se debe proseguir con cuidado."))
    ## Genera los folders: doetest_out, tablas y reportes
    folders::create_folders(here::here("doetest_out", c("tablas", "reportes")))
    
    ## Despues de crear las carpetas, inicializa un Directorio.
    cli::cli_alert_info("Se procede a generar un nuevo {s.dir('Directorio')}: \n")
    clearDirectorio()
    cli::cli_alert_success("Se estableció un nuevo {s.dir('Directorio')}")
    Dir_vacio <- TRUE #FLAG: Directorio esta vacio!
    
  }else{
    ## Escenario 3. Carpetas existen, resetear el Directorio con los archivos que ahi se encuentran
    cli::cli_alert_success(s.succ("Se encontró la carpeta ~/doetest_out/"))
    cli::cli_alert_info("Se procede a cargar el {s.dir('Directorio')} \n")
    resetDirectorio()
    cli::cli_alert_success("{s.dir('Directorio')} cargado.")
    
    Dir_vacio <- FALSE #FLAG: Directorio no esta vacio
  }
  
  cli::cat_rule()
  
  ###----------- SEGUNDA PARTE: SET-UP DE LAS CARPETAS
  cli::cli_h2("PARTE 2: Tablas")
  ## Presentar al usuario una descripción del Directorio. #CHANGED Ya no!  No es necesario presentarlo dos veces
  #cat("Información del directorio actual: \n")
  #describirDirectorio()
  
  cat("\n")
  ## Preguntar: ¿Se hara un examen desde 0, o se desea usar las tablas del directorio?
  optablas <- menu(c("Generar nuevas tablas", "Usar tablas del Directorio actual"),
                   title = s.note("¿Qué tablas se usaran para generar el examen?"))
  
  if(Dir_vacio == TRUE){
    ### AQUI UN RIESGO! Si el directorio esta vacio,
    cli::cli_alert_warning(s.warn("Nota! El {s.dir('Directorio')} esta vacio. Se forzará la opción 1 (Generar nuevas tablas)"))
    optablas <- 1 #Se fuerza la opción 1. Protección contra posibles errores
  }
  
  cli::cat_line()
  optema <- menu(c("Media Muestral", 
                   "Pruebas de Hipotesis", 
                   "Anova de 1 Factor",
                   "Anova 1Factor + Bloque", 
                   "Diseños 2^k"),
                    title = s.note("¿Para qué tema se generarán las preguntas?"))
  
  
  if(optablas == 1){
    cli::cli_alert_info("Se procederá a generar tablas para todos los temas.")
    
    ##--- Cuando no se habia implementado los temas 4 y 5
    #if(optema == 4 | optema == 5){
      ###TODO - Para una version futura, añadir  funciónes para examen Parcial 2
     #cli::cli_alert_warning(s.warn("Paquete {.pkg doe.testR Version 2.0.0}, no tiene preparadas funciones para armar problemas de este tema."))
     #stop(cli::cli_alert_danger(s.error("FIN: Se aborta la generación del examen. Intente otra opción o espere una versión futura.")), call. = FALSE)
      #}else {
      ## Temas del primer parcial
      
      poblarDirectorio(optema,aislado = TRUE)
      #}

    describirDirectorio()
  }else if(optablas == 2){
    cli::cli_alert_info("Se procederá con las tablas ya existentes.")
    #resetDirectorio() #! Ya se reinicio arriba, no volver a correr la función innecesariamente.
    ## Hay que llamar a la función: genExamen(). Darle un query desde aqui!
  }

  cli::cat_line()#Newline
  cli::cat_rule()
  
  ###------------ PARTE 3: GENERAR EXAMEN
  
  cli::cli_h2("PARTE 3: Examen")
  
  oplang <- menu(c("Español", "Ingles"), title = s.note("¿Idioma?"))
  opformat <- menu(c("HTML", "LaTeX"), title = s.note("¿Formato de salida?"))
  
  lang <- dplyr::case_when(oplang == 1 ~ "Esp", oplang == 2 ~ "Eng")
  format <- dplyr::case_when(opformat == 1 ~ "HTML", opformat == 2 ~ "LaTeX")
  
  ### Independiente del tema, se llamara a la función aglomerante genPractica()
  #Examen <- genExamen(parcial = opparcial, lang = lang, format = format)
  Practica <- genPractica(tema = optema, lang = lang, format = format)
  
  cli::cat_line()
  cli::cli_alert_success(s.succ("Los problemas se generaron con éxito. Saliendo de la applicación."))
  
  cli::cli_text(s.note("∠( ᐛ 」∠)＿"))
  
  return(Practica)
}


#dirCarpeta <- function(){
#  cli::cli_text("La carpeta raiz esta ubicada en: \n {.path {here::here()}}\n")
#  cambio <- menu(c("Si", "No"), title = "¿Se desea cambiar la ubicación de la carpeta raíz?")
#  if(cambio == "Si"){
#    path <- readline("Inserte el PATH de la carpeta que se desea usar: ")
#    path <- gsub("\\\\", "/", path)
#    here::set_here(path)
#  }
#  }
