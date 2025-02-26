#### Exportar base de datos --------------------------
#' Exportar tabla de datos
#'
#' V2.1 - usar los paquetes here y folders para los paths relativos
#' V2 - usar saveRDS y readRDS para los archivos
#' V1 - usar dput y dget para los archivos
#'
#' @param DB Tabla de datos a exportar
#'
#' @return
#' @export
#'
#' @examples
exportDB <- function(DB){
  #wd <- "D:/Documents/ITESM/IBT21/BT2004B/003 - Actividades/Examen/DB/"
  ##Path relativo donde estan las tablas guardadas
  outpath <- here::here("doetest_out","tablas", paste0(attr(DB,"DBname"),".RDS"))
  #V2
  saveRDS (DB,outpath) #Archivo RDS, más sencillo.
}


#### Parsear query de id de base de datos--------------------------
#' Parsear query de id de la tabla de datos
#'
#' Version 2.2 - Mejorado para los casos donde se presentan más de un match, no se presentan matches, y se da un query incompleto.
#'
#' @param query String, puede ser el nombre de la tabla completo, incompleto. Si se presentan mas de uno, se genera un menu
#'
#' @return Otorga el nombre oficial de la tabla DBname, con el formato [Tema]-[Tipo]-[Fecha]-[id_byte]-[Idioma, si se requiere]
#'
#' @examples
parsearQuery <- function(query){
  ## QUE HACER CUANDO SE REPITEN??? -- Ya no deberian repetirse, se cambio el codigo de la funcion que genera metadatos
    ##-- Aunque, si se repite un elemento (pokemon o animal), entonces dará a elegir
  ## Y CUANDO SE DA EL QUERY EQUIVOCADO/NO APARECE??
      ## Se genera un "integer(0)", un objeto de longitud 0. length(x) = 0. Loop repeat
  pokemon <- tolower(rcorpora::corpora("games/pokemon")$pokemon$name)
  flag <- 0

  repeat{ ##not tested done

    ###--- Parseador de input/query
    if (stringr::str_detect(query, "_") == T){
      #Al contener "_", se dio el id_pokemon o el id_animal
      query_3 <- stringr::str_split_1(query, "_")[3]
      if (sum(stringr::str_detect(pokemon, query_3)) > 0){ #Si true, es id_pokemon
        query_fila <- grep(query, Directorio$id_pokemon)
        query_nombre <- Directorio$DBname[query_fila]
      }else {#Se trata de un id_animal
        query_fila <- grep(query, Directorio$id_animal)
        query_nombre <- Directorio$DBname[query_fila]
      }
    }else if (stringr::str_detect(query, "-") == T){ #Ya es el nombre (DBname)
      query_nombre <- query

    }else{ ## Es el id_bytes O, se puede dar solo uno de los adjetivos/pokemones/animales
      if(length(grep(query, Directorio$id_pokemon)) > 0){ #Se trata de un pokemon
        query_fila <- grep(query, Directorio$id_pokemon)
        query_nombre <- Directorio$DBname[query_fila]

      }else if(length(grep(query, Directorio$id_animal)) > 0){ #Se trata de un animal
        query_fila <- grep(query, Directorio$id_animal)
        query_nombre <- Directorio$DBname[query_fila]

      }else {
        query_fila <- grep(query, Directorio$id_bytes)
        query_nombre <- Directorio$DBname[query_fila]
      }
    }

    ## Posibles resultados
    if(length(query_nombre) == 0){ #No encontró match, regreso characer(0)
      cat("No se encontró la tabla. Intente otra vez. \n")
      
      ###!! Aqui el codigo se rompe! Si se llama a parsearQuery y no existe la tabla, se necesita una forma de forzar a armar la tabla!
          ## Se debe atrapar el error desde antes. 
            ## Ya quedó.
  
      #¿Como regresar al inicio? Pedir una nueva query y volver al inicio del parseo
      query <-  readline("Ingresa un ID valido para buscar en el Directorio: ")
    }else if (length(query_nombre) > 1) { #Hay más de 2 nombres, se debe elegir uno.
      cat("Se encontró más de una tabla con un nombre similar, elige uno para proseguir: \n")
      query_choice <- menu(query_nombre)
      query_nombre <- query_nombre[query_choice]
      break
    }else { #length(query_nombre) == 1
      break
    }

  } #Es un ciclo repeat

  return(query_nombre)
}


### Importar base de datos -----------------
#' Importar una tabla de datos del directorio
#'
#' V2.1 - usar los paquetes here y folders para los paths relativos
#' V2 - usar saveRDS y readRDS para los archivos
#' V1 - usar dput y dget para los archivos
#'
#' @param query ID de la tabla de datos, un string.
#'
#' @return Regresa una tabla de datos desde el directorio
#' @export
#'
#' @examples
importDB <- function(query){
  #wd <- "D:/Documents/ITESM/IBT21/BT2004B/003 - Actividades/Examen/DB/"
  #wd <- here::here("doetest_out","tabl")
  query <- parsearQuery(query)

  if (grepl(".RDS",query, fixed = T)){
    #query <- paste0(wd,query) #Ya tiene la extension
    query_path <- here::here("doetest_out","tablas",query)
  }else {
    #query <- paste0(wd,query,".RDS") #Añade la extension
    query_path <- here::here("doetest_out","tablas",paste0(query,".RDS"))
  }
  DB <- readRDS(query_path)
  actualizarDirectorio(DB, Directorio) ##! Detalle! Llama a la variable global para actuar.
  return(DB)
}


#### Generar metadatos genericos para la base de datos-----------------------
#' Generar metadatos genericos para la base de datos
#'
#' V2.2 - Actualizado para funcionar tambien para examenes de practica
#' V2.1 - Incluye ahora para los temas de Anova con Bloque y Diseño 2k
#' V2 - Ahora la función loopea si detecta que se generaron id's repetidos en el Directorio
#'
#' @param Data Tabla de datos a la cual generar los metadatos
#' @param Tema Tema de la tabla: MM - Media Muestral, PH - Prueba de Hipotesis, A1 - Anova, etc
#' @param Cont Tipo de tabla: Datos muestrales, Preguntas, Examen, etc.
#'
#' @return
#'
#' @examples
metadataDB <- function(Data,Tema,Cont){
  attr(Data, "Tema") <- dplyr::case_when(
    Tema == "MM" ~ "Media Muestral",
    Tema == "PH" ~ "Prueba de Hipotesis",
    Tema == "A1" ~ "Anova de 1 Factor",
    Tema == "AB" ~ "Anova 1F + Bloque",
    Tema == "2k" ~ "Diseño factorial 2^k",
    Tema == "EX" ~ "Examen")
  attr(Data, "Contenido") <- dplyr::case_when(
    Cont == "data" ~ "Datos muestrales",
    Cont == "ques" ~ "Preguntas formuladas",
    Cont == "stat" ~ "Templates de enunciados",
    Cont == "exam" ~ "Examen",
    Cont == "prac" ~ "Ejercicio de practica")
  attr(Data, "Fecha") <- lubridate::now()

  ## adjective_pokemon internamente dentro de la función.
  adjective_pokemon <- function(n = 1, m = 1, style = "snake") {
    pokemon <- tolower(rcorpora::corpora("games/pokemon")$pokemon$name)
    adjectives <- tolower(rcorpora::corpora("words/adjs")$adjs)

    if (m == 1) {
      ids::ids(n, adjectives,
               pokemon, style = style) #6.37e5 combinaciones
    }else if (m == 2) {
      ids::ids(n, adjectives, adjectives,
               pokemon, style = style) #6.13e8 combinaciones
    }else if (m ==  3) {
      ids::ids(n, adjectives, adjectives, adjectives,
               pokemon, style = style) #5.90e11 combinaciones
    }else if (m == 4){
      ids::ids(n, adjectives, adjectives, adjectives, adjectives,
               pokemon, style = style) #5.69e14 combinaciones
    }else {ids::ids(n, adjectives, pokemon, style = style)} #Default: 1 adjetivo
  }

  #Codigos de identificacion
  attr(Data, "id_bytes") <- ids::random_id(1,3)
  attr(Data, "id_animal") <- ids::adjective_animal(1,2)
  attr(Data, "id_pokemon") <- adjective_pokemon(1,2)

  ## REVISAR QUE NO SE REPITAN! Un ciclo While.
  while (attr(Data, "id_bytes") %in% Directorio$id_bytes){
    #Si esa repetido, genera uno nuevo
    attr(Data, "id_bytes") <- ids::random_id(1,3)
  }
  while (attr(Data, "id_animal") %in% Directorio$id_animal){
    #Si esa repetido, genera uno nuevo
    attr(Data, "id_animal") <- ids::adjective_animal(1,2)
  }
  while (attr(Data, "id_pokemon") %in% Directorio$id_pokemon){
    #Si esa repetido, genera uno nuevo
    attr(Data, "id_pokemon") <- adjective_pokemon(1,2)
  }

  #DBname = [Tema]-[Tipo de Tabla]-[YMD]-[Id_bytes3]
  attr(Data, "DBname") <- stringr::str_c(
    Tema,
    Cont,
    format(attr(Data,"Fecha"),"%Y%m%d"),attr(Data,"id_bytes"),
    sep = "-") #Usar DBname para sacar la tabla a un archivo.

  ## Lista de elementos contenidos en la tabla
  if (Cont == "ques"){
    attr(Data, "Elementos") <- paste(Data$Code, collapse = ", ")
  }else if (Cont == "exam"){
    attr(Data, "Elementos") <- paste(Data$Code, collapse = ", ")
  }else if (Cont == "data"){
    #attr(Data, "Elementos") <- unlist(Data$id, use.names = FALSE)
    attr(Data, "Elementos") <- NA
  }else if (Cont == "prac"){
    attr(Data, "Elementos") <- paste(Data$Code, collapse = ", ")
  }
  return(Data)
}
