#### Inicializando el directorio ---------------------------
#' Limpiar el directorio
#'
#' @return Se genera una variable global: Directorio, vacio.
#' @export
#'
#' @examples
clearDirectorio <- function(){
  assign("Directorio", tibble::tibble(
    id_bytes = character(),
    id_animal = character(),
    id_pokemon = character(),
    DBname = character(),
    Tema = character(),
    Cont = character(),
    n = numeric(),
    Elementos = character(),
    Usado = character(),
    Fecha = character(),
    Idioma = character()), envir = .GlobalEnv)
} ## Se debe borrar manualmente los archivos ya existentes si se va a cambiar


#### Función para actualizar automaticamente el directorio ---------------------------
#' Actualizar el Directorio despues de generar una tabla
#'
#' Ver 2.4 - Genera una copia fisica del directorio.
#' Ver 2.3 - Función que no necesita ser reasignada a una variable
#'
#' @param DB Tabla de datos a introducir al Directorio
#' @param Directorio Tabla de Directorio, guardada en una Variable global
#'
#' @return Modifica al objeto Directorio
#'
#' @examples
actualizarDirectorio <- function(DB, Directorio=Directorio) {
  ## V2.3 - Función que no necesita ser reasignada a una variable
  assign("Directorio", dplyr::add_row(Directorio,
                                      id_bytes = attr(DB, "id_bytes"),
                                      id_animal = attr(DB, "id_animal"),
                                      id_pokemon = attr(DB, "id_pokemon"),
                                      DBname = attr(DB, "DBname"),
                                      Tema = attr(DB, "Tema"),
                                      Cont = attr(DB, "Contenido"),
                                      n = nrow(DB),
                                      Elementos = attr(DB, "Elementos"),
                                      Usado = attr(DB, "Usado"),
                                      Fecha = as.character(attr(DB, "Fecha")),
                                      Idioma = attr(DB, "Idioma")
  ) %>% dplyr::distinct(), #Remueve duplicados automaticamente
  envir = .GlobalEnv)

  ## Añade a la copia fisica del Directorio
  readr::write_delim(Directorio, here::here("doetest_out","Directorio.csv"), delim = "\t")

}


### Buscar en la carpeta las tablas ya creadas y reconstruye el Directorio -----------------
#' Reinicia el Directorio con las tablas guardadas en la memoria
#'
#' Vers 3.0 Se ajusta para los casos de inicialización del directorio. (i.e. El directorio esta vacio).
#' Vers 2.0 Se cambia para usar el los paquetes here y folders
#'
#' @return Vuelve a genera la variable Directorio a partir de las tablas de datos existentes
#' @export
#'
#' @examples
resetDirectorio <- function(){
  #### Estilos de cli() ##########;
  s.error <- cli::combine_ansi_styles("red", "bold")
  s.warn <- cli::combine_ansi_styles("yellow", "underline")
  s.succ <- cli::combine_ansi_styles("green", "italic")
  s.note <- cli::col_cyan
  s.dir <- cli::col_magenta
  s.inst <- cli::combine_ansi_styles("blue", "underline")
  ################################;

  ##-- Genera las carpetas de salida si no existen.
  folders::create_folders(here::here("doetest_out", c("tablas", "reportes")))

  #wd <- "D:/Documents/ITESM/IBT21/BT2004B/003 - Actividades/Examen/DB"
  wd <- here::here("doetest_out", "tablas") ##Path relativo donde estan las tablas guardadas
  direc <- list.files(wd, pattern = ".RDS") #Lista todos los archivos de tablas
    ## CUIDADO! ¿Y si el directorio esta vacio?

  clearDirectorio() #Genera de 0 el directorio y guarda el objeto.

  if(length(direc) == 0){ #Directorio vacio
    cli::cli_alert_danger(s.warn("El {s.dir('Directorio')} esta vacio."))

  }else{
    for (i in 1:length(direc)){
      importDB(direc[i]) ##Importa temporalmente cada tabla y actualiza el directorio. Pero guarda la tabla en el Environment si no se le asigna una variable.
    }
    #Función para nombrar lo que hay en el directorio
    describirDirectorio()
  }

  #Guarda una instancia del Directorio en la memoria
  readr::write_csv(Directorio, here::here("doetest_out","Directorio.csv"))
  readr::write_delim(Directorio, here::here("doetest_out","Directorio.tsv"), delim = "\t")
}


#### Función interna para describir el Directorio ---------------------------
#' Cuenta los elementos del Directorio segun tema y tipo
#'
#' @return Genera, en mensajes, un listado de los elementos contenidos en el Directorio
#'
#' @examples
describirDirectorio <- function(){
  ##usar filter y nrow para contabilizar los elementos que hay por tema, tipo e idioma (examen)
  message("En el Directorio ubicado en:\n", here::here("doetest_out","tablas"),"\n\n")
  ## Conteo por temas
  for(i in 1:length(unique(Directorio$Tema))){
    n <- Directorio %>% dplyr::filter(Tema == unique(Directorio$Tema)[i]) %>% nrow()
    message("Hay ", n, " tablas del tema '", unique(Directorio$Tema)[i],
            "' en el Directorio,")
  }
  message("--------------")
  ## Conteo por tipo
  for(i in 1:length(unique(Directorio$Cont))){
    n <- Directorio %>% dplyr::filter(Cont == unique(Directorio$Cont)[i]) %>% nrow()
    message("Hay ", n, " tablas del tipo '" , unique(Directorio$Cont)[i],
            "' en el Directorio,")
  }
}
