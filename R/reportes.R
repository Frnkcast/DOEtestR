
#' Seleccionador de función para elaboracion del reporte
#'
#' @param DB Tabla para la cual se busca generar un reporte.
#' @param format "txt" o "Rmd" según el tipo de reporte a generar
#'
#' @returns Da el nombre de la función rep a utilizar segun la tabla otorgada
#'
#' @examples
rep_select <- function(DB,format = "txt"){
  attr(DB,"DBname")
  name_i <- stringr::str_split(attr(DB,"DBname"),"-")[[1]]
  YY <- name_i[1]
  X <- dplyr::case_when(name_i[2]  == "ques" ~ "Q", 
                 name_i[2]  == "data" ~ "D")
  rep_name <- stringr::str_c("rep_",X,"_",YY)
  if(format == "Rmd"){
    rep_name <- stringr::str_c(rep_name,"_Rmd")
  }
  return(rep_name)
}


#### TEMA 1 - Media Muestral -------------------

## Reporte de sets de datos
#' Media Muestral: Reporte de la tabla de datos muestrales
#'
#' @param DB tabla de datos del tema Media Muestral
#'
#' @return Genera un archivo de texto
#'
#' @examples
rep_D_MM <- function(DB){
  wd <- here::here("doetest_out", "reportes")
  outpath <- here::here(wd,paste0(attr(DB,"DBname"), ".txt"))

  sink(outpath)
  on.exit(sink())
  cat("Inicializando.... Base de datos muestrales simulados","\n")
  cat("   id: ", attr(DB, "id_bytes"), "\n")
  cat("   id: ", attr(DB, "id_pokemon"), "\n")
  cat("- Elaborado en: ", attr(DB,"Fecha"), "\n")
  cat("- Para el tema: ", attr(ans,"Tema"),"\n\n")

  for(i in 1:length(DB$mu)){
    cat("-----------------------------------------------------------\n")
    cat(" Entrada #: ", "M",as.character(DB[[i,1]]), "\n")
    cat("-----------------------------------------------------------\n")
    cat("Población::: \n")
    cat("   Mu = ", as.character(DB[[i,"mu"]]), "\n")
    cat("   Sig = ", as.character(DB[[i,"sig"]]), "\n \n")
    cat("Muestra:: \n")
    cat("   n:  ", as.character(DB[[i,"n"]]), "\n")
    cat("   Datos:  ", as.character(DB[[i,"data"]]), "\n")
    cat("   Media muestal = ", as.character(DB[[i, "sam_mu"]]), "\n")
    cat("   Desv. Muestral(S) = ", as.character(DB[[i, "sam_sig"]]), "\n")
    cat("\n \n")
    cat("Probabilidades:: \n")
    cat("   Distribución: ",as.character(DB[[i, "distr"]]) ,"\n")
    cat("   Valor de query: ",as.character(DB[[i, "q"]]) ,"\n")
    cat("   Valor de query(Critico): ",as.character(DB[[i, "q_est"]]) ,"\n")
    cat("   P(X > Query): ",as.character(DB[[i, "p_right"]]) ,"\n")
    cat("   P(X < Query): ",as.character(DB[[i, "p_left"]]) ,"\n")
    cat("\n \n")
    cat("-----------------------------------------------------------\n")
    cat("\n")
  }
  sink()
  file.show(outpath)
}

## Reporte de Pregunta
#' Media Muestral: Reporte de preguntas generadas
#'
#' @param Data Tabla de datos muestrales
#' @param Ques Tabla de ejercicios generados a parir de la tabla de datos muestrales
#' @param i Id de la pregunta especifica a describir
#'
#' @return
#'
#' @examples
rep_Q_MM <- function(Data,Ques,i){
  cat("-----------------------------------------------------------\n")
  cat(" Entrada #: ",as.character(Ques[[i,"Code"]]), "\n")
  cat("-----------------------------------------------------------\n")
  cat("Población::: \n")
  cat("   Mu = ",
      as.character(Data$mu[[match(Ques$dbID[i], Data$id)]]), "\n")
  cat("   Sig = ",
      as.character(Data$sig[[match(Ques$dbID[i], Data$id)]]), "\n")
  cat("\n")
  cat("Muestra:: \n")
  cat("   n:  ", as.character(Data$n[[match(Ques$dbID[i], Data$id)]]), "\n")
  #cat("   Datos:  ", as.character(Data$data[[match(Ques$dbID[i], Data$id)]]), "\n")
  cat("   Media muestal = ", as.character(Data$sam_mu[[match(Ques$dbID[i], Data$id)]]), "\n")
  cat("   Desv. Muestral(S) = ", as.character(Data$sam_sig[[match(Ques$dbID[i], Data$id)]]), "\n")
  cat("\n")
  cat("Probabilidades:: \n")
  cat("   Distribución: ",as.character(Ques[[i, "distr"]]),"\n")
  cat("   Valor de query: ",as.character(Ques[[i,"q"]]) ,"\n")
  cat("   Valor de query(Critico): ",
      as.character(Data$q_est[[match(Ques$dbID[i], Data$id)]]) ,"\n")
  cat("   P(X ", as.character(Ques[[i,"cola"]]),
      as.character(Ques[[i,"q"]])," ) = ",
      as.character(Ques[[i, "p"]]) ,"\n")
  cat("\n")
  cat("Enunciado:: \n")
  cat("   ",Ques[[i, "z"]], "....... \n")
  cat("   Buscando: ", as.character(Ques[[i,"Ques"]]),"\n")
  cat("   Respuesta: ", as.character(Ques[[i, "Ans"]]), "\n")
  cat("   Rango aceptable: ", as.character(Ques[[i, "Min"]]), " - ", as.character(Ques[[i, "Max"]]), "\n")
  cat("\n \n")
  cat("-----------------------------------------------------------\n")
  cat("\n")
}


#' Media Muestral: Reporte de preguntas generadas (version markdown)
#'
#' @param Data Tabla de datos muestrales
#' @param Ques Tabla de ejercicios generados a parir de la tabla de datos muestrales
#' @param i Id de la pregunta especifica a describir
#'
#' @return
#'
#' @examples
rep_Q_MM_Rmd <- function(Data,Ques,i){
  cat("## Entrada #: ",as.character(Ques[[i,"Code"]]), "\n")
  cat("### Población ")
  cat("\n")
  cat("-   Media poblacional (&mu;) = ", as.character(Data$mu[[match(Ques$dbID[i], Data$id)]]), "\n")
  cat("\n")
  cat("-   Desv. poblacional (&sigma;) = ", as.character(Data$sig[[match(Ques$dbID[i], Data$id)]]), "\n")
  cat("\n")
  cat("\n")
  cat("### Muestra:: \n")
  cat("\n")
  cat("-   n:  ", as.character(Data$n[[match(Ques$dbID[i], Data$id)]]), "\n")
  #cat("-   Datos:  ", as.character(Data$data[[match(Ques$dbID[i], Data$id)]]), "\n")
  cat("\n")
  cat("-   Media muestal (X&#772;) = ", as.character(Data$sam_mu[[match(Ques$dbID[i], Data$id)]]), "\n")
  cat("\n")
  cat("-   Desv. muestral(S) = ", as.character(Data$sam_sig[[match(Ques$dbID[i], Data$id)]]), "\n")
  cat("\n")
  cat("\n")
  cat("### Probabilidades:: \n")
  cat("\n")
  cat("-   Distribución: ",as.character(Ques[[i, "distr"]]),"\n")
  cat("\n")
  cat("-   Valor de query(Original): ",as.character(Ques[[i,"q"]]) ,"\n")
  cat("\n")
  cat("-   Valor de query(Critico): ",as.character(Data$q_est[[match(Ques$dbID[i], Data$id)]]) ,"\n")
  cat("\n")
  cat("-   P(X&#772; ", as.character(Ques[[i,"cola"]]),
      as.character(Ques[[i,"q"]])," ) = ",
      as.character(Ques[[i, "p"]]) ,"\n")
  cat("\n")
  cat("\n")
  cat("### Enunciado:: \n")
  cat("\n")
  cat("-   '",Ques[[i, "z"]], "'")
  cat("\n")
  cat("-   Buscando: ", as.character(Ques[[i,"Ques"]]),"\n")
  cat("\n")
  cat("-   Respuesta: ", as.character(Ques[[i, "Ans"]]), "\n")
  cat("\n")
  cat("-   Rango aceptable: ", as.character(Ques[[i, "Min"]]), " a ", as.character(Ques[[i, "Max"]]), "\n")
  cat("\n")
  cat("-----------------------------------------------------------\n")
  cat("\n")
}

### TEMA 2 - Prueba de Hipotesis -------------------

## Reporte de datos muestrales
#' Prueba de Hipotesis: Reporte de la tabla de datos muestrales
#'
#' @param DB tabla de datos del tema Prueba de Hipotesis
#'
#' @return Genera un archivo de texto
#'
#' @examples
rep_D_PH <- function(DB){
  wd <- here::here("doetest_out", "reportes")
  outpath <- here::here(wd,paste0(attr(DB,"DBname"), ".txt"))

  sink(outpath)
  on.exit(sink())
  cat("Inicializando.... Base de datos muestrales simulados","\n")
  cat("   id: ", attr(DB, "id_bytes"), "\n")
  cat("   id: ", attr(DB, "id_pokemon"), "\n")
  cat("- Elaborado en: ", attr(DB,"Fecha"), "\n")
  cat("- Para el tema: ", attr(DB,"Tema"),"\n\n")
  for(i in 1:length(DB$id)){
    cat("-----------------------------------------------------------\n")
    cat(" Entrada #: ", "H",as.character(DB[[i,1]]), "\n")
    cat("-----------------------------------------------------------\n")
    cat("Población 1::: \n")
    cat("   Mu = ", as.character(DB[[i,"mu_1"]]), "\n")
    cat("   Sig = ", as.character(DB[[i,"sig_1"]]), "\n")
    cat("   Muestra ::: \n")
    cat("      n:  ", as.character(DB[[i,"n_1"]]), "\n")
    cat("      Datos:  ", as.character(DB[[i,"data_1"]]), "\n")
    cat("      Media muestal = ", as.character(DB[[i, "s_mu_1"]]), "\n")
    cat("      Desv. Muestral(S) = ", as.character(DB[[i, "s_sig_1"]]), "\n")
    cat("\n")
    cat("Población 2::: \n")
    cat("   Mu = ", as.character(DB[[i,"mu_2"]]), "\n")
    cat("   Sig = ", as.character(DB[[i,"sig_2"]]), "\n")
    cat("   Muestra ::: \n")
    cat("      n:  ", as.character(DB[[i,"n_2"]]), "\n")
    cat("      Datos:  ", as.character(DB[[i,"data_2"]]), "\n")
    cat("      Media muestal = ", as.character(DB[[i, "s_mu_2"]]), "\n")
    cat("      Desv. Muestral(S) = ", as.character(DB[[i, "s_sig_2"]]), "\n")
    cat("\n")
    cat("Probabilidades:: \n")
    cat("   Distribución: ",as.character(DB[[i, "distr"]]) ,"\n")
    cat("   Grados de libertad (t): ",as.character(DB[[i,"n_1"]]+DB[[i,"n_2"]]-2) ,"\n")

    cat("\n")
    cat("-----------------------------------------------------------\n")
    cat("\n")
  }
  sink()
  file.show(outpath)
}

## Reporte de preguntas
#' Prueba de Hipotesis: Reporte de preguntas generadas
#'
#' @param Data Tabla de datos muestrales
#' @param Ques Tabla de ejercicios generados a parir de la tabla de datos muestrales
#' @param i Id de la pregunta especifica a describir
#'
#' @return
#'
#' @examples
rep_Q_PH <- function(Data,Ques,i){
  cat("-----------------------------------------------------------\n")
  cat(" Entrada #: ",as.character(Ques[[i,"Code"]]), "\n")
  cat("-----------------------------------------------------------\n")
  cat("Población 1::: \n")
  cat("   Mu = ", as.character(Data$mu_1[[match(Ques$dbID[i], Data$id)]]), "\n")
  cat("   Sig = ", as.character(Data$sig_1[[match(Ques$dbID[i], Data$id)]]), "\n")
  cat("   Muestra ::: \n")
  cat("      n:  ", as.character(Data$n_1[[match(Ques$dbID[i], Data$id)]]), "\n")
  #cat("      Datos:  ", as.character(Data$data_1[[match(Ques$dbID[i], Data$id)]]), "\n")
  cat("      Media muestal = ", as.character(Data$s_mu_1[[match(Ques$dbID[i], Data$id)]]), "\n")
  cat("      Desv. Muestral(S) = ", as.character(Data$s_sig_1[[match(Ques$dbID[i], Data$id)]]), "\n")
  cat("\n")
  cat("Población 2::: \n")
  cat("   Mu = ", as.character(Data$mu_2[[match(Ques$dbID[i], Data$id)]]), "\n")
  cat("   Sig = ", as.character(Data$sig_2[[match(Ques$dbID[i], Data$id)]]), "\n")
  cat("   Muestra ::: \n")
  cat("      n:  ", as.character(Data$n_2[[match(Ques$dbID[i], Data$id)]]), "\n")
  #cat("      Datos:  ", as.character(Data$data_2[[match(Ques$dbID[i], Data$id)]]), "\n")
  cat("      Media muestal = ", as.character(Data$s_mu_2[[match(Ques$dbID[i], Data$id)]]), "\n")
  cat("      Desv. Muestral(S) = ", as.character(Data$s_sig_2[[match(Ques$dbID[i], Data$id)]]), "\n")
  cat("\n")
  cat("Probabilidades:: \n")
  cat("   Enunciado:: \n", as.character(Ques[[i, "z"]]), "\n")
  cat("   Distribución: ",as.character(Ques[[i, "distr"]]) ,"\n")
  cat("   Grados de libertad (t): ",as.character(Ques[[i, "deg"]]) ,"\n")
  cat("   Hipotesis nula: $\\mu_1 - \\mu_2 = ", as.character(Ques[[i, "q"]]) ,"$ \n")
  cat("   Hipotesis alternativa: $\\mu_1 - \\mu_2 ", as.character(Ques[[i,"cola"]])," ",as.character(Ques[[i, "q"]]) ,"$ \n")

  if(Ques[[i,"cola"]] == "ne"){ ## Cuando es de dos colas
    cat("   Prueba de dos colas.","\n")
    cat("   Alfa: ",as.character(Ques[[i, "alfa"]]) ,"\n")
    cat("   Alfa/2: ",as.character(Ques[[i, "alfa"]]/2) ,"\n")
    cat("   Estadistico de Prueba: ",as.character(Ques[[i, "q_est"]]) ,"\n")
    cat("   Valor Critico: ",as.character(Ques[[i, "crit"]]) ,"\n")
    cat("   P(X > |EP|) = p_value = " ,as.character(Ques[[i, "pval"]]) ,"\n")
    cat("   Conclusion: ",as.character(Ques[[i, "comp"]]) ," H0. \n")

  }else { ## Cuando NO es de dos colas.
    cat("   Prueba de una cola.","\n")
    cat("   Alfa: ",as.character(Ques[[i, "alfa"]]) ,"\n")
    cat("   Estadistico de Prueba: ",as.character(Ques[[i, "q_est"]]) ,"\n")
    cat("   Valor Critico: ",as.character(Ques[[i, "crit"]]) ,"\n")
    cat("   P(X",as.character(Ques[[i,"cola"]])," EP) = p_value = " ,as.character(Ques[[i, "pval"]]) ,"\n")
    cat("   Conclusion: ",as.character(Ques[[i, "comp"]]) ," H0. \n")
  }

  cat("\n")
  cat("-----------------------------------------------------------\n")
  cat("\n")
}


#' Prueba de Hipotesis: Reporte de preguntas generadas (version markdown)
#'
#' @param Data Tabla de datos muestrales
#' @param Ques Tabla de ejercicios generados a parir de la tabla de datos muestrales
#' @param i Id de la pregunta especifica a describir
#'
#' @return
#'
#' @examples
rep_Q_PH_Rmd <- function(Data,Ques,i){
  cat("## Entrada #: ",as.character(Ques[[i,"Code"]]), "\n")
  cat("### Datos", "\n")
  cat("\n")
  cat("#### Población 1 \n")
  cat("-   Media poblacional (&mu;) = ", as.character(Data$mu_1[[match(Ques$dbID[i], Data$id)]]), "\n")
  cat("\n")
  cat("-   Desv. poblacional (&sigma;) = ", as.character(Data$sig_1[[match(Ques$dbID[i], Data$id)]]), "\n")
  cat("\n")
  cat("#### Muestra 1 \n")
  cat("   -   n:  ", as.character(Data$n_1[[match(Ques$dbID[i], Data$id)]]), "\n")
  cat("\n")
  #cat("      Datos:  ", as.character(Data$data_1[[match(Ques$dbID[i], Data$id)]]), "\n")
  cat("   -   Media muestal (X&#772;) = ", as.character(Data$s_mu_1[[match(Ques$dbID[i],
                                                                           Data$id)]]), "\n")
  cat("\n")
  cat("   -   Desv. Muestral(S) = ", as.character(Data$s_sig_1[[match(Ques$dbID[i], Data$id)]]), "\n")
  cat("\n")
  cat("#### Población 2 \n")
  cat("-   Media poblacional (&mu;) = ", as.character(Data$mu_2[[match(Ques$dbID[i], Data$id)]]), "\n")
  cat("\n")
  cat("-   Desv. poblacional (&sigma;) = ", as.character(Data$sig_2[[match(Ques$dbID[i], Data$id)]]), "\n")
  cat("\n")
  cat("#### Muestra 2 \n")
  cat("   -   n:  ", as.character(Data$n_2[[match(Ques$dbID[i], Data$id)]]), "\n")
  cat("\n")
  #cat("      Datos:  ", as.character(Data$data_2[[match(Ques$dbID[i], Data$id)]]), "\n")
  cat("   -   Media muestal (X&#772;) = ", as.character(Data$s_mu_2[[match(Ques$dbID[i], Data$id)]]), "\n")
  cat("\n")
  cat("   -   Desv. Muestral(S) = ", as.character(Data$s_sig_2[[match(Ques$dbID[i], Data$id)]]), "\n")
  cat("\n")
  cat("### Enunciado \n")
  cat("-   ",as.character(Ques[[i, "z"]]), "\n")
  cat("\n")
  cat("### Probabilidades \n")
  cat("-   Distribución: ",as.character(Ques[[i, "distr"]]) ,"\n")
  cat("\n")
  cat("-   Grados de libertad (para t-student): ",as.character(Ques[[i, "deg"]]) ,"\n")
  cat("\n")
  cat("-   Hipotesis nula: &mu;1 - &mu;2 = ", as.character(Ques[[i, "q"]]) ," \n")
  cat("\n")
  
  cola <- case_when(Ques[[i,"cola"]] == "\\ne" ~ "&ne;",
                    Ques[[i,"cola"]] == "<" ~ "<",
                    Ques[[i,"cola"]] == ">" ~ ">")
  
  cat("-   Hipotesis alternativa: &mu;1 - &mu;2 ", cola,"", as.character(Ques[[i, "q"]]) ," \n")
  cat("\n")
  if(Ques[[i,"cola"]] == "\\ne"){ ## Cuando es de dos colas
    cat("**Prueba de dos colas.**","\n")
    cat("\n")
    cat("-   Alfa (&alpha;): ",as.character(Ques[[i, "alfa"]]) ,"\n")
    cat("\n")
    cat("-   Alfa/2 (&alpha;/2): ",as.character(Ques[[i, "alfa"]]/2) ,"\n")
    cat("\n")
    cat("-   Estadistico de Prueba: ",as.character(Ques[[i, "q_est"]]) ,"\n")
    cat("\n")
    cat("-   Valor Critico: ",as.character(Ques[[i, "crit"]]) ,"\n")
    cat("\n")
    cat("-   P(",as.character(Ques[[i, "distr"]])," > |EP|) = p_value = " ,as.character(Ques[[i, "pval"]]) ,"\n")
    cat("\n")
    cat("-   Conclusion: ",as.character(Ques[[i, "comp"]]) ," H0. \n")
    
  }else { ## Cuando NO es de dos colas.
    cat("**Prueba de una cola.**","\n")
    cat("\n")
    cat("-   Alfa (&alpha;): ",as.character(Ques[[i, "alfa"]]) ,"\n")
    cat("\n")
    cat("-   Estadistico de Prueba: ",as.character(Ques[[i, "q_est"]]) ,"\n")
    cat("\n")
    cat("-   Valor Critico: ",as.character(Ques[[i, "crit"]]) ,"\n")
    cat("\n")
    cat("-   P(",as.character(Ques[[i, "distr"]])," ",as.character(Ques[[i,"cola"]])," EP) = p_value = " ,as.character(Ques[[i, "pval"]]) ,"\n")
    cat("\n")
    cat("-   Conclusion: ",as.character(Ques[[i, "comp"]]) ," H0. \n")
  }
  cat("\n")
  cat("-----------------------------------------------------------\n")
  cat("\n")
}


#### TEMA 3 - Anova de 1 Factor -------------------

## Reporte de datos muestrales
#' Anova de 1 Factor: Reporte de la tabla de datos muestrales
#'
#' @param DB tabla de datos del tema Anova de 1 Factor
#'
#' @return Genera un archivo de texto
#'
#' @examples
rep_D_A1 <- function(DB){
  wd <- here::here("doetest_out", "reportes")
  outpath <- here::here(wd,paste0(attr(DB,"DBname"), ".txt"))

  sink(outpath)
  on.exit(sink())
  cat("Inicializando.... Base de datos muestrales simulados","\n")
  cat("   id: ", attr(DB, "id_bytes"), "\n")
  cat("   id: ", attr(DB, "id_pokemon"), "\n")
  cat("- Elaborado en: ", attr(DB,"Fecha"), "\n")
  cat("- Para el tema: ", attr(DB,"Tema"),"\n\n")

  for(i in 1:length(DB$id)){
    cat("-----------------------------------------------------------\n")
    cat(" Entrada #: ", as.character(DB[[i,"id"]]), "\n")
    cat("-----------------------------------------------------------\n")
    cat("   # Niveles de A: ", as.character(DB[[i,"a"]]), "\n")
    cat("   Replicas: ", as.character(DB[[i,"n"]]), "\n \n")
    cat("Datos crudos: \n")
    cat(knitr::kable(DB[[i,"data"]], "pipe"))
    cat("\n \n")
    cat("Tabla de ANOVA: \n")
    cat(knitr::kable(DB[[i,"anova"]], "pipe"))
    cat("\n")
    cat("\n")
    cat("   F estadistica: ", DB[[i,"Fes"]], "\n")
    cat("   F critica: ", DB[[i,"Fcr"]], "\n")
    cat("\n")
    cat("   Comparación F0 vs Fest: ", DB[[i,"signif"]])
    cat("\n")
    cat("   Conclusión sobre H0: ", DB[[i,"H0"]])
    cat("\n \n")
    cat("Pruebas Post Hoc: \n")

    rep_PostHoc(DB[i,])

    cat("-----------------------------------------------------------\n")
    cat("\n")
  }
  sink()
  file.show(outpath)
}

## Elemento especifico para prueba de hipotesis
#' Anova de 1 Factor: Reporte de resultados de Pruebas Post Hoc
#'
#' @param Data Fila de la tabla de datos a describir
#'
#' @return Apoyo para la construccion de un archivo de texto
#'
#' @examples
rep_PostHoc <- function(Data){
  #Data es la fila de la base de datos
  #x1 es solo la tabla de datos muestrales
  x1 <- Data$data[[1]] #La tabla.

  ## Calcula los promedios por nivel de la variable
  x1m <- x1 %>% tidyr::gather(key = "X", value = "Y")%>%
    dplyr::group_by(X) %>% dplyr::summarise(Y=mean(Y))

  ## Prepara la matriz de comparación por parejas
  names(x1m$Y) <- x1m$X
  x1pw <- abs(outer(x1m$Y, x1m$Y, FUN = "-"))
  x1pw <- upper.tri(x1pw)*x1pw
  x1pw[x1pw == 0] <- NA
  #x3pw

  ## Guarda los valores de LSD y HSD
  x1_LSD <- Data$lsd[[1]][[6]]
  x1_HSD <- Data$tukey[[1]][[6]]

  x1_PostHoc <- list(outer(x1pw, x1_LSD, FUN = ">")[,,1],
                     outer(x1pw, x1_HSD, FUN = ">")[,,1]) #[[1]] es LSD, [[2]] es HSD

  #return(x1_PostHoc)

  cat("#### Fisher:")
  cat("\n")
  cat(knitr::kable(list(Data[[1,"lsd"]]), "pipe"))
  cat("\n\n")
  cat("-   Semejanza entre grupos.
  (Grupos que no comparten una letra son DIFERENTES.) \n")
  cat("\n")
  cat(knitr::kable(list(Data[[1,"lsd_groups"]]), "pipe"))
  cat("\n\n")
  cat("-   Matriz de diferencia entre parejas.
  (¿Es la diferencia mayor que el valor de LSD? TRUE = Si, FALSE = No) \n")
  cat("\n")
  cat(knitr::kable(x1_PostHoc[1],"pipe"))
  cat("\n\n")
  cat("\n")
  cat("#### Tukey: ")
  cat("\n")
  cat(knitr::kable(list(Data[[1,"tukey"]]), "pipe"))
  cat("\n\n")
  cat("Semejanza entre grupos.
  (Grupos que no comparten una letra son DIFERENTES.) \n")
  cat("\n")
  cat(knitr::kable(list(Data[[1,"tukey_groups"]]), "pipe"))
  cat("\n\n")
  cat("Matriz de diferencia entre parejas.
  (¿Es la diferencia mayor que el valor de HSD? TRUE = Si, FALSE = No) \n")
  cat("\n")
  cat(knitr::kable(x1_PostHoc[2],"pipe"))
  cat("\n\n")
}

## Reporte de preguntas elaboradas
#' Anova de 1 Factor: Reporte de preguntas generadas
#'
#' @param Data Tabla de datos muestrales
#' @param Ques Tabla de ejercicios generados a parir de la tabla de datos muestrales
#' @param i Id de la pregunta especifica a describir
#'
#' @return Genera un archivo de texto
#'
#' @examples
rep_Q_A1 <- function(Data,Ques,i){
  cat("-----------------------------------------------------------\n")
  cat(" Entrada #: ", as.character(Ques[[i,"Code"]]), "\n")
  cat("-----------------------------------------------------------\n")
  cat("   # Niveles de A: ", as.character(Data$a[[match(Ques$id[i], Data$id)]]), "\n")
  cat("   Replicas: ", as.character(Data$n[[match(Ques$id[i], Data$id)]]), "\n \n")
  cat("Datos crudos: \n")
  cat(knitr::kable(Ques[[i,"table"]], "pipe"))
  cat("\n \n")
  cat("Tabla de ANOVA: \n")
  cat(knitr::kable(Ques[[i,"table2"]], "pipe"), "\n")
  cat("\n")
  cat("   F estadistica: ", Data$Fes[[match(Ques$id[i], Data$id)]], "\n")
  cat("   F critica: ", Data$Fcr[[match(Ques$id[i], Data$id)]], "\n")
  cat("\n")
  cat("   Comparación F0 vs Fest: ", Data$signif[[match(Ques$id[i], Data$id)]])
  cat("\n")
  cat("   Conclusión sobre H0: ", Data$H0[[match(Ques$id[i], Data$id)]])
  cat("\n \n")
  cat("Pruebas Post Hoc:: \n")

  rep_PostHoc(Data[match(Ques$id[i], Data$id),])
  #cat("   Fisher: \n")
  #cat("\n")
  #cat(knitr::kable(list(Data$lsd[[match(Ques$id[i], Data$id)]]), "pipe"))
  #cat("\n")
  #cat("\n")
  #cat(knitr::kable(list(Data$lsd_groups[[match(Ques$id[i], Data$id)]]), "pipe"))
  #cat("\n")
  #cat("\n")
  #cat("   Tukey: \n")
  #cat("\n")
  #cat(knitr::kable(list(Data$tukey[[match(Ques$id[i],Data$id)]]), "pipe"))
  #cat("\n")
  #cat("\n")
  #cat(knitr::kable(list(Data$tukey_groups[[match(Ques$id[i],Data$id)]]), "pipe"))
  #cat("\n")
  cat("\n")
  cat("-----------------------------------------------------------\n")
  cat("\n")
}


#' Anova de 1 Factor: Reporte de preguntas generadas (version markdown)
#'
#' @param Data Tabla de datos muestrales
#' @param Ques Tabla de ejercicios generados a parir de la tabla de datos muestrales
#' @param i Id de la pregunta especifica a describir
#'
#' @return Genera un archivo de texto
#'
#' @examples
rep_Q_A1_Rmd <- function(Data,Ques,i){
  cat("## Entrada #: ", as.character(Ques[[i,"Code"]]), "\n")
  cat("### Datos del problema")
  cat("-   Niveles de A: ", as.character(Data$a[[match(Ques$id[i], Data$id)]]), "\n")
  cat("\n")
  cat("-   Replicas: ", as.character(Data$n[[match(Ques$id[i], Data$id)]]), "\n \n")
  cat("\n")
  cat("-   Datos crudos: \n")
  cat("\n")
  cat(knitr::kable(Ques[[i,"table"]], "pipe"))
  cat("\n \n")
  cat("### Tabla de ANOVA: \n")
  cat(knitr::kable(Ques[[i,"table2"]], "pipe"), "\n")
  cat("\n")
  cat("\n")
  cat("-   F estadistica: ", Data$Fes[[match(Ques$id[i], Data$id)]], "\n")
  cat("\n")
  cat("-   F critica: ", Data$Fcr[[match(Ques$id[i], Data$id)]], "\n")
  cat("\n")
  cat("-   Comparación F0 vs Fest: ", Data$signif[[match(Ques$id[i], Data$id)]])
  cat("\n")
  cat("-   Conclusión sobre H0: ", Data$H0[[match(Ques$id[i], Data$id)]])
  cat("\n \n")
  cat("\n")
  cat("### Pruebas Post Hoc:: \n")
  
  
  rep_PostHoc(Data[match(Ques$id[i], Data$id),])
  #x1 <- Data$data[[i]] #La tabla.

  ## Calcula los promedios por nivel de la variable
  #x1m <- x1 %>% tidyr::gather(key = "X", value = "Y")%>%
  #  dplyr::group_by(X) %>% dplyr::summarise(Y=mean(Y))

  ## Prepara la matriz de comparación por parejas
  #names(x1m$Y) <- x1m$X
  #x1pw <- abs(outer(x1m$Y, x1m$Y, FUN = "-"))
  #x1pw <- upper.tri(x1pw)*x1pw
  #x1pw[x1pw == 0] <- NA

  ## Guarda los valores de LSD y HSD
  #x1_LSD <- Data$lsd[[i]][[6]]
  #x1_HSD <- Data$tukey[[i]][[6]]

  #x1_PostHoc <- list(outer(x1pw, x1_LSD, FUN = ">")[,,1],
  #                   outer(x1pw, x1_HSD, FUN = ">")[,,1]) #[[1]] es LSD, [[2]] es HSD

  #return(x1_PostHoc)

  #cat("#### Fisher: \n")
  #cat(knitr::kable(list(Data[[i,"lsd"]]), "pipe"))
  #cat("\n\n")
  #cat("-   Semejanza entre grupos.
  #    (Grupos que no comparten una letra son DIFERENTES.) \n")
  #cat("\n")
  #cat(knitr::kable(list(Data[[i,"lsd_groups"]]), "pipe"))
  #cat("\n\n")
  #cat("-   Matriz de diferencia entre parejas.
  #(¿Es la diferencia mayor que el valor de LSD? TRUE = Si, FALSE = No) \n")
  #cat("\n")
  #cat(knitr::kable(x1_PostHoc[1],"pipe"))
  #cat("\n\n")
  #cat("\n")
  #cat("#### Tukey: \n")
  #cat(knitr::kable(list(Data[[i,"tukey"]]), "pipe"))
  #cat("\n\n")
  #cat("-   Semejanza entre grupos.
  #    (Grupos que no comparten una letra son DIFERENTES.) \n")
  #cat("\n")
  #cat(knitr::kable(list(Data[[i,"tukey_groups"]]), "pipe"))
  #cat("\n\n")
  #cat("Matriz de diferencia entre parejas.
  #    (¿Es la diferencia mayor que el valor de HSD? TRUE = Si, FALSE = No) \n")
  #cat("\n")
  #cat(knitr::kable(x1_PostHoc[2],"pipe"))
  cat("\n")
  cat("-----------------------------------------------------------\n")
  cat("\n")
}


#### TEMA 4 - Anova 1F + Bloque -------------------

## Reporte de sets de datos
#' Anova 1F + Bloque: Reporte de sets de datos
#'
#' @param DB Tabla de datos muestrales
#'
#' @return Genera un archivo de texto
#'
#' @examples
rep_D_AB <- function(DB){
  outpath <- paste0(attr(DB,"DBname"), ".txt")  
  
  sink(outpath)
  on.exit(sink())
  cat("Inicializando.... Base de datos muestrales simulados","\n")
  cat("   id: ", attr(DB, "id_bytes"), "\n")
  cat("   id: ", attr(DB, "id_pokemon"), "\n")
  cat("- Elaborado en: ", attr(DB,"Fecha"), "\n")
  cat("- Para el tema: ", attr(DB,"Tema"),"\n\n") 
  for(i in 1:length(DB$id)){
    cat("-----------------------------------------------------------\n")
    cat(" Entrada #: ", as.character(DB[[i,"id"]]), "\n")
    cat("-----------------------------------------------------------\n")
    cat("   # Niveles de A: ", as.character(DB[[i,"a"]]), "\n")
    cat("   # Niveles de B (Bloque): ", as.character(DB[[i,"b"]]), "\n \n")
    cat("Datos crudos: \n")
    cat(knitr::kable(DB[[i,"data"]], "pipe"))
    cat("\n\n")
    cat("#-- Diseño con Bloques --# \n")
    cat("Tabla de ANOVA: \n")
    cat(knitr::kable(DB[[i,"anova"]], "pipe"), "\n")
    cat("\n")
    cat("   F estadistica: ", DB[[i,"Fes"]], "\n")
    cat("   F critica: ", DB[[i,"Fcr"]], "\n")
    cat("\n")
    cat("   Comparación F0 vs Fest: ", DB[[i,"signif"]])
    cat("\n")
    cat("   Conclusión sobre H0: ", DB[[i,"H0"]])
    cat("\n \n")
    cat("#-- Diseño sin Bloques --# \n")
    cat("Tabla de ANOVA: \n")
    cat(knitr::kable(DB[[i,"anova_NB"]], "pipe"), "\n")
    cat("\n")
    cat("   F estadistica: ", DB[[i,"Fes_NB"]], "\n")
    cat("   F critica: ", DB[[i,"Fcr_NB"]], "\n")
    cat("\n")
    cat("   Comparación F0 vs Fest: ", DB[[i,"signif_NB"]])
    cat("\n")
    cat("   Conclusión sobre H0: ", DB[[i,"H0_NB"]])
    cat("\n")
    if (DB[[i,"H0"]] == DB[[i,"H0_NB"]]){
      diffNB <- "El bloque no genera diferencia en la conclusión"
    }else {
      diffNB <- "El bloque cambia la conclusión"
    }
    cat("   ¿Diferente con resepcto al diseño con Bloques?: ", diffNB)
    cat("\n \n")
    cat("Pruebas Post Hoc: \n")
    
    rep_PostHoc(DB[i,])
    
    #cat("   Fisher: \n")
    #cat(knitr::kable(DB[[i,"lsd"]], "pipe"), "\n")
    #cat(knitr::kable(DB[[i,"lsd_groups"]], "pipe"), "\n")
    #cat("\n")
    #cat("   Tukey: \n")
    #cat(knitr::kable(DB[[i,"tukey"]], "pipe"), "\n")
    #cat(knitr::kable(DB[[i,"tukey_groups"]], "pipe"), "\n")
    cat("-----------------------------------------------------------\n")
    cat("\n")
  }
  sink()
  file.show(outpath)
}

## Reporte de preguntas elaboradas
#' Anova 1F + Bloque: Reporte de preguntas elaboradas
#'
#' @param Data Tabla de datos muestrales
#' @param Ques Tabla de ejercicios generados a parir de la tabla de datos muestrales
#' @param i Id de la pregunta especifica a describir
#'
#' @return Genera un archivo de texto
#'
#' @examples
rep_Q_AB <- function(Data,Ques,i){
  
  cat("-----------------------------------------------------------\n")
  cat(" Entrada ", i,"B --  ", as.character(Ques[[i,"Code"]]), "\n")
  cat("-----------------------------------------------------------\n\n")
  cat("   # Niveles de A: ", as.character(Data$a[[match(Ques$id[i], Data$id)]]), "\n")
  cat("   # Niveles de B (Bloque): ", as.character(Data$b[[match(Ques$id[i], Data$id)]]), "\n \n")
  cat("  Enunciado:::  \n")
  #cat(DB[[i,"out"]], "\n")
  
  cat(Ques[[i,"statem"]], "\n\n")
  
  cat("  Datos::  \n")
  cat(knitr::kable(Ques[[i,"table"]], "pipe"), "\n\n")
  
  #cat("\n\n")
  cat("  Preguntas y sus respuestas:: \n\n")
  
  cat("- A1 (2 pts) Reporta la suma de cuadrados de los tratamientos (SSt), del bloque (SSb) y del error (SSE), redondeando a 3 cifras significativas. Importante: indica en tu respuesta cual es cual", "\n")
  cat("- A2 (2 pts) Reporta la media de cuadrados de los tratamientos (MSt), del bloque (MSb) y del error (MSE) a 3 cifras significativas. Importante: indica en tu respuesta cual es cual.", "\n")
  cat("- A3 (1 pts) Reporta tu estadistico de prueba, F0", "\n")
  cat("Res: \n")
  cat(knitr::kable(Ques[[i,"table2"]], "pipe"),  "\n\n")
  
  cat("- A4 (1 pts) A partir del F0 calculado, y el Fcritico (revisa el enunciado del problema) para una confianza del 95%, considerando una hipotesis nula donde ninguno de los niveles del factor afectan a la variable de respuesta, y una hipotesis alternativa donde al menos uno de los niveles tiene un efecto significativo. ¿Puedes concluir en que la hipotesis nula se rechaza o no? Escribe el enunciado completo de la conclusión, usando la definición de las variables involucradas. ", "\n")
  Signi <- dplyr::case_when(
    Data$signif[[match(Ques$id[i], Data$id)]] == "Signif" ~ "Se rechaza H0. El factor A es significativo.",
    Data$signif[[match(Ques$id[i], Data$id)]] == "No signif" ~ "No se rechaza H0. El factor A no es significativo.")
  cat("Res: \n    ", Signi, "\n\n")
  
  cat("- A5 (2 pts) Realiza una Prueba de Tukey de Comparaciones Múltiples para identificar cual (o cuales) de los niveles del factor A es (o son) significativamente diferente del resto. (En el enunciado del problema está el valor del estadistico q de Tukey a utilizar para la prueba)", "\n")
  cat("Res: \n")
  cat(knitr::kable(list(Data$tukey[[match(Ques$id[i], Data$id)]]), "pipe"), "\n\n")
  cat(knitr::kable(list(Data$tukey_groups[[match(Ques$id[i], Data$id)]]), "pipe"), "\n\n")
  
  ### Matriz de comparacion...
  #Data$signif_NB[[match(Ques$id[i], Data$id)]]
  x1 <- Data$data[[match(Ques$id[i], Data$id)]] #La Tabla
  ## Calcula los promedios por nivel de la variable
  x1m <- x1 %>% tidyr::gather(key = "X", value = "Y", A1:last_col())%>%
    dplyr::group_by(X) %>% dplyr::summarise(Y=mean(Y))
  ## Prepara la matriz de comparación por parejas
  names(x1m$Y) <- x1m$X
  x1pw <- abs(outer(x1m$Y, x1m$Y, FUN = "-"))
  x1pw <- upper.tri(x1pw)*x1pw
  x1pw[x1pw == 0] <- NA
  #x3pw
  ## Guarda los valores de LSD y HSD 
  x1_LSD <- Data$lsd[[match(Ques$id[i], Data$id)]][[6]]
  x1_HSD <- Data$tukey[[match(Ques$id[i], Data$id)]][[6]]
  x1_PostHoc <- list(
    outer(x1pw, x1_LSD, FUN = ">")[,,1],
    outer(x1pw, x1_HSD, FUN = ">")[,,1]
  ) #[[1]] es LSD, [[2]] es HSD
  ###########;
  
  cat(knitr::kable(x1_PostHoc[2],"pipe"))
  cat("\n\n")
  
  cat("- A6 (2 pts)  Vuelve a realizar tu analisis ANOVA, pero ahora sin considerar la variable Bloque, recordando ajustar las sumas de cuadrados y grados de libertad apropiados. El valor de F critico a utilizar se encuentra en el enunciado de este problema. ¿Identificas alguna diferencia con respecto a tu conclusión anterior? ¿Qué puedes concluir con respecto al bloque incorporado en este experimento?", "\n")
  cat("Res:\n")
  cat(knitr::kable(list(Data$anova_NB[[match(Ques$id[i], Data$id)]]), "pipe"), "\n")
  cat("    ", dplyr::case_when(
    Data$signif_NB[[match(Ques$id[i], Data$id)]] == "Signif" ~ "Se rechaza H0. El factor A es significativo",
    Data$signif_NB[[match(Ques$id[i], Data$id)]] == "No signif" ~ "No se rechaza H0. El factor A no es significativo"),
    "\n")
  
  cat("    ", dplyr::case_when(
    Data$signif[[match(Ques$id[i], Data$id)]] != Data$signif_NB[[match(Ques$id[i], Data$id)]] ~ "Quitar el bloque cambia la conclusión sobre H0",
    Data$signif[[match(Ques$id[i], Data$id)]] == Data$signif_NB[[match(Ques$id[i], Data$id)]] ~ "Quitar el bloque no cambia la conclusion sobre H0"),
    "\n\n")
  
  cat("\n")
  cat("-----------------------------------------------------------\n")
  cat("\n")
  
}


#' Anova 1F + Bloque: Reporte de preguntas elaboradas (version markdown)
#'
#' @param Data Tabla de datos muestrales
#' @param Ques Tabla de ejercicios generados a parir de la tabla de datos muestrales
#' @param i Id de la pregunta especifica a describir
#'
#' @return Genera un archivo de texto
#'
#' @examples
rep_Q_AB_Rmd <- function(Data,Ques,i){
  cat("## Entrada ", i,"B --  ", as.character(Ques[[i,"Code"]]), "\n")
  cat("-   Niveles de A: ", as.character(Data$a[[match(Ques$id[i], Data$id)]]), "\n")
  cat("\n")
  cat("-   Niveles de B (Bloque): ", as.character(Data$b[[match(Ques$id[i], Data$id)]]), "\n \n")
  cat("###  Enunciado:::  \n")
  #cat(DB[[i,"out"]], "\n")
  
  cat(Ques[[i,"statem"]], "\n\n")
  
  cat("### Datos::  \n")
  cat(knitr::kable(Ques[[i,"table"]], "pipe"), "\n\n")
  
  #cat("\n\n")
  cat("###  Preguntas y sus respuestas:: \n\n")
  
  cat("-   A1 (2 pts) Suma de cuadrados de los tratamientos (SSt), del bloque (SSb) y del error (SSE), redondeando a 3 cifras significativas.", "\n")
  cat("\n")
  cat("-   A2 (2 pts) Media de cuadrados de los tratamientos (MSt), del bloque (MSb) y del error (MSE) a 3 cifras significativas.", "\n")
  cat("\n")
  cat("-   A3 (1 pts) Estadistico de prueba, F0", "\n")
  cat("\n")
  cat("      Res: \n")
  cat(knitr::kable(Ques[[i,"table2"]], "pipe"),  "\n\n")
  
  cat("\n")
  cat("-   A4 (1 pts) A partir del F0 calculado, y el Fcritico (revisa el enunciado del problema) para una confianza del 95%, considerando H0: Ninguno de los niveles del factor afectan a la variable de respuesta, y Ha: Al menos uno de los niveles tiene un efecto significativo. ¿Conclusion? ", "\n")
  cat("\n")
  Signi <- dplyr::case_when(
    Data$signif[[match(Ques$id[i], Data$id)]] == "Signif" ~ "Se rechaza H0. El factor A es significativo.",
    Data$signif[[match(Ques$id[i], Data$id)]] == "No signif" ~ "No se rechaza H0. El factor A no es significativo.")
  cat("\n")
  cat("      Res:")
  cat("\n")
  cat("      ",Signi, "\n\n")
  
  cat("- A5 (2 pts) Realiza una Prueba de Tukey de Comparaciones Múltiples para identificar cual (o cuales) de los niveles del factor A es (o son) significativamente diferente del resto.", "\n")
  cat("\n")
  cat("      Res: \n")
  cat("\n")
  cat(knitr::kable(list(Data$tukey[[match(Ques$id[i], Data$id)]]), "pipe"), "\n\n")
  cat("\n")
  cat(knitr::kable(list(Data$tukey_groups[[match(Ques$id[i], Data$id)]]), "pipe"), "\n\n")
  
  ### Matriz de comparacion...
  #Data$signif_NB[[match(Ques$id[i], Data$id)]]
  x1 <- Data$data[[match(Ques$id[i], Data$id)]] #La Tabla
  ## Calcula los promedios por nivel de la variable
  x1m <- x1 %>% tidyr::gather(key = "X", value = "Y", A1:last_col())%>%
    dplyr::group_by(X) %>% dplyr::summarise(Y=mean(Y))
  ## Prepara la matriz de comparación por parejas
  names(x1m$Y) <- x1m$X
  x1pw <- abs(outer(x1m$Y, x1m$Y, FUN = "-"))
  x1pw <- upper.tri(x1pw)*x1pw
  x1pw[x1pw == 0] <- NA
  #x3pw
  ## Guarda los valores de LSD y HSD 
  x1_LSD <- Data$lsd[[match(Ques$id[i], Data$id)]][[6]]
  x1_HSD <- Data$tukey[[match(Ques$id[i], Data$id)]][[6]]
  x1_PostHoc <- list(
    outer(x1pw, x1_LSD, FUN = ">")[,,1],
    outer(x1pw, x1_HSD, FUN = ">")[,,1]
  ) #[[1]] es LSD, [[2]] es HSD
  ###########;
  cat("\n")
  cat(knitr::kable(x1_PostHoc[2],"pipe"))
  cat("\n\n")
  
  cat("- A6 (2 pts)  Vuelve a realizar el ANOVA, ahora sin la variable Bloque. ¿Identificas alguna diferencia con respecto a tu conclusión anterior? ¿Qué puedes concluir con respecto al bloque incorporado en este experimento?", "\n")
  cat("\n")
  cat("-      Res:\n")
  cat(knitr::kable(list(Data$anova_NB[[match(Ques$id[i], Data$id)]]), "pipe"), "\n")
  cat("\n")
  cat("    ", dplyr::case_when(
    Data$signif_NB[[match(Ques$id[i], Data$id)]] == "Signif" ~ "Se rechaza H0. El factor A es significativo",
    Data$signif_NB[[match(Ques$id[i], Data$id)]] == "No signif" ~ "No se rechaza H0. El factor A no es significativo"),
    "\n")
  
  cat("    ", dplyr::case_when(
    Data$signif[[match(Ques$id[i], Data$id)]] != Data$signif_NB[[match(Ques$id[i], Data$id)]] ~ "Quitar el bloque cambia la conclusión sobre H0",
    Data$signif[[match(Ques$id[i], Data$id)]] == Data$signif_NB[[match(Ques$id[i], Data$id)]] ~ "Quitar el bloque no cambia la conclusion sobre H0"),
    "\n\n")
  
  cat("\n")
  cat("-----------------------------------------------------------\n")
  cat("\n")
  
}


#### TEMA 5 - Diseño 2^k -------------------
## Reporte de sets de datos
#' Diseño 2^k: Reporte de sets de datos
#'
#' @param DB Tabla de datos muestrales
#'
#' @return Genera un archivo de texto
#'
#' @examples
rep_D_2k <- function(DB){
  #outpath <- paste0(out, ".txt")
  outpath <- paste0(attr(DB,"DBname"), ".txt")  
  
  sink(outpath)
  cat ("Inicializando.... Base de datos: ",  deparse(substitute(DB)), "\n")
  for(i in 1:length(DB$id)){
    cat("-----------------------------------------------------------\n")
    cat(" Entrada #: ", as.character(DB[[i,1]]), "\n")
    cat("-----------------------------------------------------------\n")
    cat("   # Factores: ", as.character(DB[[i,4]]), "\n")
    cat("   Replicas: ", as.character(DB[[i,5]]), "\n \n")
    cat("Datos crudos: \n")
    cat(knitr::kable(DB[[i,2]], "pipe"))
    cat("\n \n")
    df <- as.data.frame(DB[[i,2]]) %>% 
      tidyr::gather(key="rep", value = "Y", Y:last_col()) %>% 
      dplyr::select(-rep)
    model <- lm(Y~(.)^6,df)
    dfCoEf <- as.data.frame((coefficients(model)))  %>% 
      tibble::rownames_to_column("Factor") %>% 
      dplyr::transmute(Factor,Coef =(coefficients(model)),Eff=Coef*2) %>% 
      dplyr::mutate_if(is.numeric,round,3)  #Guarda Coeficientes y Efectos en un DF
    
    cat("\n Tabla de Efectos y Coeficientes: \n")
    cat(knitr::kable(list(dfCoEf), "pipe"))
    cat("\n \n")
    cat("Tabla de ANOVA: \n")
    cat(knitr::kable(DB[[i,3]], "pipe"), "\n")
    cat("\n")
    cat("   F critica: ", DB[[i,9]], "\n")
    cat("\n")
    cat("   Efectos significaivos: ", paste(DB[[i,10]]))
    cat("\n")
    cat("   Ecuación de Regresion: ", DB[[i,12]])
    cat("\n \n")
    cat("Predicción de la respuesta con el mejor modelo: \n")
    cat(knitr::kable(DB[[i,13]], "pipe"), "\n")
    cat("\n")
    cat("Mejor combinación de niveles \n")
    cat("   - Minimizar: \n")
    cat(knitr::kable(DB[[i,15]], "pipe"), "\n")
    cat("   - Maximizar: \n")
    cat(knitr::kable(DB[[i,14]], "pipe"))
    cat("\n \n")
    cat("-----------------------------------------------------------\n")
    cat("\n")
  }
  sink()
  file.show(outpath)
}

## Reporte de preguntas elaboradas
#' Diseño 2^k: Reporte de preguntas elaboradas
#'
#' @param Data Tabla de datos muestrales
#' @param Ques Tabla de ejercicios generados a parir de la tabla de datos muestrales
#' @param i Id de la pregunta especifica a describir
#'
#' @return Genera un archivo de texto
#'
#' @examples
rep_Q_2k <- function(Data, Ques, i){
  cat("-----------------------------------------------------------\n")
  cat(" Entrada ", i,"#: -- ", as.character(Ques[[i,"Code"]]), "\n")
  cat("-----------------------------------------------------------\n")
  cat("   # Factores: ", as.character(Data$k[[match(Ques$id[i], Data$id)]]), "\n")
  cat("   Replicas: ", as.character(Data$n[[match(Ques$id[i], Data$id)]]), "\n \n")
  
  cat("  Enunciado:::  \n")
  #cat(DB[[i,"out"]], "\n")
  cat(Ques[[i,"statem"]], "\n\n")
  
  cat("  Datos::  \n")
  cat(knitr::kable(list(Data$data[[match(Ques$id[i], Data$id)]]), "pipe"), "\n\n")
  
  cat("  Preguntas y sus respuestas:: \n\n")
  
  cat("- B1 (2 pts) Calcula y reporta los efectos de los factores principales y las interacciones. \n")
  cat("Res: \n")
  
  df <- as.data.frame(Data$data[[match(Ques$id[i], Data$id)]]) %>% 
    tidyr::gather(key="rep", value = "Y", Y:last_col()) %>% 
    dplyr::select(-rep)
  model <- lm(Y~(.)^6,df)
  dfCoEf <- as.data.frame((coefficients(model)))  %>% 
    tibble::rownames_to_column("Factor") %>% 
    dplyr::mutate(Coef=(coefficients(model)),Efecto=Coef*2) %>%
    dplyr::transmute(Factor,Efecto,Coef) %>% 
    dplyr::mutate_if(is.numeric,round,3)  #Guarda Coeficientes y Efectos en un DF
  #cat("\n Tabla de Efectos y Coeficientes: \n")
  cat(knitr::kable(list(dfCoEf), "pipe"))
  cat("\n\n")
  
  cat("- B2 (2 pts) En tu hoja de respuestas, dibuja las gráficas de Efectos Principales para los 3 factores. En el espacio a continuación (aqui en Canvas) explica si, a partir de las gráficas, pudiera identificar si uno de los factores pareciera tener un efecto significativo, y explica por qué. \n")
  cat("Res: NA \n ")
  cat("\n\n")
  
  cat("- B3 (3 pts) Realiza un analisis de varianza Anova y reporta cuales factores e interacciones son significativos. Utiliza un alfa de 0.05 (el valor de F critico a utilizar en las comparaciones está dado en el enunciado del problema) \n")
  cat("Res: \n")
  cat(knitr::kable(list(Data$anova[[match(Ques$id[i], Data$id)]]), "pipe"), "\n")
  cat("Significativos: ", Data$sign[[match(Ques$id[i], Data$id)]][[1]])
  cat("\n\n")
  
  cat("- B4 (1 pts) A partir de los factores e interacciones que resultaron significativos, da el Modelo de Regresión Abreviado (unicamente con los coeficientes de los  factores e interacciones que resultaron significativos) \n")
  cat("Res: \n")
  cat("Ecuacion de regresion abreviada ", Data$EqR[[match(Ques$id[i], Data$id)]])
  cat("\n\n")
  
  cat("Predicción de la respuesta con el mejor modelo: \n")
  cat(knitr::kable(list(Data$Fit[[match(Ques$id[i], Data$id)]]), "pipe"), "\n")
  cat("\n")
  
  cat("- B5 (2 pts) Utilizando la formula del modelo de regresión abreviada, de la pregunta anterior, contesta:
        a) ¿Cuál es el valor minimo de la variable respuesta predicho por el modelo abreviado
        b) ¿Qué combinación de niveles en unidades originales (es decir, NO como -1 y +1) de las variables sugerirías para minimizar la respuesta? \n")
  cat("Res: \n")
  cat("   - Minimizar: \n")
  cat(knitr::kable(list(Data$Fit_Min[[match(Ques$id[i], Data$id)]]), "pipe"), "\n\n")
  
  cat("- B6 (2 pts) Utilizando la formula del modelo de regresión abreviada, de la pregunta anterior, contesta:
        a) ¿Cuál es el valor maximo de la variable respuesta predicho por el modelo abreviado
        b) ¿Qué combinación de niveles en unidades originales (es decir, NO como -1 y +1) de las variables sugerirías para maximizar la respuesta? \n")
  cat("Res: \n")
  cat("   - Maximizar: \n")
  cat(knitr::kable(list(Data$Fit_Max[[match(Ques$id[i], Data$id)]]), "pipe"))
  cat("\n \n")
  cat("-----------------------------------------------------------\n")
  cat("\n")
  
}

#' Diseño 2^k: Reporte de preguntas elaboradas (version markdown)
#'
#' @param Data Tabla de datos muestrales
#' @param Ques Tabla de ejercicios generados a parir de la tabla de datos muestrales
#' @param i Id de la pregunta especifica a describir
#'
#' @return Genera un archivo de texto
#'
#' @examples
rep_Q_2k_Rmd <- function(Data, Ques, i){
  cat("## Entrada ", i,"#: -- ", as.character(Ques[[i,"Code"]]), "\n")
  cat("-   Factores: ", as.character(Data$k[[match(Ques$id[i], Data$id)]]), "\n")
  cat("\n")
  cat("-   Replicas: ", as.character(Data$n[[match(Ques$id[i], Data$id)]]), "\n \n")
  cat("\n")
  
  cat("### Enunciado  \n")
  #cat(DB[[i,"out"]], "\n")
  cat(Ques[[i,"statem"]], "\n\n")
  cat("\n")
  
  cat("### Datos  \n")
  cat(knitr::kable(list(Data$data[[match(Ques$id[i], Data$id)]]), "pipe"), "\n\n")
  cat("\n")
  
  cat("### Preguntas y sus respuestas \n\n")
  cat("-   B1 (2 pts) Calcula y reporta los efectos de los factores principales y las interacciones. \n")
  cat("\n")
  cat("      Res: \n")
  
  df <- as.data.frame(Data$data[[match(Ques$id[i], Data$id)]]) %>% 
    tidyr::gather(key="rep", value = "Y", Y:last_col()) %>% 
    dplyr::select(-rep)
  model <- lm(Y~(.)^6,df)
  dfCoEf <- as.data.frame((coefficients(model)))  %>% 
    tibble::rownames_to_column("Factor") %>% 
    dplyr::mutate(Coef=(coefficients(model)),Efecto=Coef*2) %>%
    dplyr::transmute(Factor,Efecto,Coef) %>% 
    dplyr::mutate_if(is.numeric,round,3)  #Guarda Coeficientes y Efectos en un DF
  #cat("\n Tabla de Efectos y Coeficientes: \n")
  cat(knitr::kable(list(dfCoEf), "pipe"))
  cat("\n\n")
  
  cat("-   B2 (2 pts) En tu hoja de respuestas, dibuja las gráficas de Efectos Principales para los 3 factores. En el espacio a continuación (aqui en Canvas) explica si, a partir de las gráficas, pudiera identificar si uno de los factores pareciera tener un efecto significativo, y explica por qué. \n")
  cat("\n")
  cat("      Res: NA \n ")
  cat("\n\n")
  
  cat("- B3 (3 pts) Realiza un analisis de varianza Anova y reporta cuales factores e interacciones son significativos. Utiliza un alfa de 0.05 (el valor de F critico a utilizar en las comparaciones está dado en el enunciado del problema) \n")
  cat("\n")
  cat("      Res: \n")
  cat("\n")
  cat(knitr::kable(list(Data$anova[[match(Ques$id[i], Data$id)]]), "pipe"), "\n")
  cat("\n")
  cat("Significativos: ", Data$sign[[match(Ques$id[i], Data$id)]][[1]])
  cat("\n\n")
  
  cat("-   B4 (1 pts) A partir de los factores e interacciones que resultaron significativos, da el Modelo de Regresión Abreviado (unicamente con los coeficientes de los  factores e interacciones que resultaron significativos) \n")
  cat("\n")
  cat("      Res: \n")
  cat("\n")
  cat("Ecuacion de regresion abreviada ", Data$EqR[[match(Ques$id[i], Data$id)]])
  cat("\n\n")
  
  cat("Predicción de la respuesta con el mejor modelo: \n")
  cat(knitr::kable(list(Data$Fit[[match(Ques$id[i], Data$id)]]), "pipe"), "\n")
  cat("\n")
  
  cat("-   B5 (2 pts) Utilizando la formula del modelo de regresión abreviada, de la pregunta anterior, contesta:
        a) ¿Cuál es el valor minimo de la variable respuesta predicho por el modelo abreviado
        b) ¿Qué combinación de niveles en unidades originales (es decir, NO como -1 y +1) de las variables sugerirías para minimizar la respuesta? \n")
  cat("\n")
  cat("      Res: \n")
  cat("\n")
  cat("-      Minimizar: \n")
  cat("\n")
  cat(knitr::kable(list(Data$Fit_Min[[match(Ques$id[i], Data$id)]]), "pipe"), "\n\n")
  cat("\n")
  cat("-   B6 (2 pts) Utilizando la formula del modelo de regresión abreviada, de la pregunta anterior, contesta:
        a) ¿Cuál es el valor maximo de la variable respuesta predicho por el modelo abreviado
        b) ¿Qué combinación de niveles en unidades originales (es decir, NO como -1 y +1) de las variables sugerirías para maximizar la respuesta? \n")
  cat("\n")
  cat("     Res: \n")
  cat("\n")
  cat("-     Maximizar: \n")
  cat("\n")
  cat(knitr::kable(list(Data$Fit_Max[[match(Ques$id[i], Data$id)]]), "pipe"))
  cat("\n \n")
  cat("-----------------------------------------------------------\n")
  cat("\n")
}

