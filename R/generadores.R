#### TEMA 1 - Media Muestral -------------------

## Generador de datos ----------------------------
#' Media Muestral: Generador de datos muestrales
#'
#' Version 3.7.3 - usar los paquetes here y folders para los paths relativos.
#' Version 3.7.2
#'
#' @param r Numero de sets de datos a generar
#'
#' @return Una tabla de datos, se guarda en un archivo, y se puede asignar a un objeto
#' @export
#'
#' @examples
genMediaMuestral <- function (r){
  #####------------ Incializa la Base de Datos ----------#####
  Data <- tibble::tibble(id = numeric(),
                         data = list(),
                         n = numeric(),
                         distr = character(),
                         mu = numeric(),
                         sig = numeric(),
                         sam_mu = numeric(),
                         sam_sig = numeric(),
                         q = numeric(),
                         q_est = numeric(),
                         p_left = numeric(),
                         p_right = numeric())

  ####---------- Generador: Datos de población y muestra -------------
  for (j in 1:r) { #r es cuantos SETS a preparar
    n <- ceiling(runif(1,5,40))
    mu <- round((100*runif(1)),3)
    sig <- (round(runif(1,1,10)*(50*runif(1)),3))/n #Sigma no debe ser más grande que mu
    distr <- dplyr::case_when(n >= 30 ~ "z", #Segun tamaño muestral, la distribucion a usar
                              n < 30 ~ "t")
    ####-------- Genera una muestra
    data <- rnorm(n,mu,sig)
    sam_mu <- mean(data)
    sam_sig <- sd(data)

    ####---------- Estimación estadistica
    alfa <- sample(seq(0.01,0.1,by = 0.005),1) #Alfa solo para T-student

    #q_est Puede dar positivo o negativo
    q_est <- dplyr::case_when(distr == "z" ~ runif(1,-2.85,2.85),
                              #Cuando es Z, se genera aleatoreamente
                              distr == "t"~ qt(alfa,n-1,lower.tail = sample(c(F,T),1)))

    # El estadistico en unidades originales
    q <- round(dplyr::case_when(distr == "z" ~ (q_est*(sig/sqrt(n)))+mu,
                                distr == "t" ~ (q_est*(sam_sig/sqrt(n)))+mu),3)

    # Calculo de la probabilidad
    p_left <- dplyr::case_when(distr == "z" ~ pnorm(q_est,0,1,lower.tail=T),
                               distr == "t" ~ pt(q_est, df = n-1, lower.tail = T))
    p_right <- dplyr::case_when(distr == "z" ~ pnorm(q_est,0,1,lower.tail=F),
                                distr == "t" ~ pt(q_est, df = n-1, lower.tail = F))

    #####----------- Construye base de datos -------------
    Data <- Data %>% dplyr::add_row(id = j, # Indice del set de datos
                             data = list(data), # Datos muestrales
                             n = n, # Tamaño de la muestra
                             distr = distr,
                             mu = round(mu,3),
                             sig = round(sig,3),
                             sam_mu = round(sam_mu,3),
                             sam_sig = round(sam_sig,3),
                             q = q,
                             q_est = q_est,
                             p_left = round(p_left,3),
                             p_right = round(p_right,3))
  }
  ######------- Metadatos de la base de datos
  Data <- metadataDB(Data, "MM", "data")

  ####----- Actualizar la Tabla Directorio
  actualizarDirectorio(Data, Directorio)
  exportDB(Data)

  return(Data)
}

## Generador de preguntas --------------------------------------------
#' Media Muestral: Generador de preguntas
#'
#' Vers 3.7.3 - Ajuste del foermato del enunciado  para incorporar  HTML Entities
#' Vers 3.7.2 - Se cambian los ´on.exit(sink())´ y ´sink()´ por  ´closeAllConnections()´
#' Version 3.7.1 - Adaptado para generar enunciados en ingles y español simultaneamente
#'
#' @param Data Tabla de datos muestrales a usar para la generación de preguntas
#' @param Q Numero de pregunas a generar
#' @param format Formato para los enunciados de preguntas. "HTML" o "LaTeX".
#' @param silent Si `TRUE`, enonces no se abre el archivo de texto generado
#'
#' @return Una tabla de datos, se guarda en un archivo, y se puede asignar a un objeto
#' @export
#'
#' @examples
questionMediaMuestral <- function(Data,Q,format="HTML",silent=T){
  #outpath <- paste0(out,".md")
  DBorigin <- attr(Data, "DBname")
  colas <- c("<",">") ## Signo
  elementos <- c("m", "q", "p") ## Cual falta?

  ####------------- Setup -------------------
  Data <- tibble::as_tibble(lapply(Data, as.character)) %>% dplyr::mutate(
    sig = dplyr::case_when(distr == "z" ~ sig,
                           distr == "t" ~ "desconocido"))

  ###------- Enunciados para la presentación de datos del problema.
  w<-c()  #Enunciado para la resolución del problema
  x1<-c() #Datos poblacion
  x2<-c() #Datos muestra
  y<-c()  #Enunciado de probabilidad
  z<-c()  #Encuneciado del problema
  z_Eng<-c()
  z_Esp<-c()

  ###----- Genera base de datos para guardar los problemas
  ans <- tibble::tibble(Index = 1:Q,
                        dbID = sample(Data$id,Q,replace=TRUE), #Elige aleatoreamente los datos para elaborar las preguntas
                        Topic = "M",
                        Ques = sample(elementos,Q, replace = TRUE), #Elige aleatoreamente el tipo de problema a generar
                        #Code = str_c(Index,Topic,dbID,Ques), #Genera la clave identificadora
                        Code = stringr::str_c(sprintf("%02d",Index),
                                              Topic,"01",
                                              Ques,sprintf("%02d",as.numeric(dbID))),
                        distr = NA,
                        Ans = NA,
                        n = NA,
                        mu = NA,
                        sig = NA,
                        sam_sig = NA,
                        q = NA,
                        cola = NA,
                        p = NA,
                        z = NA,
                        z_Esp = NA,
                        z_Eng = NA)

  ### -------- Formación de preguntas utilizando la base de datos externa

  ## ----  Recolectando los datos externos.
  for (i in 1:length(ans$Index)){
    #ans[i,"Index"] <- i
    ans[i,"distr"] <- Data$distr[match(ans$dbID[i], Data$id)] #Distribución
    ans[i,"n"] <- Data$n[match(ans$dbID[i], Data$id)] #Distribución
    ans[i,"mu"] <- Data$mu[match(ans$dbID[i], Data$id)]
    ans[i,"sig"] <- Data$sig[match(ans$dbID[i], Data$id)]
    ans[i,"sam_sig"] <- Data$sam_sig[match(ans$dbID[i], Data$id)]
    ans[i,"q"] <- Data$q[match(ans$dbID[i], Data$id)]
    ### Decidir: Cola para las preguntas
    ans[i,"cola"] <- sample(colas,1)
    ans[i,"p"] <- dplyr::case_when(
      ans$cola[i] == ">" ~ Data$p_right[match(ans$dbID[i],Data$id)],
      ans$cola[i] == "<" ~ Data$p_left[match(ans$dbID[i],Data$id)])
  }

  ######------- Metadatos de la base de datos
  ans <- metadataDB(ans, "MM", "ques")
  attr(ans,"DBorigin") <- DBorigin

  ##--- Actualización del Directorio
  actualizarDirectorio(ans, Directorio)

  #wd <- "D:/Documents/ITESM/IBT21/BT2004B/003 - Actividades/Examen/DB/Output/"
  wd <- here::here("doetest_out", "reportes")

  outpath <- here::here(wd,paste0(attr(ans, "DBname"), ".md"))

  ###### -------- Formulando enunciados de preguntas en un archivo de salida
  #outpath
  sink(outpath)
  #on.exit(sink())
  for (i in 1:length(ans$Index)){ ##Irse pregunta por pregunta
    ques <- ans$Ques[i] #sample(elementos,1) #Elegir el tipo de pregunta

    if (format == "HTML"){
      #####-------------- HTML-------------------####

      ## ---- Generador de incognita
      if (ques == "m"){ #Falta mu. Se da n, sigma ó S, q, y la probabilidad - según la cola
        ans[i,"Ans"] <- ans[i,"mu"] #Data[i,"mu"]
        ans[i,"mu"] <- "?"
        w[i] <- "Encuentra el valor de &mu;. "
      }else if(ques == "q"){  #Se da la probabilidad, n, mu, sigma ó S
        ans[i,"Ans"] <- ans[i,"q"]
        ans[i,"q"] <- "B"
        w[i] <- "Encuentra el valor de B. "
      }else if(ques == "p"){ #Obten la probabilidad, a partir de los datos
        ans[i,"Ans"] <- ans[i,"p"]
        ans[i,"p"] <- "?"
        w[i] <- "Resuelve. "
        #}else if(ques == "s"){ #Falta sigma, a partir de los datos ya obtenidos
        #  ans[i,"Ans"] <- ans[i,"sig"]
        #  ans[i,"sig"] <- "?"
        #  w[i] <- "Encuentra el valor de $\\sigma$. "
      }

      ## ---------------- Enunciado de Problemas
      # x1[i]: Poblacion
      x1[i] <- paste0('"X &sim; N( &mu;  = ', ans$mu[i])
      x1[i] <- stringr::str_c(x1[i], dplyr::case_when(
        ans[i,"distr"] == "z" ~ stringr::str_c(", σ  = ", ans$sig[i],") "),
        ans[i,"distr"] == "t" ~ stringr::str_c(") ")))
      # x2[i]: Muestra
      x2[i] <- paste0("de tamaño ", ans$n[i]," con una desviación muestral de S = ", ans$sam_sig[i],". ")

      ## y[i]: Enunciado probabilidad
      #y[i] <- paste0("P( x̄ ", ans$cola[i], ans$q[i],") = ", ans$p[i],". \n")
      y[i] <- paste0("P(X&#772; ", ans$cola[i], ans$q[i]," ) = ", ans$p[i],". \n")
      ## z[i]: Enunciado final
      z[i] <- paste0("[", ans$Code[i],"] ",
              "A partir de una población normal con las siguientes caracteristicas: ",
              x1[i], ", se obtuvo una muestra ", x2[i],"Dado que ", y[i], w[i] ,"\n")
      ans$z[i] <- z[i]
    }

    else if (format == "LaTeX"){
      #####------------------ LaTEx ------------####

      ## ---- Generador de incognita
      if (ques == "m"){ #Falta mu. Se da n, sigma ó S, q, y la probabilidad - según la cola
        ans[i,"Ans"] <- ans[i,"mu"] #Data[i,"mu"]
        ans[i,"mu"] <- "?"
        w[i] <- "Encuentra el valor de $\\mu$. "
      }else if(ques == "q"){  #Se da la probabilidad, n, mu, sigma ó S
        ans[i,"Ans"] <- ans[i,"q"]
        ans[i,"q"] <- "B"
        w[i] <- "Encuentra el valor de B. "
      }else if(ques == "p"){ #Obten la probabilidad, a partir de los datos ya obtenidos
        ans[i,"Ans"] <- ans[i,"p"]
        ans[i,"p"] <- "?"
        w[i] <- "Resuelve. "
        #}else if(ques == "s"){ #Falta sigma, a partir de los datos ya obtenidos
        #  ans[i,"Ans"] <- ans[i,"sig"]
        #  ans[i,"sig"] <- "?"
        #  w[i] <- "Encuentra el valor de $\\sigma$. "
      }

      ## ---------------- Enunciado de Problemas
      # x1[i]: Poblacion
      x1[i] <- paste0("$X \\sim N\\left( {\\mu  = ", ans$mu[i])
      x1[i] <- stringr::str_c(x1[i], dplyr::case_when(
        ans[i,"distr"] == "z" ~ stringr::str_c(",\\sigma  = ", ans$sig[i],"} \\right)$"),
        ans[i,"distr"] == "t" ~ stringr::str_c("} \\right)$")))
      # x2[i]: Muestra
      x2[i] <- paste0("de tamaño ", ans$n[i]," con una desviación muestral de S = $", ans$sam_sig[i],"$. ")

      ## y[i]: Enunciado probabilidad
      y[i] <- paste0("$P\\left( {\\overline{X} ", ans$cola[i], ans$q[i]," } \\right) = ", ans$p[i],"$. \n")
      ## z[i]: Enunciado final
      z[i] <- paste0("[", ans$Code[i],"] ",
                     "A partir de una población normal con las siguientes caracteristicas: ",
                     x1[i], ", se obtuvo una muestra ", x2[i],"Dado que ", y[i], w[i] ,"\n")
      ans$z[i] <- z[i]
    }

    ###------------ Independientemente del formato, se añade al archivo
    cat(z[i]) #Añade enunciado al archivo
    cat("\n")

  }

  #sink()
  closeAllConnections()   # .........................
  if(silent == F){
    file.show(outpath)
  }

  ## Conversion al ingles
  ans <- ans %>%
    dplyr::mutate(z_Esp = z, z_Eng = z, ##Guarda un respaldo en español, para poder manipular z segun el idioma elegido
      z_Eng = stringr::str_replace(z_Eng, "A partir de una población normal con las siguientes caracteristicas","From a normal population with the following characteristics"),
      z_Eng = stringr::str_replace(z_Eng, "desconocido","unknown"),
      z_Eng = stringr::str_replace(z_Eng, "se obtuvo una muestra de tamaño","a sample was taken of size"),
      z_Eng = stringr::str_replace(z_Eng, "con una desviación muestral de","with a sample standard deviation of"),
      z_Eng = stringr::str_replace(z_Eng, "Dado que","Given that"),
      z_Eng = stringr::str_replace(z_Eng, "Encuentra el valor de","Find"),
      z_Eng = stringr::str_replace(z_Eng, "Resuelve","Solve"))

  ### Exportar base de datos DESPUEs de todas las modificaciones
  exportDB(ans)

  #####--------------- ARCHIVO DE REPORTE ----------------

  ans <- ans %>%
    dplyr::mutate(Ans= as.numeric(Ans),
                  Min = dplyr::case_when(
                    distr == "t" ~ (Ans - (0.075*Ans)),
                    distr == "z" ~ (Ans - (0.015*Ans))),
                  Max = dplyr::case_when(
                    distr == "t" ~ (Ans + (0.075*Ans)),
                    distr == "z" ~ (Ans + (0.015*Ans))))

  #wd <- "D:/Documents/ITESM/IBT21/BT2004B/003 - Actividades/Examen/DB/Output/"
  ###--- Datos de encabezado
  outpath2 <- here::here(wd, paste0(attr(ans, "DBname"),"_RES", ".txt"))
  sink(outpath2)
  #on.exit(sink())
  cat("Inicializando.... Base de datos de Preguntas","\n")
  cat("   id: ", attr(ans, "id_bytes"), "\n")
  cat("   id: ", attr(ans, "id_pokemon"), "\n")
  cat("- Elaborado en: ", attr(ans,"Fecha"), "\n")
  cat("- Utilizando datos de la tabla: ", DBorigin, "\n")
  cat("- Para el tema: ", attr(ans,"Tema"),"\n")
  cat("- Lista de problemas generados: ", attr(ans, "Elementos"), "\n\n\n")

  #Inicia la generación del reporte de cada pregunta.
  for(i in 1:length(ans$Index)){
    ##--- Formato generico,
    rep_Q_MM(Data,ans,i) ##Usando una función para simplificar...
  }
  #sink()
  closeAllConnections()   # .........................
  if(silent == F){
    file.show(outpath2)
  }

  return(ans)
}


#### TEMA 2 - Prueba de Hipotesis -------------------
## Generador de datos -------------------------------
#' Prueba de Hipotesis: Generador de datos muestrales
#'
#' Version 3.7.2
#'
#' @param r Numero de sets de datos a generar
#'
#' @return Una tabla de datos, se guarda en un archivo, y se puede asignar a un objeto
#' @export
#'
#' @examples
genPruebaHipotesis <- function (r){
  #####------------ Incializa la Base de Datos ----------#####
  Data <- tibble::tibble(id = numeric(),
                         ## Data para Pob1
                         data_1 = list(),
                         n_1 = numeric(),
                         mu_1 = numeric(),
                         sig_1 = numeric(),
                         s_mu_1 = numeric(),
                         s_sig_1 = numeric(),
                         ## Data para Pob2
                         data_2 = list(),
                         n_2 = numeric(),
                         mu_2 = numeric(),
                         sig_2 = numeric(),
                         s_mu_2 = numeric(),
                         s_sig_2 = numeric(),
                         ## Comparativa
                         distr = character(),
                         q = numeric(), #diff
                         q_est = numeric()) #TestStatistic

  ####---------- Inicializador -------------
  for (j in 1:r) { #r es cuantos SETS a preparar

    distr <- sample(c("z","t"),1)
    n_1 <- dplyr::case_when(distr == "t" ~ ceiling(runif(1,5,16)),
                            distr == "z" ~ ceiling(runif(1,35,45)))
    n_2 <- n_1 + ceiling(runif(1,-1,1)/4*10)
    #mu <- round((100*runif(1)),3)
    #sig <- (round(runif(1,1,10)*(50*runif(1)),3))/n

    ## Datos de las pobs
    mu_1 <- round(runif(1)*(100*runif(1)),4)
    sig_1 <- round(runif(1)*(25*runif(1)),4)
    mu_2 <- rnorm(1,mu_1,sig_1)
    sig_2 <- abs(rnorm(1,sig_1,abs(sig_1+sig_1*rnorm(1))))

    ## Datos de las muestras
    data_1 <- rnorm(n_1,mu_1,sig_1)
    s_mu_1 <- mean(data_1)
    s_sig_1 <- sd(data_1)

    data_2 <- rnorm(n_2,mu_2,sig_2)
    s_mu_2 <- mean(data_2)
    s_sig_2 <- sd(data_2)

    #q <- floor(rnorm(1)*runif(1)+(mu_1-mu_2)/2)
    q <- 0 ##Diferencia entre mu_1 y mu_2
    q_est <- dplyr::case_when (
      distr == "t" ~ round(((s_mu_1-s_mu_2)-q)/(sqrt((s_sig_1^2/n_1)+(s_sig_2^2/n_2))),4),
      distr == "z" ~ round(((s_mu_1-s_mu_2)-q)/(sqrt((sig_1^2/n_1)+(sig_2^2/n_2))),4))

    #####----------- Construye base de datos -------------
    Data <- Data %>% dplyr::add_row(id = j,
                             ## Data para Pob1
                             data_1 = list(data_1),
                             n_1 = n_1,
                             mu_1 = round(mu_1,3),
                             sig_1 = round(sig_1,3),
                             s_mu_1 = round(s_mu_1,3),
                             s_sig_1 = round(s_sig_1,3),
                             ## Data para Pob2
                             data_2 = list(data_2),
                             n_2 = n_2,
                             mu_2 = round(mu_2,3),
                             sig_2 = round(sig_2,3),
                             s_mu_2 = round(s_mu_2,3),
                             s_sig_2 = round(s_sig_2,3),
                             ## Comp
                             distr = distr,
                             q = round(q,3),
                             q_est = round(q_est,3))
  }

  ######------- Metadatos de la base de datos
  Data <- metadataDB(Data, "PH", "data")

  ##--- Actualización del Directorio
  actualizarDirectorio(Data, Directorio)
  exportDB(Data)

  return(Data)
}


## Generador de preguntas ---------------------------------------
#' Prueba de Hipotesis: Generador de preguntas
#'
#' Ver 3.6.6 - Ajuste del foermato del enunciado  para incorporar  HTML Entities
#' Ver 3.6.5 - Se cambian los ´on.exit(sink())´ y ´sink()´ por  ´closeAllConnections()´
#' Ver 3.6.4 - usar los paquetes here y folders para los paths relativos
#' Ver 3.6.3 - Se trata de corregir el problema con las pruebas de dos colas
#' Version 3.6.1 - Adaptado para generar enunciados en ingles y español simultaneamente
#'
#' @param Data Tabla de datos muestrales a usar para la generación de preguntas
#' @param Q Numero de pregunas a generar
#' @param format Formato para los enunciados de preguntas. "HTML" o "LaTeX".
#' @param silent Si `TRUE`, enonces no se abre el archivo de texto generado
#'
#' @return Una tabla de datos, se guarda en un archivo, y se puede asignar a un objeto
#' @export
#'
#' @examples
questionPruebaHipotesis <- function(Data,Q,format = "HTML", silent = T){
  #outpath <- paste0(out,".md")
  DBorigin <- attr(Data, "DBname")

  ####------------- Setup -------------------
  colas <- c("<",">","\\ne") ## Signo
  alfa <- c(0.2,0.1,0.05,0.01) ## Alfa de rechazo

  ###------- Enunciados para la presentación de datos del problema.
  w<-c()  #Enunciado para la resolución del problema
  x1<-c() #Datos poblacion
  x2<-c() #Datos muestra
  y1<-c()  #Enunciado de probabilidad
  y2<-c()
  z <-c()  #Encuneciado del problema

  ###----- Genera base de datos para guardar los problemas
  ans <- tibble::tibble(Index = 1:Q,
                        dbID = sample(Data$id,Q,replace=TRUE),
                        Topic = "H",
                        Ques = NA,
                        Code = NA,
                        #Ques = NA, #Tipo de pregunta -  se elige despues
                        #Code = str_c(Index,Topic,dbID,Ques), #Codigo - se genera despues
                        distr = NA,
                        q = NA,
                        q_est = NA,
                        deg = NA,
                        crit = NA,
                        cola = NA,
                        alfa = NA,
                        pval = NA,
                        comp = NA,
                        z = NA)

  ### --------------- Formación de preguntas

  ## ----  Recolectando los datos externos.
  for (i in 1:length(ans$Index)){
    #ans[i,"Index"] <- i
    ans[i,"distr"] <- Data$distr[match(ans$dbID[i], Data$id)] #Distribución
    ans[i,"q_est"] <- Data$q_est[match(ans$dbID[i], Data$id)]
    ans[i,"q"] <- Data$q[match(ans$dbID[i], Data$id)]

    ###---- Selección de cola y calculo de estatisticos
    ans$cola[i] <- sample(colas,1)
    ans$alfa[i] <- sample(alfa,1)
    ans$deg[i] <- Data$n_1[match(ans$dbID[i], Data$id)]+
      Data$n_2[match(ans$dbID[i],Data$id)]-2

    ans$crit[i] <- dplyr::case_when(
      #Seleccion del valor critico segun distribución y colas
      ans$distr[i] == "t" ~ dplyr::case_when(
        ans$cola[i] == "<" ~ qt(ans$alfa[i],ans$deg[i],lower.tail=T),
        ans$cola[i] == ">" ~ qt(ans$alfa[i],ans$deg[i],lower.tail=F),
        ans$cola[i] == "\\ne" ~ qt((ans$alfa[i]/2),ans$deg[i],lower.tail=F)),
      ans$distr[i] == "z" ~ dplyr::case_when(
        ans$cola[i] == "<" ~ qnorm(ans$alfa[i], 0, 1, lower.tail=T),
        ans$cola[i] == ">" ~ qnorm(ans$alfa[i], 0, 1, lower.tail=F),
        ans$cola[i] == "\\ne" ~ qnorm((ans$alfa[i]/2),0,1,lower.tail=F)))
    ans$crit[i] <- round(ans$crit[i],4) #Redondeo a 4 cifras
    ans$pval[i] <- dplyr::case_when(
      #Calculo del pvalue segun distribución y cola
      ans$distr[i] == "t" ~ dplyr::case_when(
        ans$cola[i] == "<" ~ pt(ans$q_est[i],ans$deg[i],lower.tail=T),
        ans$cola[i] == ">" ~ pt(ans$q_est[i],ans$deg[i],lower.tail=F),
        ans$cola[i] == "\\ne" ~ pt(abs(ans$q_est[i]),ans$deg[i],lower.tail=F)),
      ans$distr[i] == "z" ~ dplyr::case_when(
        ans$cola[i] == "<" ~ pnorm(ans$q_est[i],0,1,lower.tail=T),
        ans$cola[i] == ">" ~ pnorm(ans$q_est[i],0,1,lower.tail=F),
        ans$cola[i] == "\\ne" ~ pnorm(abs(ans$q_est[i]),0,1,lower.tail=F)))
    ans$pval[i] <- round(ans$pval[i],4)

    if (ans$cola[i] == "\\ne"){
      ans$comp[i] <- dplyr::case_when(
        ans$pval[i] < (ans$alfa[i]/2) ~ "Reject",
        ans$pval[i] > (ans$alfa[i]/2) ~ "Fail to Reject")
    }else {
      ans$comp[i] <- dplyr::case_when(
        ans$pval[i] < ans$alfa[i] ~ "Reject",
        ans$pval[i] > ans$alfa[i] ~ "Fail to Reject")
    }

    ###----- Codigo
    ans[i,"Ques"] <- dplyr::case_when(
      ans$cola[i] == ">" ~ "rt",
      ans$cola[i] == "<" ~ "lt",
      ans$cola[i] == "\\ne" ~ "tt")
    ans[i,"Code"] <- stringr::str_c(sprintf("%02d",ans$Index[i]),
                                    ans$Topic[i],"01",
                                    ans$Ques[i],
                                    sprintf("%02d",ans$dbID[i]))
  }

  Data <- tibble::as_tibble(lapply(Data, as.character)) #Parseo a caracter para facilitar el armado de los enunciados de pregunta


  ######------- Metadatos de la base de datos
  ans <- metadataDB(ans, "PH", "ques")
  attr(ans,"DBorigin") <- DBorigin


  ###--- Actualización del Directorio
  actualizarDirectorio(ans, Directorio)


  ### --- Formulando las preguntas en un archivo de salida
  #wd <- "D:/Documents/ITESM/IBT21/BT2004B/003 - Actividades/Examen/DB/Output/"
  wd <- here::here("doetest_out", "reportes")
  outpath <- here::here(wd,paste0(attr(ans, "DBname"),".md"))
  sink(outpath)
  #on.exit(sink())


  for (i in 1:length(ans$Index)){
    ###-------------- HTML
    if (format == "HTML"){
      ## --- Enunciado de Datos
      #x1[i]: Muestra 1
      x1[i] <- paste0("Del primer grupo, ",
                      dplyr::case_when(ans$distr[i] == "t" ~ " ",
                                       ans$distr[i] == "z" ~ paste0(
                                         "que tenía una desviación de &sigma;<sub>1</sub> =",
                                         Data$sig_1[match(ans$dbID[i], Data$id)],", ")),
                      "se obtuvo una muestra de tamaño n<sub>1</sub> = ",
                      Data$n_1[match(ans$dbID[i], Data$id)],", ",
                      "que tuvo un promedio de X&#772;<sub>1</sub> = ",
                      Data$s_mu_1[match(ans$dbID[i], Data$id)],", ",
                      "y una desviación S<sub>1</sub> = ",
                      Data$s_sig_1[match(ans$dbID[i], Data$id)]," ","\n")
      #x2[i]: Muestra 2
      x2[i] <- paste0("Del segundo grupo, ",
                      dplyr::case_when(ans$distr[i] == "t" ~ " ",
                                       ans$distr[i] == "z" ~ paste0(
                                         "que tenía una desviación de &sigma;<sub>2</sub> =",
                                         Data$sig_2[match(ans$dbID[i], Data$id)],", ")),
                      "se obtuvo una muestra de tamaño n<sub>2</sub> = ",
                      Data$n_2[match(ans$dbID[i], Data$id)],", ",
                      "que tuvo un promedio de X&#772;<sub>2</sub> = ",
                      Data$s_mu_2[match(ans$dbID[i], Data$id)],", ",
                      "y una desviación S<sub>2</sub> = ",
                      Data$s_sig_2[match(ans$dbID[i], Data$id)]," ","\n")

      ## --- Enunciado de Hipotesis
      y1[i] <- paste0("H0: &mu;<sub>1</sub> - &mu;<sub>2</sub> = ",
                      ans$q[i] ," ") #Hipotesis nula
      y2[i] <- paste0("HA: &mu;<sub>1</sub> - &mu;<sub>2</sub> ",
                      dplyr::case_when(
        ans$cola[i]== "<" ~ "<",
        ans$cola[i]== ">" ~ ">",
        ans$cola[i]== "\\ne" ~ "&ne;"),
        " ", ans$q[i] ," ") #Hipotesis alternativa
      ## Enunciado final
      z[i] <- paste0("[", ans$Code[i],"] ",
                     "A partir de dos grupos, se tomaron las siguientes muestras: \n",
                     x1[i],x2[i],
                     "Utilizando un valor de &alpha; = ", ans$alfa[i],
                     ", prueba las siguientes hipotesis: \n ",
                     y1[i]," vs ",y2[i],", y da tu conclusión. \n")
      ans$z[i] <- z[i]
    }

    ####----------------- Formato LaTeX
    else if (format == "LaTeX"){
      ## --- Enunciado de Datos
      #x1[i]: Muestra 1
      x1[i] <- paste0("Del primer grupo, ",
                      dplyr::case_when(ans$distr[i] == "t" ~ " ",
                                       ans$distr[i] == "z" ~ paste0(
                                         "que tenía una desviación de ${{\\sigma}_1} =",
                                         Data$sig_1[match(ans$dbID[i], Data$id)],"$, ")),
                      "se obtuvo una muestra de tamaño ${n_1} = ",
                      Data$n_1[match(ans$dbID[i], Data$id)],"$, ",
                      "que tuvo un promedio de ${{\\overline X}_1} = ",
                      Data$s_mu_1[match(ans$dbID[i], Data$id)],"$, ",
                      "y una desviación ${S_1} = ",
                      Data$s_sig_1[match(ans$dbID[i], Data$id)],"$ ","\n")
      #x2[i]: Muestra 2
      x2[i] <- paste0("Del segundo grupo, ",
                      dplyr::case_when(ans$distr[i] == "t" ~ " ",
                                       ans$distr[i] == "z" ~ paste0(
                                         "que tenía una desviación de ${{\\sigma}_2} =",
                                         Data$sig_2[match(ans$dbID[i], Data$id)],"$, ")),
                      "se obtuvo una muestra de tamaño ${n_2} = ",
                      Data$n_2[match(ans$dbID[i], Data$id)],"$, ",
                      "que tuvo un promedio de ${{\\overline X}_2} = ",
                      Data$s_mu_2[match(ans$dbID[i], Data$id)],"$, ",
                      "y una desviación ${S_2} = ",
                      Data$s_sig_2[match(ans$dbID[i], Data$id)],"$ ","\n")

      ## --- Enunciado de Hipotesis
      y1[i] <- paste0("${H_0}:{\\mu _1} - {\\mu _2} = ",
                      ans$q[i] ,"$ ") #Hipotesis nula
      y2[i] <- paste0("${H_A}:{\\mu _1} - {\\mu _2} "
                      ,ans$cola[i]," ",ans$q[i] ,"$ ") #Hipotesis alternativa
      ## Enunciado final
      z[i] <- paste0("[", ans$Code[i],"] ",
                     "A partir de dos grupos, se tomaron las siguientes muestras: \n",
                     x1[i],x2[i],
                     "Utilizando un valor de $\\alpha = ", ans$alfa[i],
                     "$, prueba las siguientes hipotesis: \n ",
                     y1[i]," vs ",y2[i],", y da tu conclusión. \n")
      ans$z[i] <- z[i]
    }

    ### --- Independientemente del formato, concatenar al archivo.
    cat(z[i])
    cat("\n")
  }

  #sink()
  closeAllConnections()   # .........................
  if(silent == F){
    file.show(outpath)
  }


  ## Traducir al ingles!
  ans <- ans %>% dplyr::mutate(z_Esp = z, z_Eng = z, ##Guarda un respaldo en español, para poder manipular z segun el idioma elegido
                               z_Eng = stringr::str_replace(z_Eng, "A partir de dos grupos, se tomaron las siguientes muestras:","From two populations, the following samples where taken:"),
                               z_Eng = stringr::str_replace(z_Eng, "Del primer grupo","From the first group"),
                               z_Eng = stringr::str_replace_all(z_Eng, "que tenía una desviación de","which had a standard deviation of"),
                               z_Eng = stringr::str_replace_all(z_Eng, "se obtuvo una muestra de tamaño","a sample was taken of size"),
                               z_Eng = stringr::str_replace_all(z_Eng, "que tuvo un promedio de","which had a mean of"),
                               z_Eng = stringr::str_replace_all(z_Eng, "y una desviación","and a standard deviation"),
                               z_Eng = stringr::str_replace(z_Eng, "Del segundo grupo","From the second group"),
                               z_Eng = stringr::str_replace(z_Eng, "Utilizando un valor de","Using an"),
                               z_Eng = stringr::str_replace(z_Eng, "prueba las siguientes hipotesis","test the following hypothesis"),
                               z_Eng = stringr::str_replace(z_Eng, "y da tu conclusión","and give your conclusion")
  )


  exportDB(ans)

  #####--------------- ARCHIVO DE REPORTE ----------------

  ###----- Datos de encabezado
  #wd <- "D:/Documents/ITESM/IBT21/BT2004B/003 - Actividades/Examen/DB/Output/"
  outpath2 <- here::here(wd,paste0(attr(ans, "DBname"),"_RES", ".txt"))

  sink(outpath2)
  #on.exit(sink())
  cat("Inicializando.... Base de datos de Preguntas","\n")
  cat("   id: ", attr(ans, "id_bytes"), "\n")
  cat("   id: ", attr(ans, "id_pokemon"), "\n")
  cat("- Elaborado en: ", attr(ans,"Fecha"), "\n")
  cat("- Utilizando datos de la tabla: ", DBorigin, "\n")
  cat("- Para el tema: ", attr(ans,"Tema"),"\n")
  cat("- Lista de problemas generados: ", attr(ans, "Elementos"), "\n\n\n")

  ## Generacion de reporte para cada pregunta
  for(i in 1:length(ans$Index)){
    ##--- Formato generico,
    rep_Q_PH(Data,ans,i)
  }
  #sink()
  closeAllConnections()   # .........................
  if(silent == F){
    file.show(outpath2)
  }

  return(ans)
}


#### TEMA 3 - Anova de 1 Factor -------------------

## Generador de datos --------------------------------
#' Anova de 1 Factor: Generador de datos muestrales
#'
#' Version 3.6.1
#'
#' @param r Numero de sets de datos a generar
#'
#' @return Una tabla de datos, se guarda en un archivo, y se puede asignar a un objeto
#' @export
#'
#' @examples
gen1Anova <- function(r){ # r = Cuantos problemas a generar
  ###------------ Incializa la Base de Datos
  Data <- tibble::tibble(id = numeric(),
                         data = list(),
                         anova = list(),
                         a = numeric(),
                         n = numeric(),
                         alfa = numeric(),
                         mu = numeric(),
                         sigma = numeric(),
                         Fes = numeric(),
                         Fcr = numeric(),
                         signif = character(),
                         H0 = character(),
                         lsd = list(),
                         lsd_groups = list(),
                         tukey = list(),
                         tukey_groups = list())

  ####---------- Inicializador
  for (j in 1:r) { #r es cuantos SETS a preparar
    n <- round(runif(1,3,5)) #Replicas
    a <- round(runif(1,3,5)) #Tratamientos
    df <- tibble::tibble(i = 1:n) # Añadir indice (replicas) para evitar un error
    X <- numeric()

    mu <- (runif(1,1,100))
    sigma <- (runif(1,1,100))/mu

    ####-------------- Generación de tabla de datos
    for (i in 1:a){
      X <- matrix(abs(rnorm(n,rnorm(1,mu,sigma),rnorm(1,sigma,sigma/mu)))) #Generar columna por columna ##
      df <- df %>% tibble::add_column(X, .name_repair = make.unique) #Añadir la columna al dataframe
    }

    names <- c()
    ###Selecciona un vector para los nombres de los tratamientos segun el numero de tratamientos
    if(a == 3){
      names <- c("A","B","C")
    }else if(a == 4){
      names <- c("A","B","C","D")
    }else if(a == 5){
      names <- c("A","B","C","D","E")}

    df <- df %>% dplyr::mutate_if(is.numeric,round,digits=2) #Redondear numeros, pero no quitar indice

    ####--------------- Genera ANOVA
    df_reorder <- df %>%
      tidyr::gather(key = "X", value = "Y", 2:last_col()) %>% #Junta los datos en una columna
      dplyr::mutate(X = factor(X))

    df <- df %>% tidyr::gather(key = "X", value = "Y", 2:last_col()) %>%
      dplyr::mutate(X = forcats::fct_recode(X,!!!setNames(levels(df_reorder$X), names))) %>% #Cambiar nombre de tratamientos a A,B,C,...
      tidyr::spread(key = "X", value = "Y") %>% dplyr::select(!i) #Vuelve a restructurar la lista de datos, con nuevos nombres de columnas, y quita el indice


    df_lm <- lm(Y~X, df_reorder) ##Modelo lineal para el ANOVA y las PostHoc

    df_Anova <- car::Anova(df_lm) %>%  ##Parsear tabla de ANOVA en un dataframe
      data.frame() %>%
      tibble::rownames_to_column(var = "Fuente") %>%
      dplyr::add_row( Fuente = "Total", Sum.Sq = sum(.$Sum.Sq),
                      Df = sum(.$Df), F.value = NA, Pr..F. = NA) %>%
      dplyr::transmute(Fuente,
                       SS_ = Sum.Sq,
                       Df,
                       MS_ = Sum.Sq/Df,
                       F0 = F.value) %>%
      dplyr::mutate_if(is.numeric,round,digits = 3)

    #####-------- POST HOC!
    ### Fisher
    lsd_groups <- agricolae::LSD.test(df_lm, "X")$groups
    #lsd_mse <- LSD.test(df_lm, "X")$statistics[1] ## MSE
    #lsd_est <- LSD.test(df_lm, "X")$statistics[5] ## T-estadistico
    #lsd_SD <- LSD.test(df_lm, "X")$statistics[6] ## LSD
    lsd_groups <- df_reorder %>%
      dplyr::group_by(X) %>%
      dplyr::summarise(Y = mean(Y)) %>%
      dplyr::left_join(lsd_groups, by = "Y")
    lsd_statistics <- tibble::as.tibble(agricolae::LSD.test(df_lm, "X")$statistics) #Estadisticos generales

    ### Tukey
    tukey_groups <- agricolae::HSD.test(df_lm, "X")$groups
    #tukey_mse <- HSD.test(df_lm, "X")$statistics[1] ## MSE
    #tukey_est <- HSD.test(df_lm, "X")$parameters[4] ## q-estadistico
    #tukey_SD <- HSD.test(df_lm, "X")$statistics[5] ## HSD
    tukey_groups <- df_reorder %>%
      dplyr::group_by(X) %>%
      dplyr::summarise(Y = mean(Y)) %>%
      dplyr::left_join(tukey_groups, by = "Y")
    tukey_statistics <- tibble::as.tibble(agricolae::HSD.test(df_lm, "X")$statistics) %>%
      dplyr::transmute(MSerror, Df, Mean, CV, StudRange = agricolae::HSD.test(df_lm, "X")$parameters[[4]], HSD = MSD) #Estadisticos generales

    #####----------- Construye base de datos
    Data <- Data %>% dplyr::add_row(id = j,
                             data = list(df),
                             anova = list(df_Anova),
                             a = a,
                             n = n,
                             alfa = 0.05,
                             mu = round(mu, 2),
                             sigma = round(sigma, 2),
                             Fes = round(df_Anova[1,5], digits = 3),
                             Fcr = round(qf(alfa,df_Anova[1,3],df_Anova[2,3],lower.tail = F),3),
                             signif = dplyr::case_when(
                               Fes >= Fcr ~ "Signif",
                               Fes < Fcr ~ "No signif"),
                             H0 = dplyr::case_when(
                               Fes >= Fcr ~ "Se rechaza H0",
                               Fes < Fcr ~ "No se rechaza H0"),
                             lsd = list(lsd_statistics),
                             lsd_groups = list(lsd_groups),
                             tukey = list(tukey_statistics),
                             tukey_groups = list(tukey_groups))
  }

  ######------- Metadatos de la base de datos
  Data <- metadataDB(Data, "A1", "data")

  ###--- Actualización del Directorio
  actualizarDirectorio(Data, Directorio)
  exportDB(Data)

  return(Data)
}

## Generador de preguntas ------------------------------
#' Anova de 1 Factor: Generador de preguntas
#'
#' Version 3.8.5 - Se cambian los ´on.exit(sink())´ y ´sink()´ por  ´closeAllConnections()´
#' Version 3.8.4 - usar los paquetes here y folders para los paths relativos
#' Version 3.8.3 - Adaptado para trabajar en un package. Tabla "statement" ahora se guarda dentro del objeto interno "sysdata.rda"
#' Version 3.8.2 - Adaptado para generar enunciados en ingles y español simultaneamente
#'
#' @param Data Tabla de datos muestrales a usar para la generación de preguntas
#' @param Q Numero de pregunas a generar
#' @param format Formato para los enunciados de preguntas. "HTML" o "LaTeX".
#' @param lang Eng o Esp. Idiomas para los enunciados
#' @param silent Si `TRUE`, enonces no se abre el archivo de texto generado
#'
#' @return Una tabla de datos, se guarda en un archivo, y se puede asignar a un objeto
#' @export
#'
#' @examples
question1Anova <- function(Data,Q,format = "HTML", lang = "Esp", silent = T){
  DBorigin <- attr(Data, "DBname")
  ####------- Enunciados

  #statement <- readr::read_delim("ANOVA_Statement.csv", delim ="\t")
  statement <- Anov_statement
  ## Ver 3.8.3 - Se usó usethis::use_data(statement) para guardar la tabla "ANOVA_Statement" en el archivo sysdata.rda ... En teoria, ya no sera necesario importar el archivo.

  #X1: el enunciado, num: el numero de enunciado, Lang: Esp o Eng
  #.. datatype tendra que ser una List, con [[1]] siendo la tabla en español,  [[2]] la tabla en ingles
  ####---------- Tipo de problema
  #a = Genera anova, b = rellena anova, c = mixto
  datatype <- list(
    tibble::tibble(type = c("a","b","c"),  ## [[1]] es Esp
                   typestm = c("la tabla con los datos obtenidos.\n %T% ", "la tabla del ANOVA generada con los datos.\n  %A% ","la tabla con los datos obtenidos.\n %T%")),
    tibble::tibble(type = c("a","b","c"), ## [[2]] es Eng
                   typestm = c("you're presented with the data obtained from the experiment.\n %T% ","you're presented with the an ANOVA table generated with the data obtained from the experiment.\n  %A% ", "you're presented with the data obtained from the experiment.\n %T%")))

  #[1] es Esp, [2] es Eng
  anovT <- c("\n A continuación, se presenta la tabla de ANOVA generada con los datos. \n %A%",
             "\n Below, you're presented with the an ANOVA table generated with the data obteined from the experiment. \n %A%")
  Fconf <- c("\n Para un 95% de confianza, considera un valor de F critico de %C%  \n","\n For a 95% confidence, consider a critical F value of %C%  \n")

  ####---------- Genera tabla de Preguntas
  Ques <- tibble::tibble(
    i = 1:Q,
    id = sample(Data$id,Q, replace = TRUE), #ID de los datos en la base de datos
    #type = sample(datatype$type,Q, replace = TRUE), #ALEATOREAS
    #type = "a", #SOLO PREGUNTAS TIPO A
    #type = "b", #SOLO PREGUNTAS TIPO B
    type = "c", #SOLO PREGUNTAS TIPO C
    #type = rep(c("a", "b"),Q/2), #MISMO NUMERO DE PREGUNTAS DE CADA TIPO
    num = sample(statement$num,Q, replace = TRUE), #Numero de enunciado según el tipo de enunciado. La nueva tabla 2 idiomas no cambia el muestreo.
    Topic = "A",
    Code = stringr::str_c(sprintf("%02d",as.numeric(i)),
                          Topic,
                          sprintf("%02d",as.numeric(num)),
                          type,
                          sprintf("%02d",as.numeric(id)))
  )

  ####---------- ADICIONA STATEMENT Y TABLA
  Ques <- Ques %>% dplyr::mutate(
    statem = statement[statement$Lang == "Esp",]$X1[match(Ques$num, statement$num)],
    statemENG = statement[statement$Lang == "Eng",]$X1[match(Ques$num, statement$num)],
    table = dplyr::case_when(type == "a" ~ Data$data[match(Ques$id, Data$id)], #a = Genera ANOVA con datos
                             type == "b" ~ Data$anova[match(Ques$id, Data$id)],
                             type == "c" ~ Data$data[match(Ques$id, Data$id)]), #c = Rellena tabla ANOVA con datos
    table2 = Data$anova[match(Ques$id, Data$id)],
    z = NA, z_Esp = NA, z_Eng = NA) #b = Rellena tabla ANOVA dada

  ######------------ METADATOS DE LA TABLA
  Ques2 <- metadataDB(Ques,"A1","ques")
  attr(Ques2,"DBorigin") <- DBorigin

  ###--- Actualización del Directorio
  actualizarDirectorio(Ques2, Directorio)

  ######------------ PARSEO DE PREGUNTA
  caption <- tibble::tibble(
    x1 = rep(c("Esp","Eng"), each = 2),
    x2 = c("Tabla de datos","Tabla de ANOVA","Raw data","ANOVA table"),
    x3 = rep(c(1,2), 2))

  #####------------- IMPRIME ENUNCIADOS

  #... Nota: Sobre el cambio de idioma. Los elementos que cambian de idioma son "statement", "datatype", "anovT", "Fconf", "caption"
  # "statement" ahora es una tabla, Lang es una columna. statement[statement$Lang == "Esp",]
  # "datatype" ahora es una lista de tablas. datatype[[1]] es la tabla en Español, datatype[[2]] es la tabla en Ingles.
  # "anovT" y "Fconf" son vectores. [1] es español, [2] es ingles
  # "caption"es una tabla, con x1 como columna representando el idioma. caption[caption$x1 == "Esp",]

  #wd <- "D:/Documents/ITESM/IBT21/BT2004B/003 - Actividades/Examen/DB/Output/"
  wd <- here::here("doetest_out", "reportes")
  outpath <- here::here(wd,paste0(attr(Ques2, "DBname"),".md"))

  z <- c()
  z_Eng <- c()

  sink(outpath)
  #on.exit(sink())
  for (j in 1:length(Ques2$i)){ ## Pregunta  por pregunta

    ###----------------------- ESPAÑOL
    z[j] <- Ques2$statem[j]
    z[j] <- stringr::str_replace(z[j], "([0-9])", paste0("",Ques2$Code[j]))
    z[j] <- stringr::str_replace(z[j], "%D%", datatype[[1]]$typestm[match(Ques2$type[j],datatype[[1]]$type)])
    z[j] <- stringr::str_replace(z[j], "%a%", as.character(Data$a[match(Ques2$id[j],Data$id)]))

    #Generación de tablas
    if (format == "HTML"){
      #### HTML ####
      tabl <- knitr::kable(
        Ques2$table[j], "html",
        caption = dplyr::case_when(
          Ques2$type[j] == "a" ~ caption[caption$x1 == "Esp",][[1,2]],
          Ques2$type[j] == "b" ~ caption[caption$x1 == "Esp",][[2,2]],
          Ques2$type[j] == "c" ~ caption[caption$x1 == "Esp",][[1,2]]),
        padding = 5, table.attr = "class=\"ic-Table ic-Table--condensed ic-Table--striped ic-Table--hover-row\" style=\" width: 400px; \"") %>%
        kableExtra::kable_minimal()
      tablAnov <- knitr::kable(
        Ques2$table2[j], "html",
        caption = caption[caption$x1 == "Esp",][[2,2]], padding = 5, table.attr = "class=\"ic-Table ic-Table--condensed ic-Table--striped ic-Table--hover-row\" style=\" width: 400px; \"") %>%
        kableExtra::kable_minimal()
    }else if(format == "LaTeX"){
      #### LATEX ####
      tabl <- (knitr::kable(
        Ques2$table[j], "latex", booktabs = T, escape = T,
        caption = dplyr::case_when(
          Ques2$type[j] == "a" ~ caption[caption$x1 == "Esp",][[1,2]],
          Ques2$type[j] == "b" ~ caption[caption$x1 == "Esp",][[2,2]],
          Ques2$type[j] == "c" ~ caption[caption$x1 == "Esp",][[1,2]])))
      #kableExtra::kable_minimal()
      tablAnov <- (knitr::kable(
        Ques2$table2[j], "latex", booktabs = T, escape = T, caption = caption[caption$x1 == "Esp",][[2,2]])) #%>% kableExtra::kable_minimal()
    }

    z[j] <- paste0(stringr::str_replace(z[j], "%T%",  ""), tabl, anovT[1])
    z[j] <- paste0(stringr::str_replace(z[j], "%A%",  ""), tablAnov, Fconf[1])
    z[j] <- stringr::str_replace(z[j], "%C%", as.character(Data$Fcr[match(Ques2$id[j],Data$id)]))
    Ques2$z[j] <- z[j]
    Ques2$z_Esp[j] <- z[j]

    ###---------------------------- ENGLISH
    z_Eng[j] <- Ques2$statemENG[j]
    z_Eng[j] <- stringr::str_replace(z_Eng[j], "([0-9])",
                                     paste0("",Ques2$Code[j]))
    z_Eng[j] <- stringr::str_replace(z_Eng[j],  "%D%", datatype[[2]]$typestm[match(Ques2$type[j],datatype[[2]]$type)])
    z_Eng[j] <- stringr::str_replace(z_Eng[j], "%a%", as.character(Data$a[match(Ques2$id[j],Data$id)]))
    #Generación de tablas
    if (format == "HTML"){
      #### HTML ####
      tabl <- knitr::kable(
        Ques2$table[j], "html",
        caption = dplyr::case_when(
          Ques2$type[j] == "a" ~ caption[caption$x1 == "Eng",][[1,2]],
          Ques2$type[j] == "b" ~ caption[caption$x1 == "Eng",][[2,2]],
          Ques2$type[j] == "c" ~ caption[caption$x1 == "Eng",][[1,2]]),
        padding = 5, table.attr = "class=\"ic-Table ic-Table--condensed ic-Table--striped ic-Table--hover-row\" style=\" width: 400px; \"") %>%
        kableExtra::kable_minimal()
      tablAnov <- knitr::kable(
        Ques2$table2[j], "html",
        caption = caption[caption$x1 == "Eng",][[2,2]], padding = 5, table.attr = "class=\"ic-Table ic-Table--condensed ic-Table--striped ic-Table--hover-row\" style=\" width: 400px; \"") %>%
        kableExtra::kable_minimal()
    }else if(format == "LaTeX"){
      #### LATEX ####
      tabl <- (knitr::kable(
        Ques2$table[j], "latex", booktabs = T, escape = T,
        caption = dplyr::case_when(
          Ques2$type[j] == "a" ~ caption[caption$x1 == "Eng",][[1,2]],
          Ques2$type[j] == "b" ~ caption[caption$x1 == "Eng",][[2,2]],
          Ques2$type[j] == "c" ~ caption[caption$x1 == "Eng",][[1,2]])))
      #kableExtra::kable_minimal()
      tablAnov <- (knitr::kable(
        Ques2$table2[j], "latex", booktabs = T, escape = T, caption = caption[caption$x1 == "Eng",][[2,2]])) #%>% kableExtra::kable_minimal()
    }
    z_Eng[j] <- paste0(stringr::str_replace(z_Eng[j],"%T%",""), tabl, anovT[2])
    z_Eng[j] <- paste0(stringr::str_replace(z_Eng[j],"%A%",""), tablAnov, Fconf[2])
    z_Eng[j] <- stringr::str_replace(z_Eng[j], "%C%", as.character(Data$Fcr[match(Ques2$id[j],Data$id)]))

    Ques2$z_Eng[j] <- z_Eng[j]

    cat("\n")
    if (lang == "Esp"){
      cat(z[j])
    }else if(lang == "Eng"){
      cat(z_Eng[j])
    }
    cat("\n")
  }
  #sink()
  closeAllConnections()   # .........................
  if(silent == F){
    file.show(outpath)
  }

  exportDB(Ques2)

  #######----------- REPORTE DE RESULTADOS -----------------######
  ###----- Datos de encabezado
  #wd <- "D:/Documents/ITESM/IBT21/BT2004B/003 - Actividades/Examen/DB/Output/"
  outpath2 <- here::here(wd,paste0(attr(Ques2, "DBname"),"_RES", ".txt"))

  sink(outpath2)
  #on.exit(sink())
  cat("Inicializando.... Base de datos de Preguntas","\n")
  cat("   id: ", attr(Ques2, "id_bytes"), "\n")
  cat("   id: ", attr(Ques2, "id_pokemon"), "\n")
  cat("- Elaborado en: ", attr(Ques2,"Fecha"), "\n")
  cat("- Utilizando datos de la tabla: ", DBorigin, "\n")
  cat("- Para el tema: ", attr(Ques2,"Tema"),"\n")
  cat("- Lista de problemas generados: ", attr(Ques2, "Elementos"), "\n\n\n")

  ## Generacion de reporte para cada pregunta
  for(i in 1:length(Ques2$i)){
    rep_Q_A1(Data,Ques2,i)
  }
  #sink()
  closeAllConnections()   # .........................
  if(silent == F){
    file.show(outpath2)
  }

  return(Ques2)
}


#### TEMA 4 - Anova 1Factor + Bloque -------------------

## Generador de datos ----------------------------
#' Anova 1F + Bloque: Generador de datos muestrales
#'
#' Ver 2.1.3 - La generacion se repite hasta que se genera un set donde el anova con bloque da significativo. 
#' Ver 2.1.2 - Cambios menores
#' Ver 2.1.1 - Cambios menores para importar funciones externas
#' Ver 2.1 - Incluye integracion al directorio.
#  Ver 2.0
#' @param r Numero de sets de datos a generar
#'
#' @return Una tabla de datos, se guarda en un archivo, y se puede asignar a un objeto
#' @export
#'
#' @examples
gen1FBlock <- function(r){
  
  ####-------- Incializa la Base de Datos
  DB <- tibble::tibble(id = numeric(),
                       data = list(),
                       a = numeric(),
                       b = numeric(),
                       alfa = numeric(),
                       mu = numeric(),
                       sigma = numeric(),
                       #Modelo con Bloque
                       anova = list(),
                       Fes = numeric(),
                       Fcr = numeric(),
                       signif = character(),
                       H0 = character(),
                       #Modelo sin Bloque
                       anova_NB = list(),
                       Fes_NB = numeric(),
                       Fcr_NB = numeric(),
                       signif_NB = character(),
                       H0_NB = character(),
                       #Post-Hoc
                       lsd = list(),
                       lsd_groups = list(),
                       tukey = list(),
                       tukey_groups = list())
  
  #### Generador de ANOVAs
  for (L in 1:r) { #10 SETS
    
    repeat { ## Se repite hasta que se vuelve significativo (con el Bloque)
       #n <- round(runif(1,3,5)) #Replicas
      a <- round(runif(1,3,5)) #Tratamientos
      b <- round(runif(1,3,5)) #Bloques
      df <- tibble::tibble(B = 1:b) %>% # Añadir indice (bloques) para evitar un error
        dplyr::mutate(B = stringr::str_c("B",B)) # Nombra a los bloques como tal para evitar confusión
      
      X <- numeric()
      #k <- 3
      mu <- abs(runif(1,1,100))
      sigma <- abs(runif(1,1,100))/(mu)
      
      
      ####-------------- Generación de tabla de datos
      for (i in 1:a){
        X <- matrix(abs(rnorm(b,rnorm(1,mu,sigma),rnorm(1,sigma,sigma/mu)))) #Generar columna por columna ##
        df <- df %>% tibble::add_column(X, .name_repair = make.unique) #Añadir la columna al dataframe
      }
      
      names <- c()
      #-- Selecciona un vector para los nombres de los tratamientos segun el numero de tratamientos
      if(a == 3){
        names <- c("A1","A2","A3")
      }else if(a == 4){
        names <- c("A1","A2","A3","A4")
      }else if(a == 5){
        names <- c("A1","A2","A3","A4","A5")}
      
      df <- df %>% dplyr::mutate_if(is.numeric,round,digits=2) #Redondear numeros, pero no quitar indice
      
      ###------- Genera ANOVA
      df_reorder <- df %>% 
        tidyr::gather(key = "X", value = "Y", 2:last_col()) %>% #Junta los datos en una columna
        dplyr::mutate(X = factor(X))
      
      #df_reorder
      model_B <- lm(Y~X+B,df_reorder) #Genera modelo 1F+B
      #summary(aov(model_B)) #ANOVA con Bloque
      
      model_NB <- lm(Y~X,df_reorder) #Genera modelo 1F sin Bloque
      #summary(aov(model_NB)) #ANOVA sin Bloque
      
      #Formato a la tabla de datos
      df <- df %>% tidyr::gather(key = "X", value = "Y", 2:last_col()) %>% 
        dplyr::mutate(X = forcats::fct_recode(X,!!!setNames(levels(df_reorder$X), names))) %>% #Cambiar nombre de tratamientos a A1,A2,A3
        tidyr::spread(key = "X", value = "Y") #%>% select(!i) #Vuelve a restructurar la lista  
      
      ##Parsear tabla de ANOVA (Bloque) en un dataframe
      df_Anova_B <- car::Anova(model_B) %>%  
        data.frame() %>%
        tibble::rownames_to_column(var = "Fuente") %>%
        dplyr::add_row( Fuente = "Total", Sum.Sq = sum(.$Sum.Sq), 
                        Df = sum(.$Df), F.value = NA, Pr..F. = NA) %>%
        dplyr::transmute(Fuente, 
                         SS_ = Sum.Sq, 
                         Df, 
                         MS_ = Sum.Sq/Df,
                         F0 = F.value,  
                         Pval = Pr..F. ) %>%
        dplyr::mutate_if(is.numeric,round,digits = 3)
      
      ##Parsear tabla de ANOVA(sin Bloque) en un dataframe
      df_Anova_NB <- car::Anova(model_NB) %>%  
        data.frame() %>%
        tibble::rownames_to_column(var = "Fuente") %>%
        dplyr::add_row( Fuente = "Total", Sum.Sq = sum(.$Sum.Sq), 
                 Df = sum(.$Df), F.value = NA, Pr..F. = NA) %>%
        dplyr::transmute(Fuente, 
                         SS_ = Sum.Sq, 
                         Df, 
                         MS_ = Sum.Sq/Df,
                         F0 = F.value,  
                         Pval = Pr..F. ) %>%
        dplyr::mutate_if(is.numeric,round,digits = 3)
      
      #df_Anova_B  #Anova con Bloque
      #model_B
      #df_Anova_NB  #Anova sin Bloque
      #model_NB
      
      #####-------- POST HOC! CON BLOQUE
      ### Fisher
      lsd_groups <- agricolae::LSD.test(model_B, "X")$groups
      #lsd_mse <- LSD.test(df_lm, "X")$statistics[1] ## MSE
      #lsd_est <- LSD.test(df_lm, "X")$statistics[5] ## T-estadistico
      #lsd_SD <- LSD.test(df_lm, "X")$statistics[6] ## LSD
      lsd_groups <- df_reorder %>% dplyr::group_by(X) %>% 
        dplyr::summarise(Y = mean(Y)) %>% 
        dplyr::left_join(lsd_groups, by = "Y")
      lsd_statistics <- tibble::as_tibble(agricolae::LSD.test(model_B, "X")$statistics) #Estadisticos generales
      
      ### Tukey
      tukey_groups <- agricolae::HSD.test(model_B, "X")$groups
      #tukey_mse <- HSD.test(df_lm, "X")$statistics[1] ## MSE
      #tukey_est <- HSD.test(df_lm, "X")$parameters[4] ## q-estadistico
      #tukey_SD <- HSD.test(df_lm, "X")$statistics[5] ## HSD
      tukey_groups <- df_reorder %>% dplyr::group_by(X) %>% 
        dplyr::summarise(Y = mean(Y)) %>% 
        dplyr::left_join(tukey_groups, by = "Y")
      tukey_statistics <- tibble::as_tibble(agricolae::HSD.test(model_B, "X")$statistics) %>% 
        dplyr::transmute(MSerror, Df, Mean, CV, 
                         StudRange = agricolae::HSD.test(model_B, "X")$parameters[[4]], HSD = MSD) #Estadisticos generales
    
      
      alfa <- 0.05
      fes <- round(df_Anova_B[1,5], digits = 3)
      fcr <- round(qf(alfa,df_Anova_B[1,3],df_Anova_B[3,3],
                     lower.tail = F),3) 
      signif <- dplyr::case_when(
        fes >= fcr ~ "Signif",
        fes < fcr ~ "No signif")
      
    ### Rompe el bucle si A resulta significativo en el bloque.
      if(signif == "No signif"){ #No hay significativos
        ##Repetir
      }else{ #Hay significativos
        break
      }
    }## Acaba la repeticion
    
    
    #####----------- Construye base de datos
    DB <- DB %>% dplyr::add_row(
      id = L,
      data = list(df),
      anova = list(df_Anova_B),
      a = a,
      b = b,
      alfa = 0.05,
      mu = round(mu, 2),
      sigma = round(sigma, 2),
      #Con el Bloque
      #Fes = round(df_Anova_B[1,5], digits = 3),
      #Fcr = round(qf(alfa,df_Anova_B[1,3],df_Anova_B[3,3],
      #               lower.tail = F),3), 
      Fes = fes,
      Fcr = fcr,
      #df_Anova_B[3,3] son los gdL del Error
      signif = dplyr::case_when(
        Fes >= Fcr ~ "Signif",
        Fes < Fcr ~ "No signif"),
      H0 = dplyr::case_when(
        Fes >= Fcr ~ "Se rechaza H0",
        Fes < Fcr ~ "No se rechaza H0"),
      #Sin el Bloque
      anova_NB = list(df_Anova_NB),
      Fes_NB = round(df_Anova_NB[1,5], digits = 3),
      Fcr_NB = round(qf(alfa,df_Anova_NB[1,3],df_Anova_NB[2,3],
                        lower.tail = F),3),
      signif_NB = dplyr::case_when(
        Fes_NB >= Fcr_NB ~ "Signif",
        Fes_NB < Fcr_NB ~ "No signif"),
      H0_NB = dplyr::case_when(
        Fes_NB >= Fcr_NB ~ "Se rechaza H0",
        Fes_NB < Fcr_NB ~ "No se rechaza H0"),
      #Post-Hoc
      lsd = list(lsd_statistics),
      lsd_groups = list(lsd_groups),
      tukey = list(tukey_statistics),
      tukey_groups = list(tukey_groups))
  }
  
  ######------- Metadatos de la base de datos
  DB <- metadataDB(DB, "AB", "data")
  
  ###--- Actualización del Directorio
  actualizarDirectorio(DB, Directorio)
  exportDB(DB)
  
  return(DB)
}

## Generador de preguntas ------------------------
#' Anova 1F + Bloque: Generador de Preguntas
#'
#' Ver 2.2.1 - Ajustes menores
#' Ver 2.2 - Integración al Directorio
#' Ver 2.1 - Integracion del idioma con tablas externas
#' Ver 2.0 - Solo HTML
#'
#' @param DB Tabla de datos muestrales a usar para la generación de preguntas
#' @param Q Numero de pregunas a generar
#' @param format Formato para los enunciados de preguntas. "HTML" o "LaTeX".
#' @param lang Eng o Esp. Idiomas para los enunciados
#' @param silent Si `TRUE`, enonces no se abre el archivo de texto generado
#'
#' @return Una tabla de datos, se guarda en un archivo, y se puede asignar a un objeto
#' @export
#'
#' @examples
question1FBlock <- function(DB,Q,format = "HTML",lang = "Esp", silent = T){
  DBorigin <- attr(DB, "DBname")
  ####------- Enunciados
  ## ARMAR TODO EN UNA TABLA EXTERNA!!
  #statement <- case_when(lang == "Esp" ~ read_delim("1FBlock_Statement.txt", delim ="\n", col_names = F),
  #                       lang == "Eng" ~ read_delim("1FBlock_Statement - ENG.txt", delim ="\n", col_names = F)) %>% 
  #  mutate(num = str_sub(X1,2,2))
  
  ####------- Enunciados
  #statement <- readr::read_delim("1FBloque_Statement.csv", delim ="\t") 
  statement <- A1FBloq_statement
  #X1: el enunciado, num: el numero de enunciado, Lang: Esp o Eng
  
  #[1] es Esp, [2] es Eng
  St_datos <- c("\n\n </li> <p> A continuación se presentan la tabla con los datos del exerimento: </p> \n %T% ",
                "\n\n </li> <p> Below, you're presented with the data from the experiment: </p> \n %T% ")
  
  anovT <- c("\n A continuación, se presenta parcialmente la tabla de ANOVA generada con los datos. \n %A%",
             "\n Below, you're presented with a partial ANOVA table generated from the data. \n %A%")
  
  Fconf <- c("\n <p> Para una prueba con 95% de confianza, con un modelo que  incorpora los bloques en el Anova, usa un valor de F critico de %F%  </p>\n",
             "\n <p> For a test with 95% confidence, when using a model that includes the Blocking variable in the ANOVA, consider a critical F value of %F%  </p>\n")
  
  Fconf_NB <- c("<p> Para realizar un analisis a un modelo donde no se consideren los bloques, usa un valor de F critico de %FnB%  </p>\n",
                "<p> For an analysis in which the Blocking variable isn't included, use a  critical F value of %FnB%  </p>\n")
  
  Tukval <- c("<p> Para realizar la prueba de Tukey, considera un valor de la distribución Tukey(q) de %ts%  </p>\n",
              "<p> To perform the Tukey test, consider a Tukey's q-distribution value of %ts%  </p>\n")
  
  
  
  ####----- Incialización: Cuantas preguntas a generar
  #Q <- 10
  #outpath <- paste0(out,".txt")
  
  ####-------- Genera tabla de Preguntas
  Ques <- tibble::tibble(
    i = 1:Q,
    id = sample(DB$id, Q, replace = TRUE),
    num = sample(statement$num,Q, replace = TRUE),
    type = "t", 
    Topic = "B",
    Code = stringr::str_c(sprintf("%02d",as.numeric(i)),
                          Topic,
                          sprintf("%02d",as.numeric(num)),
                          type,
                          sprintf("%02d",as.numeric(id)))
  ) #Indice de Pregunta, Tema, Numero de Enunciado, Tipo, ID del Banco de Datos
  
  ####------- Adiciona statements y datos a la Tabla de Preguntas
  Ques <- Ques %>% dplyr::mutate(
    statem = statement[statement$Lang == "Esp",]$X1[match(Ques$num, statement$num)],
    statemENG = statement[statement$Lang == "Eng",]$X1[match(Ques$num, statement$num)],
    
    statem = stringr::str_c(statem, St_datos[1], anovT[1]),
    statemENG = stringr::str_c(statemENG, St_datos[2], anovT[2]),
    
    table = DB$data[match(Ques$id, DB$id)],
    table2 = DB$anova[match(Ques$id, DB$id)],
    #out = NA) 
    z = NA, z_Esp = NA, z_Eng = NA)
  
  #Ver 2.1 - Reemplazar out por z.
  
  ## Reemplaza strings.
  #DB$data[match(DB$id,Ques$id)]
  #str_replace_all(Ques$statem, "%O%", "\n")
  
  ######------------ METADATOS DE LA TABLA
  Ques <- metadataDB(Ques,"AB","ques")
  attr(Ques,"DBorigin") <- DBorigin
  
  ###--- Actualización del Directorio
  actualizarDirectorio(Ques, Directorio)
  
  ## Ver 2.1 -- Sobre la implementacion del idioma.
  ### "Fconf", "FconfNB", "Tukval" son vectores. [1] es español, [2] es ingles.
  
  ####----------------- IMPRIME ENUNCIADOS 
  wd <- here::here("doetest_out", "reportes")
  outpath <- here::here(wd,paste0(attr(Ques, "DBname"),".txt"))
  
  z <- c()
  z_Eng <- c()
  
  sink(outpath)
  for (j in 1:length(Ques$i)){ ## Pregunta por pregunta
    
    ###----------------------- ESPAÑOL
    z[j] <- Ques$statem[j]
    z[j] <- stringr::str_replace(z[j], "([0-9])", paste0("",Ques$Code[j]))
    z[j] <- stringr::str_replace(z[j], "%a%", as.character(DB$a[match(Ques$id[j],DB$id)])) #Niveles de A
    z[j] <- stringr::str_replace(z[j], "%b%", as.character(DB$a[match(Ques$id[j],DB$id)])) #Niveles de B
    z[j] <- stringr::str_replace_all(z[j], "%V%", "\n  </p> <li>") #Empieza lista de variables
    z[j] <- stringr::str_replace_all(z[j], "%O%", "\n  </li><li>") #Lista de variables
    
    #Generacion de tablas
    if (format == "HTML"){
      ### HTML ###
      tabl <- knitr::kable(
        Ques$table[j], "html", 
        caption = "Tabla de datos",
        padding = 5, 
        table.attr = "class=\"ic-Table ic-Table--condensed ic-Table--striped ic-Table--hover-row\" style=\" width: 400px; \"") %>% 
        kableExtra::kable_minimal()
      tablAnov <- knitr::kable(
        Ques$table2[j], "html", 
        caption =  "Tabla de Anova",
        padding = 5, 
        table.attr = "class=\"ic-Table ic-Table--condensed ic-Table--striped ic-Table--hover-row\" style=\" width: 400px; \"") %>% 
        kableExtra::kable_minimal()
    } #TODO: FALTA LATEX, format == "LaTeX"
    
    z[j] <- stringr::str_replace(z[j], "%T%",  tabl)
    z[j] <- stringr::str_replace(z[j], "%A%",  tablAnov)
    z[j] <- stringr::str_c(z[j], "\n", Fconf[1], "\n",
                           Fconf_NB[1],"\n",Tukval[1])
    z[j] <- stringr::str_replace(z[j], "%F%", as.character(DB$Fcr[match(Ques$id[j],DB$id)] %>% round(digits = 3))) 
    z[j] <- stringr::str_replace(z[j], "%FnB%", as.character(DB$Fcr_NB[match(Ques$id[j],DB$id)] %>% round(digits = 3)))
    z[j] <- stringr::str_replace(z[j], "%ts%", as.character(DB$tukey[[match(Ques$id[j],DB$id)]][5] %>% round(digits = 3)))
    
    Ques$z[j] <- z[j] 
    Ques$z_Esp[j] <- z[j] 
    
    ###----------------------- INGLES
    z_Eng[j] <- Ques$statemENG[j]
    z_Eng[j] <- stringr::str_replace(z_Eng[j], "([0-9])", 
                                     paste0("",Ques$Code[j]))
    z_Eng[j] <- stringr::str_replace(z_Eng[j], "%a%", as.character(DB$a[match(Ques$id[j],DB$id)])) #Niveles de A
    z_Eng[j] <- stringr::str_replace(z_Eng[j], "%b%", as.character(DB$a[match(Ques$id[j],DB$id)])) #Niveles de B
    z_Eng[j] <- stringr::str_replace_all(z_Eng[j], "%V%", "\n  </p> <li>") #Empieza lista de variables
    z_Eng[j] <- stringr::str_replace_all(z_Eng[j], "%O%", "\n  </li><li>") #Lista de variables
    
    #Generacion de tablas
    if (format == "HTML"){
      ### HTML ###
      tabl <- knitr::kable(
        Ques$table[j], "html", 
        caption = "Dataset",
        padding = 5, 
        table.attr = "class=\"ic-Table ic-Table--condensed ic-Table--striped ic-Table--hover-row\" style=\" width: 400px; \"") %>% 
        kableExtra::kable_minimal()
      tablAnov <- knitr::kable(
        Ques$table2[j], "html", 
        caption = "ANOVA table", 
        padding = 5, 
        table.attr = "class=\"ic-Table ic-Table--condensed ic-Table--striped ic-Table--hover-row\" style=\" width: 400px; \"") %>% 
        kableExtra::kable_minimal()
    } #TODO: format == "LaTeX"
    
    z_Eng[j] <- stringr::str_replace(z_Eng[j], "%T%",  tabl)
    z_Eng[j] <- stringr::str_replace(z_Eng[j], "%A%",  tablAnov)
    z_Eng[j] <- stringr::str_c(z_Eng[j], "\n", Fconf[2], "\n", 
                               Fconf_NB[2],"\n",Tukval[2])
    z_Eng[j] <- stringr::str_replace(z_Eng[j],"%F%", as.character(DB$Fcr[match(Ques$id[j],DB$id)] %>% round(digits = 3)))
    z_Eng[j] <- stringr::str_replace(z_Eng[j],"%FnB%", as.character(DB$Fcr_NB[match(Ques$id[j],DB$id)] %>% round(digits = 3)))
    z_Eng[j] <- stringr::str_replace(z_Eng[j],"%ts%", as.character(DB$tukey[[match(Ques$id[j],DB$id)]][5] %>% round(digits = 3)))
    
    Ques$z_Eng[j] <- z_Eng[j]     
    
    cat("\n")
    cat("\n <p> ")
    cat("\n")
    if (lang == "Esp"){
      cat(z[j])
    }else if(lang == "Eng"){
      cat(z_Eng[j])
    }
    cat("\n")
  }
  #sink()
  closeAllConnections()   # .........................
  if(silent == F){
    file.show(outpath)
  }
  
  exportDB(Ques)
  
  
  #######----------- REPORTE DE RESULTADOS -----------------######
  ###----- Datos de encabezado
  #wd <- "D:/Documents/ITESM/IBT21/BT2004B/003 - Actividades/Examen/DB/Output/"
  outpath2 <- here::here(wd,paste0(attr(Ques, "DBname"),"_RES", ".txt"))
  
  sink(outpath2)
  #on.exit(sink())
  cat("Inicializando.... Base de datos de Preguntas","\n")
  cat("   id: ", attr(Ques, "id_bytes"), "\n")
  cat("   id: ", attr(Ques, "id_pokemon"), "\n")
  cat("- Elaborado en: ", attr(Ques,"Fecha"), "\n")
  cat("- Utilizando datos de la tabla: ", DBorigin, "\n")
  cat("- Para el tema: ", attr(Ques,"Tema"),"\n")
  cat("- Lista de problemas generados: ",attr(Ques, "Elementos"),"\n\n\n")
  
  ## Generacion de reporte para cada pregunta
  for(i in 1:length(Ques$i)){
    rep_Q_AB(DB,Ques,i)
  }
  #sink()
  closeAllConnections()   # .........................
  if(silent == F){
    file.show(outpath2)
  }
  
  return(Ques)
}


#### TEMA 5 - Diseño 2^k -------------------

## Generador de datos ----------------------------
#' Diseño 2^k: Generador de datos muestrales
#'
#' Ver 2.1.1 - Ajustes menores
#' Ver 2.1.0 - Integración del directorio
#' Ver 2.0.0 - Inicial
#'
#' @param r  Numero de sets de datos a generar
#'
#' @return Una tabla de datos, se guarda en un archivo, y se puede asignar a un objeto
#' @export
#'
#' @examples
gen2k <- function(r){
  
  ####-------- Incializa la Base de Datos
  DB <- tibble::tibble(id = numeric(),
                       data = list(),
                       anova = list(),
                       k = numeric(),
                       n = numeric(),
                       alfa = numeric(),
                       mu = numeric(),
                       sigma = numeric(),
                       Fcr = numeric(),
                       sign = list(),
                       CoEf = list(),
                       EqR = character(),
                       Fit = list(),
                       Fit_Min = list(),
                       Fit_Max = list()
  )
  
  
  #### Generador de ANOVAs
  for (L in 1:r) { #10 SETS
    
    repeat{
      n <- sample(c(2,2,3,3,3,4),1) #Replicas
      #k <- sample(c(3,3,3,3,4,4,5),1) #Factores
      k <- 3
      mu <- abs(runif(1,1,100))
      sigma <- abs(runif(1,1,100))/(mu)
      
      names <- c()
      #Selecciona un vector para los nombres de los tratamientos segun el numero de tratamientos
      if(k == 3){
        names <- c("A","B","C")
      }else if(k == 4){
        names <- c("A","B","C","D")
      }else if(k == 5){
        names <- c("A","B","C","D","E")}
      
      ###------- Genera Datos
      df <- AlgDesign::gen.factorial(2,k, varNames = names) 
      Y0 <- matrix(abs(rnorm(2^k,mu,sigma))) # Primera columna se genera en aleatoriamente
      Y0 <- round(Y0, digits = 2) #Redondear a dos digitos
      Y <- Y0
      df <- df %>% tibble::add_column(Y, .name_repair = make.unique)   
      for (i in 1:(n-1)){ #Se añadiran nuevas filas según el número de replicas
        Y <- numeric()
        for (j in 1:length(Y0)){
          Y[j] <- abs(rnorm(1,Y0[j],sd(Y0))) #Tratamientos son coherentes entre replicas
        } 
        Y <- matrix(round(Y, digits = 2)) #Redondear
        df <- df %>% tibble::add_column(Y, .name_repair = make.unique) #Añadir las columnas de replicas generadas
      }
      #df
      
      
      ###------- Genera ANOVA
      df_reorder <- df %>% tidyr::gather(key="rep", value = "Y", Y:last_col()) %>% dplyr::select(-rep) #Reordena replicas en una columna
      model <- lm(Y~(.)^6,df_reorder) #Genera modelo (0rden^5 para considerar todas las interacciones)
      
      
      df_Anova <- car::Anova(model) %>%  ##Parsear tabla de ANOVA en un dataframe
        data.frame() %>%
        tibble::rownames_to_column(var = "Fuente") %>%
        dplyr::add_row( Fuente = "Total", Sum.Sq = sum(.$Sum.Sq), 
                        Df = sum(.$Df), F.value = NA, Pr..F. = NA) %>%
        dplyr::transmute(Fuente, 
                         SS_ = Sum.Sq, 
                         Df, 
                         MS_ = Sum.Sq/Df,
                         F0 = F.value,  #)%>%
                         Pval = Pr..F. ) %>%
        dplyr::mutate_if(is.numeric,round,digits = 3)
      
      
      ###-------- Modelo de Regresion
      df_AnovaT <- df_Anova %>% dplyr::slice(1:(length(df_Anova$Df)-2)) #ANOVA Sin las ultimas 2 filas (Error y Total)
      Fcr <- qf(0.05,df1 = 1, df2 =((n-1)*(2^k)),lower.tail=F) #F critico. Mismo para todos los factores 2^k (mismo Df)
      
      df_Sig <- df_AnovaT$Fuente[df_AnovaT$F0 > Fcr] #Los significativos, según F vs Fcr. Un vector de tipo chr
      
      df_CoEf <- as.data.frame((coefficients(model)))  %>% 
        tibble::rownames_to_column("Factor") %>% 
        dplyr::transmute(Factor,Coef = (coefficients(model)), Eff = Coef*2)   #Guarda Coeficientes y Efectos en un DF
      df_CoEf <- df_CoEf %>% dplyr::mutate_if(is.numeric, round,digits=3) %>% 
        dplyr::filter_all(dplyr::any_vars(. %in% c("(Intercept)",df_Sig))) #Filtra para solo quedarse con los significativos
      #df_CoEf #Dataframe - extracto de los coeficientes, solo considerando los significativos + Interseccion
      
      # (!) Expresión para arrojar la ecuación de regresión
      df_Eq <- stringr::str_c("y = ", df_CoEf[1,2])
      for (i in 2:length(df_CoEf$Factor)){
        df_Eq <- stringr::str_c(df_Eq," + ",as.character(df_CoEf[i,2]), " ", df_CoEf[i,1])
      }
      
      if(length(df_Sig) == 0){ #No hay significativos
        ##Repetir
      }else{ #Hay significativos
        break
      }
    } ### Si NO hay significativos, repetir la generacio´n...
    
    ###------- Predicción
    #?reformulate()
    ## AGUAS!!! reformulate se muere si NO encuentra significativos!!
        ## Repetir...?
    model_abr <- lm(stats::reformulate(df_Sig, "Y"), data = df_reorder) #Generar modelo solo con los significativos
    df_fit <- df_reorder %>% dplyr::mutate(Y_fit = fitted(model_abr)) #Guarda las predicciones en un dataframe
    
    #Predicción: Cual es el máximo?
    df_fit_Max <- df_fit%>% 
      dplyr::arrange(desc(Y_fit)) %>% 
      dplyr::slice_head(n=1)
    #df_fit_Max
    
    #Predicción: Cual es el mínimo?
    df_fit_Min <- df_reorder %>% 
      dplyr::mutate(Y_fit = fitted(model_abr)) %>% 
      dplyr::arrange(Y_fit) %>% 
      dplyr::slice_head(n=1)
    #df_fit_Min
    
    
    #####----------- Construye base de datos
    DB <- DB %>% dplyr::add_row(id = L,
                                data = list(df),
                                anova = list(df_Anova),
                                k = k,
                                n = n,
                                alfa = 0.05,
                                mu = round(mu, 2),
                                sigma = round(sigma, 2),
                                Fcr = round(Fcr, 3),
                                sign = list(df_Sig),
                                CoEf = list(df_CoEf),
                                EqR = df_Eq,
                                Fit = list(df_fit),
                                Fit_Min = list(df_fit_Max),
                                Fit_Max = list(df_fit_Min)
    )
    
  }
  
  ######------- Metadatos de la base de datos
  DB <- metadataDB(DB, "2k", "data")
  
  ###--- Actualización del Directorio
  actualizarDirectorio(DB, Directorio)
  exportDB(DB)
  
  return(DB)
}

## Generador de preguntas ------------------------
#' Diseño 2^k: Generador de preguntas
#' 
#' Ver 2.2.2 - Se incluye la tabla de anova en el enunciado - se debe eliminar todo menos el SST
#' Ver 2.2.1 - Cambios menores
#' Ver 2.2.0 - Integración del directorio
#' Ver 2.1.0 - Incorporacion de idioma
#' Ver 2.0.0 - Inicial
#'
#' @param DB Tabla de datos muestrales a usar para la generación de preguntas
#' @param Q Numero de pregunas a generar
#' @param format Formato para los enunciados de preguntas. "HTML" o "LaTeX".
#' @param lang Eng o Esp. Idiomas para los enunciados
#' @param silent Si `TRUE`, enonces no se abre el archivo de texto generado
#'
#' @return  Una tabla de datos, se guarda en un archivo, y se puede asignar a un objeto
#' @export
#'
#' @examples
question2k <- function(DB,Q, format = "HTML", lang = "Esp", silent = T){
  DBorigin <- attr(DB, "DBname")
  ####------- Enunciados
  #statement <- readr::read_delim("2k_Statement.csv", delim ="\t") 
  statement <- D2k_statement
  #X1: el enunciado, num: el numero de enunciado, Lang: Esp o Eng
  
  #[1] es Esp, [2] es Eng
  St_datos <- c("\n\n </li> <p> A continuación se presentan la matriz de diseño (en unidades codificadas) y los resultados de %n% réplicas: </p> \n %T% ",
                "\n\n </li> <p> Below, the design matrix for the experiment (in codified units) and the results of %n% replicates: </p> \n %T% ")
  Fconf <- c("\n <p> Para un 95% de confianza, considera un valor de F critico de %F%  </p>\n",
             "\n <p> For a test with 95% confidence, consider a critical F value of %F%  </p>\n")
  anovT <- c("\n A continuación, se presenta parcialmente la tabla de ANOVA generada con los datos. \n %A%",
             "\n Below, you're presented with a partial ANOVA table generated from the data. \n %A%")
  
  
  ####----- Incialización: Cuantas preguntas a generar
  #Q <- 10
  wd <- here::here("doetest_out", "reportes")
  
  ####-------- Genera tabla de Preguntas
  Ques <- tibble::tibble(
    i = 1:Q,
    id = sample(DB$id, Q, replace = TRUE),
    num = sample(statement$num,Q, replace = TRUE),
    type = "t", 
    Topic = "k",
    Code = stringr::str_c(sprintf("%02d",as.numeric(i)),
                          Topic,
                          sprintf("%02d",as.numeric(num)),
                          type,
                          sprintf("%02d",as.numeric(id)))
  )
  
  ####------- Adiciona statements y datos a la Tabla de Preguntas
  Ques <- Ques %>% dplyr::mutate(
    statem = statement[statement$Lang == "Esp",]$X1[match(Ques$num, statement$num)],
    statemENG = statement[statement$Lang == "Eng",]$X1[match(Ques$num, statement$num)],
    
    statem = stringr::str_c(statem, St_datos[1], anovT[1]),
    statemENG = stringr::str_c(statemENG, St_datos[2], anovT[2]),
    
    table = DB$data[match(Ques$id, DB$id)],
    table2 = DB$anova[match(Ques$id, DB$id)],
    z = NA, z_Esp = NA, z_Eng = NA)
  
  #Ques$tabl
  
  ######------------ METADATOS DE LA TABLA
  Ques <- metadataDB(Ques,"2k","ques")
  attr(Ques,"DBorigin") <- DBorigin
  
  ###--- Actualización del Directorio
  actualizarDirectorio(Ques, Directorio)
  
  ## Reemplaza strings.
  #DB$data[match(DB$id,Ques$id)]
  #str_replace_all(Ques$statem, "%O%", "\n")
  
  z <- c()
  z_Eng <- c()
  
  wd <- here::here("doetest_out", "reportes")
  outpath <- here::here(wd,paste0(attr(Ques, "DBname"),".txt"))
  ####----------------- IMPRIME ENUNCIADOS 
  sink(outpath)
  for (j in 1:length(Ques$i)){
    
    ###----------------------- ESPAÑOL
    z[j] <- Ques$statem[j]
    z[j] <- stringr::str_replace(z[j], "([0-9])", paste0("",Ques$Code[j]))
    z[j] <- stringr::str_replace_all(z[j], "%V%", "\n  </p> <li>")
    z[j] <- stringr::str_replace_all(z[j], "%O%", "\n  </li><li>")
    z[j] <- stringr::str_replace(z[j], "%n%", as.character(DB$n[match(Ques$id[j],DB$id)]))
    
    #Generación de tablas
    if (format == "HTML"){
      #### HTML ####
      tabl <- knitr::kable(Ques$table[j], "html", 
                           caption = "Tabla de datos",
                           padding = 5, 
                           table.attr = "class=\"ic-Table ic-Table--condensed ic-Table--striped ic-Table--hover-row\" style=\" width: 400px; \"") %>% 
        kableExtra::kable_minimal()
      
      tablAnov <- knitr::kable(
        Ques$table2[j], "html", 
        caption =  "Tabla de Anova",
        padding = 5, 
        table.attr = "class=\"ic-Table ic-Table--condensed ic-Table--striped ic-Table--hover-row\" style=\" width: 400px; \"") %>% 
        kableExtra::kable_minimal()
      
    } #TODO: format == "LaTeX"
    
    z[j] <- stringr::str_replace(z[j], "%A%",  tablAnov)
    z[j] <- stringr::str_c(z[j], "\n", Fconf[1]) %>%
      stringr::str_replace("%T%",  tabl) %>%
      stringr::str_replace("%F%", as.character(DB$Fcr[match(Ques$id[j],DB$id)] %>% round(digits = 3)))
    
    Ques$z[j] <- z[j] 
    Ques$z_Esp[j] <- z[j] 
    
    ###----------------------- INGLES
    z_Eng[j] <- Ques$statemENG[j]
    z_Eng[j] <- stringr::str_replace(z_Eng[j], "([0-9])", paste0("",Ques$Code[j]))
    z_Eng[j] <- stringr::str_replace_all(z_Eng[j], "%V%", "\n  </p> <li>")
    z_Eng[j] <- stringr::str_replace_all(z_Eng[j], "%O%", "\n  </li><li>")
    z_Eng[j] <- stringr::str_replace(z_Eng[j], "%n%", as.character(DB$n[match(Ques$id[j],DB$id)]))
    
    #Generación de tablas
    if (format == "HTML"){
      #### HTML ####
      tabl <- knitr::kable(Ques$table[j], "html", 
                           caption = "Dataset",
                           padding = 5, 
                           table.attr = "class=\"ic-Table ic-Table--condensed ic-Table--striped ic-Table--hover-row\" style=\" width: 400px; \"") %>% 
        kableExtra::kable_minimal()
      
      tablAnov <- knitr::kable(
        Ques$table2[j], "html", 
        caption =  "Anova Table",
        padding = 5, 
        table.attr = "class=\"ic-Table ic-Table--condensed ic-Table--striped ic-Table--hover-row\" style=\" width: 400px; \"") %>% 
        kableExtra::kable_minimal()
    } #TODO: format == "LaTeX"
    
    z_Eng[j] <- stringr::str_replace(z_Eng[j], "%A%",  tablAnov)
    z_Eng[j] <- stringr::str_c(z_Eng[j], "\n", Fconf[2]) %>%
      stringr::str_replace("%T%",  tabl) %>%
      stringr::str_replace("%F%", as.character(DB$Fcr[match(Ques$id[j],DB$id)] %>% round(digits = 3)))

    Ques$z_Eng[j] <- z_Eng[j] 
    
    
    #out[j] <- str_replace(out[j], "%F%", as.character(DB$Fcr[match(Ques$id[j],DB$id)] %>% round(digits = 3)))
    cat("\n")
    cat("\n <p> ")
    if (lang == "Esp"){
      cat(z[j])
    }else if(lang == "Eng"){
      cat(z_Eng[j])
    }
    cat("\n")
    cat("\n")
    #cat(str_replace(Fconf, "%F%", as.character(DB$Fcr[match(Ques$id[j]
  }
  #sink()
  closeAllConnections()   # .........................
  if(silent == F){
    file.show(outpath)
  }
  
  exportDB(Ques)
  
  
  #######----------- REPORTE DE RESULTADOS -----------------######
  ###----- Datos de encabezado
  #wd <- "D:/Documents/ITESM/IBT21/BT2004B/003 - Actividades/Examen/DB/Output/"
  outpath2 <- here::here(wd,paste0(attr(Ques, "DBname"),"_RES", ".txt"))
  
  sink(outpath2)
  #on.exit(sink())
  cat("Inicializando.... Base de datos de Preguntas","\n")
  cat("   id: ", attr(Ques, "id_bytes"), "\n")
  cat("   id: ", attr(Ques, "id_pokemon"), "\n")
  cat("- Elaborado en: ", attr(Ques,"Fecha"), "\n")
  cat("- Utilizando datos de la tabla: ", DBorigin, "\n")
  cat("- Para el tema: ", attr(Ques,"Tema"),"\n")
  cat("- Lista de problemas generados: ",attr(Ques, "Elementos"),"\n\n\n")
  
  ## Generacion de reporte para cada pregunta
  for(i in 1:length(Ques$i)){
    rep_Q_2k(DB,Ques,i)
  }
  #sink()
  closeAllConnections()   # .........................
  if(silent == F){
    file.show(outpath2)
  }
  
  return(Ques)
}


#' Diseño 2^k: Generador de preguntas con enfasis en Regresión Lineal
#' 
#' Ver 2.2.1 - Cambios menores
#' Ver 2.2.0 - Integración del directorio
#' Ver 2.1.0 - Incorporacion de idioma
#' Ver 2.0.0 - Inicial
#'
#' @param DB Tabla de datos muestrales a usar para la generación de preguntas
#' @param Q Numero de pregunas a generar
#' @param format Formato para los enunciados de preguntas. "HTML" o "LaTeX".
#' @param lang Eng o Esp. Idiomas para los enunciados
#' @param silent Si `TRUE`, enonces no se abre el archivo de texto generado
#'
#' @return  Una tabla de datos, se guarda en un archivo, y se puede asignar a un objeto
#' @export
#'
#' @examples
question2k_reg <- function(DB,Q, format = "HTML", lang = "Esp", silent = T){
  DBorigin <- attr(DB, "DBname")
  ####------- Enunciados
  #statement <- readr::read_delim("2k_Statement.csv", delim ="\t") 
  statement <- D2k_statement
  #X1: el enunciado, num: el numero de enunciado, Lang: Esp o Eng
  
  #[1] es Esp, [2] es Eng
  St_datos <- c("\n\n </li> <p> A continuación se presentan la matriz de diseño (en unidades codificadas) y los resultados de %n% réplicas: </p> \n %T% ",
                "\n\n </li> <p> Below, the design matrix for the experiment (in codified units) and the results of %n% replicates: </p> \n %T% ")
  Fconf <- c("\n <p> Para un 95% de confianza, considera un valor de F critico de %F%  </p>\n",
             "\n <p> For a test with 95% confidence, consider a critical F value of %F%  </p>\n")
  RegrEq <- c("\n <p> A partir de estos datos, se obtuvo la siguiente ecuación de regresión abreviada (en unidades codificadas): </p> %R% ",
              "\n <p> From this data, the following abreviated regression model (in codified units) was generated: </p> %R% ")
  
  
  ####----- Incialización: Cuantas preguntas a generar
  #Q <- 10
  #outpath <- paste0(out,".txt")
  
  #DB_name <- deparse(substitute(DB))
  ####-------- Genera tabla de Preguntas
  Ques <- tibble::tibble(
    i = 1:Q,
    id = sample(DB$id, Q, replace = TRUE),
    num = sample(statement$num,Q, replace = TRUE),
    type = "r", 
    Topic = "k",
    Code = stringr::str_c(sprintf("%02d",as.numeric(i)),
                          Topic,
                          sprintf("%02d",as.numeric(num)),
                          type,
                          sprintf("%02d",as.numeric(id)))
  )
  
  ####------- Adiciona statements y datos a la Tabla de Preguntas
  Ques <- Ques %>% dplyr::mutate(
    statem = statement[statement$Lang == "Esp",]$X1[match(Ques$num, statement$num)],
    statemENG = statement[statement$Lang == "Eng",]$X1[match(Ques$num, statement$num)],
    statem = stringr::str_c(statem, St_datos[1]),
    statemENG = stringr::str_c(statemENG, St_datos[2]),
    table = DB$data[match(Ques$id, DB$id)],
    z = NA, z_Esp = NA, z_Eng = NA)
  
  #Ques$table
  
  ######------------ METADATOS DE LA TABLA
  Ques <- metadataDB(Ques,"2k","ques")
  attr(Ques,"DBorigin") <- DBorigin
  
  ###--- Actualización del Directorio
  actualizarDirectorio(Ques, Directorio)
  
  ## Reemplaza strings.
  #DB$data[match(DB$id,Ques$id)]
  #str_replace_all(Ques$statem, "%O%", "\n")
  
  z <- c()
  z_Eng <- c()
  
  wd <- here::here("doetest_out", "reportes")
  outpath <- here::here(wd,paste0(attr(Ques, "DBname"),".txt"))
  ####----------------- IMPRIME ENUNCIADOS 
  sink(outpath)
  for (j in 1:length(Ques$i)){
    
    ##-------------------- ESPAÑOL
    z[j] <- Ques$statem[j]
    z[j] <- stringr::str_replace(z[j],"([0-9])",paste0("",Ques$Code[j]))
    z[j] <- stringr::str_replace_all(z[j], "%V%", "\n  </p> <li>")
    z[j] <- stringr::str_replace_all(z[j], "%O%", "\n  </li><li>")
    z[j] <- stringr::str_replace(z[j], "%n%", as.character(DB$n[match(Ques$id[j],DB$id)]))
    
    #Generación de tablas
    if (format == "HTML"){
      #### HTML ####
      tabl <- knitr::kable(Ques$table[j], "html", 
                           caption = "Tabla de datos",
                           padding = 5, 
                           table.attr = "class=\"ic-Table ic-Table--condensed ic-Table--striped ic-Table--hover-row\" style=\" width: 400px; \"") %>% 
        kableExtra::kable_minimal()
    } #TODO: Incluir format == "LaTeX
    
    z[j] <- stringr::str_replace(z[j], "%T%",  tabl)
    #out[j] <- str_replace(out[j], "%F%", as.character(DB$Fcr[match(Ques$id[j],DB$id)] %>% round(digits = 3)))
    
    ##-------------------- INGLES
    z_Eng[j] <- Ques$statemENG[j]
    z_Eng[j] <- stringr::str_replace(z_Eng[j], "([0-9])", paste0("",Ques$Code[j]))
    z_Eng[j] <- stringr::str_replace_all(z_Eng[j], "%V%", "\n  </p> <li>")
    z_Eng[j] <- stringr::str_replace_all(z_Eng[j], "%O%", "\n  </li><li>")
    z_Eng[j] <- stringr::str_replace(z_Eng[j], "%n%", as.character(DB$n[match(Ques$id[j],DB$id)]))
    
    #Generación de tablas
    if (format == "HTML"){
      #### HTML ####
      tabl <- knitr::kable(Ques$table[j], "html", 
                           caption = "Dataset",
                           padding = 5, 
                           table.attr = "class=\"ic-Table ic-Table--condensed ic-Table--striped ic-Table--hover-row\" style=\" width: 400px; \"") %>% 
        kableExtra::kable_minimal()
    } #TODO format == "LaTeX"
    z_Eng[j] <- stringr::str_replace(z_Eng[j], "%T%",  tabl)
    #out[j] <- str_replace(out[j], "%F%", as.character(DB$Fcr[match(Ques$id[j],DB$id)] %>% round(digits = 3)))
    cat("\n")
    
    ##--------------- Adición de la Eq de Regresion
    Reg <- stringr::str_c(as.character(DB$EqR[match(Ques$id[j],DB$id)]))
    if(format == "HTML"){
      Reg <- stringr::str_replace(Reg, "y", "y&#770;")
    }else if(format == "LaTeX"){
      Reg <- stringr::str_replace(Reg, "y", "\\hat{y}")
    }
    
    #str_replace(RegrEq, "%R%", Reg)
    z[j] <- stringr::str_c(z[j], "\n" ,RegrEq[1])
    z[j] <- stringr::str_replace(z[j], "%R%", Reg)
    z_Eng[j] <- stringr::str_c(z_Eng[j], "\n" ,RegrEq[2])
    z_Eng[j] <- stringr::str_replace(z_Eng[j], "%R%", Reg)
    
    Ques$z[j] <- z[j] 
    Ques$z_Esp[j] <- z[j] 
    Ques$z_Eng[j] <- z_Eng[j] 
    
    #cat("\n")
    #cat(str_replace(Fconf, "%F%", as.character(DB$Fcr[match(Ques$id[j],DB$id)] %>% round(digits = 3))))
    
    cat("\n <p> ")
    if (lang == "Esp"){
      cat(z[j])
    }else if(lang == "Eng"){
      cat(z_Eng[j])
    }
    cat("\n")
    
    cat("\n")
    cat("")
  }
  closeAllConnections()   # .........................
  if(silent == F){
    file.show(outpath)
  }
  
  exportDB(Ques)
  
  
  #######----------- REPORTE DE RESULTADOS -----------------######
  ###----- Datos de encabezado
  #wd <- "D:/Documents/ITESM/IBT21/BT2004B/003 - Actividades/Examen/DB/Output/"
  outpath2 <- here::here(wd,paste0(attr(Ques, "DBname"),"_RES", ".txt"))
  
  sink(outpath2)
  #on.exit(sink())
  cat("Inicializando.... Base de datos de Preguntas","\n")
  cat("   id: ", attr(Ques, "id_bytes"), "\n")
  cat("   id: ", attr(Ques, "id_pokemon"), "\n")
  cat("- Elaborado en: ", attr(Ques,"Fecha"), "\n")
  cat("- Utilizando datos de la tabla: ", DBorigin, "\n")
  cat("- Para el tema: ", attr(Ques,"Tema"),"\n")
  cat("- Lista de problemas generados: ",attr(Ques, "Elementos"),"\n\n\n")
  
  ## Generacion de reporte para cada pregunta
  for(i in 1:length(Ques$i)){
    rep_Q_2k(DB,Ques,i)
  }
  #sink()
  closeAllConnections()   # .........................
  if(silent == F){
    file.show(outpath2)
  }
  
  
  return(Ques)
}