% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/examen.R
\name{ensamblarExamen}
\alias{ensamblarExamen}
\title{Ensamblador del examen}
\usage{
ensamblarExamen(Data = NULL, format = "HTML", lang = "Esp")
}
\arguments{
\item{Data}{Una tabla de Examen. Si no se proporciona, se pregunta por un id para buscar en el Directorio}

\item{format}{Formato para el archivo de texto de salida. HTML o LaTeX}

\item{lang}{Idioma para los enunciados. "Eng" o "Esp"}
}
\value{
Un archivo de salida, .txt, con el formato especificado
}
\description{
TODO: Ajustar para cuando se hagan ejercicios de practica (input: modo = "exam" o "prac")
Ver 2.2.0 - Se incorporan el framework para trabajar con los temas B y K
Ver 2.1.0 - Se ajusta la implementación del idioma
Ver 2.0.0 - Se separa de la función genExam() para facilitar el manejo de tablas de examen, y dar mas flexibilidad
}
