#library(devtools)


#load_all() NO CORRER EN EL SCRIPT!!

##Vaciar Directorio
#unlink(here::here("doetest_out", "tablas"), recursive = T)
#unlink(here::here("doetest_out", "reportes"), recursive = T)
#folders::cleanup_folders(here::here("doetest_out", c("tablas", "reportes")))

#Examen1 <- doeExam()

#### TODO: Si se trata de actualizar el directorio, o importar una tabla, Y ESTA VACIO, marcara error.
#### Especialmente cuando se intenta armar un directorio nuevo.
#### Ver la forma de generar una carpeta temporal.

#Directorio

#Exam <- Examen1
#Examen2 <- doeExam()

## No se generó correctamente  en ingles... PERO! Excelente opción para probar el ensamblador por si solo

#Examen2
#ensamblarExamen(Examen2, "HTML", "Eng")





############
#Armado de sysdata.rda
#wd0 <- "D:/Documents/ITESM/IBT21/BT2004B/003 - Actividades/Examen/"

## De: question1Anova() y similares:
## Ver 3.8.3 - Se usó usethis::use_data(statement) para guardar la tabla "ANOVA_Statement" en el archivo sysdata.rda ... En teoria, ya no sera necesario importar el archivo.
#Anov_statement <- readr::read_delim(paste0(wd0,"ANOVA_Statement.csv"), delim ="\t")
#A1FBloq_statement <- readr::read_delim(paste0(wd0,"1FBloque_Statement.csv"), delim ="\t")
#D2k_statement <- readr::read_delim(paste0(wd0,"2k_Statement.csv"), delim ="\t")

## De: ensamblarExamen() y similares:
###---------- SETUP: Enunciados de ANOVAS
## Ver 2.6.6 (genExamen) - Anov_quest0 y MComp_statement ya estan precargados en sysdata.rda
#Anov_quest0 <- readr::read_delim(paste0(wd0,"ANOVA_EnunProb.csv"),delim ="\t")
#Anov_quest <- Anov_quest0 %>% dplyr::filter(Lang == lang)
#A1FBloq_quest0 <- readr::read_delim(paste0(wd0,"1FBloque_EnunProb.csv"),delim ="\t")
#D2k_quest0 <- readr::read_delim(paste0(wd0,"2k_EnunProb.csv"),delim ="\t")

#MComp_statement0 <- readr::read_delim(paste0(wd0,"ANOVA_MultiComp.csv"),delim ="\t")
#MComp_statement <- MComp_statement0 %>% dplyr::filter(Lang == lang)
#[1] es Fisher, [2] es Tukey

##Instrucciones HTML
## Ver 2.6.6 - Anov_quest0 y MComp_statement ya estan precargados en sysdata.rda
#Exam_INST0 <- readr::read_delim(paste0(wd0,"Examen_INST.csv"),delim = "\t")

#here::here()

#usethis::use_data(Anov_statement,Anov_quest0,
#                  A1FBloq_statement,A1FBloq_quest0,
#                  D2k_statement, D2k_quest0,
#                  MComp_statement0,Exam_INST0, 
#                  overwrite = TRUE, internal = TRUE)

###########






#cli::cli_alert_info("
#∩――--------―-∩
# ||     ∧ ﾍ　 ||
# ||    (* ´ ｰ`) ZZzz
# |ﾉ^⌒⌒づ`￣   ＼
# (　ノ　　⌒ ヽ   ＼
#  ＼　　||￣￣￣￣￣||
# 　 ＼,ﾉ|         ||
#                    ")

#check()


