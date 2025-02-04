
# Cargar librerías

library(readxl)
library(dplyr)

# Leer datos desde un archivo xlsx

datos <- read_excel("SAIC2003.xlsx")

# Crear vector sec_ent

sec_ent <- datos %>% group_by(cve_ent, cve_sec) %>% summarize(ue = sum(ue, na.rm = TRUE), 
                                                              re = sum(re, na.rm = TRUE),
                                                              pb = sum(as.numeric(pb), na.rm = TRUE), 
                                                              va = sum(as.numeric(va), na.rm = TRUE),
                                                              fb = sum(as.numeric(fb), na.rm = TRUE),
                                                              af = sum(as.numeric(af), na.rm = TRUE),
                                                              po = sum(as.numeric(po), na.rm = TRUE))


# Crear vector tot_ent

tot_ent <- datos %>% group_by(cve_ent) %>% summarize(ue = sum(ue, na.rm = TRUE), 
                                                                     re = sum(re, na.rm = TRUE),
                                                                     pb = sum(as.numeric(pb), na.rm = TRUE), 
                                                                     va = sum(as.numeric(va), na.rm = TRUE),
                                                                     fb = sum(as.numeric(fb), na.rm = TRUE),
                                                                     af = sum(as.numeric(af), na.rm = TRUE),
                                                                     po = sum(as.numeric(po), na.rm = TRUE))