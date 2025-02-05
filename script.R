
# Cargar librerías

library(readxl)
library(dplyr)

# Leer datos desde un archivo xlsx

datos <- read_excel("SAIC2003.xlsx")

# Crear vector sec_ent

sec_ent <- datos %>% group_by(cve_ent, cve_sec, nom_ent) %>% summarize(ue = sum(ue, na.rm = TRUE), 
                                                              re = sum(re, na.rm = TRUE),
                                                              pb = sum(as.numeric(pb), na.rm = TRUE), 
                                                              va = sum(as.numeric(va), na.rm = TRUE),
                                                              fb = sum(as.numeric(fb), na.rm = TRUE),
                                                              af = sum(as.numeric(af), na.rm = TRUE),
                                                              po = sum(as.numeric(po), na.rm = TRUE))


# Crear vector tot_ent

tot_ent <- datos %>% group_by(cve_ent, nom_ent) %>% summarize(ue = sum(ue, na.rm = TRUE), 
                                                                     re = sum(re, na.rm = TRUE),
                                                                     pb = sum(as.numeric(pb), na.rm = TRUE), 
                                                                     va = sum(as.numeric(va), na.rm = TRUE),
                                                                     fb = sum(as.numeric(fb), na.rm = TRUE),
                                                                     af = sum(as.numeric(af), na.rm = TRUE),
                                                                     po = sum(as.numeric(po), na.rm = TRUE))

# Crear vector sec_nac

sec_nac <- datos %>% group_by(cve_sec) %>% summarize(ue = sum(ue, na.rm = TRUE), 
                                                                       re = sum(re, na.rm = TRUE),
                                                                       pb = sum(as.numeric(pb), na.rm = TRUE), 
                                                                       va = sum(as.numeric(va), na.rm = TRUE),
                                                                       fb = sum(as.numeric(fb), na.rm = TRUE),
                                                                       af = sum(as.numeric(af), na.rm = TRUE),
                                                                       po = sum(as.numeric(po), na.rm = TRUE))

# Crear vector tot_nac

tot_nac <- sec_nac %>% summarize(ue = sum(ue, na.rm = TRUE), 
                                                     re = sum(re, na.rm = TRUE),
                                                     pb = sum(as.numeric(pb), na.rm = TRUE), 
                                                     va = sum(as.numeric(va), na.rm = TRUE),
                                                     fb = sum(as.numeric(fb), na.rm = TRUE),
                                                     af = sum(as.numeric(af), na.rm = TRUE),
                                                     po = sum(as.numeric(po), na.rm = TRUE))


## Coeficiente de localización económica (QL)

# Numerador

numerador <- left_join(sec_ent, tot_ent, by = c("cve_ent" = "cve_ent",  "nom_ent" = "nom_ent")) %>% 
  mutate(ue = ue.x/ue.y,
         re = re.x/re.y,
         pb = pb.x/pb.y,
         va = va.x/va.y,
         fb = fb.x/fb.y,
         af = af.x/af.y,
         po = po.x/po.y) %>% 
  select(-ue.x, -ue.y, -re.x, -re.y, -pb.x, -pb.y, -va.x, -va.y, -fb.x, -fb.y, -af.x, -af.y, -po.x, -po.y)

# Denominador

# Primero, si tot_nac es un único registro, lo convertimos a un vector de totales
tot_nac_vector <- tot_nac[1, ]

# Ahora, aplicamos la división en cada columna de sec_nac por el total correspondiente
denominador <- as.data.frame(mapply(`/`, sec_nac[, -1], tot_nac_vector))

# Si es necesario conservar la primera columna original (por ejemplo, un identificador), añadimos esa columna de nuevo
denominador <- cbind(sec_nac[, 1], denominador)

# Cambiamos los nombres de las columnas

colnames(denominador) <- colnames(sec_nac)

# Resultado final QL

# Unir numerador y denominador por cve_sec

QL <- left_join(numerador, denominador, by = c("cve_sec"))

View(QL)

# Dividir cada variable de subsec_mun_div entre la variable correspondiente de subsec_tot_zm_div

QL <- QL %>% 
  mutate(ue = ue.x/ue.y,
         re = re.x/re.y,
         pb = pb.x/pb.y,
         va = va.x/va.y,
         fb = fb.x/fb.y,
         af = af.x/af.y,
         po = po.x/po.y) %>% 
  select(-ue.x, -ue.y, -re.x, -re.y, -pb.x, -pb.y, -va.x, -va.y, -fb.x, -fb.y, -af.x, -af.y, -po.x, -po.y)



