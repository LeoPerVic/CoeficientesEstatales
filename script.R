
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

# Dividir las columnas de `sec_nac` por los totales de `tot_nac`

denominador <- sec_nac

# Aplicar la división para cada columna correspondiente

denominador$ue <- sec_nac$ue / tot_nac$ue
denominador$re <- sec_nac$re / tot_nac$re
denominador$pb <- sec_nac$pb / tot_nac$pb
denominador$va <- sec_nac$va / tot_nac$va
denominador$fb <- sec_nac$fb / tot_nac$fb
denominador$af <- sec_nac$af / tot_nac$af
denominador$po <- sec_nac$po / tot_nac$po

# Resultado final QL

# Unir numerador y denominador por cve_sec

QL <- left_join(numerador, denominador, by = c("cve_sec"))

# Dividir cada variable de subsec_mun_div entre la variable correspondiente de subsec_tot_zm_div

QL <- QL %>% 
  mutate(QLue = ue.x/ue.y,
         QLre = re.x/re.y,
         QLpb = pb.x/pb.y,
         QLva = va.x/va.y,
         QLfb = fb.x/fb.y,
         QLaf = af.x/af.y,
         QLpo = po.x/po.y) %>% 
  select(-ue.x, -ue.y, -re.x, -re.y, -pb.x, -pb.y, -va.x, -va.y, -fb.x, -fb.y, -af.x, -af.y, -po.x, -po.y)

## Coeficiente Hirschman-Herfindahl (HH)

# Cociente 1

Cociente_1 <- sec_ent %>% 
  left_join(sec_nac, by = c("cve_sec")) %>% 
  mutate(ue = ue.x/ue.y,
         re = re.x/re.y,
         pb = pb.x/pb.y,
         va = va.x/va.y,
         fb = fb.x/fb.y,
         af = af.x/af.y,
         po = po.x/po.y) %>% 
  select(-ue.x, -ue.y, -re.x, -re.y, -pb.x, -pb.y, -va.x, -va.y, -fb.x, -fb.y, -af.x, -af.y, -po.x, -po.y)

# Cociente 2

# Dividir las columnas de `tot_ent` por los totales de `tot_nac`

Cociente_2 <- tot_ent

# Aplicar la división para cada columna correspondiente

Cociente_2$ue <- tot_ent$ue / tot_nac$ue
Cociente_2$re <- tot_ent$re / tot_nac$re
Cociente_2$pb <- tot_ent$pb / tot_nac$pb
Cociente_2$va <- tot_ent$va / tot_nac$va
Cociente_2$fb <- tot_ent$fb / tot_nac$fb
Cociente_2$af <- tot_ent$af / tot_nac$af
Cociente_2$po <- tot_ent$po / tot_nac$po

# Estimar HH

HH <- Cociente_1 %>% 
  left_join(Cociente_2, by = c("cve_ent")) %>% 
  mutate(HHue = ue.x-ue.y,
         HHre = re.x-re.y,
         HHpb = pb.x-pb.y,
         HHva = va.x-va.y,
         HHfb = fb.x-fb.y,
         HHaf = af.x-af.y,
         HHpo = po.x-po.y) %>% 
  select(-ue.x, -ue.y, -re.x, -re.y, -pb.x, -pb.y, -va.x, -va.y, -fb.x, -fb.y, -af.x, -af.y, -po.x, -po.y)



