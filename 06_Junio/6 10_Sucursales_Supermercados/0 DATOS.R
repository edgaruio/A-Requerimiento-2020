# Librerias
library(dplyr); library(RODBC); library(data.table); library(readxl); library(scales)
library(tools)

# ********************************************************************************
# Infraestructura ----

channel <- odbcDriverConnect(
  "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//bogak08beimrodc/bi/Contacto/Infraestructura/Infraestructura.accdb"
)
sqlTables(channel)

infra <- sqlQuery(channel, paste ("select * from COLSUBISIDIO_INFRAESTRUCTURA_PROPIA"), as.is=T) %>% 
  data.frame()
odbcCloseAll() 
str(infra)
table(infra$TIPO)

super <- infra %>% 
  filter(TIPO == "SUPERMERCADO") %>% 
  filter(COD %in% c("S011","S037","S008","S098","S115","S118","S047","S061","S030","S119")) %>% 
  select(NOMBRE, CX, CY, BARRIO, LOCALIDAD)

# Para cosulta en geo_afiliados
fwrite(super %>% select(NOMBRE, CX, CY), "Resultados/Super_obj.csv", sep = ";", dec = ".")

# ***************************************************************************************
### SALIDAS V1 ====

consolidada <- readRDS("//Bogak08beimrodc/bi/Base_Mes/ConsolidadosMensuales/ConsolidacionMAY2020.rds") %>% 
  data.frame() %>% 
  mutate(Genero = toupper(as.character(Genero))) %>% 
  filter(marca_afiliado_unico) %>% 
  select(id_persona, NumIdPersona, Genero, Edad, Salario, Categoria, Segmento_poblacional, 
         id_empresa, RazonSocial, nombre_empresa_principal, Piramide1, Piramide2) %>% 
  mutate(NumIdPersona = as.character(NumIdPersona)) 
str(consolidada)
table(consolidada$Genero)

compras_afil <- read_excel("Datos/Clientes_10_Sucursales_trx_Compras.xlsx") %>% 
  data.frame() %>% 
  mutate(NumIdPersona = substr(ID.Cliente, start = 4, stop = nchar(ID.Cliente))) %>% 
  select(NumIdPersona) %>% 
  distinct()
str(compras_afil)
table(duplicated(compras_afil$NumIdPersona))

afil_obj <- fread("Resultados/Afiliados_cercanos.csv") %>% 
  select(-c(Dis_v, Dis_t, Punto, NOMBRE, Segmento_poblacional)) %>% 
  distinct() %>% 
  mutate(Genero = toupper(as.character(Genero))) %>% 
  mutate(nid_persona = gsub("\\D","", id_persona)) %>% 
  left_join(consolidada %>% select(id_persona, Segmento_poblacional, 
                                   id_empresa, RazonSocial, nombre_empresa_principal, Piramide1, Piramide2), 
            by = "id_persona") %>% 
  mutate(dian = ifelse(id_empresa == "NIT8001972684", "Si", "No"),
         compra_super = ifelse(nid_persona %in% compras_afil$NumIdPersona, "Si", "No"))
str(afil_obj)
table(afil_obj$Genero)
sum(afil_obj$dian == "Si")

# CC52916074
afil_obj_super_v <- fread("Resultados/Afiliados_cercanos.csv", sep = ";", dec = ",") %>% 
  select(id_persona, Dis_v, NOMBRE) %>% 
  mutate(NOMBRE = gsub("SUPERMERCADO ", "", NOMBRE, fixed = T)) %>% 
  arrange(id_persona, Dis_v) %>% 
  group_by(id_persona) %>% 
  filter(row_number()==1) %>% 
  ungroup() %>% 
  data.frame() %>% 
  left_join(afil_obj %>% select(id_persona, dian, compra_super), by = "id_persona")
str(afil_obj_super_v)

afil_obj_super_t <- fread("Resultados/Afiliados_cercanos.csv", sep = ";", dec = ",") %>% 
  select(id_persona, Dis_t, NOMBRE) %>% 
  mutate(NOMBRE = gsub("SUPERMERCADO ", "", NOMBRE, fixed = T)) %>% 
  arrange(id_persona, Dis_t) %>% 
  group_by(id_persona) %>% 
  filter(row_number()==1) %>% 
  ungroup() %>% 
  data.frame() %>% 
  left_join(afil_obj %>% select(id_persona, dian, compra_super), by = "id_persona")
str(afil_obj_super_t)

afil_obj_super_v %>% 
  group_by(NOMBRE) %>% 
  summarise(Conteo = n()) %>% 
ggplot(.) +
  aes(x = reorder(NOMBRE, Conteo), weight = Conteo) +
  geom_bar(fill = "#2171b5") +
  geom_text(stat='count', aes(label=comma(..count..)), vjust=0.5, hjust= 1, color = "white") +
  labs(x = "SUPERMERCADO", y = "AFILIADOS", title = "Distribución Afiliados", subtitle = "Afiliados que viven cerca") +
  coord_flip() +
  theme_ft_rc() +
  scale_y_continuous(labels = comma)

afil_obj_super_t %>% 
  group_by(NOMBRE) %>% 
  summarise(Conteo = n()) %>% 
  ggplot(.) +
  aes(x = reorder(NOMBRE, Conteo), weight = Conteo) +
  geom_bar(fill = "#2171b5") +
  geom_text(stat='count', aes(label=comma(..count..)), vjust=0.5, hjust= 1, color = "white") +
  labs(x = "SUPERMERCADO", y = "AFILIADOS", title = "Distribución Afiliados", subtitle = "Afiliados que trabajan cerca") +
  coord_flip() +
  theme_ft_rc() +
  scale_y_continuous(labels = comma)


afil_obj %>% 
  filter(dian == "Si") %>% 
  group_by(Categoria) %>% 
  summarise(conteo = n())

tb0 <- afil_obj %>% 
  group_by(Genero, compra_super) %>% 
  summarise(Conteo = n()) %>% 
  mutate(Porcentaje = round(100*(Conteo/sum(Conteo)), 1))
tb0

ggplot(tb0) +
  aes(x = Genero, fill = compra_super, weight = Porcentaje) +
  geom_bar() +
  scale_fill_brewer(palette = "Paired") +
  labs(x = "Genero", y = "Porcentaje", title = "Participación Afiliados", subtitle = "Compra Supermercados", 
       fill = "Compra") +
  coord_flip() +
  theme_ft_rc() + 
  geom_text(data=tb0, aes(x = Genero, y = Porcentaje, label = paste0(Porcentaje,"%")), size=4,
            color = "white", position = position_stack(vjust = .5))



tb1 <- afil_obj %>% 
  group_by(dian, compra_super) %>% 
  summarise(Conteo = n()) %>% 
  mutate(Porcentaje = round(100*(Conteo/sum(Conteo)), 1))
tb1

ggplot(tb1) +
  aes(x = dian, fill = compra_super, weight = Porcentaje) +
  geom_bar() +
  scale_fill_brewer(palette = "Paired") +
  labs(x = "Afiliados DIAN", y = "Porcentaje", title = "Participación Afiliados DIAN", subtitle = "Compra Supermercados", 
       fill = "Compra") +
  coord_flip() +
  theme_ft_rc() + 
  geom_text(data=tb1, aes(x = dian, y = Porcentaje, label = paste0(Porcentaje,"%")), size=4,
            color = "white", position = position_stack(vjust = .5))


tb2_aux <- afil_obj %>% 
  filter(Piramide1 == "1 Emp Grandes") %>% 
  group_by(nombre_empresa_principal, compra_super) %>% 
  summarise(Conteo = n()) %>% 
  mutate(Porcentaje = round(100*(Conteo/sum(Conteo)), 1)) %>% 
  ungroup() %>% 
  mutate(flag = ifelse(compra_super == "Si" & Porcentaje >= 20, 1, 0)) %>% 
  filter(flag == 1) %>% 
  arrange(desc(Porcentaje)) %>% 
  select(nombre_empresa_principal)
tb2_aux

tb2 <- afil_obj %>% 
  filter(Piramide1 == "1 Emp Grandes") %>% 
  filter(nombre_empresa_principal %in% tb2_aux$nombre_empresa_principal) %>% 
  group_by(nombre_empresa_principal, compra_super) %>% 
  summarise(Conteo = n()) %>% 
  mutate(Porcentaje = round(100*(Conteo/sum(Conteo)), 1)) %>% 
  ungroup() %>% 
  arrange(desc(Porcentaje))
tb2

ggplot(tb2) +
  aes(x = nombre_empresa_principal, fill = compra_super, weight = Porcentaje) +
  geom_bar() +
  scale_fill_brewer(palette = "Paired") +
  labs(x = "Afiliados", y = "Porcentaje", title = "Participación Afiliados Emp. Grandes", subtitle = "Compra Supermercados", 
       fill = "Compra") +
  coord_flip() +
  theme_ft_rc() + 
  geom_text(data=tb2, aes(x = nombre_empresa_principal, y = Porcentaje, label = paste0(Porcentaje,"%")), size=4,
            color = "white", position = position_stack(vjust = .5))

fwrite(afil_obj, "Resultados/Base_consolidada.csv", sep = ";", dec = ",")

### SALIDAS V2 ====

tb3_aux <- consolidada %>% 
  filter(Piramide1 == "1 Emp Grandes") %>% 
  mutate(dentro_radio = ifelse(id_persona %in% afil_obj$id_persona, "Si", "No")) %>% 
  group_by(nombre_empresa_principal, dentro_radio) %>% 
  summarise(Conteo = n()) %>% 
  mutate(Porcentaje = round(100*(Conteo/sum(Conteo)), 1)) %>% 
  ungroup() %>% 
  mutate(flag = ifelse(dentro_radio == "Si" & Porcentaje >= 70, 1, 0)) %>% 
  filter(flag == 1) %>%
  arrange(desc(Conteo)) %>% 
  select(nombre_empresa_principal) %>%
  data.frame()
tb3_aux

tb3 <- consolidada %>% 
  filter(Piramide1 == "1 Emp Grandes") %>% 
  mutate(dentro_radio = ifelse(id_persona %in% afil_obj$id_persona, "Si", "No")) %>% 
  filter(nombre_empresa_principal %in% tb3_aux$nombre_empresa_principal) %>% 
  group_by(nombre_empresa_principal, dentro_radio) %>% 
  summarise(Conteo = n()) %>% 
  mutate(Porcentaje = round(100*(Conteo/sum(Conteo)), 1)) %>% 
  ungroup() %>% 
  arrange(desc(Conteo))
tb3

ggplot(tb3) +
  aes(x = nombre_empresa_principal, fill = dentro_radio, weight = Porcentaje) +
  geom_bar() +
  scale_fill_brewer(palette = "Paired") +
  labs(x = "EMPRESAS", y = "Porcentaje", title = "Cercanía Afiliados a Supermercados", 
       subtitle = "Empresas Grandes", 
       fill = "Afiliados cercanos") +
  coord_flip() +
  theme_ft_rc() + 
  geom_text(data=tb3, aes(x = nombre_empresa_principal, y = Porcentaje, label = paste0(Porcentaje,"%")), size=4,
            color = "white", position = position_stack(vjust = .5))

sum(afil_obj$compra_super=="Si")
