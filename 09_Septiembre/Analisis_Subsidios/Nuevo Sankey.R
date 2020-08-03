# Librerias y Funciones. ----
rm(list = ls(all=T))
source("Analisis_Subsidios/Funciones.r")
pqts <- c("data.table", "tidyverse", "tictoc", "networkD3", "readxl", "scales", "htmlwidgets", "rbokeh")
Loadpkg(pqts)


# Total Subsidios ----

TipoSubsidio<-"Total"

tmp1 <- readRDS("Analisis_Subsidios/test_sub_info.rds") %>% 
  mutate(Persona_1_Genero=case_when(Persona_1_Genero %in% c("m", "M") ~ "Masculino",
                                   Persona_1_Genero == "F" ~ "Femenino")) %>% 
  filter(marca_afiliado_unico=="X")

tmp2 <- tmp1 %>% 
  filter(marca_afiliado_unico=="X", parentesco != "CONYUGUE") %>%
  group_by(id_persona) %>% 
  summarise(N_Beneficiarios=n_distinct(id_persona_familiar)) %>% 
  mutate(N_Beneficiarios=case_when(N_Beneficiarios == 1 ~ "1",
                                   N_Beneficiarios == 2 ~ "2",
                                   N_Beneficiarios >= 3 ~ "3 o más"))

data <- tmp1 %>% left_join(tmp2, by = "id_persona") %>% 
  arrange(categoria, Persona_1_Edad) %>% 
  filter(bono_girado=="Si" | kit_girada=="Si" |cm_girada=="Si") %>% 
  mutate(
    GrupoEdad=case_when(Persona_1_Edad < 18 ~ paste(Persona_1_Edad, "año(s)"),
                        Persona_1_Edad >=18 & Persona_1_Edad <25 ~ "18 a 25 años",
                        Persona_1_Edad >=25 & Persona_1_Edad <60 ~ "25 a 60 años",
                        Persona_1_Edad >=25 & Persona_1_Edad <60 ~ "25 a 60 años",
                        Persona_1_Edad >=60 ~ "Mayor a 60")
  ) %>% 
  group_by(Categoria=categoria, Edad=GrupoEdad, Genero=Persona_1_Genero, N_Beneficiarios) %>% 
  summarise(ValorSubsidio=n()) %>% 
  ungroup() %>% 
  filter(!is.na(N_Beneficiarios), !is.na(Genero))

rm(tmp1, tmp2)

aux1 <-  bind_rows(
  data %>% 
    mutate(Inicio="", Fin=TipoSubsidio) %>% 
    group_by(Inicio, Fin) %>% 
    summarise(ValorSubsidio=sum(ValorSubsidio)) %>% 
    select(Inicio=Inicio, Fin=Fin, ValorSubsidio),
  data %>% 
    mutate(Inicio=TipoSubsidio) %>% 
    group_by(Inicio,Categoria) %>% 
    summarise(ValorSubsidio=sum(ValorSubsidio)) %>% 
    select(Inicio=Inicio, Fin=Categoria, ValorSubsidio),
  data %>% 
    group_by(Categoria, N_Beneficiarios) %>% 
    summarise(ValorSubsidio=sum(ValorSubsidio)) %>% 
    select(Inicio=Categoria, Fin=N_Beneficiarios, ValorSubsidio),
  data %>% 
    group_by(N_Beneficiarios, Genero) %>% 
    summarise(ValorSubsidio=sum(ValorSubsidio)) %>% 
    select(Inicio=N_Beneficiarios, Fin=Genero, ValorSubsidio),
  data %>% 
    group_by(Genero, Edad) %>% 
    summarise(ValorSubsidio=sum(ValorSubsidio)) %>% 
    select(Inicio=Genero, Fin=Edad, ValorSubsidio)
)

n_seq <- length(c(unique(data$Categoria), unique(data$N_Beneficiarios), unique(data$Genero), unique(data$Edad)))
Nodes <- bind_cols(node=seq(0,n_seq), 
                   bind_rows(
                     data %>% mutate(name=TipoSubsidio) %>% select(name) %>% distinct(),
                     data %>% select(name=Categoria) %>% distinct(),
                     data %>% select(name=N_Beneficiarios) %>% distinct() %>% mutate(name=str_sort(name, numeric = TRUE)) ,
                     data %>% select(name=Genero) %>% distinct(),
                     data %>% select(name=Edad) %>% distinct() %>% mutate(name=str_sort(name, numeric = TRUE)))
)

tot <- aux1 %>% filter(Inicio==TipoSubsidio) %>% summarise(Valor=sum(ValorSubsidio)) %>% select(Valor) %>% as.numeric()
aux2 <- aux1 %>%
  left_join(Nodes, by=c("Inicio"="name")) %>% 
  left_join(Nodes, by=c("Fin"="name")) %>% 
  ungroup() %>% 
  group_by(Fin) %>% 
  summarise(ValorSubsidio=sum(ValorSubsidio, na.rm = T)) %>% ungroup() %>% 
  mutate(Pct=ValorSubsidio/tot) %>% 
  arrange(Fin)

Nodes2 <- Nodes %>% left_join(aux2, by=c("name"="Fin"))  %>% 
  mutate(Name=paste0(name, " ",comma(ValorSubsidio), " (", percent(Pct), ")"))


Links <- aux1 %>%
  filter(Inicio!="") %>% 
  left_join(Nodes, by=c("Inicio"="name")) %>% 
  left_join(Nodes, by=c("Fin"="name")) %>% 
  ungroup() %>% 
  select(source=node.x, target=node.y, value=ValorSubsidio)
colnames(Links) <- c("source", "target", "value")

sn <- networkD3::sankeyNetwork(Links = Links, Nodes = Nodes2, 
                         Source = 'source', 
                         Target = 'target', 
                         Value = 'value', 
                         NodeID = 'Name',
                         units = 'Subsidio',
                         iterations = 0,
                         fontSize=14,
                         nodeWidth = 50,
                         sinksRight = T
                         )
onRender(
  sn,
  '
  function(el, x) {
    d3.selectAll(".node text").attr("text-anchor", "begin").attr("x", 20)
    .attr("font-weight",function(d,i) {return 900;});
  }
  '
  )

fwrite(data, file = "Analisis_Subsidios/data_total.csv", row.names = F)




# Cuota Monetaria ----

TipoSubsidio<-"Cuota Monetaria"

tmp1 <- readRDS("Analisis_Subsidios/test_sub_info.rds") %>% 
  mutate(Persona_1_Genero=case_when(Persona_1_Genero %in% c("m", "M") ~ "Masculino",
                                    Persona_1_Genero == "F" ~ "Femenino")) %>% 
  filter(marca_afiliado_unico=="X")

tmp2 <- tmp1 %>% 
  filter(marca_afiliado_unico=="X", parentesco != "CONYUGUE") %>%
  group_by(id_persona) %>% 
  summarise(N_Beneficiarios=n_distinct(id_persona_familiar)) %>% 
  mutate(N_Beneficiarios=case_when(N_Beneficiarios == 1 ~ "1",
                                   N_Beneficiarios == 2 ~ "2",
                                   N_Beneficiarios >= 3 ~ "3 o más"))

data <- tmp1 %>% left_join(tmp2, by = "id_persona") %>% 
  arrange(categoria, Persona_1_Edad) %>% 
  filter(parentesco!="CONYUGUE", cm_girada=="Si") %>% 
  mutate(
    GrupoEdad=case_when(Persona_1_Edad < 18 ~ paste(Persona_1_Edad, "año(s)"),
                        Persona_1_Edad >=18 & Persona_1_Edad <25 ~ "18 a 25 años",
                        Persona_1_Edad >=25 & Persona_1_Edad <60 ~ "25 a 60 años",
                        Persona_1_Edad >=25 & Persona_1_Edad <60 ~ "25 a 60 años",
                        Persona_1_Edad >=60 ~ "Mayor a 60")
  ) %>% 
  group_by(Categoria=categoria, Edad=GrupoEdad, Genero=Persona_1_Genero, N_Beneficiarios) %>% 
  summarise(ValorSubsidio=n()) %>% 
  ungroup() %>% 
  filter(!is.na(N_Beneficiarios), !is.na(Genero))

rm(tmp1, tmp2)

aux1 <-  bind_rows(
  data %>% 
    mutate(Inicio="", Fin=TipoSubsidio) %>% 
    group_by(Inicio, Fin) %>% 
    summarise(ValorSubsidio=sum(ValorSubsidio)) %>% 
    select(Inicio=Inicio, Fin=Fin, ValorSubsidio),
  data %>% 
    mutate(Inicio=TipoSubsidio) %>% 
    group_by(Inicio,Categoria) %>% 
    summarise(ValorSubsidio=sum(ValorSubsidio)) %>% 
    select(Inicio=Inicio, Fin=Categoria, ValorSubsidio),
  data %>% 
    group_by(Categoria, N_Beneficiarios) %>% 
    summarise(ValorSubsidio=sum(ValorSubsidio)) %>% 
    select(Inicio=Categoria, Fin=N_Beneficiarios, ValorSubsidio),
  data %>% 
    group_by(N_Beneficiarios, Genero) %>% 
    summarise(ValorSubsidio=sum(ValorSubsidio)) %>% 
    select(Inicio=N_Beneficiarios, Fin=Genero, ValorSubsidio),
  data %>% 
    group_by(Genero, Edad) %>% 
    summarise(ValorSubsidio=sum(ValorSubsidio)) %>% 
    select(Inicio=Genero, Fin=Edad, ValorSubsidio)
)

n_seq <- length(c(unique(data$Categoria), unique(data$N_Beneficiarios), unique(data$Genero), unique(data$Edad)))
Nodes <- bind_cols(node=seq(0,n_seq), 
                   bind_rows(
                     data %>% mutate(name=TipoSubsidio) %>% select(name) %>% distinct(),
                     data %>% select(name=Categoria) %>% distinct(),
                     data %>% select(name=N_Beneficiarios) %>% distinct() %>% mutate(name=str_sort(name, numeric = TRUE)) ,
                     data %>% select(name=Genero) %>% distinct(),
                     data %>% select(name=Edad) %>% distinct() %>% mutate(name=str_sort(name, numeric = TRUE)))
)

tot <- aux1 %>% filter(Inicio==TipoSubsidio) %>% summarise(Valor=sum(ValorSubsidio)) %>% select(Valor) %>% as.numeric()
aux2 <- aux1 %>%
  left_join(Nodes, by=c("Inicio"="name")) %>% 
  left_join(Nodes, by=c("Fin"="name")) %>% 
  ungroup() %>% 
  group_by(Fin) %>% 
  summarise(ValorSubsidio=sum(ValorSubsidio, na.rm = T)) %>% ungroup() %>% 
  mutate(Pct=ValorSubsidio/tot) %>% 
  arrange(Fin)

Nodes2 <- Nodes %>% left_join(aux2, by=c("name"="Fin"))  %>% 
  mutate(Name=paste0(name, " ",comma(ValorSubsidio), " (", percent(Pct), ")"))


Links <- aux1 %>%
  filter(Inicio!="") %>% 
  left_join(Nodes, by=c("Inicio"="name")) %>% 
  left_join(Nodes, by=c("Fin"="name")) %>% 
  ungroup() %>% 
  select(source=node.x, target=node.y, value=ValorSubsidio)
colnames(Links) <- c("source", "target", "value")

sn <- networkD3::sankeyNetwork(Links = Links, Nodes = Nodes2, 
                               Source = 'source', 
                               Target = 'target', 
                               Value = 'value', 
                               NodeID = 'Name',
                               units = 'Subsidio',
                               iterations = 0,
                               fontSize=14,
                               nodeWidth = 50,
                               sinksRight = T
)
onRender(
  sn,
  '
  function(el, x) {
    d3.selectAll(".node text").attr("text-anchor", "begin").attr("x", 20)
    .attr("font-weight",function(d,i) {return 900;});
  }
  '
)


fwrite(data, file = "Analisis_Subsidios/data_cm.csv", row.names = F)


# Bono Lonchera ----

TipoSubsidio<-"Bono Lonchera"

tmp1 <- readRDS("Analisis_Subsidios/test_sub_info.rds") %>% 
  mutate(Persona_1_Genero=case_when(Persona_1_Genero %in% c("m", "M") ~ "Masculino",
                                    Persona_1_Genero == "F" ~ "Femenino")) %>% 
  filter(marca_afiliado_unico=="X")

tmp2 <- tmp1 %>% 
  filter(marca_afiliado_unico=="X", parentesco != "CONYUGUE") %>%
  group_by(id_persona) %>% 
  summarise(N_Beneficiarios=n_distinct(id_persona_familiar)) %>% 
  mutate(N_Beneficiarios=case_when(N_Beneficiarios == 1 ~ "1",
                                   N_Beneficiarios == 2 ~ "2",
                                   N_Beneficiarios >= 3 ~ "3 o más"))

data <- tmp1 %>% left_join(tmp2, by = "id_persona") %>% 
  arrange(categoria, Persona_1_Edad) %>% 
  filter(bono_girado=="Si", parentesco=="HIJO", Persona_1_Edad<=6, cm_girada=="Si") %>% 
  mutate(
    GrupoEdad=case_when(Persona_1_Edad < 18 ~ paste(Persona_1_Edad, "año(s)"),
                        Persona_1_Edad >=18 & Persona_1_Edad <25 ~ "18 a 25 años",
                        Persona_1_Edad >=25 & Persona_1_Edad <60 ~ "25 a 60 años",
                        Persona_1_Edad >=25 & Persona_1_Edad <60 ~ "25 a 60 años",
                        Persona_1_Edad >=60 ~ "Mayor a 60")
  ) %>% 
  group_by(Categoria=categoria, Edad=GrupoEdad, Genero=Persona_1_Genero, N_Beneficiarios) %>% 
  summarise(ValorSubsidio=n()) %>% 
  ungroup() %>% 
  filter(!is.na(N_Beneficiarios), !is.na(Genero))

rm(tmp1, tmp2)

aux1 <-  bind_rows(
  data %>% 
    mutate(Inicio="", Fin=TipoSubsidio) %>% 
    group_by(Inicio, Fin) %>% 
    summarise(ValorSubsidio=sum(ValorSubsidio)) %>% 
    select(Inicio=Inicio, Fin=Fin, ValorSubsidio),
  data %>% 
    mutate(Inicio=TipoSubsidio) %>% 
    group_by(Inicio,Categoria) %>% 
    summarise(ValorSubsidio=sum(ValorSubsidio)) %>% 
    select(Inicio=Inicio, Fin=Categoria, ValorSubsidio),
  data %>% 
    group_by(Categoria, N_Beneficiarios) %>% 
    summarise(ValorSubsidio=sum(ValorSubsidio)) %>% 
    select(Inicio=Categoria, Fin=N_Beneficiarios, ValorSubsidio),
  data %>% 
    group_by(N_Beneficiarios, Genero) %>% 
    summarise(ValorSubsidio=sum(ValorSubsidio)) %>% 
    select(Inicio=N_Beneficiarios, Fin=Genero, ValorSubsidio),
  data %>% 
    group_by(Genero, Edad) %>% 
    summarise(ValorSubsidio=sum(ValorSubsidio)) %>% 
    select(Inicio=Genero, Fin=Edad, ValorSubsidio)
)

n_seq <- length(c(unique(data$Categoria), unique(data$N_Beneficiarios), unique(data$Genero), unique(data$Edad)))
Nodes <- bind_cols(node=seq(0,n_seq), 
                   bind_rows(
                     data %>% mutate(name=TipoSubsidio) %>% select(name) %>% distinct(),
                     data %>% select(name=Categoria) %>% distinct(),
                     data %>% select(name=N_Beneficiarios) %>% distinct() %>% mutate(name=str_sort(name, numeric = TRUE)) ,
                     data %>% select(name=Genero) %>% distinct(),
                     data %>% select(name=Edad) %>% distinct() %>% mutate(name=str_sort(name, numeric = TRUE)))
)

tot <- aux1 %>% filter(Inicio==TipoSubsidio) %>% summarise(Valor=sum(ValorSubsidio)) %>% select(Valor) %>% as.numeric()
aux2 <- aux1 %>%
  left_join(Nodes, by=c("Inicio"="name")) %>% 
  left_join(Nodes, by=c("Fin"="name")) %>% 
  ungroup() %>% 
  group_by(Fin) %>% 
  summarise(ValorSubsidio=sum(ValorSubsidio, na.rm = T)) %>% ungroup() %>% 
  mutate(Pct=ValorSubsidio/tot) %>% 
  arrange(Fin)

Nodes2 <- Nodes %>% left_join(aux2, by=c("name"="Fin"))  %>% 
  mutate(Name=paste0(name, " ",comma(ValorSubsidio), " (", percent(Pct), ")"))


Links <- aux1 %>%
  filter(Inicio!="") %>% 
  left_join(Nodes, by=c("Inicio"="name")) %>% 
  left_join(Nodes, by=c("Fin"="name")) %>% 
  ungroup() %>% 
  select(source=node.x, target=node.y, value=ValorSubsidio)
colnames(Links) <- c("source", "target", "value")

sn <- networkD3::sankeyNetwork(Links = Links, Nodes = Nodes2, 
                               Source = 'source', 
                               Target = 'target', 
                               Value = 'value', 
                               NodeID = 'Name',
                               units = 'Subsidio',
                               iterations = 0,
                               fontSize=14,
                               nodeWidth = 50,
                               sinksRight = T
)
onRender(
  sn,
  '
  function(el, x) {
    d3.selectAll(".node text").attr("text-anchor", "begin").attr("x", 20)
    .attr("font-weight",function(d,i) {return 900;});
  }
  '
)


fwrite(data, file = "Analisis_Subsidios/data_bl.csv", row.names = F)


# Kit Escolar ----

TipoSubsidio<-"Kit Escolar"

tmp1 <- readRDS("Analisis_Subsidios/test_sub_info.rds") %>% 
  mutate(Persona_1_Genero=case_when(Persona_1_Genero %in% c("m", "M") ~ "Masculino",
                                    Persona_1_Genero == "F" ~ "Femenino")) %>% 
  filter(marca_afiliado_unico=="X")

tmp2 <- tmp1 %>% 
  filter(marca_afiliado_unico=="X", parentesco != "CONYUGUE") %>%
  group_by(id_persona) %>% 
  summarise(N_Beneficiarios=n_distinct(id_persona_familiar)) %>% 
  mutate(N_Beneficiarios=case_when(N_Beneficiarios == 1 ~ "1",
                                   N_Beneficiarios == 2 ~ "2",
                                   N_Beneficiarios >= 3 ~ "3 o más"))

data <- tmp1 %>% left_join(tmp2, by = "id_persona") %>% 
  arrange(categoria, Persona_1_Edad) %>% 
  filter(kit_girada=="Si", parentesco=="HIJO", Persona_1_Edad>=5, Persona_1_Edad<=12, cm_girada=="Si") %>% 
  mutate(
    GrupoEdad=case_when(Persona_1_Edad < 18 ~ paste(Persona_1_Edad, "año(s)"),
                        Persona_1_Edad >=18 & Persona_1_Edad <25 ~ "18 a 25 años",
                        Persona_1_Edad >=25 & Persona_1_Edad <60 ~ "25 a 60 años",
                        Persona_1_Edad >=25 & Persona_1_Edad <60 ~ "25 a 60 años",
                        Persona_1_Edad >=60 ~ "Mayor a 60")
  ) %>% 
  group_by(Categoria=categoria, Edad=GrupoEdad, Genero=Persona_1_Genero, N_Beneficiarios) %>% 
  summarise(ValorSubsidio=n()) %>% 
  ungroup() %>% 
  filter(!is.na(N_Beneficiarios), !is.na(Genero))

rm(tmp1, tmp2)

aux1 <-  bind_rows(
  data %>% 
    mutate(Inicio="", Fin=TipoSubsidio) %>% 
    group_by(Inicio, Fin) %>% 
    summarise(ValorSubsidio=sum(ValorSubsidio)) %>% 
    select(Inicio=Inicio, Fin=Fin, ValorSubsidio),
  data %>% 
    mutate(Inicio=TipoSubsidio) %>% 
    group_by(Inicio,Categoria) %>% 
    summarise(ValorSubsidio=sum(ValorSubsidio)) %>% 
    select(Inicio=Inicio, Fin=Categoria, ValorSubsidio),
  data %>% 
    group_by(Categoria, N_Beneficiarios) %>% 
    summarise(ValorSubsidio=sum(ValorSubsidio)) %>% 
    select(Inicio=Categoria, Fin=N_Beneficiarios, ValorSubsidio),
  data %>% 
    group_by(N_Beneficiarios, Genero) %>% 
    summarise(ValorSubsidio=sum(ValorSubsidio)) %>% 
    select(Inicio=N_Beneficiarios, Fin=Genero, ValorSubsidio),
  data %>% 
    group_by(Genero, Edad) %>% 
    summarise(ValorSubsidio=sum(ValorSubsidio)) %>% 
    select(Inicio=Genero, Fin=Edad, ValorSubsidio)
)

n_seq <- length(c(unique(data$Categoria), unique(data$N_Beneficiarios), unique(data$Genero), unique(data$Edad)))
Nodes <- bind_cols(node=seq(0,n_seq), 
                   bind_rows(
                     data %>% mutate(name=TipoSubsidio) %>% select(name) %>% distinct(),
                     data %>% select(name=Categoria) %>% distinct(),
                     data %>% select(name=N_Beneficiarios) %>% distinct() %>% mutate(name=str_sort(name, numeric = TRUE)) ,
                     data %>% select(name=Genero) %>% distinct(),
                     data %>% select(name=Edad) %>% distinct() %>% mutate(name=str_sort(name, numeric = TRUE)))
)

tot <- aux1 %>% filter(Inicio==TipoSubsidio) %>% summarise(Valor=sum(ValorSubsidio)) %>% select(Valor) %>% as.numeric()
aux2 <- aux1 %>%
  left_join(Nodes, by=c("Inicio"="name")) %>% 
  left_join(Nodes, by=c("Fin"="name")) %>% 
  ungroup() %>% 
  group_by(Fin) %>% 
  summarise(ValorSubsidio=sum(ValorSubsidio, na.rm = T)) %>% ungroup() %>% 
  mutate(Pct=ValorSubsidio/tot) %>% 
  arrange(Fin)

Nodes2 <- Nodes %>% left_join(aux2, by=c("name"="Fin"))  %>% 
  mutate(Name=paste0(name, " ",comma(ValorSubsidio), " (", percent(Pct), ")"))


Links <- aux1 %>%
  filter(Inicio!="") %>% 
  left_join(Nodes, by=c("Inicio"="name")) %>% 
  left_join(Nodes, by=c("Fin"="name")) %>% 
  ungroup() %>% 
  select(source=node.x, target=node.y, value=ValorSubsidio)
colnames(Links) <- c("source", "target", "value")

sn <- networkD3::sankeyNetwork(Links = Links, Nodes = Nodes2, 
                               Source = 'source', 
                               Target = 'target', 
                               Value = 'value', 
                               NodeID = 'Name',
                               units = 'Subsidio',
                               iterations = 0,
                               fontSize=14,
                               nodeWidth = 50,
                               sinksRight = T
)
onRender(
  sn,
  '
  function(el, x) {
    d3.selectAll(".node text").attr("text-anchor", "begin").attr("x", 20)
    .attr("font-weight",function(d,i) {return 900;});
  }
  '
)

fwrite(data, file = "Analisis_Subsidios/data_kit.csv", row.names = F)


# Subsidios en Especie ----

df_descuento <- fread("Analisis_Subsidios/acumulado descuentos con cuota.txt") %>% 
  mutate(SumaDeVALOR = as.numeric(gsub(",",".",SumaDeVALOR,fixed = T)),
         num_persona = as.character(NoIdentAfiliado)) %>% 
  select(-NoIdentAfiliado,-mes) %>% 
  group_by(num_persona,CATEGORIA,TipoSubsidio) %>% 
  summarise(valor = sum(SumaDeVALOR, na.rm = T)) %>% 
  data.frame()
str(df_descuento)
table(df_decuento$mes)

