# Librerias y Funciones. ----
rm(list = ls(all=T))
# source("Analisis_Subsidios/Funciones.r")
# pqts <- c("data.table", "tidyverse", "tictoc", "networkD3", "readxl", "scales", "htmlwidgets", "rbokeh")
# Loadpkg(pqts)
library(data.table); library(dplyr)


persona <- fread("Analisis_Subsidios/Consulta_persona.txt") %>% 
  distinct() %>% 
  mutate(num_persona = gsub("\\D","",Id_persona)) %>% 
  select(-Genero,-Id_persona)
str(persona)

data <- bind_rows(fread("Analisis_Subsidios/acumulado descuentos con cuota.txt") %>% 
                    filter(SumaDeVALOR!=0) %>% 
                    mutate(SumaDeVALOR = as.numeric(gsub(",",".",SumaDeVALOR,fixed = T)),
                           num_persona = as.character(NoIdentAfiliado)) %>% 
                    left_join(persona, by = "num_persona") %>% 
                    select(-mes,-NoIdentAfiliado) %>% 
                    mutate(
                      Edad=case_when(Edad < 18 ~ paste(Edad, "año(s)"),
                                          Edad >=18 & Edad <25 ~ "18 a 25 años",
                                          Edad >=25 & Edad <30 ~ "25 a 30 años",
                                          Edad >=30 & Edad <40 ~ "30 a 40 años",
                                          Edad >=40 & Edad <50 ~ "40 a 50 años",
                                          Edad >=50 & Edad <60 ~ "50 a 60 años",
                                          Edad >=60 ~ "Mayor a 60")
                    ) %>% 
                    group_by(CATEGORIA,Edad) %>% 
                    summarise(ValorSubsidio = sum(SumaDeVALOR),
                              num_persona = n())) %>% 
  na.omit() %>% 
  data.frame()
str(data)

table(data$Edad)
test <- data %>% 
  filter(Edad %in% c("18 a 25 años","25 a 30 años","30 a 40 años","40 a 50 años","50 a 60 años","Mayor a 60")) %>% 
  janitor::add_totals_row()

aux <- data %>% 
  mutate(Subsidio="Subsidio En Especie") %>% 
  group_by(Subsidio,CATEGORIA) %>% 
  summarise(ValorSubsidio=sum(ValorSubsidio)) %>% 
  select(CATEGORIA=Subsidio, Edad=CATEGORIA, ValorSubsidio) %>%
  data.frame()
str(aux)

n_seq <- length(c(unique(data$CATEGORIA), unique(data$Edad)))

Nodes <- bind_cols(node=seq(0,n_seq), 
                  bind_rows(
                    data %>% mutate(name="Subsidio En Especie") %>% select(name) %>% distinct() %>% mutate(name=str_sort(name, numeric = TRUE)),
                    data %>% select(name=CATEGORIA) %>% distinct()%>% mutate(name=str_sort(name, numeric = TRUE)),
                    data %>% select(name=Edad) %>% distinct()%>% mutate(name=str_sort(name, numeric = TRUE)))) %>% 
  data.frame()

bd_descuento <- bind_rows(data, aux) %>% 
  mutate(Edad=str_sort(Edad, numeric = TRUE))
fwrite(bd_descuento, file = "Analisis_Subsidios/bd_descuento.csv", row.names = F)
 
Links <- bind_rows(data, aux) %>% 
  left_join(Nodes, by=c("CATEGORIA"="name")) %>% 
  left_join(Nodes, by=c("Edad"="name")) %>% 
  select(source=node.x, target=node.y, value=ValorSubsidio)
colnames(Links) <- c("source", "target", "value")

sn <- networkD3::sankeyNetwork(Links = Links, 
                         Nodes = Nodes, 
                         Source = 'source', 
                         Target = 'target', 
                         Value = 'value', 
                         NodeID = 'name',
                         units = 'Subsidio',
                         iterations = 0,
                         fontSize=14)

onRender(
  sn,
  '
  function(el, x) {
    d3.selectAll(".node text").attr("text-anchor", "begin").attr("x", 20)
    .attr("font-weight",function(d,i) {return 900;});
  }
  '
)
