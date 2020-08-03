# Librerias y Funciones. ----
rm(list = ls(all=T))
source("~/CamiloYate/Funciones.r")
pqts <- c("data.table", "tidyverse", "tictoc", "networkD3", "readxl")
Loadpkg(pqts)


data <- bind_rows(read_excel("Sankey Subsidios.xlsx") %>% filter(ValorSubsidio!=0))

aux <- data %>% 
  mutate(Subsidio="Subsidio En Especie") %>% 
  group_by(Subsidio,Categoria) %>% 
  summarise(ValorSubsidio=sum(ValorSubsidio)) %>% 
  select(Categoria=Subsidio, Edad=Categoria, ValorSubsidio)

n_seq <- length(c(unique(data$Categoria), unique(data$Edad)))

Nodes <- bind_cols(node=seq(0,n_seq), 
                  bind_rows(
                    data %>% mutate(name="Subsidio") %>% select(name) %>% distinct(),
                    data %>% select(name=Categoria) %>% distinct(),
                    data %>% select(name=Edad) %>% distinct())
)

Links <- bind_rows(data, aux) %>% 
  left_join(Nodes, by=c("Categoria"="name")) %>% 
  left_join(Nodes, by=c("Edad"="name")) %>% 
  select(source=node.x, target=node.y, value=ValorSubsidio)
colnames(Links) <- c("source", "target", "value")

networkD3::sankeyNetwork(Links = Links, Nodes = Nodes, 
                         Source = 'source', 
                         Target = 'target', 
                         Value = 'value', 
                         NodeID = 'name',
                         units = 'Subsidio',
                         iterations = 0,
                         fontSize=14)

links
nodes
