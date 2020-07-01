# Cargamos librerias
options(scipen = 999)
library(shiny); library(ggplot2); library(dplyr); library(plotly)
library(shinydashboard);library(DT); library(shinyjs); library(reshape)
library(data.table); library(tidyr); library(stringr); library(shinydashboardPlus)
library(Hmisc); library(shinythemes); library(dashboardthemes)
library(scales); library(shinycustomloader); library(readxl)

### Contratos 
contratos <- read_excel("Data/Datos 12 03.xlsx") %>% 
  data.frame()
names(contratos) <- tolower(gsub(".","_",names(contratos),fixed = T))
names(contratos) <- chartr("áéíóú","aeiou",names(contratos))
str(contratos)
gerencias <- c("OFICINA JURIDICA Y SECRETARIA GENERAL", "OFICINA JURIDICA Y SECRETARIA GENERAL", names(table(contratos$dependencia)))

# Tabla tipo contrato
tb_tipo_contrato <- read_excel("Data/tb_tipo_contrato.xlsx") %>% 
  mutate(Llave = as.character(Llave))
str(tb_tipo_contrato)

### Claves para usuario
credenciales <- data.frame(
  usuarios = c("ana.garzon@colsubsidio.com","diego.bastidas@colsubsidio.com",
               "gerente1@colsubsidio.com","gerente2@colsubsidio.com",
               "gerente3@colsubsidio.com","gerente4@colsubsidio.com",
               "gerente5@colsubsidio.com","gerente6@colsubsidio.com",
               "gerente7@colsubsidio.com","gerente8@colsubsidio.com",
               "gerente9@colsubsidio.com","gerente10@colsubsidio.com",
               "jorgguah@colsubsidio.com","gerente12@colsubsidio.com",
               "gerente13@colsubsidio.com","gerente14@colsubsidio.com",
               "gerente15@colsubsidio.com","gerente16@colsubsidio.com",
               "gerente17@colsubsidio.com"),
  gerencia = gerencias,
  clave = c("contratos1", "contratos1",
            "gerencia1", "gerencia2", "gerencia3", "gerencia4", "gerencia5", "gerencia6", "gerencia7", "gerencia8",
            "gerencia9", "gerencia10", "gerencia11", "gerencia12", "gerencia13", "gerencia14", "gerencia15", "gerencia16", "gerencia17")) %>% 
  mutate_all(.funs = as.character)

### Union
contratos <- contratos %>% 
  left_join(credenciales, by = c("dependencia"="gerencia"))
str(contratos)

