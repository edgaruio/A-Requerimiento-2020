Loadpkg <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = T)
  sapply(pkg, require, character.only = TRUE)
}

convertir.contabilidad <- function(x){
  if(grepl("\\(.*\\)", x)){
    as.numeric(paste0("-", gsub("\\(|\\)", "", gsub("[\\$, ]", "", x))))
  } else {
    as.numeric(gsub("[\\$, ]", "", x))
  }
}

Limpiar.Cadenas <-function(x, espacios=T){
  require("stringi")
  x<-gsub("\\.", "_",tolower(gsub("\\W","",x)))
  x<-gsub("([\\W])\\1+","\\1",stri_trans_general(x,id="Latin-ASCII"), perl=T)
  if(!espacios){
    x<-gsub("\\s","",iconv(x,to="ASCII//TRANSLIT"), perl=T)
  } else {
        x
    }
}

Unir.Cadenas <- function(..., sep = " ", collapse = NULL, na.rm = F) {
    if (na.rm == F)
        paste(..., sep = sep, collapse = collapse)
    else
        if (na.rm == T) {
            paste.na <- function(x, sep) {
                x <- gsub("^\\s+|\\s+$", "", x)
                ret <- paste(na.omit(x), collapse = sep)
                is.na(ret) <- ret == ""
                return(ret)
            }
            df <- data.frame(..., stringsAsFactors = F)
            ret <- apply(df, 1, FUN = function(x) paste.na(x, sep))
            
            if (is.null(collapse))
                ret
            else {
                paste.na(ret, sep = collapse)
            }
        }
}

Cargar.Datos<-function (carpeta, extension="csv", exhaustivo=F, Fuente=T, n_ultimos=0, ordenado=T, ausentes=getOption("datatable.na.strings","NA"), 
                        separador="auto", dec=".", quote="\"", header="auto", clases=NULL){
    
    require("dplyr"); require("data.table"); require("qdapRegex")
    
    if(ordenado) file.list <- paste0(carpeta,"/",list.files(carpeta, pattern = paste0("+.",extension), recursive = exhaustivo))
    else file.list <- sort(paste0(carpeta,"/",list.files(carpeta, pattern = paste0("+.",extension), recursive = exhaustivo)))
    m=length(file.list)
    n=ifelse((n_ultimos>=m| n_ultimos<1),1, m-(n_ultimos-1))
    
    file.list<-file.list[n:length(file.list)]
    
    print("Importando:: ")
    print(file.list)
    Union <- do.call("bind_rows",
                     lapply(file.list, FUN = function(file) {
                         fread(file, sep=separador, dec=dec, quote=quote, header = header,
                               na.strings = ausentes,
                               col.names = Limpiar.Cadenas(names(fread(file.list[1], nrows = 1))),
                               colClasses = clases
                         ) %>% mutate(Source=as.character(rm_between(gsub(carpeta, "", file), "/", ".", extract=TRUE)[[1]]))
                       }
                     )
    )
    if(!Fuente)
      Union<-Union %>% select(-Source)
    return(Union)
}

Calcular.Edad = function(from, to) {
  from_lt = as.POSIXlt(from)
  to_lt = as.POSIXlt(to)
  
  age = to_lt$year - from_lt$year
  
  ifelse(to_lt$mon < from_lt$mon |
           (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
         age - 1, age)
}

Calcuar.Meses <- function(from, to) {
  sd <- as.POSIXlt(from)
  ed <- as.POSIXlt(to)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

Convertir.Fecha <- function(x, origen='1899-12-30'){
  options(warn=-1)
  x_trans <- as.Date(ifelse(substr(x,5,5)=="/",  as.character(as.Date(x, "%Y/%m/%d")),
                            ifelse(substr(x,5,5)=="-",  as.character(as.Date(x, "%Y-%m-%d")),
                                   ifelse(substr(x,3,3)=="/",  as.character(as.Date(x, "%d/%m/%Y")),
                                          ifelse(!is.na(as.numeric(x)), as.character(as.Date(as.numeric(x), origin = origen)),
                                                 as.Date(NA))))))
  options(warn=0)
}

Mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}