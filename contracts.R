library(RSocrata)
# library(bit64)
library(plyr)
library(dplyr)
library(stringi)
library(zoo)
library(magrittr)
library(ggplot2)
library(plotly)
library(RCurl)
library(XML)
library(data.table)
library(jsonlite)

api_key <- "9IziW8dkKuDdG4pAh4MNNNE77"
data <- "https://www.datos.gov.co/resource/47sa-kfcp.json?$$app_token="
data_limit <- "https://www.datos.gov.co/resource/47sa-kfcp.json?$limit=1000"

#Read using Socrata REST API
a <- read.socrata(data_limit, app_token = api_key)

#Dowload and process the data using jsonlite
urls <- getURL(paste0(data, api_key))

#Read the csv downloaded from datosabiertos.gov.co and select the variables for analysis.
contracts <- fread("~/Downloads/SECOP_I_-_Consolidado.csv") 

contracts_subset <- contracts %>%
  select(starts_with("Fecha"), contains("ID"), contains("Cuantia")) %>%
  mutate_at(vars(matches("Fecha")), as.Date, format = "%m/%d/%Y") %>%
  mutate_at(vars(matches("Cuantia")), funs(as.numeric(stri_sub(., 2)))) %>%
  mutate(mes_creacion = format(`Fecha de Firma del Contrato`, format = "%Y-%m")) %>%
  group_by(mes_creacion) %>%
  summarize(sum_cuantia = sum(`Cuantia Contrato`),
            numero_contratos = n()) %>%
  mutate(mes_creacion_data = as.Date(paste(mes_creacion, 1, sep="-"),"%Y-%m-%d"))
  

#Aggregate data by entity


g <- ggplot(contracts_subset, aes(x= mes_creacion_data, y = numero_contratos)) + geom_line()
g <- g + scale_x_date(date_labels = "%Y-%m-%d")
g

g2 <- ggplot(contracts_subset, aes(x= mes_creacion_data, y= sum_cuantia)) + geom_line()
g2 <- g2 + scale_x_date(date_labels = "%Y-%m-%d")
g2
ggplotly(g2)


ggplotly(g)


