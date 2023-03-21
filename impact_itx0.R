library(readxl)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(shiny)
library(rsconnect)
library(DT)

# import the data

info_fin <- read_excel('saiku-export_final.xlsx', sheet = "Sheet 1")
#info_fin[is.na(info_fin)] <- 0

nombres <- function(x){
  ifelse(x == 'J611001', 'fijos',
         ifelse(x == 'J611002', 'tv_cable',
                ifelse(x == 'J611003', 'portador',
                       ifelse(x == 'J611004', 'sai',
                              ifelse(x == 'J612001' | x == 'J612002' | x == 'J612003', 'móviles',
                                     ifelse(x == 'J613001' | x == 'J613002', 'satélite', 0))))))
}
servicios <- sapply(info_fin$`ACTIVIDAD ECONOMICA`, nombres)
info_fin['servicios'] <- servicios

fijos <- info_fin |> filter(MeasuresLevel == 'TOTAL COSTOS (797)') |>
  group_by(servicios) |> filter(servicios == 'fijos')
apply(fijos[, 5:18], 1, mean)
apply(fijos[, 5:18], 1, max)

#_____Read Super-Compañía files EEFF________
unzip("estadosFinancieros_2021.zip", list = TRUE)

#Bd for economic groups
grupos_eeff <- read.table("balances_2021_1.txt", sep = '\t', dec = ',', quote = "",
                     encoding="latin1")
grupos_eeff <- grupos_eeff |> mutate_all(list(~str_replace(., "CUENTA_", "")))
colnames(grupos_eeff) <- grupos_eeff[1,]

grupos_catalogo <- read.table("catalogo_2021_1.txt", sep = '\t', dec = ',', quote = "",
                              encoding="latin1")
grupos_catalogo <- grupos_catalogo |> pivot_wider(names_from = V1, values_from = V2)

grupos_eeff <- full_join(grupos_eeff, grupos_catalogo)
colum_names <- cbind(grupos_eeff[1,1:7], grupos_eeff[1524,8:ncol(grupos_eeff)])
colnames(grupos_eeff) <- colum_names
grupos_eeff <- grupos_eeff[-1,]
rownames(grupos_eeff) <- NULL # Resetting index numbers of rows
grupos_eeff <- grupos_eeff[-1523,]
grupos_eeff <- grupos_eeff[,-630]

#Bd for enterprise
empresa_eeff <- read.table("balances_2021_2.txt", sep = "\t", dec = ',',
                           encoding="latin1")
industria <- empresa_eeff |> filter(V3 == '1791287541001')
teleco_eeff <- empresa_eeff |> filter(V6 == 'INFORMACIÓN Y COMUNICACIÓN.')

x <- data.table::fread("balances_2021_2.txt", encoding="Latin-1")
industria <- x |> filter(RUC == '1791287541001')

