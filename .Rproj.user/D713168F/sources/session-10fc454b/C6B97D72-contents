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
grupos_eeff <- grupos_eeff |> mutate(across(.cols = everything(), str_replace, ',', '.'))
grupos_eeff[,8:629] <- grupos_eeff |> select(c(8:629)) |> mutate_if(is.character, as.numeric)

#Bd for enterprise
empresas_eeff <- data.table::fread("balances_2021_2.txt", encoding="Latin-1")
columnas <-  (colnames(empresas_eeff))
columnas <- str_replace(columnas, "CUENTA_", "")
colnames(empresas_eeff) <- columnas
empresas_catalogo <- data.table::fread("catalogo_2021_2.txt", encoding="Latin-1")
empresas_catalogo <- empresas_catalogo |> pivot_wider(names_from = V1, values_from = V2)
empresas_eeff <- empresas_eeff[,-933]

# to all data change to character to numerical
#empresas_eeff <- empresas_eeff |> mutate(across(.cols = everything(), str_replace, ',', '.'))
#empresas_eeff[,8:932] <- empresas_eeff |> select(c(8:932)) |> mutate_if(is.character, as.numeric)


industria <- empresas_eeff |> filter(RUC == '1791287541001')
teleco_eeff <- empresas_eeff |> filter(DESCRIPCION_RAMA == 'INFORMACIÓN Y COMUNICACIÓN.')
teleco_eeff <- rbind(teleco_eeff, empresas_catalogo, fill=TRUE)
total_col <- c(colnames(teleco_eeff)[1:7], unlist(teleco_eeff[4263,8:932]))

teleco_eeff <- teleco_eeff[-4263,]
rownames(teleco_eeff) <- NULL # Resetting index numbers of rows
teleco_eeff <- teleco_eeff |> mutate(across(.cols = everything(), str_replace, ',', '.'))
teleco_eeff[,8:932] <- teleco_eeff |> select(c(8:932)) |> mutate_if(is.character, as.numeric)
teleco_eeff <- teleco_eeff |> filter(CIIU == 'J6110.01'|
                                       CIIU == 'J6110.02'|
                                       CIIU == 'J6110.03'|
                                       CIIU == 'J6110.04'|
                                       CIIU == 'J6120.01'|
                                       CIIU == 'J6120.02'|
                                       CIIU == 'J6130.01'|
                                       CIIU == 'J6190.01'|
                                       CIIU == 'J6190.02'|
                                       CIIU == 'J6190.03'|
                                       CIIU == 'J6190.04'|
                                       CIIU == 'J6190.05')
# revisar J6110.03  J6120.02  J6190.01  J6190.03
df_tel <- teleco_eeff
colnames(df_tel) <- total_col

costos_gastos <- teleco_eeff |> select(NOMBRE, `7991`) |>
  filter(`7991` !=0) |> arrange(desc(`7991`))

#Plot bars
teleco_eeff |> select(NOMBRE, `7991`) |> filter(`7991` !=0) |> arrange(desc(`7991`)) |>
  slice_max(`7991`, n = 15) |> mutate(NOMBRE = (reorder(NOMBRE, -`7991`))) |>
  ggplot(aes(NOMBRE, `7991`)) + geom_col(position = "dodge", width=0.9, fill="#0E8388", color = "black") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Costos") + xlab("")

#plot density
teleco_eeff |> select(`7991`) |> arrange(desc(`7991`)) |>
  ggplot(aes(log10(`7991`))) + geom_density(fill="#94AF9F") +
  labs(x = "Costos y gastos telco (normalizado)")

# mean costos y gastos
mean(costos_gastos$`7991`)
# sd costos y gastos
sd(costos_gastos$`7991`)

teleco_eeff |> filter(RUC == 1791251237001) |> pull(`1005`)
