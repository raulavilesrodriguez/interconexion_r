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
info_fin[is.na(info_fin)] <- 0

nombres <- function(x){
  ifelse(x == 'J611001'| x == 'J611002' | x == 'J611003' | x == 'J611004', 'alámbricos',
         ifelse(x == 'J612001' | x == 'J612002', 'inalámbricos',
                ifelse(x == 'J613001', 'satelitales',
                       ifelse(x == 'J619001' | x== 'J619002' | x== 'J619003' | x== 'J619004' | x== 'J619005', 'otros',
                              0))))
}
servicios <- sapply(info_fin$`ACTIVIDAD ECONOMICA`, nombres)
info_fin['servicios'] <- servicios

fijos <- info_fin |> filter(MeasuresLevel == 'TOTAL COSTOS (797)') |>
  group_by(servicios) |> filter(servicios == 'alámbricos')
apply(fijos[, 5:18], 1, mean)
apply(fijos[, 5:18], 1, max)

info_fin <- info_fin |> pivot_longer(cols = 3:19, names_to = 'año', values_to = 'indicador') |>
  filter(año != 2022)
info_fin <- info_fin |> group_by(MeasuresLevel, servicios, año) |> summarise(valor = sum(indicador))

#_____Read Super-Compañía files EEFF________
desempaquetar <- function(paqueto){
  unzip(paqueto, list = TRUE)
}
eeff <- "estadosFinancieros_2021.zip"
desempaquetar(eeff)

#Bd for economic groups
procesamiento_grupos <- function(documento){
  grupos_eeff <- read.table(documento, sep = '\t', dec = ',', quote = "",
                            encoding="latin1")
  grupos_eeff <- grupos_eeff |> mutate_all(list(~str_replace(., "CUENTA_", "")))
  colnames(grupos_eeff) <- grupos_eeff[1,]

  grupos_catalogo <- read.table("catalogo_2021_1.txt", sep = '\t', dec = ',', quote = "",
                                encoding="latin1")
  grupos_catalogo <- grupos_catalogo |> pivot_wider(names_from = V1, values_from = V2)

  grupos_eeff <- full_join(grupos_eeff, grupos_catalogo)
  colum_names <- cbind(grupos_eeff[1,1:7], grupos_eeff[nrow(grupos_eeff),8:ncol(grupos_eeff)])
  colnames(grupos_eeff) <- colum_names
  grupos_eeff <- grupos_eeff[-1,]
  rownames(grupos_eeff) <- NULL # Resetting index numbers of rows
  grupos_eeff <- grupos_eeff[-nrow(grupos_eeff),]
  grupos_eeff <- grupos_eeff[,-ncol(grupos_eeff)]
  grupos_eeff <- grupos_eeff |> mutate(across(.cols = everything(), str_replace, ',', '.'))
  grupos_eeff[,8:ncol(grupos_eeff)] <- grupos_eeff |> select(c(8:ncol(grupos_eeff))) |> mutate_if(is.character, as.numeric)
  grupos_eeff <- grupos_eeff |> filter(CIIU == 'J6110.01'|
                                         CIIU == 'J6110.02'|
                                         CIIU == 'J6110.03'| #
                                         CIIU == 'J6110.04'|
                                         CIIU == 'J6120.01'|
                                         CIIU == 'J6120.02'| #
                                         CIIU == 'J6130.01'|
                                         CIIU == 'J6190.01'| #
                                         CIIU == 'J6190.02'|
                                         CIIU == 'J6190.03'| #
                                         CIIU == 'J6190.04'|
                                         CIIU == 'J6190.05')

  grupos_eeff
}

balances_1 <- "balances_2021_1.txt"
resultado_1 <- procesamiento_grupos(balances_1)
df_grupo <- resultado_1
grupos_eeff <- resultado_1 |> select(RUC, NOMBRE,
                                     `COSTO DE VENTAS Y PRODUCCIÓN`,
                      GASTOS, `INGRESOS DE ACTIVIDADES ORDINARIAS`) |>
              mutate(`7999` = `COSTO DE VENTAS Y PRODUCCIÓN` + GASTOS,
                     `1005` = `INGRESOS DE ACTIVIDADES ORDINARIAS`)
grupos_eeff <- grupos_eeff[, !names(grupos_eeff) %in% c(
  "COSTO DE VENTAS Y PRODUCCIÓN", "GASTOS", "INGRESOS DE ACTIVIDADES ORDINARIAS"
)]

#Bd for enterprise
procesamiento_empresas <- function(documento){
  empresas_eeff <- data.table::fread(documento, encoding="Latin-1")
  empresas_eeff <- tibble(empresas_eeff)
  columnas <-  (colnames(empresas_eeff))
  columnas <- str_replace(columnas, "CUENTA_", "")
  colnames(empresas_eeff) <- columnas
  empresas_catalogo <- data.table::fread("catalogo_2021_2.txt", encoding="Latin-1")
  empresas_catalogo <- tibble(empresas_catalogo)
  empresas_catalogo <- empresas_catalogo |> pivot_wider(names_from = V1, values_from = V2)
  empresas_eeff <- empresas_eeff[,-ncol(empresas_eeff)]

  # filter only telco enterprise
  teleco_eeff <- empresas_eeff |> filter(DESCRIPCION_RAMA == 'INFORMACIÓN Y COMUNICACIÓN.')
  total_col <- c(colnames(teleco_eeff)[1:7], unlist(empresas_catalogo[1,]))
  rownames(teleco_eeff) <- NULL # Resetting index numbers of rows
  teleco_eeff <- teleco_eeff |> dplyr::mutate(dplyr::across(.cols = everything(), str_replace, ',', '.'))
  teleco_eeff[,8:ncol(teleco_eeff)] <- teleco_eeff |> select(c(8:ncol(teleco_eeff))) |> mutate_if(is.character, as.numeric)
  teleco_eeff <- teleco_eeff |> filter(CIIU == 'J6110.01'|
                                         CIIU == 'J6110.02'|
                                         CIIU == 'J6110.03'| #
                                         CIIU == 'J6110.04'|
                                         CIIU == 'J6120.01'|
                                         CIIU == 'J6120.02'| #
                                         CIIU == 'J6130.01'|
                                         CIIU == 'J6190.01'| #
                                         CIIU == 'J6190.02'|
                                         CIIU == 'J6190.03'| #
                                         CIIU == 'J6190.04'|
                                         CIIU == 'J6190.05')
  df_tel <- teleco_eeff
  colnames(df_tel) <- total_col
  list(teleco_eeff, df_tel)
}

financial_state <- "balances_2021_2.txt"
results <- procesamiento_empresas(financial_state)
teleco_eeff <- results[[1]]
df_tel <- results[[2]]
duplicated_names <- duplicated(colnames(df_tel))
df_tel <- df_tel[!duplicated_names]

costos_gastos <- teleco_eeff |> select(RUC, NOMBRE, `1005`, `7999`) |>
  filter(`7999` !=0) |> arrange(desc(`7999`))
costos_gastos <- rbind(costos_gastos, grupos_eeff)

#Plot bars
costos_gastos |> select(NOMBRE, `7999`) |> filter(`7999` !=0) |> arrange(desc(`7999`)) |>
  slice_max(`7999`, n = 15) |> mutate(NOMBRE = (reorder(NOMBRE, -`7999`))) |>
  ggplot(aes(NOMBRE, `7999`)) + geom_col(position = "dodge", width=0.9, fill="#0E8388", color = "black") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Costos y Gastos") + xlab("")

#plot density
costos_gastos |> select(`7999`) |> arrange(desc(`7999`)) |>
  ggplot(aes(log10(`7999`))) + geom_density(fill="#94AF9F") +
  labs(x = "Costos y gastos telco (normalizado)")

# mean costos y gastos
mean(costos_gastos$`7999`)
# sd costos y gastos
sd(costos_gastos$`7999`)

costos_gastos |> filter(RUC == 1791251237001) |> pull(`1005`)

# internet telecom service
sai <- read_excel('Abonados SAI art 34 IV 2021 CDRM.xls', sheet = 'SAI oct dic 21')
sai <- sai[, -c(1,3:7)]
colnames(sai) <- c('NOMBRE', 'sai_con')
sai <- sai[-889,]
costos_gastos <- costos_gastos |> left_join(sai, by = 'NOMBRE')

# fixed telecom service
stf <- read_xlsx("stf_2021.xlsx")
colnames(stf)[2] <- "stf_con"
costos_gastos <- costos_gastos |> left_join(stf, by = 'NOMBRE')

# video telecom service
avs <- read_xlsx("avs_2021.xlsx")
colnames(avs)[2] <- "avs_con"
avs <- avs |> filter(!is.na(avs_con))
costos_gastos <- costos_gastos |> left_join(avs, by = 'NOMBRE')
costos_gastos |> select(avs_con) |> filter(!is.na(avs_con)) |> sum()

# sma telecom service
sma <- read_xlsx("sma_2021.xlsx")
costos_gastos <- costos_gastos |> left_join(sma, by = 'NOMBRE')

# portador telecom service
portador <- read_xls("5.1.1-Usuarios-enlaces-SPT-Diciembre-2022.xls", sheet = 'Abonados y enlaces')
portador <- portador[-(1:8),] #delet rows
portador <- portador[(1:33), c(1,133)] # corresponding columns to 2021 year
colnames(portador) <- c('NOMBRE', 'portador_con')
portador <- portador[-(1:3),]
portador <- portador |> filter(!is.na(portador_con))|>
  mutate(portador_con = as.numeric(portador_con))|>
  arrange(desc(portador_con))
costos_gastos <- costos_gastos |> left_join(portador, by = 'NOMBRE')

# trunking telecom service
troncalizado <- read_xlsx("troncalizado_2021.xlsx")
costos_gastos <- costos_gastos |> left_join(troncalizado, by = 'NOMBRE')

# satellite finals telecom service
finales_satelite <- read_xlsx("finales_satelite_2021.xlsx")
costos_gastos <- costos_gastos |> left_join(finales_satelite, by = 'NOMBRE')

# segmento espacial telecom service
segmento_espacial <- read_xlsx("segmento_espacial_2021.xlsx")
costos_gastos <- costos_gastos |> left_join(segmento_espacial, by = 'NOMBRE')

#calculus cost
costos_gastos[is.na(costos_gastos)] <- 0

costos_gastos <- costos_gastos |>
  mutate(con_total = ifelse(sai_con >= portador_con, rowSums(across(c(
    sai_con, stf_con, avs_con, sma_con, tronc_con, satelite_con, espacial_con
  ))), rowSums(across(c(
    portador_con, stf_con, avs_con, sma_con, tronc_con, satelite_con, espacial_con
  )))),
         costo_usuario = `7999`/(con_total*12),
         income_usuarios = `1005`/(con_total*12),
         profit = income_usuarios - costo_usuario) |>
  filter(costo_usuario != Inf & !is.na(costo_usuario)) |>
  mutate(servicio = ifelse(sai_con != 0 & rowSums(across(c(
    stf_con, avs_con, sma_con, tronc_con, satelite_con, espacial_con
  )))==0, "sai", ifelse(tronc_con !=0 & rowSums(across(c(
    sai_con, portador_con, stf_con, avs_con, sma_con, satelite_con, espacial_con
  )))==0, "troncalizado", ifelse(espacial_con !=0 & rowSums(across(c(
    sai_con, portador_con, stf_con, avs_con, sma_con, satelite_con, tronc_con
  )))==0, "espacial", ifelse(satelite_con !=0 & rowSums(across(c(
    sai_con, portador_con, stf_con, avs_con, sma_con, tronc_con, espacial_con
  )))==0, "satelital", ifelse(avs_con !=0 & rowSums(across(c(
    sai_con, portador_con, stf_con, espacial_con, sma_con, satelite_con, tronc_con
  )))==0, "avs", ifelse(stf_con !=0 & rowSums(across(c(
    sai_con, portador_con, avs_con, sma_con, tronc_con, espacial_con, satelite_con
  )))==0, "fija", ifelse(portador_con != 0 & rowSums(across(c(
    sai_con, stf_con, avs_con, espacial_con, sma_con, satelite_con, tronc_con
  )))==0, 'portador', 'sma_otros'))))))))

#Plot bars
costos_gastos |> mutate(NOMBRE = (reorder(NOMBRE, -costo_usuario))) |>
  slice_min(costo_usuario, n = 15) |>
  mutate(NOMBRE = (reorder(NOMBRE, costo_usuario))) |>
  ggplot(aes(NOMBRE, costo_usuario, fill = servicio)) + geom_col(show.legend = FALSE) +
  geom_col(position = "dodge", width=0.9, color = "black") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Costo_Conexión") + xlab("")

costos_gastos |> filter(servicio != "espacial") |>
  ggplot(aes(log10(costo_usuario), fill = servicio)) + geom_density(alpha=.25) +
  facet_wrap(~servicio, ncol = 2, scales = "free_x") +
  labs(x = "Costos y gastos telco (normalizado)")

costos_gastos |> filter(servicio == "sai" ) |>
  summarise(median_costo_u = median(costo_usuario),
            median_income_u = median(income_usuarios))

#in ggplot2 how to plot two densities at the same time and  facet_wrap in r
df <- costos_gastos |> select(costo_usuario, income_usuarios, servicio)
df <- df |> pivot_longer(cols = 1:2, names_to = "grupo", values_to = 'valores')
df |> filter(servicio != "espacial") |>
ggplot(aes(log10(valores), fill = grupo)) + geom_density(adjust=1.5, alpha=.25) +
facet_wrap(~servicio, ncol = 2, scales = "free_x") +
labs(x = "Costos y gastos telco (normalizado)")

df |> filter(servicio != "espacial") |>
  ggplot(aes(log10(valores), fill = grupo)) +
  geom_density(adjust=1.5, alpha=.25) +
  labs(x = "Costos y gastos telco (normalizado)")

#________________Shiny_______________
datos_1 <- list(info_fin, costos_gastos)

itxApp <- function(){
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "sandstone"),
  titlePanel('', windowTitle = "AIR_ITX"),
  HTML(r"(
         <h1 style="text-align:center">AIR DE INTERCONEXIÓN Y ACCESO</h1>
         <h6 style="color:#FC2947;">
         <a style="text-decoration: none"
         target="_blank"
         href="https://raulaviles.netlify.app/">
         Designed by: Byron Raúl Avilés Rodríguez
         </a>
         </h6>
    )"),
  textOutput("panel"),
  HTML(r"(<br>)"),
  tabsetPanel(
    id = "tabset",
    tabPanel("Empresas Telecom",
             sidebarLayout(
               sidebarPanel(
                  selectInput('setdata', 'BD Super Compañías',
                              choices = c("General Histórico"="1",
                                          "Por Empresa"="2")),
                  selectInput('empresa', 'Empresa', choices = NULL)
               ),
               mainPanel(
                  plotOutput('plot_general'),
                  textOutput('prueba')
               )
             ),
             fluidRow(
               column(12,titlePanel(textOutput("titulo_tabla"))),
               column(12, DT::dataTableOutput("my_tabla"))
             )
             ),
    tabPanel("Análisis Costo")
  )
)

server <- function(input, output, session) {
  output$panel <- renderText({
    paste("Actual Panel:", input$tabset)
  })
  data_1 <- reactive({
    req(input$setdata) #to be known
    tibble(datos_1[[as.numeric(input$setdata)]])
  })
  # Tables totals
  entrada <- reactive(input$setdata)
  observeEvent(data_1(),
    if(colnames(data_1())[1]=="MeasuresLevel"){
      string <- reactive(paste0(input$setdata))
      output$prueba <- renderText(string())
      updateSelectInput(session, "empresa", choices = colnames(data_1()[-3]))
      output$my_tabla <- DT::renderDataTable(data_1(), rownames = FALSE)
    }
    else{
      updateSelectInput(session, "empresa", choices = colnames(data_1())[-c(1,2)])
      output$my_tabla <- DT::renderDataTable(data_1()|>select(any_of('NOMBRE'), any_of(input$empresa)), rownames = FALSE)
    }
  )

}

shinyApp(ui, server)
}

itxApp()

