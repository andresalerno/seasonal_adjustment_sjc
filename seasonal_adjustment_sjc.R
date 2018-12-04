
# 1) Document Information ----
my.d <- rstudioapi::getActiveDocumentContext()

# 2) Document Path ----
my.file.location <- rstudioapi::getActiveDocumentContext()$path

# 3) Directory Path ----
my.dir <- dirname(my.file.location)

# 4) Setting up the working directory ----
setwd(my.dir)

# 5) Checking required packages ----

if(!require("DBI")) install.packages("DBI")
if(!require("dplyr")) install.packages("dplyr")
if(!require("fpp")) install.packages("fpp")
if(!require("fpp2")) install.packages("fpp2")
if(!require("gridExtra")) install.packages("gridExtra")
if(!require("here")) install.packages("here")
if(!require("magick")) install.packages("magick")
if(!require("magrittr")) install.packages("magrittr")
if(!require("sidrar")) install.packages("sidrar")
if(!require("RMySQL")) install.packages("RMySQL")
if(!require("scales")) install.packages("scales")
if(!require("seasonal")) install.packages("seasonal")
if(!require("xts")) install.packages("xts")
if(!require("zoo")) install.packages("zoo")

# 6) Loading important libraries ----

library(DBI)
library(dplyr)
library(fpp)
library(fpp2)
library(gridExtra)
library(here)
library(magick)
library(magrittr)
library(sidrar)
library(RMySQL)
library(scales)
library(seasonal)
library(xts)
library(zoo)

# 7) Fetching data using read.table() ----

file_csv <- read.table(file = "balancete_sjc.csv",
                       header = TRUE,
                       na.strings = "EMPTY",
                       colClasses = c("character", "character", "character", "character", "character", "character", "character", "numeric", "numeric", "numeric", "integer", "character"),
                       sep = ";",
                       strip.white = TRUE)

# 8) Deleting empty columns ----

file_csv[,13:16] <- NULL

# 9) Connection ----

con <- dbConnect(RMySQL::MySQL(),
                 dbname = "public_data",
                 host = "198.199.73.180",
                 port = 3306,
                 user = "root",
                 password = "")

dbSendQuery(con, "USE public_data")

dbWriteTable(con, file_csv, name = "gestao_sjc", append = TRUE, row.names = FALSE)

dbListTables(con)

# 10) Fetching Current Revenues and IPCA ----

receitas_correntes <- dbGetQuery(con,
                                 "SELECT `yyyy.mm`,
                                 descricao,
                                 valores_mes from gestao_sjc where descricao = 'RECEITAS CORRENTES';")

tabela_ipca <- get_sidra(api='/t/1419/p/201201-201809/v/63/C315/7169/n7/3501')

times <- seq(as.Date('2012-01-01'), as.Date('2018-09-01'), by='month')

ipca <- data.frame(time=times, ipca=tabela_ipca$Valor)

write.csv(ipca, file = "ipca.csv")

# 11) Adjusting specific columns ----

# Table review
print(dbGetQuery(con, "gestao_sjc;"))

# Adding ipca$ipca month
receitas_correntes <- cbind(receitas_correntes, ipca$ipca)

# Changing column name
names(receitas_correntes)[4]<-paste("ipca_mes")

# Deflation
receitas_correntes <- receitas_correntes %>%
  mutate(valores_mes, valores_deflac = valores_mes / (1 + (ipca$ipca/100)))

df <- data.frame(times, receitas_correntes$valores_deflac)

# 12) Changing to Time Series ----

receitas_correntes_ts <- ts(data=df$receitas_correntes.valores_deflac, frequency = 12,
                               start=c(2012,1), end=c(2018,09))

receitas_correntes_ts

# 13) Applying Seasonal Adjustment using fpp package ----

decompose_rc_ts = decompose(receitas_correntes_ts, "multiplicative")
adjust_rc = receitas_correntes_ts / decompose_rc_ts$seasonal

# Plotting ----
plot(adjust_rc)
plot(adjust_rc, col = "red", lwd = 2)

# 14) Applying Seasonal Adjustment using Seasonal Package (X13 ARIMA SEATS) ----

# Checking installation
checkX13(fail = FALSE, fullcheck = TRUE, htmlcheck = TRUE)

# Seasonal adjustment using X13-ARIMA-SEATS
ajuste <- seas(receitas_correntes_ts)

# Significância dos parâmetros
summary(ajuste)

# Manually identify outliers
identify(ajuste, type = c("ao", "tc", "ls"))

# Plotting
plot_seasonal <- plot(ajuste, main = "Receitas Correntes (R$) São José dos Campos")
plot_seasonal

# Seasonal adjustment serie
serie_adjust <- series(ajuste, "s11")
plot(serie_adjust, main = "Receitas Correntes em R$ - São José dos Campos")

df_ajuste <- data.frame(x= times, y= serie_adjust)


# 15) Plotting ----
plot1 <- ggplot(df_ajuste, aes(x=df_ajuste$x, y = df_ajuste$y))+
            geom_line(size=.8, colour="darkblue")+
            scale_x_date(breaks = date_breaks("3 months"),
                         labels = date_format("%b/%Y"))+
            theme(axis.text.x=element_text(angle=90, hjust=1))+
            xlab('')+ylab('Valores absolutos')+
            labs(title='Receita Corrente',
                  subtitle='Valores deflacionados e dessazonalizados',
                  caption='Fonte: AS Partners Finance & Tech Solutions (Portal da Transparência - SJC)')

plot1 <- plot1 + scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))+
  ggsave(filename = paste0(here("/"), last_plot()$labels$title, ".png"),
       width = 10, height = 5, dpi = 300)

# 16) Adding footer mark ----

#Call back the plot
plot <- image_read(paste0(here("/"), "Receita Corrente.png"))

# And bring in a logo
logo_raw <- image_read("http://hexb.in/hexagons/ggplot2.png")

# Scale down the logo and give it a border and annotation
# This is the cool part because you can do a lot to the image/logo before adding it
logo <- logo_raw %>%
  image_scale("100") %>% 
  image_background("lightgrey", flatten = TRUE) %>%
  image_border("lightgrey", "600x10") %>%
  image_annotate("Powered By R", color = "white", size = 30, 
                 location = "+10+50", gravity = "northeast")

# Stack them on top of each other
final_plot <- image_append(image_scale(c(plot, logo), "700"), stack = TRUE)

# And overwrite the plot without a logo
image_write(final_plot, paste0(here("/"), last_plot()$labels$title, ".png"))



