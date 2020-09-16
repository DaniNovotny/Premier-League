# Pasar a listas la probabilidad de G, P, o E (de Excel a R).
library(tidyverse)
library(xlsx)
library(readxl)
library(dplyr)
library(readr)
library(writexl)

path <- "PL_Junto.xlsx"
path %>%
  excel_sheets()
mad <- path %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_excel,
      path = path)
mad[is.na(mad)] <- 0
mad <- mad %>% 
  replace_na(0)

# Partidos
PL2020 <- read_excel("PL2020.xlsx")

# Primero hay que jalar la tabla con las medias de los tiros.
path2 <- "PL_HT.xlsx"
path2 %>%
  excel_sheets()
mad2 <- path2 %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_excel,
      path = path2)
mad2[is.na(mad2)] <- 0
mad2 <- mad2 %>% 
  replace_na(0)

# jalar la tabla con las desviaciones de los tiros.
path3 <- "PL_HTstdev.xlsx"
path3 %>%
  excel_sheets()
mad3 <- path3 %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_excel,
      path = path3)
mad3[is.na(mad3)] <- 0
mad3 <- mad3 %>% 
  replace_na(0)

# jalar la tabla con medias las razones de goles/tiros
path4 <- "PL_Gmean.xlsx"
path4 %>%
  excel_sheets()
mad4 <- path4 %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_excel,
      path = path4)
mad4[is.na(mad4)] <- 0
mad4 <- mad4 %>% 
  replace_na(0)

# desviacion de razon de goles Home
path5 <- "PL_Gstdev.xlsx"
path5 %>%
  excel_sheets()
mad5 <- path5 %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_excel,
      path = path5)
mad5[is.na(mad5)] <- 0
mad5 <- mad5 %>% 
  replace_na(0)

# Primero hay que jalar la tabla con las medias de los tiros.
path7 <- "PL_ATmean.xlsx"
path7 %>%
  excel_sheets()
mad7 <- path7 %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_excel,
      path = path7)
mad7[is.na(mad7)] <- 0
mad7 <- mad7 %>% 
  replace_na(0)

# jalar la tabla con las medias de los tiros.
path8 <- "PL_ATstdev.xlsx"
path8 %>%
  excel_sheets()
mad8 <- path8 %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_excel,
      path = path8)
mad8[is.na(mad8)] <- 0
mad8 <- mad8 %>% 
  replace_na(0)

# jalar la tabla con medias las razones de goles/tiros Away
path10 <- "PL_GAmean.xlsx"
path10 %>%
  excel_sheets()
mad10 <- path10 %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_excel,
      path = path10)
mad10[is.na(mad10)] <- 0
mad10 <- mad10 %>% 
  replace_na(0)

# desviacion de razon de goles Away
path11 <- "PL_GAstdev.xlsx"
path11 %>%
  excel_sheets()
mad11 <- path11 %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_excel,
      path = path11)
mad11[is.na(mad11)] <- 0
mad11 <- mad11 %>% 
  replace_na(0)


# ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ *~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * 

#.-.     .-.     .-.     .-.     .-.     .-.     .-.     .-.     .-.     .-.
#   `._.'   `._.'   `._.'   `._.'   `._.'   `._.'   `._.'   `._.'   `._.'   

# ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ *~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ *


funcion <- function (lineas) {
  matriz <- matrix(data = NA,lineas,4)
  colnames(matriz) <- c("Eq.H","Eq.A","gH","gA")
  
  i <- 1
  while (i <= lineas) {
    hnombre <- as.character(PL2020[i,1]) 
    anombre <- as.character(PL2020[i,2])
    # Leer probabilidades del eq. Home de G,P,E.
    p <- mad[[hnombre]]
    pos <- which(p == anombre)
    p <- p[pos,]
    # Con probabilidad discreta, simular si el eq. Home va a G,P,E.
    a <- as.numeric(p[2])
    b <- as.numeric(p[3])
    c <- as.numeric(p[4])
    if (is.na(a)) { a <- 0 }
    if (is.na(b)) { b <- 0 }
    p_ADH <-runif(1,0,1)
    ADH <- if ( 0 < p_ADH & p_ADH <= a) { "A" 
    } else if (a < p_ADH & p_ADH <= a+b) { "D" 
    } else if (a+b < p_ADH & p_ADH <= 1) { "H" 
    } 
    # obtener la media de tiros del eq. Home sabiendo si G,P,E.
    p2 <- mad2[[hnombre]]
    pos2 <-which(p2 == anombre)
    media <- as.numeric(p2[pos2,ADH])
    # obtener la desviacion estandar de los tiros del eq. Home sabiendo si G,P,E.
    p3 <- mad3[[hnombre]]
    pos3 <- which(p3 == anombre)
    stdev <- as.numeric(p3[pos3,ADH])
    # cantidad de tiros que hara el equipo
    p_t <- runif(1,0,1)
    tiros <- round(qnorm(p_t,media,stdev))
    # obtener media de razon de goles Home
    p4 <- mad4[[hnombre]]
    pos4 <- which(p4 == anombre)
    gmean <- as.numeric(p4[pos4,ADH])
    # obtener media de razon de goles Home
    p5 <- mad5[[hnombre]]
    pos5 <- which(p5 == anombre)
    stdev <- as.numeric(p5[pos5,ADH])
    # razon de cantidad de goles que hara el equipo Home
    p_g <- runif(1,0,1)
    razon <- qnorm(p_g,gmean,stdev)
    # goles Home
    goles <- round(tiros*razon)
    # Leer probabilidades del eq. Home de ganar, perder o empatar.
    p6 <- mad[[hnombre]]
    pos6 <- which(p6 == anombre)
    p6 <- p6[pos6,]
    # Con probabilidad discreta, simular si el eq. Home va a ganar, perder o empatar.
    a <- as.numeric(p6[2])
    b <- as.numeric(p6[3])
    c <- as.numeric(p6[4])
    if (is.na(a)) { a <- 0 }
    if (is.na(b)) { b <- 0 }
    p6_ADH <-runif(1,0,1)
    ADH6 <- if ( 0 < p6_ADH & p6_ADH <= a) { "A" 
    } else if (a < p6_ADH & p6_ADH <= a+b) { "D" 
    } else if (a+b < p6_ADH & p6_ADH <= 1) { "H" 
    } 
    # obtener la media del eq. Away sabiendo si gana, pierde o empata.
    p7 <- mad7[[anombre]]
    pos7 <- which(p7 == hnombre)
    media2 <- as.numeric(p7[[pos7,ADH]])
    # obtener la desviacion estandar de los tiros del eq. Away sabiendo si gana, pierde o empata.
    p8 <- mad8[[anombre]]
    pos8 <- which(p8 == hnombre)
    stdev2 <- as.numeric(p8[pos8,ADH])
    # cantidad de tiros que hara el equipo Away
    p_t2 <- runif(1,0,1)
    tiros2 <- round(qnorm(p_t2,media2,stdev2))
    # obtener media de razon de goles Away
    p10 <- mad10[[hnombre]]
    pos10 <- which(p10 == anombre)
    gmean2 <- as.numeric(p10[pos10,ADH6])
    # obtener media de razon de goles Away
    p11 <- mad11[[hnombre]]
    pos11 <- which(p11 == anombre)
    stdev3 <- as.numeric(p11[pos11,ADH6])
    # razon de cantidad de goles que hara el equipo Away
    p_g3 <- runif(1,0,1)
    razon2 <- qnorm(p_g3,gmean2,stdev3)
    # goles Away
    golesA <- round(tiros2*razon2)
    if (is.na(goles)) { 
      goles <- 0 }
    if (is.na(golesA)) { 
      golesA <- 0 }
    matriz[i,1] <- hnombre
    matriz[i,2] <- anombre
    matriz[i,3] <- goles
    matriz[i,4] <- golesA
    
    i <- i +1
  }
  
  write.xlsx(matriz, file="PLsim.xlsx")
  return(matriz)
}

funcion(378)




