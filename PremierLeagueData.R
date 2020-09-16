# juntar todos los data sets de los distintos años

X2014_E0 <- read_csv("~/Documents/6to semestre/IO II/Premier League/2014_E0.csv")
db_13_14 <- X2014_E0[,c(2:7,14:15)]

X2014_2015 <- read_csv("~/Documents/6to semestre/IO II/Premier League/2014-2015.csv")
db_14_15 <- X2014_2015[,c(2:7,14:15)]

X2015_2016 <- read_csv("~/Documents/6to semestre/IO II/Premier League/2015-2016.csv")
db_15_16 <- X2015_2016[,c(1:6,13:14)]

X2016_2017 <- read_csv("~/Documents/6to semestre/IO II/Premier League/2016-2017.csv")
db_16_17 <- X2016_2017[,c(1:6,13:14)]

X2017_2018 <- read_csv("~/Documents/6to semestre/IO II/Premier League/2017-2018.csv")
db_17_18 <- X2017_2018[,c(2:7,14:15)]

X2018_2019 <- read_csv("~/Documents/6to semestre/IO II/Premier League/2018-2019.csv")
db_18_19 <- X2018_2019[,c(2:7,14:15)]

todo_junto <- rbind(db_13_14,db_14_15,db_15_16,db_16_17,db_17_18,db_18_19)

# pasarlo a excel porque visualmente es mas facil de entender

library(xlsx)

write.xlsx(todo_junto, file="PremierLeague2.xlsx")


library(tidyverse)
library(tidyr)
library(reshape2)


#a <- todo_junto[1:5,2:8]
#melt(a,"HomeTeam")


# dm1 <- melt(todo_junto[,c("HomeTeam","AwayTeam","FTHG","FTAG")], id=c("HomeTeam","AwayTeam"))
# dm2 <- melt(todo_junto[,c("HomeTeam","AwayTeam","HST","AST")], id=c("HomeTeam","AwayTeam"))
# colnames(dm2) <- c("HomeTeam", "AwayTeam", "variable2", "value2")
# dm <- merge(dm1, dm2)
# head(dm)

dm1 <- melt(todo_junto[,c("HomeTeam","AwayTeam","FTHG","FTAG","HST","AST")], id=c("HomeTeam","AwayTeam"))
write.xlsx(dm1, file="PL_melted.xlsx")


dm2 <- melt(todo_junto[,c("HomeTeam","AwayTeam","FTHG","FTAG","FTR","HST","AST")], id=c("HomeTeam","AwayTeam","FTR"))
head(dm2)
write.xlsx(dm2, file="PL_GPE.xlsx")


home <- todo_junto[,c(2:4,6:7)]
head(home)
dm3 <- melt(home, id=c("HomeTeam","AwayTeam","FTR"))
write.xlsx(dm3, file="PL_home.xlsx")

away <- todo_junto[,c(2:3,5,6,8)]
head(away)
dm4 <- melt(away, id=c("HomeTeam","AwayTeam","FTR"))
write.xlsx(dm4, file="PL_away.xlsx")

# nuevos para medias y desviaciones segun si estan home o away

home_t <- todo_junto[,c(2:3,6:7)]
head(home_t)
dm6 <- melt(home_t, id=c("HomeTeam","AwayTeam","FTR"))
write.xlsx(dm6, file="PL_home_tiros.xlsx")

home_g <- todo_junto[,c(2:4,6)]
head(home_g)
dm7 <- melt(home_g, id=c("HomeTeam","AwayTeam","FTR"))
write.xlsx(dm7, file="PL_home_goles.xlsx")

away_t <- todo_junto[,c(2:3,6,8)]
head(away_t)
dm8 <- melt(away_t, id=c("HomeTeam","AwayTeam","FTR"))
write.xlsx(dm8, file="PL_away_tiros.xlsx")

away_g <- todo_junto[,c(2:3,5,6)]
head(away_g)
dm9 <- melt(away_g, id=c("HomeTeam","AwayTeam","FTR"))
write.xlsx(dm9, file="PL_away_goles.xlsx")

# tabla
# esta no la pude hacer en excel, asi que la hice en R y luego la pasé a excel

todo_junto %>% 
  select(HomeTeam,FTR) %>% 
  group_by(HomeTeam) %>% 
  count(FTR)

todo_junto %>% 
  arrange(HomeTeam,AwayTeam)

dm5 <- todo_junto %>% 
  group_by(HomeTeam,AwayTeam) %>% 
  count(FTR) 
head(dm5)
write.xlsx(dm5, file="PL_partidos.xlsx")

# adjuntar la razon goles/tiros

dm10 <- todo_junto %>% 
  group_by(HomeTeam,AwayTeam,razon=FTHG/HST) %>% 
  count(FTR) 
head(dm10)
dm10 <- dm10[complete.cases(dm10),]
write.xlsx(dm10, file="PL_razon.xlsx")


dm11 <- todo_junto %>% 
  group_by(HomeTeam,AwayTeam,razon=FTAG/AST) %>% 
  count(FTR) 
head(dm11)
dm11 <- dm11[complete.cases(dm11),]
write.xlsx(dm11, file="PL_razonA.xlsx")


# ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ *~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * 

#.-.     .-.     .-.     .-.     .-.     .-.     .-.     .-.     .-.     .-.
#   `._.'   `._.'   `._.'   `._.'   `._.'   `._.'   `._.'   `._.'   `._.'   

# ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ *~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * 


# Pasar a listas la probabilidad de G, P, o E (de Excel a R).
library(tidyverse)
library(fs)
library(readxl)

path <- "PL_Junto.xlsx"

path %>%
  excel_sheets()
       
mad <- path %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_excel,
      path = path)

# Partidos
PL2020 <- read_excel("PL2020.xlsx")

# de un partido: jalar quienes juegan en Home, y quienes Away.
hnombre <- as.character(PL2020[34,1]) 
anombre <- as.character(PL2020[34,2])

# PARA EQUIPO HOME

# Leer probabilidades del eq. Home de ganar, perder o empatar.

# posicion <- which(mad$Fulham == anombre)
# d <- mad$Fulham[posicion,]

p <- mad[[hnombre]]
pos <- which(p == anombre)
p <- p[pos,]
#if (is.na(p)) {
#  p <- matrix(c("Otro",0.490608937889904,0.305649217585693,0.203741844524403),1,4) 
#  colnames(p) <- c("Row Labels","A","D","H") }

# Con probabilidad discreta, simular si el eq. Home va a ganar, perder o empatar.
a <- as.numeric(p[2])
b <- as.numeric(p[3])
c <- 1
p_ADH <-runif(1,0,1)
if ( 0 < p_ADH & p_ADH <= a) { 
  ADH <-"A" 
} 
if (a < p_ADH & p_ADH <= a+b) { 
  ADH <- "D" 
} 

ADH <- if (a+b < p_ADH && p_ADH <= 1) { 
  ADH <- "H" 
} else if ( 0 < p_ADH & p_ADH <= a) { 
  ADH <-"A" 
} else if (a < p_ADH & p_ADH <= a+b) { 
  ADH <- "D" 
} 



# Ya que sabemos si gano, perdio o empato, ver cuantos tiros a la porteria hicieron
# estos tiros siguen una distribucion normal.
# Primero hay que jalar la tabla con las medias de los tiros.

path2 <- "PL_HT.xlsx"

path2 %>%
  excel_sheets()

mad2 <- path2 %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_excel,
      path = path2)

# obtener la media del eq. Home sabiendo si gana, pierde o empata.

#posicion2 <- which(mad2$Fulham == anombre)
#media <- as.numeric(mad2$Fulham[posicion2,ADH])

p2 <- mad2[[hnombre]]
pos2 <-which(p2 == anombre)
media <- as.numeric(p2[pos2,ADH])

# jalar la tabla con las desviaciones de los tiros.

path3 <- "PL_HTstdev.xlsx"

path3 %>%
  excel_sheets()

mad3 <- path3 %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_excel,
      path = path3)

# obtener la desviacion estandar de los tiros del eq. Home sabiendo si gana, pierde o empata.

#posicion3 <- which(mad3$Fulham == anombre)
#stdev <- as.numeric(mad3$Fulham[posicion2,ADH])

p3 <- mad3[[hnombre]]
pos3 <- which(p3 == anombre)
stdev <- as.numeric(p3[pos3,ADH])

# cantidad de tiros que hara el equipo
p_t <- runif(1,0,1)
tiros <- round(qnorm(p_t,media,stdev))

# prueba: si funciono
# esto no es parte de la funcion final
p <- mad[[hnombre]]
pos <- which(p == anombre)
p[pos,]

# jalar la tabla con medias las razones de goles/tiros

path4 <- "PL_Gmean.xlsx"

path4 %>%
  excel_sheets()

mad4 <- path4 %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_excel,
      path = path4)

# obtener media de razon de goles Home

p4 <- mad4[[hnombre]]
pos4 <- which(p4 == anombre)
gmean <- as.numeric(p4[pos4,ADH])

# desviacion de razon de goles Home

path5 <- "PL_Gstdev.xlsx"

path5 %>%
  excel_sheets()

mad5 <- path5 %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_excel,
      path = path5)

# obtener media de razon de goles Home

p5 <- mad5[[hnombre]]
pos5 <- which(p5 == anombre)
stdev <- as.numeric(p5[pos5,ADH])

# razon de cantidad de goles que hara el equipo Home

p_g <- runif(1,0,1)
razon <- qnorm(p_g,gmean,stdev)

# goles Home

goles <- round(tiros*razon)


# ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ *~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * 

#.-.     .-.     .-.     .-.     .-.     .-.     .-.     .-.     .-.     .-.
#   `._.'   `._.'   `._.'   `._.'   `._.'   `._.'   `._.'   `._.'   `._.'   

# ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ *~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * 


# PARA EL EQUIPO AWAY

# Leer probabilidades del eq. Home de ganar, perder o empatar.

p6 <- mad[[hnombre]]
pos6 <- which(p6 == anombre)
p6 <- p6[pos6,]

# Con probabilidad discreta, simular si el eq. Home va a ganar, perder o empatar.

a <- as.numeric(p6[2])
b <- as.numeric(p6[3])
c <- as.numeric(p6[4])
p6_ADH <-runif(1,0,1)
ADH6 <- if ( 0 < p6_ADH & p6_ADH <= a) { "A" 
} else if (a < p6_ADH & p6_ADH <= a+b) { "D" 
} else if (a+b < p6_ADH & p6_ADH <= 1) { "H" 
} 

# Ya que sabemos si gano, perdio o empato, ver cuantos tiros a la porteria hicieron
# estos tiros siguen una distribucion normal.
# Primero hay que jalar la tabla con las medias de los tiros.

path7 <- "PL_ATmean.xlsx"

path7 %>%
  excel_sheets()

mad7 <- path7 %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_excel,
      path = path7)

# obtener la media del eq. Away sabiendo si gana, pierde o empata.

p7 <- mad7[[anombre]]
pos7 <- which(p7 == hnombre)
media2 <- as.numeric(p7[pos7,ADH])

# jalar la tabla con las medias de los tiros.

path8 <- "PL_ATstdev.xlsx"

path8 %>%
  excel_sheets()

mad8 <- path8 %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_excel,
      path = path8)

# obtener la desviacion estandar de los tiros del eq. Away sabiendo si gana, pierde o empata.

p8 <- mad8[[anombre]]
pos8 <- which(p8 == hnombre)
stdev2 <- as.numeric(p8[pos8,ADH])

# cantidad de tiros que hara el equipo Away
p_t2 <- runif(1,0,1)
tiros2 <- round(qnorm(p_t2,media2,stdev2))

# jalar la tabla con las medias de los goles

#path9 <- "PL_AGmean.xlsx"

#path9 %>%
#  excel_sheets()

#mad9 <- path9 %>%
#  excel_sheets() %>%
#  set_names() %>%
#  map(read_excel,
#      path = path9)

# jalar la tabla con medias las razones de goles/tiros Away

path10 <- "PL_GAmean.xlsx"

path10 %>%
  excel_sheets()

mad10 <- path10 %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_excel,
      path = path10)

# obtener media de razon de goles Away

p10 <- mad10[[hnombre]]
pos10 <- which(p10 == anombre)
gmean2 <- as.numeric(p10[pos10,ADH6])

# desviacion de razon de goles Away

path11 <- "PL_GAstdev.xlsx"

path11 %>%
  excel_sheets()

mad11 <- path11 %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_excel,
      path = path11)

# obtener media de razon de goles Away

p11 <- mad11[[hnombre]]
pos11 <- which(p11 == anombre)
stdev3 <- as.numeric(p11[pos11,ADH6])
stdev3

# razon de cantidad de goles que hara el equipo Away

p_g3 <- runif(1,0,1)
razon2 <- qnorm(p_g3,gmean2,stdev3)

# goles Away

golesA <- round(tiros2*razon2)


# ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ *~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * 

#.-.     .-.     .-.     .-.     .-.     .-.     .-.     .-.     .-.     .-.
#   `._.'   `._.'   `._.'   `._.'   `._.'   `._.'   `._.'   `._.'   `._.'   

# ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ *~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * 


# COMPARAR DOS EQUIPOS & ASIGNAR PUNTOS
# en excel

matriz <- matrix(data = 1:8,2,4,TRUE)
matriz[i,1] <- hnombre
matriz[i,2] <- anombre

z <- NA
if (is.na(z)) { z <- 0 }

puntos <- read_excel("PL_puntos.xlsx")
head(puntos)

puntos1 <- puntos[,c(1,7)]
puntos1 <- melt(puntos1)




