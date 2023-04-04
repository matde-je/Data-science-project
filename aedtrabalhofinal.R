##### Projeto AED - Caso Prático: Saude

#diretoria de trabalho
getwd()

library(openxlsx)

#bibliotecas 
#install.packages("tidyverse")
library("tidyverse")
library(dplyr)

#install.packages("ggplot2")
library(ggplot2)

#install.packages("jmv")
library(jmv)

#install.packages("treemap")
library(treemap)

#install.packages("descr")
library(descr)

#install.packages("moments")
library(moments)

#install.packages("flextable")
library(flextable)

#install.packages("olsrr")
library(olsrr)

#install.packages("foreign")
library(foreign)

# Ler e definir um data frame
cd <- read.xlsx("Aed_Trabalho_Saude.xlsx")
colnames(cd)

# Alterar nome de variável
cd <- rename(cd,  CC = ID ,  Região = NUTII)
names(cd)
dt <- data.frame(cd)
dt

# Identificar valores omissos
dt$Horas.de.sono[dt$Horas.de.sono == 99] <- NA
unique(dt$Horas.de.sono)
is.na(dt$Horas.de.sono)

dt$Tempo.de.ecrã[dt$Tempo.de.ecrã == 99] <- NA
unique(dt$Tempo.de.ecrã)
is.na(dt$Tempo.de.ecrã)

# Imputar os valores omissos de uma variável com a mediana dos seus valores
mediana2 <- round(median(dt$Horas.de.sono, na.rm=TRUE)) 
mediana2

mediana3 <- round(median(dt$Tempo.de.ecrã, na.rm=TRUE))
mediana3

Horas.de.sono <- replace(dt$Horas.de.sono,is.na(dt$Horas.de.sono), 8)
Horas.de.sono

Tempo.ecra <- replace(dt$Tempo.de.ecrã,is.na(dt$Tempo.de.ecrã), 5)
Tempo.ecra

## Horas de sono
thorassono <- table(cd$Horas.de.sono)
thorasperc <- round(prop.table(thorassono)*100,1) 
thorasperc
thorassono
bind_rows(thorassono, thorasperc)

# Group data into 3 categories
thorassono_grouped <- c(sum(thorassono[1:3]), sum(thorassono[4:6]), thorassono[7])

# Create pie chart with sorted slices
pie(thorassono_grouped, 
    main="Horas de Sono", 
    labels = c(" <6", " 7 - 9 ", " 10+"), 
    col = c("#66c2a5", "#fc8d62", "#8da0cb"), 
    border = "white")

## Tempo de Ecrã
ttempoecra <- table(cd$Tempo.de.ecrã)
ttempoecraperc <- round(prop.table(ttempoecra)*100,1)
ttempoecraperc
ttempoecra
bind_rows(ttempoecra, ttempoecraperc)

## Idade
tidade <- table(cd$Idade)
tidadeperc <- round(prop.table(tidade)*100,1) # prop.table -> criar tabela com valores de propor
tidadeperc
tidade
bind_rows(tidade, tidadeperc)

# Create the frequency table
tidade <- table(cd$Idade)

# Calculate the percentage values and round to one decimal place
tidadeperc <- round(prop.table(tidade) * 100, 1)

# Combine the tables into one using the bind_rows() function from the dplyr package
bind2 <- bind_rows(data.frame(Idade = names(tidade), Absolutas = tidade),
                   data.frame(Idade = names(tidadeperc), Percentagens = paste0(tidadeperc, "%")))

# Print the resulting table
flextable(bind2)


# Satisfação com a vida
#nomes = c("Pior vida possível","1","2","3","4","5","6","7","8","9","Melhor vida possível")
satisfacao_vida <- cd$Satisfação
boxplot(satisfacao_vida, main = "Satisfação com a vida", col = "yellow", ylim = c(0,10))          

# Estatisticas sobre Horas de Sono
nn <- length(Horas.de.sono)
nn
media <- round(mean(Horas.de.sono, na.rm=TRUE),1)
media
mediana <-  median(Horas.de.sono, na.rm=TRUE)
mediana
desvpadrao <- round(sd(Horas.de.sono, na.rm=TRUE),1)
desvpadrao
variacao <- round(var(Horas.de.sono, na.rm=TRUE),1)
variacao
minimo <- min(Horas.de.sono, na.rm=TRUE)
minimo
maximo <- max(Horas.de.sono, na.rm=TRUE)
maximo
primeiroqua <- round(quantile(Horas.de.sono, 0.25, na.rm=TRUE),1)
primeiroqua
tercqua <- round(quantile(Horas.de.sono, 0.75, na.rm=TRUE),1)
tercqua
assim <- round(skewness(Horas.de.sono, na.rm=TRUE),1)
assim
curtose <- round(kurtosis(Horas.de.sono, na.rm=TRUE),1)
curtose

# Tabela 1
Valores <- c(nn,media,mediana,desvpadrao,variacao,minimo,maximo,primeiroqua,tercqua,assim,curtose)
Medidas <- c("N", "Media", "Mediana", "Desvio padrao", "Variancia", "Minimo", "Maximo", "Primeiro Quartil", "Terceiro Quartil", "Assimetria","Curtose")
tabela <- data.frame(Medidas, Valores)
flex_tab <- flextable(tabela)
flex_tab <- bg(flex_tab, bg ="light blue", part="header")
flex_tab

# Estatisticas sobre Tempo de Ecrã
nn2 <- length(Tempo.ecra)
nn2
media2 <- round(mean(Tempo.ecra, na.rm=TRUE),1)
media2
mediana2 <-  median(Tempo.ecra, na.rm=TRUE)
mediana2
desvpadrao2 <- round(sd(Tempo.ecra, na.rm=TRUE),1)
desvpadrao2
variacao2 <- round(var(Tempo.ecra, na.rm=TRUE),1)
variacao2
minimo2 <- min(Tempo.ecra, na.rm=TRUE)
minimo2
maximo2 <- max(Tempo.ecra, na.rm=TRUE)
maximo2
primeiroqua2 <- round(quantile(Tempo.ecra, 0.25, na.rm=TRUE),1)
primeiroqua2
tercqua2 <- round(quantile(Tempo.ecra, 0.75, na.rm=TRUE),1)
tercqua2
assim2 <- round(skewness(Tempo.ecra, na.rm=TRUE),1)
assim2
curtose2 <- round(kurtosis(Tempo.ecra, na.rm=TRUE),1)
curtose2

# Tabela 2
Valores <- c(nn2,media2,mediana2,desvpadrao2,variacao2,minimo2,maximo2,primeiroqua2,tercqua2,assim2,curtose2)
Medidas <- c("N", "Media", "Mediana", "Desvio padrao", "Variancia", "Minimo", "Maximo", "Primeiro Quartil", "Terceiro Quartil", "Assimetria","Curtose")
tabela <- data.frame(Medidas, Valores)
flex_tab <- flextable(tabela)
flex_tab <- bg(flex_tab, bg ="light green", part="header")
flex_tab

cd$Horas.de.sono <- factor(cd$Horas.de.sono)

p <- ggplot(cd, aes(x = Horas.de.sono, y = Tempo.ecra)) + 
  geom_violin(trim=FALSE, fill='blue', color="blue") +
  theme_minimal() +
  labs(x = "Horas de sono", y = "Tempo de ecrã")

# Género
tab_genero <- sort(table(cd$Género), decreasing = TRUE)
barplot(tab_genero, col = "lightblue", main = "Gênero", ylim=c(0,600))

# Distrito
tab_distrito = table(cd$Distrito)
tab_distrito

flex_tab <- round(prop.table(tab_distrito)*100,2)
flex_tab

barplot(sort(tab_distrito,decreasing = TRUE), col = "lightgreen", main = "Distrito", ylim=c(0,350))

# Nível Escolar
tab_nivel_escolar = table(cd$Nível.escolar.do.EN)
tab_nivel_escolar

barplot(sort(tab_nivel_escolar,decreasing = TRUE), col = "lightgreen", main = "Nível escolar do Encarregado de Educação", ylim=c(0,500))

# Dificuldade em tomar iniciativa: tabela
tab_tomar_iniciativa <- table(cd$Dificuldade.em.tomar.iniciativa)
tab_tomar_iniciativa 

aa1 <- as.numeric(tab_tomar_iniciativa)[1]
aa2 <- as.numeric(tab_tomar_iniciativa)[2]
aa3 <- as.numeric(tab_tomar_iniciativa)[3]
aa4 <- as.numeric(tab_tomar_iniciativa)[4]
Dificuldade.em.tomar.iniciativa <- c("Não se aplica a mim", "Aplica-se algumas vezes", "Aplica-se muitas vezes", "Aplica-se muita parte do meu tempo")
N <- c(aa1,aa2,aa3,aa4)
table_dificuldade_iniciativa <- data.frame(Dificuldade.em.tomar.iniciativa, N)
ftab_dificuldade_iniciativa <- flextable(table_dificuldade_iniciativa)
ftab_dificuldade_iniciativa <- set_header_labels(ftab_dificuldade_iniciativa, Dificuldade.em.tomar.iniciativa = "Dificuldade em tomar iniciativa", N = "N")
ftab_dificuldade_iniciativa <- bg(ftab_dificuldade_iniciativa, bg ="lightblue", part="header")
ftab_dificuldade_iniciativa <- autofit(ftab_dificuldade_iniciativa) 
ftab_dificuldade_iniciativa

# Tabela para as seguintes variáveis: "Dificuldade Iniciativa", Expectativa do futuro", "Depressão", "Pouco entusiasmo", "Valor Pessoal" e "Pensamentos Suicidas".
a1 <- as.numeric(table(cd$Dificuldade.em.tomar.iniciativa))[1]
a2 <- as.numeric(table(cd$Dificuldade.em.tomar.iniciativa))[2]
a3 <- as.numeric(table(cd$Dificuldade.em.tomar.iniciativa))[3]
a4 <- as.numeric(table(cd$Dificuldade.em.tomar.iniciativa))[4]

b1 <- as.numeric(table(cd$Expectativa.de.futuro))[1]
b2 <- as.numeric(table(cd$Expectativa.de.futuro))[2]
b3 <- as.numeric(table(cd$Expectativa.de.futuro))[3]
b4 <- as.numeric(table(cd$Expectativa.de.futuro))[4]

c1 <- as.numeric(table(cd$Depressão))[1]
c2 <- as.numeric(table(cd$Depressão))[2]
c3 <- as.numeric(table(cd$Depressão))[3]
c4 <- as.numeric(table(cd$Depressão))[4]

d1 <- as.numeric(table(cd$Pouco.entusiasmo))[1]
d2 <- as.numeric(table(cd$Pouco.entusiasmo))[2]
d3 <- as.numeric(table(cd$Pouco.entusiasmo))[3]
d4 <- as.numeric(table(cd$Pouco.entusiasmo))[4]

e1 <- as.numeric(table(cd$Valor.pessoal))[1]
e2 <- as.numeric(table(cd$Valor.pessoal))[2]
e3 <- as.numeric(table(cd$Valor.pessoal))[3]
e4 <- as.numeric(table(cd$Valor.pessoal))[4]

f1 <- as.numeric(table(cd$Pensamentos.suicidas))[1]
f2 <- as.numeric(table(cd$Pensamentos.suicidas))[2]
f3 <- as.numeric(table(cd$Pensamentos.suicidas))[3]
f4 <- as.numeric(table(cd$Pensamentos.suicidas))[4]

# Construir tabela
Dificuldade.Iniciativa <- c(a1, a2, a3, a4)
Expectativa.Futuro <- c(b1, b2, b3, b4)
Depressão <- c(c1, c2, c3, c4)
Pouco.Entusiasmo <- c(d1, d2, d3, d4)
Valor.Pessoal <- c(e1, e2, e3, e4)
Pensamentos.Suicidas <- c(f1, f2, f3, f4)

Legenda <- c("Não se aplicou a mim", "Aplicou-se algumas vezes", "Aplicou-se muitas vezes", "Aplicou-se muita parte do meu tempo")
tabela <- data.frame(Legenda, Dificuldade.Iniciativa, Expectativa.Futuro, Depressão, Pouco.Entusiasmo, Valor.Pessoal, Pensamentos.Suicidas)
flex_tab <- flextable(tabela)
flex_tab <- set_header_labels(flex_tab, Dificuldade.Iniciativa = "Dificuldade em tomar iniciativa", Expectativa.Futuro = "Expectativa de Futuro", Depressão = "Depressão", Pouco.Entusiasmo = "Pouco entusiasmo", Valor.Pessoal = "Valor pessoal", Pensamentos.Suicidas = "Pensamentos suicidas")
flex_tab <- bg(flex_tab, bg ="orange", part="header")
flex_tab
