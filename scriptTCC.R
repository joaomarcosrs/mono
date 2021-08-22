rm(list = ls())

#if(!require(readxl))
#    install.packages('readxl')
#if(!require(tidyverse))
#  install.packages('tidyverse')
#if(!require(mFilter))
#  install.packages('mFilter')
#if(!require(pspline))
#  install.packages('pspline')
#if(!require(npreg))
#  install.packages('npreg')
#if(!require(ggplot2))
#  install.packages('ggplot2')
#if(!require(itsadug))
#  install.packages('itsadug')
#if(!require(visreg))
#  install.packages('visreg')
#if(!require(plyr))
#  install.packages('plyr')

########################### Pacotes #########################################
library(readxl)
library(tidyverse)
library(mFilter)
library(pspline)
library(npreg)
library(ggplot2)
library(mgcv)
library(lmtest)
library(itsadug)
library(visreg)
library(plyr)


#Leitura dos dados em xlsx
dadosTCC <- read_excel('Dados.xlsx')
attach(dadosTCC)

# Criando as classes
date <- as.Date(Date)
class(ResultadoPrimario)
class(DLSP)
class(ReceitaTotal)
class(DespesaTotal)
class(date)

#Frequencia HP-filter
#640########Hodrick e Prescott (1997, p.6)
#14400######Ahumada e  Garegnani,  1999; Balcilar,  2009
#129600#####Ravn e Uhlig (2002)


#Remoção da tendência das séries DLSP e Resultado Primário
GVar_hp <-
  hpfilter(
    DespesaTotal,
    freq = 14400,
    type = c("lambda"),
    drift = FALSE
  )
YVar_hp <-
  hpfilter(
    ReceitaTotal,
    freq = 14400,
    type = c("lambda"),
    drift = FALSE
  )

#Definição das varáveis
b <- DLSP
s <- ResultadoPrimario
GVar <- GVar_hp$cycle
YVar <- YVar_hp$cycle

#Organização em um data frame
df_dadosTCC <- data.frame(date, s, b, GVar, YVar)

#Função para criar o lag da variável DLSP,time e data
shift <- function(x, shift_by) {
  stopifnot(is.numeric(shift_by))
  stopifnot(is.numeric(x))
  
  if (length(shift_by) > 1)
    return(sapply(shift_by, shift, x = x))
  
  out <- NULL
  abs_shift_by = abs(shift_by)
  if (shift_by > 0)
    out <- c(tail(x, -abs_shift_by), rep(NA, abs_shift_by))
  else if (shift_by < 0)
    out <- c(rep(NA, abs_shift_by), head(x, -abs_shift_by))
  else
    out <- x
  out
}
df_dadosTCC$blag <- shift(df_dadosTCC$b, -1)
df_dadosTCC$time <- c(1:nrow(df_dadosTCC))
attach(df_dadosTCC)

#lag da data
#date_df <- data.frame(date)
#date_lag <- date_df[-1,]


# Criando o modelo com o lag para evitar endogeneidade
modelo <- s ~ blag + GVar + YVar + s(time, by=blag)

#Usando o gam para spline
modeloP <- gam(modelo, data = df_dadosTCC)
attach(modeloP)


#Estatística de teste
summary(modeloP)
#Teste de Durbin-watson
dwtest(modeloP)
#Teste Shapiro
shapiro.test(modeloP$residuals)




########################## plots ###################################

#Plot da DLSP
ggplot(df_dadosTCC,
       aes(
         x = date,
         y = b,
         xmin = as.Date('2001-12-01', '%Y-%m-%d'),
         xmax = as.Date('2021-05-01', '%Y-%m-%d'),
       )) +
  geom_line(size = 1) +
  scale_x_date(date_labels = '%b/%Y', date_breaks = '12 months') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1.)) +
  #coord_cartesian(xlim = c('12/01','05/21')) +
  labs(x = 'Tempo',
       y = 'DLSP(%)')

#Plot do Resultado Primário
ggplot(df_dadosTCC,
       aes(
         x = date,
         y = s,
         xmin = as.Date('2001-12-01', '%Y-%m-%d'),
         xmax = as.Date('2021-05-01', '%Y-%m-%d'),
       )) +
  geom_line(size = 1) +
  scale_x_date(date_labels = '%b/%Y', date_breaks = '12 months') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1.)) +
  #coord_cartesian(xlim = c('12/01','05/21')) +
  labs(x = 'Tempo',
       y = 'Resultado Primário (%)') +
  geom_smooth(method = 'gam', se = FALSE)

#plot Coeficiente de reação fiscal
ggplot(df_dadosTCC, aes(x = seq_data, y = modeloP)) +
  geom_line(size = 1) +
  scale_x_date(date_labels = '%b/%Y', date_breaks = '12 months') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1.)) +
  #coord_cartesian(xlim = c('12/01','05/21')) +
  labs(x = 'Tempo',
       y = 'Coeficiente')


# Plot do coeficiente de reação
seq_data <- data.frame(dates = c('2001-12-01', '2003-01-01', '2004-02-01', '2005-03-01', '2006-04-01',
                                 '2007-05-01', '2008-06-01', '2009-07-01', '2010-08-01', '2011-09-01',
                                 '2012-10-01', '2013-11-01', '2014-12-01', '2016-01-01', '2017-02-01',
                                 '2018-03-01', '2019-04-01', '2020-05-01'),
                       values = 1:18)

data_new <- seq_data                                          
data_new$dates <- as.Date(data_new$dates)                 
data_new <- data_new[order(data_new$dates), ]             
data_new

plot(modeloP, xaxt = 'n', yaxt='n', xlab = ' ')
mtext('Data', side = 1, line = 3)
xtick <- seq(1, 234, by = 13)
axis(1, at = xtick, labels = format(data_new$dates, '%b\n%Y'), las = 1, cex.axis = .8)
axis(2, las=1, cex.axis=.8)






