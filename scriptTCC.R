rm(list=ls())

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
GVar_hp <- hpfilter(DespesaTotal, freq = 14400, type=c("lambda"), drift = FALSE)
YVar_hp <- hpfilter(ReceitaTotal, freq = 14400, type=c("lambda"), drift = FALSE)

#Definição das varáveis 
b <- DLSP
s <- ResultadoPrimario
GVar <- GVar_hp$cycle
YVar <- YVar_hp$cycle

#Organização em um data frame
df_dadosTCC <- data.frame(date, s, b, GVar, YVar)

#Função para criar o lag da variável DLSP
shift<-function(x,shift_by){
  stopifnot(is.numeric(shift_by))
  stopifnot(is.numeric(x))
  
  if (length(shift_by)>1)
    return(sapply(shift_by,shift, x=x))
  
  out<-NULL
  abs_shift_by=abs(shift_by)
  if (shift_by > 0 )
    out<-c(tail(x,-abs_shift_by),rep(NA,abs_shift_by))
  else if (shift_by < 0 )
    out<-c(rep(NA,abs_shift_by), head(x,-abs_shift_by))
  else 
    out<-x
  out
}
df_dadosTCC$blag <- shift(df_dadosTCC$b,-1)
df_dadosTCC$time <- c(1:nrow(df_dadosTCC))
attach(df_dadosTCC)

#lag da data
date_df <- data.frame(date)
date_lag <- date_df[-1,]

# Criando o modelo com o lag para evitar endogeneidade
modelo <- s ~ blag + GVar + YVar + s(time, by=blag)

#Usando o gam para spline
modeloP <- gam(modelo, data = df_dadosTCC)

#Estatística de teste
summary(modeloP)
#Teste de Durbin-watson
dwtest(modeloP)
#Teste Shapiro
shapiro.test(modeloP$residuals)


########################## plots ###################################

#Plot da DLSP
ggplot(df_dadosTCC, 
      aes(x=date, 
          y=b,
          xmin = as.Date('2001-12-01', '%Y-%m-%d'),
          xmax = as.Date('2021-05-01', '%Y-%m-%d'),
          )
      ) + 
  geom_line(size = 1) +
  scale_x_date(date_labels = '%b/%Y', date_breaks = '12 months') +
  theme(axis.text.x = element_text(angle=45, hjust=1.)) +
  #coord_cartesian(xlim = c('12/01','05/21')) +
  labs(x = 'Tempo',
       y = 'DLSP(%)')

#Plot do Resultado Primário
ggplot(df_dadosTCC, 
       aes(x=date, 
           y=s,
           xmin = as.Date('2001-12-01', '%Y-%m-%d'),
           xmax = as.Date('2021-05-01', '%Y-%m-%d'),
       )
) + 
  geom_line(size = 1) +
  scale_x_date(date_labels = '%b/%Y', date_breaks = '12 months') +
  theme(axis.text.x = element_text(angle=45, hjust=1.)) +
  #coord_cartesian(xlim = c('12/01','05/21')) +
  labs(x = 'Tempo',
       y = 'Resultado Primário (%)') +
  geom_smooth(method = 'gam', se = FALSE )


# Plots testes do modelo estimado
plot(modeloP, pages=1, residual=TRUE)
plot(modeloP, pages=1, seWithMean=TRUE, xlab = 'Data')
plot(modeloP,pages=1,scheme=1,unconditional=TRUE)



