library(R2jags)
library(coda)
library(parallel)
library(foreach)
library(doParallel)
library(gplots) #bivariate empirical CI
library(datalimited2) #cmsy (Froese 2017 CMSY)
library(ggplot2)
library("rfishbase") #query Fishbase

# getwd()
# rm(list=ls())
# list.files()

# Read data (ICCAT web 2018 06 21) 
bon <- read.csv('/media/josetxu/Seagate Expansion Drive/Segurtasun_2016_20160516/2018_C/proiektuak_berri/bon_gizajoa/input/BonCatch_ICCAT20180621.csv',
                header=TRUE)

# Exploratory graph
ggplot(data=bon,aes(x=year,y=catch))+
  geom_line(size=0.5)+
  geom_smooth(colour='red',se=FALSE)+
  scale_x_continuous(breaks=seq(1950,2016,4),
                     limits=c(1950,2016))+
  scale_y_continuous(name='catch (t)',
                     breaks=seq(0,75000,5000),
                     limits=c(0,75000))+
  theme(axis.text.x = element_text(angle=20))


ggsave('/media/josetxu/Seagate Expansion Drive/Segurtasun_2016_20160516/2018_C/proiektuak_berri/bon_gizajoa/tex/img/bon_catch.pdf')

# Get data from Fish base
fish <- validate_names(c("Sarda sarda"))
resil <- stocks(fish, fields="Resilience")
bon$resilience <- resil$Resilience

# --------------------------------------------------------
# CMSY

# Parámetros
# resilence (species intrinsic growth rate, r)
# \textbf{scenario B: r.low;r.hi (NA;NA) (FishBase; BON: 'Medium';0.2;0.8)}

# depletion rates B/K at the start of the series 
# scenario: stb.low;stb.hi 0.7;0.9
# \textbf{scenario: stb.low;stb.hi 0.7;0.9}

# user-specified year of intermediate biomass (optional)
# By default CMSY uses an intermediate depletion rate
# (10 years before the end of the time series, with values 0.2 to 0.6).
# scenario: int.yr;intb.low;intb.hi: year 2000;0.1;0.9.

# A user-specified prior on biomass relative to unfished biomass at the end
# of the catch time series (optional)
# \textbf{endb.low,endb.hi: 0.2;0.7}.

# errors:
# \textbf{observation: error variance for the catch data 0.1 (default)}
# \textbf{process: variance of the process error 0.1 (default)}
# Brunel hace un análisis de sensibilidad con 0.2
# (no se puede implementar con cmsy2, que utiliza en valor por defecto: 0.1)

# Resultados (cmsy2{datalimited2})

year  <- bon$year
catch <- bon$catch

outputBONrFinal <-  cmsy2(
  year = year,
  catch = catch,
  resilience = 'Medium',
  r.low = NA, r.hi = NA,
  stb.low = 0.7, stb.hi = 0.9,
  int.yr = 2006,
  intb.low = 0.1, intb.hi = 0.9,
  endb.low = 0.2, endb.hi = 0.7,
  verbose = T)





