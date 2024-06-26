

rm(list=ls())

## Preprocess data, write TAF data tables

## Before:la.txt,lw.txt,ln.txt,dn.txt,dw.txt,sw.txt,mo.txt,nm.txt,pf.txt,pm.txt,tun.txt
## After:sol7fg_stock.RData (stock and tun), sol7fg_data.RData (SAM data object) and TAF tables

taf.library(TMB)
taf.library(stockassessment)
taf.library(viridis)

library(icesAdvice)
library(icesTAF)
#library(stockassessment)
#library(TMB)
#library(viridis)
library(reshape2)
library(plyr) 
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape)
library(devtools)
#library(ggloop)
library(multipanelfigure)
library(flextable)
library(FLCore)

#setwd("C:/Users/snimmegeers/OneDrive - ILVO/Development/RStudio/D1SCI/VISBIO/ndgp.ices.assess.sol7fg/WGCSE 2024/TAF/")
#getwd()

source("utilities_read_functions.R")

mkdir("data")


#  Get catch weight for Rivard calculation (done outside of R) of stock weights
## Read original data
file_names<-c("dn.txt","dw.txt", "ln.txt", "lw.txt")
names(file_names)  <- gsub(".txt","", file_names)
stock_data     <- lapply(file_names[1:4], read.ices.from.disk, stock_input_path = file.path("bootstrap/initial/data/"))

## Restrict info to minimum age
stock_data[["dn"]]<-stock_data[["dn"]][,-1] 
stock_data[["dw"]]<-stock_data[["dw"]][,-1] 

## Add catch numbers and weights
if(!any(names(stock_data)=="cn")){
  stock_data[["cn"]] <- stock_data[["ln"]] + stock_data[["dn"]]
  stock_data[["cw"]] <- (stock_data[["dw"]] * stock_data[["dn"]] + stock_data[["lw"]] * stock_data[["ln"]])/ (stock_data[["dn"]] + stock_data[["ln"]])
  # fill missing data (in case no discards were registered) with landing data
  stock_data[["cw"]][is.na(stock_data[["cw"]])] <- stock_data[["lw"]][is.na(stock_data[["cw"]])]
}

cw<-stock_data[["cw"]]
write.csv(cw,"data/cw.csv")

#  Preprocess data and create samdata
## Read original data
file_names<-c("dn.txt","dw.txt","la.txt", "ln.txt", "lw.txt", "mo.txt","nm.txt","pf.txt","pm.txt","sw.txt")
names(file_names)  <- gsub(".txt","", file_names)
stock_data     <- lapply(file_names[1:10], read.ices.from.disk, stock_input_path = file.path("bootstrap/initial/data/"))

## Restrict info to minimum age
stock_data[["dn"]]<-stock_data[["dn"]][,-1] 
stock_data[["dw"]]<-stock_data[["dw"]][,-1] 

## check if old discard data is available...
plot(rowSums(stock_data[["lw"]] * stock_data[["ln"]]),type="l",ylim=c(0,2000))
lines(rowSums(stock_data[["dw"]] * stock_data[["dn"]]),type="l")

## Restrict info to plus group
min_age    <- 1
plus_group <- 10

## Make sam_data object
sam_data <- list()

sam_data$dn              <- stock_data$dn[,min_age:plus_group]
sam_data$dn[,plus_group] <- rowSums(stock_data$dn[,plus_group:ncol(stock_data$dn)])
sam_data$dw              <- stock_data$dw[,min_age:plus_group]
sam_data$dw[,plus_group] <- rowSums(stock_data$dw[,plus_group:ncol(stock_data$dw)]  * sweep(stock_data$dn[,plus_group:ncol(stock_data$dn)],1, FUN = "/", rowSums(stock_data$dn[,plus_group:ncol(stock_data$dn)],na.rm = T)),na.rm = T)

sam_data$ln              <- stock_data$ln[,min_age:plus_group]
sam_data$ln[,plus_group] <- rowSums(stock_data$ln[,plus_group:ncol(stock_data$ln)])
sam_data$lw              <- stock_data$lw[,min_age:plus_group]
sam_data$lw[,plus_group] <- rowSums(stock_data$lw[,plus_group:ncol(stock_data$lw)]  * sweep(stock_data$ln[,plus_group:ncol(stock_data$ln)],1, FUN = "/", rowSums(stock_data$ln[,plus_group:ncol(stock_data$ln)],na.rm = T)),na.rm = T)

stock_data$cn            <- stock_data$ln + stock_data$dn
stock_data$cw            <- (stock_data$dw * stock_data$dn + stock_data$lw * stock_data$ln) / (stock_data$dn + stock_data$ln)
stock_data$cw[is.na(stock_data$cw)] <- stock_data$lw[is.na(stock_data$cw)]       
sam_data$cn              <- stock_data$cn[,min_age:plus_group]
sam_data$cn[,plus_group] <- rowSums(stock_data$cn[,plus_group:ncol(stock_data$cn)])
sam_data$cw              <- stock_data$cw[,min_age:plus_group]
sam_data$cw[,plus_group] <- rowSums(stock_data$cw[,plus_group:ncol(stock_data$cw)]  * sweep(stock_data$cn[,plus_group:ncol(stock_data$cn)],1, FUN = "/", rowSums(stock_data$cn[,plus_group:ncol(stock_data$cn)],na.rm = T)),na.rm = T)

sam_data$sw              <- stock_data$sw[,min_age:plus_group]  
sam_data$sw[,plus_group] <- rowSums(stock_data$sw[,plus_group:ncol(stock_data$sw)]  * sweep(stock_data$cn[,plus_group:ncol(stock_data$cn)],1, FUN = "/", rowSums(stock_data$cn[,plus_group:ncol(stock_data$cn)],na.rm = T)),na.rm = T)

sam_data$mo              <- stock_data$mo[,min_age:plus_group]
sam_data$nm              <- stock_data$nm[,min_age:plus_group]
sam_data$pf              <- stock_data$pf[,min_age:plus_group]
sam_data$pm              <- stock_data$pm[,min_age:plus_group]

sam_data$lf              <- sam_data$ln / sam_data$cn # Calculate the landing proportion
sam_data$lf[is.na(sam_data$lf)] <- 0 # SAM does not handle NA's -> set to zero


# Adjust stock weights for age 1 to the minimum of age 2 (WKFLATNSCS2020 decision) 
min(sam_data$sw[,1])
min(sam_data$sw[,2])
sam_data$sw[,1] <- min(sam_data$sw[,2])

# Get the tuning fleets
sam_data$tun     <- read.ices(file.path("bootstrap/initial/data/tun.txt"))

# Save the stock object 
save(sam_data,file="data/sol7fg_stock.RData")

## Prepare tables for export
landings <- xtab2taf(stock_data[["la"]])
landings <-landings[,1:2]
names(landings)[2]<-"weight"
save(landings,file="data/landings.RData")

wlandings <- t(sam_data[["lw"]])
latage <- t(sam_data[["ln"]])
wdiscards <- t(sam_data[["dw"]])
datage <- t(sam_data[["dn"]])
wcatch <- t(sam_data[["cw"]])
catage <- t(sam_data[["cn"]])
wstock <- t(sam_data[["sw"]])
maturity <- xtab2taf(sam_data[["mo"]])
propf <- xtab2taf(sam_data[["pf"]])
propm <- xtab2taf(sam_data[["pm"]])
natmort <- xtab2taf(sam_data[["nm"]])
lfrac <- xtab2taf(sam_data[["lf"]])
UK_BTS<-xtab2taf(sam_data$tun[[1]])
BE_CBT_1<-xtab2taf(sam_data$tun[[2]])
BE_CBT_2<-xtab2taf(sam_data$tun[[3]])
BE_CBT_3<-xtab2taf(sam_data$tun[[4]])
UK_CBT_1<-xtab2taf(sam_data$tun[[5]])
UK_CBT_2<-xtab2taf(sam_data$tun[[6]])

## Write TAF tables to data directory -> under TAF/bootstrap
write.taf(landings, "data/landings.csv")
write.taf(wlandings, "data/wlandings.csv")
write.taf(latage, "data/latage.csv")
write.taf(wdiscards, "data/wdiscards.csv")
write.taf(datage, "data/datage.csv")
write.taf(wcatch, "data/wcatch.csv")
write.taf(catage, "data/catage.csv")
write.taf(wstock, "data/wstock.csv")
write.taf(maturity, "data/maturity.csv")
write.taf(propf, "data/propf.csv")
write.taf(propm, "data/propm.csv")
write.taf(natmort, "data/natmort.csv")
write.taf(lfrac, "data/lfrac.csv")
write.taf(UK_BTS, "data/UK_BTS.csv")
write.taf(BE_CBT_1, "data/BE_CBT_1.csv")
write.taf(BE_CBT_2, "data/BE_CBT_2.csv")
write.taf(BE_CBT_3, "data/BE_CBT_3.csv")
write.taf(UK_CBT_1, "data/UK_CBT_1.csv")
write.taf(UK_CBT_2, "data/UK_CBT_2.csv")

## SETUP SAM DATA OBJECT

# set catch numbers age 1 and 2 to NA  for 1971-2003
sam_data$cn
sam_data$ln
sam_data$dn
sam_data$cn[1:33,1:2]  <- NA

#sam_data$tun$`BE-CBT_06-22`[17,1]<- NA

dat.sol7fg     <- setup.sam.data(
  surveys = sam_data$tun,                # tuning fleets
  residual.fleet = sam_data$cn,          # catch numbers-at-age
  prop.mature = sam_data$mo,             # maturity ogive
  stock.mean.weight = sam_data$sw,       # stock weights
  catch.mean.weight = sam_data$cw,       # catch weights
  dis.mean.weight = sam_data$dw,         # discard weights
  land.mean.weight = sam_data$lw,        # landing weights
  prop.f = sam_data$pf,                  # proportion fished before spawning
  prop.m = sam_data$pm,                  # proportion natural mortality before spawning
  natural.mortality = sam_data$nm,       # natural mortality
  land.frac = sam_data$lf )              # landing proportion

#Save the SAM data object 
save(dat.sol7fg,file="data/sol7fg_data.RData")


totDis<-rowSums(sam_data[["dn"]]*sam_data[["dw"]])
totDis<-as.data.frame(round(totDis,0))
names(totDis)[1]<-"weight"
totDis$Year<-dat.sol7fg$years
totDis<-totDis[,2:1]
rownames(totDis) <- NULL
write.csv(totDis,"data/totDis.csv", row.names =FALSE)
save(totDis,file="data/totDis.RData")




