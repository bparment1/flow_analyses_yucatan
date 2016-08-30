#################################    FLOW RESEARCH  #######################################
########################### SPACE-TIME QUINTINARO PRODUCT DATABASE  #############################################
#This script explores and analyzes data from the Quintinaro database of flow (food, wood etc).
#Data was collected by Marco Millones
#
#AUTHOR: Benoit Parmentier                                                                       
#DATE CREATED:07/11/2016 
#DATE MODIFIED: 08/30/2016
#
#PROJECT: Flow, land cover change with Marco Millones
#COMMIT: Adding land conversion area
#
##################################################################################################
#
###Loading r library and packages

library(raster)                            # loading the raster package
library(gtools)                            # loading ...
library(sp)                                # spatial objects in R
library(gplots)                            # 
library(rgdal)                             # gdal driver for R
library(RColorBrewer)                      # color scheme, palettes used for plotting
library(gdata)                             # read different format (including .xlsx)
library(plotrix)                           # plot options and functions including plotCI
library(rasterVis)                         # raster visualization
library(gridExtra)                         # graphic package
library(latticeExtra)                      # graphic package
library(colorRamps)                        # contains matlab.like palette
library(lsr)                               #
library(psych)                             # PCA
library(GPArotation)                       # PCA rotation
library(zoo)                               # Time series object and functions
library(xts)                               # Time series object and functions
library(remote)                            # EOT implementation in R/cpp
library(XML)                               # HTML funcitons
library(plyr)

#################################################
###### Functions  used in the script  ##########

create_dir_fun <- function(outDir,out_suffix){
  #if out_suffix is not null then append out_suffix string
  if(!is.null(out_suffix)){
    out_name <- paste("output_",out_suffix,sep="")
    outDir <- file.path(outDir,out_name)
  }
  #create if does not exists
  if(!file.exists(outDir)){
    dir.create(outDir)
  }
  return(outDir)
}

load_obj <- function(f){
  env <- new.env()
  nm <- load(f, env)[1]
  env[[nm]]
}

#infile1_function <- file.path("/home/bparmentier/Google Drive/Papers_writing_MEOT/R_scripts/",
#                              "PCA_EOT_comparison_data_update_function_07152016.R")
#source(infile1_function)

#############################################
######## Parameters and arguments  ########

#Input file name with the flow data
filename_flow <- "MO1-9_ALL.txt"

CRS_WGS84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0" #Station coords WGS84 # CONST 2
proj_str<- CRS_WGS84 #param 2
CRS_reg <- CRS_WGS84 # PARAM 3

file_format <- ".txt" #PARAM 4
NA_value <- -9999 #PARAM5
NA_flag_val <- NA_value #PARAM6
out_suffix <-"flow_08302016" #output suffix for the files and ouptu folder #PARAM 7
create_out_dir_param=TRUE #PARAM8
num_cores <- 4 #PARAM 9

inDir <- "/home/bparmentier/Google Drive/000_Flow_and_LUD_research/Quintana_Roo_Research/Data"
setwd(inDir)

outDir <- inDir

create_outDir_param = TRUE

#Create output directory

if(create_outDir_param==TRUE){  
  outDir <- create_dir_fun(outDir,out_suffix)
  setwd(outDir)
}else{
  setwd(outDir) #use previoulsy defined directory
}

########################################################
##############  Start of th script  ##############

###########################################
### PART 0: READ IN DATASETS RELATED TO FLOWS 


#tb <- read.table(file.path(inDir,filename_flow),sep=",")
#tb <- read.table(file.path(inDir,filename_flow),fill=T,sep=",")
tb <- read.table(file.path(inDir,filename_flow), 
           sep=',', quote=NULL,fill=TRUE, comment='', 
           #as.is=F,
           stringsAsFactors = F,
           header=TRUE)

names(tb)

### First add date fields for each flow item/transaction

l_dates <- lapply(1:length(tb$FECHA),
                  FUN=function(i,x){date_item_str <- strsplit(x[[i]]," ")[[1]][1]; as.character(as.Date(date_item_str ,format="%m/%d/%Y"))},x=tb$FECHA)
dates_range <- as.character(l_dates)

##Convert dates range to year, month and day
date_year <- strftime(dates_range, "%Y")
date_month <- strftime(dates_range , "%m") # current month of the date being processed
date_day <- strftime(dates_range , "%d")

### Add dates
tb$dates <- dates_range
tb$year <- as.numeric(date_year)
tb$month <- as.numeric(date_month)
tb$day <- as.numeric(date_day)

## Screen by date: only allow 2001 to 2009

tb <- subset(tb,tb$year> 2000 )

### Fix naming convention for Hinterland variable
tb$X1.9_ORIG_CVE_HINT[tb$X1.9_ORIG_CVE_HINT=="MEXICO"] <- "MEX" #Origin of product
tb$X1.9_DEST_CVE_HINT[tb$X1.9_DEST_CVE_HINT=="MEXICO"] <- "MEX" #Destination of product

#> unique(tb$X1.9_ORIG_CVE_HINT)
#[1] "QR"  "MEX" "GYR" NA    ""    "W"  

table(tb$X1.9_ORIG_CVE_HINT)
#[1] "QR"  "MEX" "GYR" NA    ""    "W"  

table(tb$X1.9_ORIG_CVE_HINT)

#GYR    MEX     QR      W 
#141 185025 181118  88711      2 

table(tb$X1.9_DEST_CVE_HINT)

#GYR    MEX     QR      W 
#141  41583  17261 395450     11 
x <- (table(tb$X1.9_DEST_CVE_HINT))
#barplot(formatC(x, format = "d"))
#barplot(as.integer(tb$X1.9_DEST_CVE_HINT))
barplot(x)
#histogram((tb$X1.9_DEST_CVE_HINT))
tb <- subset(tb,tb$X1.9_ORIG_CVE_HINT!="" & tb$X1.9_DEST_CVE_HINT!="")
dim(tb)
x <- (table(tb$X1.9_ORIG_CVE_HINT))
print(x)
x <- (table(tb$X1.9_DEST_CVE_HINT))
print(x)
#barplot(formatC(x, format = "d"))
#barplot(as.integer(tb$X1.9_DEST_CVE_HINT))
barplot(x)

### Now prepare the data:
#Use 
#"ORIG_CLAVE","DEST_CLAVE"
#X1.9_ORIG_CVE_HINT,X1.9_DEST_CVE_HINT

#Outflow: need to recode hinterland with: 1 if HINT is GYR, MEX, MEXICO and Inflow: Examine X1.9_ORIG_CVE_HINT and X1.9_DEST_CVE_HINT, recode 1 if: 
#  MEX-QR
#  MEXICO-QR
#  GYR-QR
#Internal consumption: QR-QR

#a) QR<->QR
#b) QR->GYR
#c) QR<-GYR
#d) QR->MX
#e) QR<-MX
#f) QR->W
#g) QR-<W


### Do a quick crosstab
xtb <- table(tb$X1.9_ORIG_CVE_HINT,tb$X1.9_DEST_CVE_HINT)
print(xtb)

#       GYR    MEX     QR      W
#GYR    153   2656 182190      3
#MEX   1232     29 179836      7
#QR   40184  14564  33122      1
#W        0      0      2      0

### For this data only these ones are allowed:
#       GYR    MEX     QR      W
#GYR      0      0      1      0
#MEX      0      0      1      0
#QR       1      1      1      1
#W        0      0      1      0

tb$ORIG_DEST_HINT <- paste(tb$X1.9_ORIG_CVE_HINT,tb$X1.9_DEST_CVE_HINT,sep="_")
unique(tb$ORIG_DEST_HINT)
table(tb$ORIG_DEST_HINT)
#[1] "QR_QR"   "MEX_QR"  "QR_GYR"  "QR_MEX"  "GYR_QR"  "GYR_MEX" "MEX_GYR" "GYR_GYR" "MEX_MEX" "MEX_W"  
#[11] "GYR_W"   "QR_W"    "W_QR"
#only 13 of the combination are realized compared to the crosstab

## Reclass 7 options out of 16, this is centering on options from QR point of view only!!!

#QR_QR <- a #internal consuption: C
#QR_GYR <- b #QR->GYR outflow: B
#GYR_QR <- c # inflow: A 
#QR_MEX <- d #d) QR->MX: B (outflow)
#MEX_QR <- e #e) QR<- MX: A (inflow) 
#QR_W <- f #f) QR->W: B (outflow)
#W_QR <- g #g) QR-<W: A (inflow)

##Using revalue from plyr package!
tb$flow_types <- revalue(tb$ORIG_DEST_HINT,
                         c("QR_QR"  = "a", # internal consuption: C
                           "QR_GYR" = "b", # QR->GYR outflow: B
                           "GYR_QR" = "c", # inflow: A 
                           "QR_MEX" = "d", # d) QR->MX: B (outflow)
                           "MEX_QR" = "e", # e) QR<- MX: A (inflow) 
                           "QR_W" = "f",   # f) QR->W: B (outflow)
                           "W_QR" = "g")) # g) QR-<W: A (inflow)
#compare:a, b-c,d-e,f-g
#compare:internal,GYR,MEX,W

#Reclassify this and then classify in inflow and outlfow + 

tb$flow_direction <- revalue(tb$ORIG_DEST_HINT,
                             c("QR_QR"  = "C", # internal consuption: C
                               "QR_GYR" = "B", # QR->GYR outflow: B
                               "GYR_QR" = "A", # inflow: A 
                               "QR_MEX" = "B", # d) QR->MX: B (outflow)
                               "MEX_QR" = "A", # e) QR<- MX: A (inflow) 
                               "QR_W" = "B",   # f) QR->W: B (outflow)
                               "W_QR" = "A")) # g) QR-<W: A (inflow)

#test <- tb[tb$flow_direction=="GYR_GYR",]


## Extraction is B+C (defined as comsumption of from land produced locally)
#this is internal production which is exported (B) or consumed locally (C)
tb$extraction[tb$flow_direction=="B" | tb$flow_direction=="C"] <- 1 
tb$extraction[tb$flow_direction=="A"] <- 0
table(tb$extraction)

## Local Consumption is A+C (defined as comsumption of from import and locally produced food)
#this is external produciton of food which is imported (A) or consumed locally (C)
tb$consumption[tb$flow_direction=="A" | tb$flow_direction=="C"] <- 1 
tb$consumption[tb$flow_direction=="B"] <- 0
table(tb$consumption)

### Consider only flows within  Quintano Roo

tb <- subset(tb,tb$flow_direction%in%c("A","B","C"))

barplot(table(tb$flow_direction))
 
###########################################
### PART 1: SCREEN DATA VALUES FOR INCONSISTENCIES AND TO SELECT BASE PRODUCTS


######################################
### We need to subset by product

#test <-subset(tb,tb$NOMPRODUCT=="BOVINOS/CARNE")
#test <- subset(tb,tb$SECCION=="AGRICOLA")
#test <- subset(test,test$flow_types%in% c("a","b","c"))
#test$y <- as.numeric(test$NV_CANT)

#NV_CANT
#tb$NOMPRODUCT
#tb$SECCION
#tb$NV_UMEDIDA

### Screening:

#exclude this values
#tb$CODPROD=86 (huevos), 95 (cormenas), 96 (miel), 97 (cerra=wax), 183 (pasterized eggs), 200 (quail eggs)

codes_to_remove <- c(86,95,96,97,183,200)

tb <- tb[!tb$CODPROD %in% codes_to_remove,]
dim(tb)

codes_to_keep <- c("TONELADA","CABEZA")

tb <- tb[tb$NV_UMEDIDA %in% codes_to_keep,]
dim(tb)

tb$product_cat[tb$SECCION=="AGRICOLA" & tb$NV_UMEDIDA=="TONELADA"] <- "agri"
tb$product_cat[tb$SECCION=="PECUARIO" & tb$NV_UMEDIDA=="TONELADA"] <- "meat"
tb$product_cat[tb$SECCION=="PECUARIO" & tb$NV_UMEDIDA=="CABEZA"] <- "livestock"

table(tb$product_cat)

#Just cabeza and tonelada in NV_UMEDIDA
#If seccion=agricola and nv_medida=tolenada then agri
#if seccion= pecuario and nv_medida= tonelada then meat
#if seccion= pecuario and nv_medida = cabeza then livestock 

## 

#First make sure we have numeric values

tb$NV_CANT <- as.numeric(tb$NV_CANT)
  
tb <- tb[!is.na(tb$NV_CANT),]

tb$y <- tb$NV_CANT
pos_col <- which(names(tb)=="y")
#tb_ordered <- tb[with(tb, order(y)), ]
tb_ordered <- tb[ order(-tb[,pos_col]), ]

tb_ordered[1:100,c("NV_CANT","product_cat","flow_direction","TRANSLATION")]
tb_ordered[101:200,c("NV_CANT","product_cat","flow_direction","TRANSLATION")]
tb_ordered[201:300,c("NV_CANT","product_cat","flow_direction","TRANSLATION")]
tb_ordered[301:400,c("NV_CANT","product_cat","flow_direction","TRANSLATION")]
tb_ordered[401:500,c("NV_CANT","product_cat","flow_direction","TRANSLATION")]
tb_ordered[501:600,c("NV_CANT","product_cat","flow_direction","TRANSLATION")]
tb_ordered[601:700,c("NV_CANT","product_cat","flow_direction","TRANSLATION")]
tb_ordered[701:800,c("NV_CANT","product_cat","flow_direction","TRANSLATION")]
tb_ordered[801:900,c("NV_CANT","product_cat","flow_direction","TRANSLATION")]


table(tb_ordered[1:100,]$TRANSLATION)
table(tb_ordered[1:1000,]$TRANSLATION)
 
table(tb_ordered[1:2000,]$TRANSLATION)    
  
table(tb_ordered[1:10000,]$TRANSLATION)  

tb_ordered_agri <- subset(tb_ordered,tb$product_cat=="agri")
tb_ordered[2000,]
tb_ordered[10000,c("NV_CANT","product_cat","flow_direction","TRANSLATION")]

#### Check for agri only
tb_ordered_agri <- subset(tb_ordered,tb_ordered$product_cat=="agri")
dim(tb_ordered_agri)
tb_ordered_agri[1:30,c("NV_CANT","product_cat","flow_direction","TRANSLATION")]

barplot(table(tb_ordered_meat$TRANSLATION))

### Check for meat only

tb_ordered_meat <- subset(tb_ordered,tb_ordered$product_cat=="meat")
dim(tb_ordered_meat)
tb_ordered_meat[1:40,c("NV_CANT","product_cat","flow_direction","FECHA","TRANSLATION")]
tb_ordered_meat[1:40,c("NV_CANT","product_cat","flow_direction","FECHA","TRANSLATION")]

View(tb_ordered_meat)
#remove potential error
val <- max(tb_ordered_meat$NV_CANT) #17000,GALLINAZA CHICKEN EXCREMENT FERTILIZER FROM MEAT CHICKENS
row_to_remove <- which(tb$NV_CANT==val & tb$product_cat=="meat") 

tb  <- tb[-row_to_remove,]

#table(tb_ordered_meat$TRANSLATION)

#test_tb <- tb[tb$SECCION=="AGRICOLA" & tb$NV_UMEDIDA=="TONELADA"] 
#subset(tb,tb$)
#& (tb$NV_CANT < 1000) ]

#View(tb[tb$dates==tb_agri_by_dates[max_pos,]$dates,])

#########################################
## Exploring data extremes and time series.tb
range_dates<- range(tb$dates)

#range_dates<- range(tb_tmp$dates)
#[1] "2001-01-01" "2009-11-25"

st <- as.Date(range_dates[1])
en <- as.Date(range_dates[2])
dseq <- seq(st, en, by="day") #Creating monthly date sequence to create a time series from a data frame

#dat_old_indices <- dat
#l_dates <- (as.Date(tb_tmp$dates))

## first summarize by dates!!!
tb_by_dates <- aggregate(NV_CANT ~ product_cat + dates + flow_direction, data = tb, sum)

tb_agri_by_dates <- subset(tb_by_dates,flow_direction=="A" & product_cat== "agri")
class(tb_agri_by_dates$NV_CANT)
sum(is.na(tb_agri_by_dates$NV_CANT))
length((tb_agri_by_dates$NV_CANT))

tb_tmp_dz <- zoo(tb_agri_by_dates,as.Date(tb_agri_by_dates$dates)) #create zoo object from data.frame and date sequence object
class(tb_tmp_dz$NV_CANT)

plot(tb_tmp_dz$NV_CANT)
range(tb_tmp_dz$NV_CANT)
range(tb_agri_by_dates$NV_CANT)

max_pos <- which.max(tb_agri_by_dates$NV_CANT)
tb_agri_by_dates[max_pos,]
dim(tb[tb$dates==tb_agri_by_dates[max_pos,]$dates,])
dim(tb)
hist(tb[tb$dates==tb_agri_by_dates[max_pos,]$dates,]$NV_CANT)
plot(table(tb[tb$dates==tb_agri_by_dates[max_pos,]$dates,]$NV_CANT),type="h")

tb_tmp_dz$NV_CANT  <- as.numeric(tb_tmp_dz$NV_CANT)
plot(as.numeric(tb_tmp_dz$NV_CANT))

plot(as.numeric(tb_tmp_dz$NV_CANT) ~ dates,ylim=c(0,1000),xlim=c(1,400),type="l",
     data=tb_tmp_dz)
plot(NV_CANT ~ dates,ylim=c(0,1000),xlim=c(1,400),type="l",
     data=tb_tmp_dz)
plot(NV_CANT ~ dates,ylim=c(0,1000),xlim=c(1,400),type="l",
     data=tb_agri_by_dates)

#plot(tb_tmp_dz)
#var_x <- 1:length(l_dates)
#var_x_dz <- zoo(var_x,l_dates)

range(tb_tmp_dz$NV_CANT)
range(tb_tmp$NV_CANT,na.rm=T)

plot(tb_tmp_dz$NV_CANT)
plot(tb_tmp$NV_CANT)


###########################################
### PART 2: AGGREGATE  FLOWS BY TYPES AND DATES/YEARS
####################################

### Aggregate by year and product_cat

#2001 to 2009

test <- aggregate(NV_CANT ~ product_cat + year + flow_direction + extraction + consumption, data = tb, sum)

#   "C", # internal consuption: C
#   = "B", # QR->GYR outflow: B
#  "A", # inflow: A 

test$comp <- test$consumption*2 + test$extraction*1

## Extraction is B+C (defined as comsumption of from land produced locally)
  
### Make a loop later, this is to explore the data
direction_val <- "A" #Inflow
plot(NV_CANT~year,subset(test,test$product_cat=="livestock" & test$flow_direction==direction_val),type="b",main=paste("livestock",direction_val,sep=" "))
plot(NV_CANT~year,subset(test,test$product_cat=="meat" & test$flow_direction==direction_val),type="b",main=paste("meat",direction_val,sep=" "))
plot(NV_CANT~year,subset(test,test$product_cat=="agri" & test$flow_direction==direction_val),type="b",main=paste("agri",direction_val,sep=" "))

direction_val <- "B" #Outflow
plot(NV_CANT~year,subset(test,test$product_cat=="livestock" & test$flow_direction==direction_val),type="b",main=paste("livestock",direction_val,sep=" "))
plot(NV_CANT~year,subset(test,test$product_cat=="meat" & test$flow_direction==direction_val),type="b",main=paste("meat",direction_val,sep=" "))
plot(NV_CANT~year,subset(test,test$product_cat=="agri" & test$flow_direction==direction_val),type="b",main=paste("agri",direction_val,sep=" "))

direction_val <- "C" #Internal flow (internal consumption)
plot(NV_CANT~year,subset(test,test$product_cat=="livestock" & test$flow_direction==direction_val),type="b",main=paste("livestock",direction_val,sep=" "))
plot(NV_CANT~year,subset(test,test$product_cat=="meat" & test$flow_direction==direction_val),type="b",main=paste("meat",direction_val,sep=" "))
plot(NV_CANT~year,subset(test,test$product_cat=="agri" & test$flow_direction==direction_val),type="b",main=paste("agri",direction_val,sep=" "))

### Need to improve this code later on!!!
xyplot(NV_CANT ~ year | flow_direction,subset(test,test$product_cat=="livestock"),type="b",
       main="livestock")
xyplot(NV_CANT ~ year | flow_direction,subset(test,test$product_cat=="meat"),type="b",
       main="meat")
xyplot(NV_CANT ~ year | flow_direction,subset(test,test$product_cat=="agri"),type="b",
       main="agri")


#### Writing the table
write.table(test,file=paste("test_aggregated_data_by_flow_by_product_year_",out_suffix,".txt",sep=""),sep=",")

## Look into extraction

#xyplot(NV_CANT ~ year | extraction,test2)

test2 <- aggregate(NV_CANT ~ product_cat + year + extraction, data = test, sum)
test3 <- aggregate(NV_CANT ~ product_cat + year + consumption, data = test, sum)

extraction_val <- 1 #B+C
#plot(NV_CANT~year,subset(test2,test2$extraction=="livestock" & test$flow_direction==direction_val),type="b",main=paste("livestock",direction_val,sep=" "))
#plot(NV_CANT~year,subset(test,test$product_cat=="meat" & test$flow_direction==direction_val),type="b",main=paste("meat",direction_val,sep=" "))
direction_val <- "0" #A, inlfow
plot(NV_CANT~year,subset(test2,test2$product_cat=="livestock" & test2$extraction==direction_val),type="b",col="blue",main=paste("livestock",direction_val,sep=" "))
direction_val <- "1" #Internal flow + outflow (internal extraction)
lines(NV_CANT~year,subset(test2,test2$product_cat=="livestock" & test2$extraction==direction_val),type="b",col="red",main=paste("livestock",direction_val,sep=" "))
plot(NV_CANT~year,subset(test2,test2$product_cat=="livestock" & test2$extraction==direction_val),type="b",col="red",main=paste("livestock",direction_val,sep=" "))

#### Writing the table
write.table(test2,file=paste("test2_aggregated_data_by_exraction_by_product_year_",out_suffix,".txt",sep=""),sep=",")

#### Writing the table
write.table(test3,file=paste("test3_aggregated_data_by_comsumption_by_product_year_",out_suffix,".txt",sep=""),sep=",")


####################################################
##### PLOTTING EXTRACTION FOR EACH PRODUCT #####

xyplot(NV_CANT ~ year | extraction,subset(test2,test2$product_cat=="livestock"),type="b",
       main="livestock")

xyplot(NV_CANT ~ year | extraction,subset(test2,test2$product_cat=="meat"),type="b",
       main="meat")

xyplot(NV_CANT ~ year | extraction,subset(test2,test2$product_cat=="agri"),type="b",
       main="agri")

#range_val <- range(subset(test2,test2$product_cat=="livestock")$NV_CANT)
#plot(NV_CANT ~ year, col="blue",type="b",ylim=range_val,
#     data=subset(test2,test2$product_cat=="livestock" & test2$extraction==1) )

#lines(NV_CANT ~ year, col="red",type="b",
#      data=subset(test2,test2$product_cat=="livestock" & test2$extraction==0) )


####################################################
##### PLOTTING CONSUMPTION FOR EACH PRODUCT #####

xyplot(NV_CANT ~ year | consumption,subset(test3,test3$product_cat=="livestock"),type="b",
       main="livestock")

xyplot(NV_CANT ~ year | consumption,subset(test3,test3$product_cat=="meat"),type="b",
       main="meat")

xyplot(NV_CANT ~ year | consumption,subset(test3,test3$product_cat=="agri"),type="b",
       main="agri")


### Apply the area factor for land

### Get an idea of export by year and hinterland category:

#test <- aggregate(NV_CANT ~ product_cat + year + flow_direction + extraction + consumption, 
#                  data = subset(tb,tb$, sum)

test_all <- aggregate(NV_CANT ~ product_cat + year + flow_direction + extraction + consumption, 
                                    data = tb, sum)
                  
################## Analysis for number of truck
## Agregate for mobilization #
#if placa & fecha  are the same in 
#table(tb$NV_UMEDIDA)

#tb$IDMOVILIZA

### Date

#as.Date(tb$FECHA)
#tb$FECHA

#i <- 1

#### NOW LOOKING INTO REGRESSIONS

#tb$flow_types <- revalue(tb$ORIG_DEST_HINT,
#                         c("QR_QR"  = "a", # internal consuption: C
#                           "QR_GYR" = "b", # QR->GYR outflow: B
#                           "GYR_QR" = "c", # inflow: A 
#                           "QR_MEX" = "d", # d) QR->MX: B (outflow)
#                           "MEX_QR" = "e", # e) QR<- MX: A (inflow) 
#                           "QR_W" = "f",   # f) QR->W: B (outflow)
#                          "W_QR" = "g")) # g) QR-<W: A (inflow)

#compare:a, b-c,d-e,f-g
#compare:internal,GYR,MEX,W

tb$flow_dist_cat <- revalue(tb$flow_types,
                         c("a"  = "0", # internal consuption: C
                           "b" = "1", # QR->GYR outflow: B
                           "c" = "1", # inflow: A 
                           "d" = "2", # d) QR->MX: B (outflow)
                           "e" = "2", # e) QR<- MX: A (inflow) 
                           "f" = "3",   # f) QR->W: B (outflow)
                           "g" = "3")) # g) QR-<W: A (inflow)

tb$NV_CANT <- as.numeric(tb$NV_CANT)
tb_tmp <- subset(tb,tb$product_cat=="agri")
tb_tmp$y <- tb_tmp$NV_CANT
### This is a quick ANOVA style regression (General Linear Model)
mod <- lm(y ~ flow_dist_cat, data= tb_tmp)
summary(mod)
barplot(table(tb_tmp$flow_dist_cat),main="agri",names.arg=c("QR","GYR","MEX","W"))

mean_y <-tapply(tb_tmp$y,tb_tmp$flow_dist_cat, mean, na.rm=TRUE)
sum_y <-tapply(tb_tmp$y,tb_tmp$flow_dist_cat, sum, na.rm=TRUE)
barplot(sum_y,main="agri",names.arg=c("QR","GYR","MEX","W"))

#### livestock,

tb_tmp <- subset(tb,tb$product_cat=="livestock")
tb_tmp$y <- tb_tmp$NV_CANT

### This is a quick ANOVA style regression (General Linear Model)
mod <- lm(y ~ flow_dist_cat, data= tb_tmp)
summary(mod)
barplot(table(tb_tmp$flow_dist_cat),names.arg=c("QR","GYR","MEX"),main="livestock")

means <- sapply(tb_tmp$y, mean,na.rm=TRUE)
sum_y <-tapply(tb_tmp$y,tb_tmp$flow_dist_cat, sum, na.rm=TRUE)
barplot(sum_y,main="livestock",names.arg=c("QR","GYR","MEX"))

##########################
#### meat

tb_tmp <- subset(tb,tb$product_cat=="meat")
tb_tmp$y <- tb_tmp$NV_CANT

### This is a quick ANOVA style regression (General Linear Model)
mod <- lm(y ~ flow_dist_cat, data= tb_tmp)
summary(mod)
barplot(table(tb_tmp$flow_dist_cat),main="meat",names.arg=c("QR","GYR","MEX","MEX"))

sd_y <-tapply(tb_tmp$y,tb_tmp$flow_dist_cat, sd, na.rm=TRUE)
mean_y <-tapply(tb_tmp$y,tb_tmp$flow_dist_cat, mean, na.rm=TRUE)
sum_y <-tapply(tb_tmp$y,tb_tmp$flow_dist_cat, sum, na.rm=TRUE)
barplot(sum_y,main="meat",names.arg=c("QR","GYR","MEX","MEX"))

##########################
#### PART 4: CONVERSION OF FLOWS INTO LAND AREA

### Step 1: conversion of flow to land use

#1. isolate  'bovino' from the aggregate livestock category, but you can try including 'caprino' and 'porcino' and 'ovino' as well
#select all that contain 'bovino' or 'caprino' or 'porcino' AND 'cabezas'

#Stocking rates fo from 0.5 1 AU (head) per ha. for Mexico, tropical areas are more extensive 0.3. If we use 1 head per ha we are being generous

tb_livestock <- subset(tb,tb$product_cat=="livestock" & tb$consumption==1)
dim(tb_livestock)
table(tb_livestock$NOMPRODUCT)
names(table(tb_livestock$NOMPRODUCT))
names(table(tb_livestock$TRANSLATION))
barplot(table(tb_livestock$NOMPRODUCT),names.arg=names(table(tb_livestock$NOMPRODUCT)),las=2)

#should calves be in?
selected_livestock <- c("BOVINOS EN PIE","BOVINOS EN PIE DE CRIA","DESTETES BOVINO","BOVINOS EN PIE DE CRIA")

additional_cat1 <- c("CAPRINOS EN PIE","PORCINOS EN PIE DE CRIA","PORCINOS EN PIE","DESTETES PORCINO")
additional_cat2 <- c("vacas")

tb_to_convert <- subset(tb_livestock ,tb_livestock$NOMPRODUCT %in% selected_livestock)

table(tb_to_convert$NV_UMEDIDA) #OK all cabeza

conversion_rate <- c(0.3,0.5,1)

tb_to_convert$land_consumption1 <- tb_to_convert$NV_CANT*conversion_rate[1]
tb_to_convert$land_consumption2 <- tb_to_convert$NV_CANT*conversion_rate[2]
tb_to_convert$land_consumption3 <- tb_to_convert$NV_CANT*conversion_rate[3]

total_land_consumed1 <- sum(tb_to_convert$land_consumption1) #this is in ha
total_land_consumed2 <- sum(tb_to_convert$land_consumption2) #this is in ha
total_land_consumed3 <- sum(tb_to_convert$land_consumption3) #this is in ha

### Step 2: conversion of flow to land use

#Use the CI land cover map in the Fire analysis script. There are 3 maps
#1990, 2000 and 2007
#it is possible that you have the change map only

#FP NFP CH  use NFP + CH

#http://www.fao.org/ag/agp/agpc/doc/counprof/mexico/Mexico.htm
#http://www.fao.org/docrep/010/a0701e/a0701e00.HTM

in_dir_CI_data <- "/home/bparmentier/Google Drive/FireYuca_2016/"
data_CI_fname <- "/home/bparmentier/Google Drive/FireYuca_2016/datasets/Firedata_05182016.txt" #contains the whole dataset
state_fname <- "/home/bparmentier/Google Drive/FireYuca_2016/IN_QGIS/State_dis_from_muni.shp"
state_fname <- "/home/bparmentier/Google Drive/FireYuca_2016/IN_QGIS/State_dis_from_muni.shp"

#data_CI_fname <- "/home/bparmentier/Google Drive/FireYuca_2016/old_data/000_GYR_FIRENDVI_2000-9.txt"
#data_CI_02062016.txt
coord_names <- c("POINT_X","POINT_Y") #PARAM 11
zonal_var_name <- "state" #name of the variable to use to run the model by zone, Yucatan state here
y_var_name <- "fpnfpch"
id_name <- "yucatan_fire_pointid" #Column with the reference point id

data_df <-read.table(data_CI_fname,stringsAsFactors=F,header=T,sep=",")
#data_Hansen <-read.table(data_CI_fname,stringsAsFactors=F,header=T,sep=",")

#Create the count variable
fire_modis_col <- c("FIRE_2000","FIRE_2001","FIRE_2002","FIRE_2003","FIRE_2004","FIRE_2005","FIRE_2006","FIRE_2007")
data_df$FIRE_freq <- data_df$FIRE_2000 + data_df$FIRE_2001 + data_df$FIRE_2002 + data_df$FIRE_2003 + data_df$FIRE_2004 + data_df$FIRE_2005 + data_df$FIRE_2006 + data_df$FIRE_2007 
data_df$FIRE_intensity <- data_df$FIRE_freq/8
data_df$FIRE_bool <- data_df$FIRE_freq > 0
data_df$FIRE_bool <- as.numeric(data_df$FIRE_bool)

data_df_spdf <- data_df
coordinates(data_df_spdf) <- coord_names
filename<-sub(extension(basename(state_fname)),"",basename(state_fname))       #Removing path and the extension from file name.
state_outline <- readOGR(dsn=dirname(state_fname), filename)

proj4string(data_df_spdf) <- proj4string(state_outline)
l_poly <- over(data_df_spdf,state_outline) #points in polygon operation
l_poly[[id_name]] <- data_df_spdf[[id_name]]
data_df_spdf<- merge(data_df_spdf,l_poly,by=id_name)

#################################  END OF FILE ###########################################


# ANOVA/MANOVA comparing
# 
# Interaction between hinterland scales
# 
# a vs b+c
# a vs d+e
# a vs f+g
# 
# b+c vs d+e
# b+c vs f+g
# 
# d+e vs f+g
# 
# Same but taking direction in mind
# 
# outflows only
# 
# b vs d vs f
# 
# inflows only
# 
# c, vs e vs g
# 
# all comparisons done in three types
# 
# movilizacion (=transactions, unitless, counts, we can put it in %)
# meat (kg)
# livestock (heads)
# agrucultural producs (tones)