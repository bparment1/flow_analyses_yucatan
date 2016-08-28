#################################    FLOW RESEARCH  #######################################
########################### SPACE-TIME QUINTINARO PRODUCT DATABASE  #############################################
#This script explores and analyzes data from the Quintinaro database of flow (food, wood etc).
#Data was collected by Marco Millones
#
#AUTHOR: Benoit Parmentier                                                                       
#DATE CREATED:07/11/2016 
#DATE MODIFIED: 08/28/2016
#
#PROJECT: Flow, land cover change with Marco Millones
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
out_suffix <-"flow_08282016" #output suffix for the files and ouptu folder #PARAM 7
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

### PART 0: READ IN DATASETS RELATED TO TELECONNECTION AND PREVIOUS LOADINGS
#http://geog.uoregon.edu/bartlein/courses/geog607/Rmd/netCDF_01.htm

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

tb$product_cat[tb$SECCION=="AGRICOLA" & tb$NV_UMEDIDA=="TONELADA"] <- "agri"
tb$product_cat[tb$SECCION=="PECUARIO" & tb$NV_UMEDIDA=="TONELADA"] <- "meat"
tb$product_cat[tb$SECCION=="PECUARIO" & tb$NV_UMEDIDA=="CABEZA"] <- "livestock"

table(tb$product_cat)

#Just cabeza and tonelada in NV_UMEDIDA
#If seccion=agricola and nv_medida=tolenada then agri
#if seccion= pecuario and nv_medida= tonelada then meat
#if seccion= pecuario and nv_medida = cabeza then livestock 

## 

### Aggregate by year and product_cat

#2001 to 2009

#First make sure we have numeric values

tb$NV_CANT <- as.numeric(tb$NV_CANT)
  

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

test <- aggregate(NV_CANT ~ product_cat + year + flow_direction + extraction + consumption, 
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

# ANOVA/MANOVA comparing
# 
# Interaction between hinterland scales
# 
#Regression 1: a vs b+c
#Regression 2: a vs d+e
#Regression 3: a vs f+g
#Regression 4: b+c vs d+e
#Regression 5: b+c vs f+g
#Regression 6: d+e vs f+g


##Regression 1: a vs b+c

#"QR_QR"  = "a", # internal consuption: C
#"QR_GYR" = "b", # QR->GYR outflow: B
#"GYR_QR" = "c", # inflow: A 

#tb$flow_types

## first subset the data for specific product and categories!!!

#tb$y <- tb$IDMOVILIZA #rethink this in terms of counts from municipios?
#tb$y <- 1



### This is a quick ANOVA style regression (General Linear Model)
#mod <- lm(y ~ flow_types, data= test)
#summary(mod)

#summary(mod)
#
#Call:
#  lm(formula = y ~ flow_types, data = test)
#
#Residuals:
#  Min     1Q Median     3Q    Max 
#-155    -23     -3     -3 276325 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)    28.28      16.45   1.720   0.0855 .  
#flow_typesb   126.71      24.34   5.206 1.93e-07 ***
#  flow_typesc   -21.93      18.21  -1.204   0.2285    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 1955 on 88648 degrees of freedom
#(1 observation deleted due to missingness)
#Multiple R-squared:  0.0006506,	Adjusted R-squared:  0.0006281 
#F-statistic: 28.86 on 2 and 88648 DF,  p-value: 2.966e-13

#coef(mod)
#str(mod)
#plot(mod)

#plot(coef(mod),type="h")
#plot(coef(mod),type="b",ylab="quant",main="Agriculture")

## We need to extract the standard error and coef values for the slope of each type!!
## Plot the coef val and CI for each categories

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