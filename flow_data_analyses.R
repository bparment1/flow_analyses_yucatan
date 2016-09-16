#################################    FLOW RESEARCH  #######################################
########################### SPACE-TIME QUINTINARO PRODUCT DATABASE  #############################################
#This script explores and analyzes data from the Quintinaro database of flow (food, wood etc).
#Data was collected by Marco Millones
#
#AUTHOR: Benoit Parmentier                                                                       
#DATE CREATED:07/11/2016 
#DATE MODIFIED: 09/16/2016
#
#PROJECT: Flow, land cover change with Marco Millones
#COMMIT: adding data from figure 9, hinterland analysis
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
library(reshape2)

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

in_dir_script <- "/home/bparmentier/Google Drive/000_Flow_and_LUD_research/Quintana_Roo_Research/scripts"
infile1_function <- file.path(in_dir_script,
                             "flow_data_analyses_function_09162016.R")
source(infile1_function)

#############################################
######## Parameters and arguments  ########

#Input file name with the flow data
filename_flow <- "MO1-9_ALL.txt"
filename_conversion_rate_crop <- "Crop_conversion_rates_09162016.csv"
  
CRS_WGS84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0" #Station coords WGS84 # CONST 2
proj_str<- CRS_WGS84 #param 2
CRS_reg <- CRS_WGS84 # PARAM 3

file_format <- ".txt" #PARAM 4
NA_value <- -9999 #PARAM5
NA_flag_val <- NA_value #PARAM6
out_suffix <-"flow_09162016" #output suffix for the files and ouptu folder #PARAM 7
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

table(tb$X1.9_ORIG_CVE_HINT)
table(tb$X1.9_ORIG_CVE_HINT)
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

barplot(x)

### Now prepare the data:
#Use 
#"ORIG_CLAVE","DEST_CLAVE"
#X1.9_ORIG_CVE_HINT,X1.9_DEST_CVE_HINT

#Outflow: need to recode hinterland with: 1 if HINT is GYR, MEX, MEXICO and Inflow: Examine X1.9_ORIG_CVE_HINT and X1.9_DEST_CVE_HINT, recode 1 if: 
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

#QR_QR <- a #internal flow consumption: C
#QR_GYR <- b #QR->GYR outflow: B
#GYR_QR <- c # inflow: A 
#QR_MEX <- d #d) QR->MX: B (outflow)
#MEX_QR <- e #e) QR<- MX: A (inflow) 
#QR_W <- f #f) QR->W: B (outflow)
#W_QR <- g #g) QR-<W: A (inflow)

##Using revalue from plyr package!
tb$flow_types <- revalue(tb$ORIG_DEST_HINT,
                         c("QR_QR"  = "a", # internal flows consumption: C
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

#Generate later:
#A+B+C: Total impact of food production and consumption on land in anchor region in a business as usual scenario
#B-A: trade balance in terms of land 

### Consider only flows within  Quintano Roo

tb <- subset(tb,tb$flow_direction%in%c("A","B","C"))

barplot(table(tb$flow_direction))
 
### For Hinterland analyses

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

###########################################
### PART 1: SCREEN DATA VALUES FOR INCONSISTENCIES AND TO SELECT BASE PRODUCTS


##################
### We need to subset by product

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

###First make sure we have numeric values

tb$NV_CANT <- as.numeric(tb$NV_CANT)
  
tb <- tb[!is.na(tb$NV_CANT),]

tb$y <- tb$NV_CANT
pos_col <- which(names(tb)=="y")
#tb_ordered <- tb[with(tb, order(y)), ]
tb_ordered <- tb[ order(-tb[,pos_col]), ]

tb_ordered[1:100,c("NV_CANT","product_cat","flow_direction","TRANSLATION")]
table(tb_ordered[1:10000,]$TRANSLATION)  

tb_ordered_agri <- subset(tb_ordered,tb$product_cat=="agri")
tb_ordered[2000,]
tb_ordered[10000,c("NV_CANT","product_cat","flow_direction","TRANSLATION")]

#### Check for agri only
tb_ordered_agri <- subset(tb_ordered,tb_ordered$product_cat=="agri")
dim(tb_ordered_agri)
tb_ordered_agri[1:30,c("NV_CANT","product_cat","flow_direction","TRANSLATION")]

#barplot(table(tb_ordered_meat$TRANSLATION))

### Check for meat only

tb_ordered_meat <- subset(tb_ordered,tb_ordered$product_cat=="meat")
dim(tb_ordered_meat)
tb_ordered_meat[1:40,c("NV_CANT","product_cat","flow_direction","FECHA","TRANSLATION")]

#View(tb_ordered_meat)
#remove potential error
val <- max(tb_ordered_meat$NV_CANT) #17000,GALLINAZA CHICKEN EXCREMENT FERTILIZER FROM MEAT CHICKENS
row_to_remove <- which(tb$NV_CANT==val & tb$product_cat=="meat") 

tb  <- tb[-row_to_remove,]

##### Additional screening: removing duplicates row (transactions)
#View(tb[tb$dates==tb_agri_by_dates[max_pos,]$dates,])
length(unique(tb$IDMOVILIZA))## 11 duplication
#remove the duplication!!!
screening_transaction <- as.data.frame(table(tb$IDMOVILIZA))
duplicate_transaction <- subset(screening_transaction,Freq >1)
duplicate_transcation_df <- subset(tb,tb$IDMOVILIZA %in% as.character(duplicate_transaction$Var1))
#This is a small number but has no effect on total. It is removed to make sure that we are doing good.
codes_to_remove <- as.character(duplicate_transaction$Var1)
tb <- subset(tb,!tb$IDMOVILIZA %in% codes_to_remove)

#### Additional screening non-food item/commoditiy related zacate
#unique(tb$NOMPRODUCT)

#<- sum(tb$NOMPRODUCT == "ZACATE")
codes_to_remove <- c("ZACATE")
tb <- subset(tb,!tb$NOMPRODUCT %in% codes_to_remove)

#filename_flow 
out_filename <- file.path(outDir,paste("tb_overall_flow_data_clean_",out_suffix,".txt",sep=""))
write.table(tb,file=out_filename,sep=",")

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

#plot(tb_tmp_dz$NV_CANT)
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
#range(tb_tmp$NV_CANT,na.rm=T)

#plot(tb_tmp_dz$NV_CANT) #problem
#plot(tb_tmp$NV_CANT)

###########################################
### PART 2: SUMMARY TABLES: AGGREGATE  FLOWS BY TYPES AND DATES/YEARS
####################################

### Aggregate by year and product_cat

#2001 to 2009

tb_summary1 <- aggregate(NV_CANT ~ product_cat + year + flow_direction + extraction + consumption, data = tb, sum)

#   "C", # internal consuption: C
#   = "B", # QR->GYR outflow: B
#  "A", # inflow: A 

tb_summary1$comp <- tb_summary1$consumption*2 + tb_summary1$extraction*1

## Extraction is B+C (defined as comsumption of from land produced locally)

## Look into extraction/production and consumption

tb_summary2 <- aggregate(NV_CANT ~ product_cat + year + extraction, data = tb_summary1, sum)
tb_summary3 <- aggregate(NV_CANT ~ product_cat + year + consumption, data = tb_summary1, sum)

### For use in generating table 4 and table 5

tb$transaction_bool <- 1 
tb_summary4 <- aggregate(transaction_bool ~ product_cat + ORIG_DEST_HINT + flow_direction , data = tb, sum)

### Reorganizing data table: to get A+B+C and A+C by year
# get the relevant data

tb_summary5 <- aggregate(NV_CANT ~ product_cat + ORIG_DEST_HINT + flow_direction , data = tb, sum)

#tb_summary5 <- dcast(tb_summary1, NV_CANT + product_cat + year ~ flow_direction)

### Write out tables genated here:

#### Writing the table
write.table(tb_summary1,file=paste("tb_summary1_aggregated_data_by_flow_by_product_year_",out_suffix,".txt",sep=""),sep=",")

#### Writing the table
write.table(tb_summary2,file=paste("tb_summary2_aggregated_data_by_production_by_product_year_",out_suffix,".txt",sep=""),sep=",")

#### Writing the table
write.table(tb_summary3,file=paste("tb_summary3_aggregated_data_by_comsumption_by_product_year_",out_suffix,".txt",sep=""),sep=",")

#### Writing the table
write.table(tb_summary4,file=paste("tb_summary4_aggregated_data_by_origin_dest_by_product_",out_suffix,".txt",sep=""),sep=",")

#### Writing the table
write.table(tb_summary5,file=paste("tb_summary5_aggregated_data_by_quantity_by_A_B_C_by_product_year",out_suffix,".txt",sep=""),sep=",")

####################################################
##### PLOTTING EXTRACTION FOR EACH PRODUCT #####

xyplot(NV_CANT ~ year | extraction,subset(tb_summary2,tb_summary2$product_cat=="livestock"),type="b",
       main="livestock")

xyplot(NV_CANT ~ year | extraction,subset(tb_summary2,tb_summary2$product_cat=="meat"),type="b",
       main="meat")

xyplot(NV_CANT ~ year | extraction,subset(tb_summary2,tb_summary2$product_cat=="agri"),type="b",
       main="agri")

####################################################
##### PLOTTING CONSUMPTION FOR EACH PRODUCT #####

xyplot(NV_CANT ~ year | consumption,subset(tb_summary3,tb_summary3$product_cat=="livestock"),type="b",
       main="livestock")

xyplot(NV_CANT ~ year | consumption,subset(tb_summary3,tb_summary3$product_cat=="meat"),type="b",
       main="meat")

xyplot(NV_CANT ~ year | consumption,subset(tb_summary3,tb_summary3$product_cat=="agri"),type="b",
       main="agri")

### Apply the area factor for land

### Get an idea of export by year and hinterland category:

test_all <- aggregate(NV_CANT ~ product_cat + year + flow_direction + extraction + consumption, 
                                    data = tb, sum)
 
##################################################
############# PART 3: Hinterland analyses ##########                 

#compare:a, b-c,d-e,f-g
#compare:internal,GYR,MEX,W

tb$NV_CANT <- as.numeric(tb$NV_CANT)

#hinterland_tb <- aggregate(NV_CANT ~ product_cat + year + flow_direction + extraction + consumption, 
#                      data = tb, sum)

hinterland_year_tb <- aggregate(NV_CANT ~ flow_dist_cat + product_cat + year, 
                           data = tb, sum)

hinterland_tb <- aggregate(NV_CANT ~ flow_dist_cat + product_cat, 
                           data = tb, sum)
#####

tb$transaction_bool <- 1 


hinterland_tb_quant <- aggregate(NV_CANT ~ product_cat + ORIG_DEST_HINT + flow_direction + flow_dist_cat , data = tb, sum)
hinterland_tb_trans <- aggregate(transaction_bool ~ product_cat + ORIG_DEST_HINT + flow_direction + flow_dist_cat , data = tb, sum)

hinterland_tb_quant_trans <- hinterland_tb_quant
hinterland_tb_quant_trans$transaction_bool <- hinterland_tb_trans$transaction_bool

hinterland_tb_quant_trans$flow_dist_label <- revalue(hinterland_tb_quant_trans$flow_dist_cat,
                                                     c("0"  = "QR", # internal consuption: C
                                                       "1" = "GYR", # QR->GYR outflow: B
                                                       "2" = "MEX", # e) QR<- MX: A (inflow) 
                                                       "3" = "W")) # g) QR-<W: A (inflow) and W->QR
#drop world because not enough data:
hinterland_tb_quant_trans <- subset(hinterland_tb_quant_trans,!ORIG_DEST_HINT%in% c("W_QR","QR_W"))

#### Writing the table
write.table(hinterland_tb,file=paste("hinterland_tb_product_sum_",out_suffix,".txt",sep=""),sep=",")
write.table(hinterland_year_tb,file=paste("hinterland_year_tb_product_sum_",out_suffix,".txt",sep=""),sep=",")
write.table(hinterland_tb_quant_trans,file=paste("hinterland_tb_quant_trans_year_tb_product_sum_",out_suffix,".txt",sep=""),sep=",")

####### Quick analysis
#### Agri,

tb_tmp <- subset(hinterland_tb_quant_trans,product_cat=="agri")
barplot(table(tb_tmp$flow_dist_cat),main="agri",names.arg=c("QR","GYR","MEX"))

tb_tmp <- subset(tb,tb$product_cat=="agri")
tb_tmp$y <- tb_tmp$NV_CANT

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
barplot(table(tb_tmp$flow_dist_cat),main="meat",names.arg=c("QR","GYR","MEX","MEX"))

sd_y <-tapply(tb_tmp$y,tb_tmp$flow_dist_cat, sd, na.rm=TRUE)
mean_y <-tapply(tb_tmp$y,tb_tmp$flow_dist_cat, mean, na.rm=TRUE)
sum_y <-tapply(tb_tmp$y,tb_tmp$flow_dist_cat, sum, na.rm=TRUE)
barplot(sum_y,main="meat",names.arg=c("QR","GYR","MEX","MEX"))

#########################################################
#### PART 4: CONVERSION OF FLOWS INTO LAND AREA: First obtain total land available ########

### Step 1: Get the area covered by Quintaroo and available land for products

#Use the CI land cover map in the Fire analysis script. There are 3 maps
#1990, 2000 and 2007
#it is possible that you have the change map only

#FP NFP CH  use NFP + CH

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
#fire_modis_col <- c("FIRE_2000","FIRE_2001","FIRE_2002","FIRE_2003","FIRE_2004","FIRE_2005","FIRE_2006","FIRE_2007")
#data_df$FIRE_freq <- data_df$FIRE_2000 + data_df$FIRE_2001 + data_df$FIRE_2002 + data_df$FIRE_2003 + data_df$FIRE_2004 + data_df$FIRE_2005 + data_df$FIRE_2006 + data_df$FIRE_2007 
#data_df$FIRE_intensity <- data_df$FIRE_freq/8
#data_df$FIRE_bool <- data_df$FIRE_freq > 0
#data_df$FIRE_bool <- as.numeric(data_df$FIRE_bool)

data_df_spdf <- data_df
coordinates(data_df_spdf) <- coord_names
filename<-sub(extension(basename(state_fname)),"",basename(state_fname))       #Removing path and the extension from file name.
state_outline <- readOGR(dsn=dirname(state_fname), filename)

proj4string(data_df_spdf) <- proj4string(state_outline)
l_poly <- over(data_df_spdf,state_outline) #points in polygon operation
l_poly[[id_name]] <- data_df_spdf[[id_name]]
data_df_spdf<- merge(data_df_spdf,l_poly,by=id_name)

data_df_spdf$state <- as.character(data_df_spdf$FIRST_NOM_)
table(data_df_spdf$state)
sum(table(data_df_spdf$state))
nrow(data_df_spdf)
#> sum(table(data_df_spdf$state))
#[1] 136311
#> nrow(data_df_spdf)
#[1] 136904

spplot(data_df_spdf,"cattledensity")
spplot(data_df_spdf,"FIRE_pre07")
spplot(data_df_spdf,"dist_roads")
spplot(data_df_spdf,"state")
#data_df_spdf$state

table(data_df_spdf$fpnfpch)#"fpnfpch"

land_area <- table(data_df_spdf$state)
land_area <- land_area*1/0.01

#total_land_consumed3

#NFP + CH
(table(data_df_spdf$state))

data_df_spdf_qr <- subset(data_df_spdf,state=="Quintana Roo")
names(data_df_spdf_qr)
data_df_spdf_qr$land_consumed <- data_df_spdf_qr$nfp + data_df_spdf_qr$ch

land_consumed_qr <- data_df_spdf_qr$land_consumed*1/0.01

total_land_consumed_qr <- sum(land_consumed_qr) #This is the total land available over 2000-2009?

###################################################################
#################### PART5: CONVERSION FOR LIVESTOCK: CATTLE COW AND RELATED #############

###### NOW SELECT PRODUCT AND COMPARE TO AVAILABLE LAND
### Step 1: conversion of flow to land use

#http://www.fao.org/ag/agp/agpc/doc/counprof/mexico/Mexico.htm
#http://www.fao.org/docrep/010/a0701e/a0701e00.HTM

#1. isolate  'bovino' from the aggregate livestock category, but you can try including 'caprino' and 'porcino' and 'ovino' as well
#select all that contain 'bovino' or 'caprino' or 'porcino' AND 'cabezas'

#Stocking rates from 0.5 1 AU (head) per ha. for Mexico, tropical areas are more extensive 0.3. If we use 1 head per ha we are being generous
tb_livestock_all <- subset(tb,tb$product_cat=="livestock")
tb_summarized_all <- aggregate(NV_CANT ~ year,data=tb_livestock_all,sum)
  
plot(NV_CANT ~ year,data=tb_summarized_all)

tb_livestock <- subset(tb,tb$product_cat=="livestock" & tb$consumption==1)
dim(tb_livestock)
table(tb_livestock$NOMPRODUCT)

#> names(table(tb_livestock$NOMPRODUCT))
#[1] "ANIMALES CIRCO"          "ANIMALES ESPECTACULOS"   "AVES EN PIE"            
#[4] "AVESTRUZ"                "BECERROS"                "BOVINOS EN PIE"         
#[7] "BOVINOS EN PIE DE CRIA"  "CAPRINOS EN PIE"         "DESTETES BOVINO"        
#[10] "DESTETES PORCINO"        "EQUINOS EN PIE"          "NOVILLOS"               
#[13] "OTROS"                   "OTROS EN PIE"            "OVINOS EN PIE"          
#[16] "PAVOS EN PIE"            "POLLITOS"                "PORCINOS EN PIE"        
#[19] "PORCINOS EN PIE DE CRIA" "VACAS"                   "VENADOS EN PIE"      

names(table(tb_livestock$NOMPRODUCT))
names(table(tb_livestock$TRANSLATION))
barplot(table(tb_livestock$NOMPRODUCT),names.arg=names(table(tb_livestock$NOMPRODUCT)),las=2)

#should calves be in?
selected_livestock1 <- c("BOVINOS EN PIE","BOVINOS EN PIE DE CRIA","DESTETES BOVINO","BOVINOS EN PIE DE CRIA")

additional_cat1 <- c("CAPRINOS EN PIE","PORCINOS EN PIE DE CRIA","PORCINOS EN PIE","DESTETES PORCINO")
additional_cat2 <- c("vacas")

selected_livestock <- c(selected_livestock1,additional_cat1,additional_cat1)

tb_to_convert <- subset(tb ,tb$NOMPRODUCT %in% selected_livestock)

table(tb_to_convert$NV_UMEDIDA) #OK all cabeza

#selected_livestock <- c(selected_livestock,additional_cat1,additional_cat1)
selected_livestock <- c(selected_livestock1,additional_cat2,additional_cat1)

#Stocking rates from 0.5 1 AU (head) per ha. for Mexico, tropical areas are more extensive 0.3. If we use 1 head per ha we are being generous
#conversion_rate <- 1/c(0.3,0.5,1) #this corresponds to head per ha

tb_conversion_rate_livestock1 <- data.frame(conversion_rate= 1/0.3 , NOMPRODUCT=selected_livestock)
tb_conversion_rate_livestock2 <- data.frame(conversion_rate= 1/0.5 , NOMPRODUCT=selected_livestock)
tb_conversion_rate_livestock3 <- data.frame(conversion_rate= 1/1 , NOMPRODUCT=selected_livestock)

tb_conversion_rate_livestock1_filename <- paste0("tb_conversion_rate_livestock1_",out_suffix,".txt")
write.table(tb_conversion_rate_livestock1, file= file.path(outDir,tb_conversion_rate_livestock1_filename))

#can use mclapply if needed
list_converted_tb_products <- lapply(1:nrow(tb_conversion_rate_livestock1),FUN=apply_conversion_rate,
                                     tb_conversion_rate=tb_conversion_rate_livestock1,
                                     tb_products=tb_to_convert)

sum(unlist(lapply(list_converted_tb_products,FUN=nrow)))
dim(tb_to_convert)
tb_land_livestock <- do.call(rbind,list_converted_tb_products)
dim(tb_land_livestock)

tb_land_livestock$land_consumption
tb_land_livestock$percent_land_consumption <- (tb_land_livestock$land_consumption/total_land_consumed_qr)*100

tb_land_summarized <- aggregate(land_consumption ~ year + product_cat , data = tb_land_livestock , sum)
tb_land_summarized$percent_land_consumption <- (tb_land_summarized$land_consumption/total_land_consumed_qr)*100
options(scipen=999)

plot(percent_land_consumption ~year,data=tb_land_summarized ,type="b",
     ylab="% of land in QR",
     main="Livestock land consumption as percentage of land")

tb_land_summarized2 <- aggregate(percent_land_consumption ~ flow_direction + year, data = tb_land_livestock, sum)

p6 <- xyplot(percent_land_consumption ~ year | flow_direction ,data=tb_land_summarized2,
             type="b",
             ylab="% of land in QR", 
             main="Livestock land consumption as percentage of land")

p6

#### Writing the tables
tb_land_livestock$total_land_consumed <- total_land_consumed_qr

tb_land_summarized_livestock_filename <- paste("tb_land_summarized_","livestock","_by_product_year",out_suffix,".txt",sep="")
tb_land_summarized2_A_B_C_livestock_filename <- paste("tb_land_summarized2_","livestock","_by_flow_A_B_C_year",out_suffix,".txt",sep="")
tb_land_livestock_filename <- paste("tb_land_","livestock", out_suffix,".txt",sep="")
  
write.table(tb_land_summarized,file= file.path(outDir,tb_land_summarized_livestock_filename) ,sep=",")
write.table(tb_land_summarized2,file=file.path(outDir,tb_land_summarized2_A_B_C_livestock_filename),sep=",")
write.table(tb_land_livestock,file=file.path(outDir,tb_land_livestock_filename),sep=",")
            
###################################################################
#################### PART 6: CONVERSION FOR A SET OF AGRICULTURAL CROPS #############

tb_conversion_rate_crop <- read.table(file.path(inDir, filename_conversion_rate_crop),
                                      header=T,stringsAsFactors = F,sep=",")
tb_conversion_rate_crop$NOMPRODUCT

#names(table(tb_livestock$NOMPRODUCT))
#names(table(tb_livestock$TRANSLATION))
#barplot(table(tb_livestock$NOMPRODUCT),names.arg=names(table(tb_livestock$NOMPRODUCT)),las=2)

#select relevant product
selected_nom_product <- tb_conversion_rate_crop$NOMPRODUCT
#additional_cat1 <- c("CAPRINOS EN PIE","PORCINOS EN PIE DE CRIA","PORCINOS EN PIE","DESTETES PORCINO")

tb_to_convert <- subset(tb , tb$NOMPRODUCT %in% selected_nom_product)
table(tb_to_convert$NV_UMEDIDA) #OK all TONELADA
barplot(table(tb_to_convert$NOMPRODUCT),names.arg=names(table(tb_to_convert$NOMPRODUCT)),las=2)

names(tb_conversion_rate_crop) <- c("conversion_rate","NOMPRODUCT")

list_converted_tb_products <- lapply(1:nrow(tb_conversion_rate_crop),FUN=apply_conversion_rate,
                              tb_conversion_rate=tb_conversion_rate_crop,
                              tb_products=tb_to_convert)

sum(unlist(lapply(list_converted_tb_products,FUN=nrow)))

tb_land_agri <- do.call(rbind,list_converted_tb_products)
dim(tb_land_agri)
### Get data for A and B etc...?
dim(tb_land_agri)

#tb_summarized <- aggregate(land_consumption ~ year + product_cat , data = tb_land_crops , sum)
tb_land_agri$percent_land_consumption <- (tb_land_agri$land_consumption/total_land_consumed_qr)*100

tb_land_summarized <- aggregate(land_consumption ~ year + product_cat , data = tb_land_agri , sum)
tb_land_summarized$total_land_consumed <- total_land_consumed_qr

tb_land_summarized$percent_land_consumption <- (tb_land_summarized$land_consumption/total_land_consumed_qr)*100
options(scipen=999)
tb_land_summarized2 <- aggregate(land_consumption ~ year + NOMPRODUCT , data = tb_land_agri , sum)
tb_land_summarized2$percent_land_consumption <- (tb_land_summarized2$land_consumption/total_land_consumed_qr)*100
tb_land_summarized2$total_land_consumed <- total_land_consumed_qr

p4 <- xyplot(percent_land_consumption ~ year | NOMPRODUCT,data=tb_land_summarized2,
             type="b",
             ylab="Head", 
             main="Livestock flow extraction total by year ")
p4 #not using right now
#drop melaza
tb_land_summarized3 <- tb_land_summarized2[!tb_land_summarized2$NOMPRODUCT=="MELAZA",]
p5 <- xyplot(percent_land_consumption ~ year | NOMPRODUCT,data=tb_land_summarized3,
             type="b",
             ylab="Head", 
             main="AGRI flow extraction total by year ")
p5
#### For the results and figures

plot(tb_land_summarized$percent_land_consumption ~year,data=tb_land_summarized,type="b",
     ylab="% of land in QR",
     main="AGRI land consumption as percentage of land")

#plot(tb_summarized$land_consumption ~year,data=tb_summarized,type="b",main="crop")

tb_land_summarized2 <- aggregate(percent_land_consumption ~ flow_direction + year, data =  tb_land_agri, sum)

p6 <- xyplot(percent_land_consumption ~ year | flow_direction ,data=tb_land_summarized2,
             type="b",
             ylab="% of land in QR", 
             main="AGRI land consumption as percentage of land")

p6

#### Writing the tables
tb_land_agri$total_land_consumed <- total_land_consumed_qr

tb_land_summarized_agri_filename <- paste("tb_land_summarized_","agri","_by_product_year",out_suffix,".txt",sep="")
tb_land_summarized2_A_B_C_agri_filename <- paste("tb_land_summarized2_","agri","_by_flow_A_B_C_year",out_suffix,".txt",sep="")
tb_land_agri_filename <- paste("tb_land_","agri", out_suffix,".txt",sep="")

write.table(tb_land_summarized,file= file.path(outDir,tb_land_summarized_agri_filename) ,sep=",")
write.table(tb_land_summarized2,file=file.path(outDir,tb_land_summarized2_A_B_C_agri_filename),sep=",")
write.table(tb_land_agri,file=file.path(outDir,tb_land_agri_filename),sep=",")

#################################  END OF FILE ###########################################
