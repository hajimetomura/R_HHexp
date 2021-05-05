# Simulate the effect of policy intervention by using the estimated coefficients of a linear regression for the reproduction number with mobility report data and household expenditure data.
# Frequency: daily.
# Use the following model.
# Model 6: Add to model 4 coefficient restrictions so that household expenditure and mobility have always positive effects on the reproduction number.
# Model 7: Add to model 5 coefficient restrictions so that household expenditure and mobility have always positive effects on the reproduction number.
# Model 10: Clothing and footwear are dropped from the explanatory variables for Model 7.
# Model 11: The error term in Model 7 is changed to AR(2) process.
# Model 12: The error term in Model 7 has a serial correlation with shocks in a week ago.
# Model 13: Dummy for the period before the first state of emergency.
# Model 14: Add a dummy for the period before the first state of emergency to Model 11.
# Model 15: Add a dummy for the period before the first state of emergency to Model 7 and remove serial correlation in error terms from Model 7.
# Model 16: Apply the distribution of incubation periods to AR(1) error term. Also add white noises as a measurement error of R to Model 13.
# Model 17: Modify the distribution of the initial value of unobserved infectious events to the unconditional distribution.
# Model 18: Set real <lower=0, upper=1> rho; // AR(1) term for unobseved infectious events.

# The saved data and estimates of either model are loaded. 
# This code loads additional data for the use of simulations.

########## Clear global workspace and plots. ################

if( dev.cur() > 1 ) dev.off() # Clear plots.
rm( list = ls( envir = globalenv() ), envir = globalenv() ) # Clear global workspace.

########## Set parameters ###################################

flag_pref_wgt <- 1 # Default value: 1. If = 1, use population share among prefectures to compute average absolute humidity across prefectures. If = 0, use new cases in the past 7 days.

log_abs_hum <- 0 # Default value: 0. If = 1, use log of absolute humidity. If = 2, use the level of absolute humidity. Otherwise, use a dummy that absolute humidity exceeds 7. Unit: g/m^3.

log_R <- 1 # Default value: 0. If = 1, use log of the reproduction number. otherwise, use the level.

log_dist_incub <- 0 #  Default value: 0. If = 1, use a log normal distribution based on Chinese data reported by Stephen A et al. (2020). =0, use the empirical distribution of incubation periods based on Sugishita (2020).

nominal_hes <- 0 # Default value: 0. If = 1, use nominal household expenditure data. If = 0, use real household expenditure data denominated by 2020 average CPI for each item.

mdl_number <- 17 # Numbering of the stan model whose result is loaded. Must be 6 or 7.

ENG <- 0 # If = 1, write labels in English.

plot_paper <- 1 # If =1 and ENG=1, the main titles of some plots are set for the paper, rather than slides.

########## Set the scale of fonts in plots. ##########

if (ENG == 1){
  
  cex_val <- 1.5 # Values of cex.**** for plots.
  cex_val_axis <- 1.2 # Values of cex.axis when dates are plotted
  
}else{
  
  cex_val <- 1 # Values of cex.**** for plots.
  cex_val_axis <- 1 # Values of cex.axis when dates are plotted
  
}

########## Set the current directory to the one in dropbox.###########################

usrnm <- Sys.getenv("USERNAME")
wdir <- paste("C:/Users/",usrnm,"/Dropbox/ongoing work/COVID19",sep = "")
setwd(wdir)

########## Call libraries and functions, and load data. ########################

library(seasonal)
library(rstan)

# Call hand-made functions.
source("./def_func_data_COVID.R",encoding="utf-8")

# Load and reform data. 
source("./localtrend_COVID_nationwide_data.R",encoding="utf-8")

# Ensure the sample period stops at 2021 Jan, due to a concern on mutations.
if(hes_end[1] !=2021  || hes_end[2] !=1){
  stop("Set the end of the sample period for estimation to 2021 Jan.")
}

# Drop household expenditure for closes and shoes from explanatory variables for model 10.
if (mdl_number == 10){
  
  H_expvals <- H_expvals[1:5,] # Drop household expenditures for closes and shoes, which is on the 6th row.
  
}


########## Load estimates of a linear regression model ################

# Suffix to file names
if (log_abs_hum == 1){
  W_sfx <- "log_abs_hum" # Use the log of absolute humidity.
}else if(log_abs_hum == 2){
  W_sfx <- "lv_abs_hum" # Use the level of absolute humidity.
}else{
  W_sfx <- "ind_abs_hum" # Use a dummy that absolute humidity exceeds ah_ub g/m^3.
}

if (log_R == 1){
  R_sfx <- "log_R" # Use the log of the reproduction number.
}else{
  R_sfx <- "lv_R" # Use the level of the reproduction number.
}

if (log_dist_incub == 1){
  incub_sfx <- "estCHN" # Based on Stephen A et al. (2020).
}else{
  incub_sfx <- "empJPN" # Based on Sugishita (2020).
}

if (nominal_hes == 1){
  nominal_sfx <- "nominal_hes" # Use nominal household expenditure.
}else{
  nominal_sfx <- "real_hes" # Use real household expenditure denominated by 2020 CPI average for each item.
}

if (flag_pref_wgt == 1){
  weath_sfx <- "popu"
  
}else{
  weath_sfx <- "nwcs"
}


# Define file names.
thm <- "localtrend_COVID_model"
file_mcmc_nm <- eval(parse(text=paste0("\"",thm,mdl_number,"_mcmc_",incub_sfx,"_",R_sfx,"_",W_sfx,"_",nominal_sfx,"_",weath_sfx,".data\""))) # Name of the file for the mcmc samples.

# Load data and estimates of a linear regression model of the reproduction number. The mcmc samples are stored in the S4 object named "fit".
load(file=file_mcmc_nm)


########## Load additional data for simulation ###########################

### Household expenditure survey for counter-factual; Daily; 2019 Jan. 01 - Dec.31; Unit: current yen.
# Extract household expenditure items that are used 
hes_var_2019 <- NULL # Initialize a matrix for household expenditure item variables.
hes_var_date_2019 <- NULL # Initialize a matrix for the dates of household expenditure item variables.

# Define the names of items to extract from the the loaded table.
if (sum(mdl_number==c(10))>0){
  hes_var_nm_2019 <- c("食事代", "喫茶代","飲酒代","宿泊料","国内パック旅行費")
}else{
  hes_var_nm_2019 <- c("食事代", "喫茶代","飲酒代","宿泊料","国内パック旅行費","被服及び履物","履物類","被服関連サービス")
  
}

for (j in 1:12){
  # Extract household expenditure data for each month in 2019.
  # The format of the table has changed from Jan 2020.
  # For an unknown reason, an empty column is read at the end. The last data column is the fourth to the last column.
  eval(parse(text=paste0("temp_dt <- read.csv(\"./data/household survey/a616_2019_",j,".csv\", header=F,stringsAsFactors=FALSE)")))
  # Convert the list containing data into a numeric matrix. 
  temp_ind <- rep(NA,length(hes_var_nm_2019)) # Initialize a vector to record the locations of items to extract. 
  for (k in 1:length(hes_var_nm_2019)){
    temp_ind[k] <- which(temp_dt[,9]==hes_var_nm_2019[k])
  }
  # Extract the specified household expenditure items.
  temp_numeric <- conv_list(temp_dt[temp_ind, 11:(ncol(temp_dt)-2-is.na(temp_dt[1,ncol(temp_dt)]))]) # Sometimes the last columns are NAs. In that case, exclude that column.
  # Extend the time series of household expenditure items. Remove commas from the matrix
  hes_var_2019 <- cbind(hes_var_2019,temp_numeric)
  
  # Extract the dates of each month from the first row of temp_dt.
  temp_dates <- conv_list(temp_dt[1,11:(ncol(temp_dt)-2-is.na(temp_dt[1,ncol(temp_dt)]))]) # Sometimes the last columns are NAs. In that case, exclude that column.
  # Extend the time series of dates.
  hes_var_date_2019 <- c(hes_var_date_2019, paste0(2019,"-",j,"-",temp_dates))
}


### Load commercial margin rate data in 2015. 

# Load data.
temp_dt <- read.csv("./data/commercial margin/27margin_hyo1.csv", header=F,stringsAsFactors=FALSE)

# Extract the retail margin for apparel.
temp_ind <- which(temp_dt[,3]=="衣服・その他の繊維既製品") # There are two rows that have this category, one for wholesale and the other for retail. Use the second.
apparel_margin_rate <- as.numeric(temp_dt[temp_ind[2],which(temp_dt[4,]=="マージン率")])/100 # Keep the raw number as the rate.


### Load nominal IO table in 2015. JIP, METI.

temp_dt <- read.csv("./data/jip2018_1-7.csv", header=F,stringsAsFactors=FALSE)

# IO matrix
IO_A <- conv_list(temp_dt[5:104,3:102])

# Output vector
IO_Z <- conv_list(temp_dt[which(temp_dt[,2]=="産出額"),3:102])

# Export vector
IO_X <- conv_list(temp_dt[5:104,which(temp_dt[4,]=="輸出")])

# Import vector
IO_M <- conv_list(temp_dt[5:104,which(temp_dt[4,]=="輸入")])

# Intermediate output vector
IO_IM <- conv_list(temp_dt[5:104,which(temp_dt[4,]=="合計")])

# Total output vector
IO_total <- conv_list(temp_dt[5:104,which(temp_dt[4,]=="産出額")])

# Names of sectors
IO_nm <- as.character(temp_dt[4,3:102])


### Load monthly data on the number of railways passengers from MILTT ###

# Load 2020 data.
temp_dt <- read.csv("./data/MILTT Railways/Railways2020.csv", header=F,stringsAsFactors=FALSE)

# Extract monthly Ryokakyu suryo for 2020.
temp <- conv_list(temp_dt[16:27,2])

# Load 2019 data.
temp_dt <- read.csv("./data/MILTT Railways/Railways2019.csv", header=F,stringsAsFactors=FALSE)

# Extract monthly Ryokakyu suryo for 2019 to form monthly data from Jan. 2019.
Rail_Pssgrs_m <- c(conv_list(temp_dt[18:29,3]), temp)


#### Temperature data for 2019; Daily; 2019 Jan. 1 - 2019 Dec. 31: Unit: %. 
# Rows: dates; Columns: capital of prefectures.
# The order of the capitals of prefectures is already set: weath_pref_nm <- c("札幌", "青森", "盛岡", "秋田", "仙台", "山形", "福島", "水戸", "宇都宮", "さいたま", "千葉", "東京", "新潟", "前橋", "長野", "甲府", "横浜", "静岡", "富山", "岐阜", "名古屋", "金沢", "福井", "大津", "津", "奈良", "和歌山", "大阪", "京都", "神戸", "鳥取", "岡山", "松江", "広島", "山口", "高松", "松山", "徳島", "高知", "福岡", "大分", "宮崎", "佐賀", "熊本", "鹿児島", "長崎", "那覇") 

temper_var_2019 <- NULL # Initialize a matrix for temperatures across the capitals of prefectures in 2019.
hum_var_2019 <- NULL # Initialize a matrix for humidity across the capitals of prefectures.
weath_date_2019 <- NULL # Initialize a matrix for the dates of weather data for 2019.

for (j in 1:12){
  # Extract temperature and humidity data for month j, year i.
  # For an unknown reason, an empty column is read at the end. The last data column is the fourth to the last column.
  eval(parse(text=paste0("temp_dt <- read.csv(\"./data/weather/weather_2019_",j,".csv\", header=F,stringsAsFactors=FALSE)"))) 
  
  # Convert the list containing data into a numeric matrix for temperature.
  # Blank rows in the csv file are already excluded by read.csv().
  temp <- conv_list(temp_dt[5:(nrow(temp_dt)-(temp_dt[nrow(temp_dt),1]=="")),seq(2, 1+47*6-3, by=6)])
  # Sort weather data in the order of weath_pref_nm. 
  temp_temper <- temp * NA # Initialize the container of sorted temperature data for the current month.
  for (k in 1:47){
    temp_temper[,k] <- temp[,as.character(temp_dt[2,seq(2, 1+47*6-3, by=6)])==weath_pref_nm[k]]
  }
  
  # Convert the list containing data into a numeric matrix for humidity.
  # Blank rows in the csv file are already excluded by read.csv().
  temp <- conv_list(temp_dt[5:(nrow(temp_dt)-(temp_dt[nrow(temp_dt),1]=="")),seq(5, 1+47*6, by=6)]) # Exclude the last row, if it is empty.
  # Sort weather data in the order of weath_pref_nm. 
  # No humidity data for Saitama and Ohtsu. Replace them by the data for Tokyo and Kyoto, respectively.
  temp_hum <- temp * NA # Initialize the container of sorted humidity data for the current month.
  for (k in 1:47){
    if (weath_pref_nm[k]=="さいたま"){
      temp_hum[,k] <- temp[,as.character(temp_dt[2,seq(5, 1+47*6, by=6)])=="東京"]
    }else if (weath_pref_nm[k]=="大津"){
      temp_hum[,k] <- temp[,as.character(temp_dt[2,seq(5, 1+47*6, by=6)])=="京都"]
    }else {
      temp_hum[,k] <- temp[,as.character(temp_dt[2,seq(5, 1+47*6, by=6)])==weath_pref_nm[k]]
    }
  }
  
  # Extend the time series of weather data.
  temper_var_2019 <- rbind(temper_var_2019,temp_temper)
  hum_var_2019 <- rbind(hum_var_2019,temp_hum)
  
  # Extract the dates of each month from the first row of temp_dt.
  temp_dates <- temp_dt[5:(nrow(temp_dt)-(temp_dt[nrow(temp_dt),1]=="")),1] # Sometimes the last columns are NAs. In that case, exclude that column.
  
  # Extend the time series of dates. 
  weath_date_2019 <- c(weath_date_2019, temp_dates)
}   

# Interpolate the missing humidity data in 2019
# 2019/12/27: Akita.
hum_var_2019[weath_date_2019=="2019/12/27", weath_pref_nm=="秋田"] <- (hum_var_2019[weath_date_2019=="2019/12/26", weath_pref_nm=="秋田"] + hum_var_2019[weath_date_2019=="2019/12/28", weath_pref_nm=="秋田"]) / 2

############## Reform data ##############################

# Save plots of reformed data in a pdf file.
eval(parse(text=paste0("pdf(paste0(\"simu0_reformed_data_model",mdl_number,"_ENG",as.logical(ENG),"_",R_sfx,".pdf\"), family=\"Japan1GothicBBB\")")))


### Assigning household expenditure categories to IO categories.
# Define elements correspond to hes_var_nm_2019 in order.

Eatout_conv_IO_nm <- rep("サービス業（民間、非営利）",3) # Corresponds to ("食事代", "喫茶代","飲酒代")
Travel_conv_IO_nm <- c("運輸・通信業","サービス業（民間、非営利）") # Corresponds to ("宿泊料", "国内パック旅行")
Apparel_conv_IO_nm <- c("繊維","その他の製造業","サービス業（民間、非営利）") # Corresponds to "被服及び履物". In this household expenditure categories, "履物類" is "その他の製造業" and "被服関連サービス" is "サービス業（民間、非営利）" in IO table. The other household items in this category is "繊維".


### Compute the shares of apparel expenditure among "履物類", "被服関連サービス", and the others for the use of IO analysis.
# This part is irrelevant for model 10, as it does not have household expenditures for Clothing and footwear as part of the explanatory variables. 
if (mdl_number != 10){
  temp_ind <-c(which(hes_var_nm_2019=="被服及び履物"), which(hes_var_nm_2019=="履物類"),which(hes_var_nm_2019=="被服関連サービス")) # Find the location of three items.  
  temp <- hes_var_2019[temp_ind,] # Extract daily expenditure values of the three items.
  temp_share <- t(apply(temp,1,function(x){x/temp[1,]})) # Convert the numbers into shares. Transpose the result to have dates for columns.
  temp_share[1,] <-  temp_share[1,] - apply(temp_share[2:3,],2,sum) # Subtract "履物類" and "被服関連サービス" from "被服及び履物", which is the total.
  Apparel_conv_IO_share_daily <- temp_share # Daily shares of apparel expenditure among "履物類", "被服関連サービス", and the others
}


### Compute the inverse IO matrix controlling for import shares.

coef_IO_A <- apply(IO_A,2,function(x){x/IO_Z}) # Compute the input coefficient matrix. 

coef_IO_M <- IO_M / (IO_total - IO_X + IO_M) # Compute the import coefficient matrix.

inv_IO_A <- solve(diag(dim(coef_IO_A)[1]) - diag(1-coef_IO_M) %*% coef_IO_A) # Compute the inverse matrix.


### Convert nominal household expenditure in 2019 into real values in 2020 CPI average price for each item.
# Except for model 10, hes_var_nm_2019 <- c("食事代", "喫茶代","飲酒代","宿泊料","国内パック旅行費","被服及び履物","履物類","被服関連サービス")
# For model 10, hes_var_nm_2019 <- c("食事代", "喫茶代","飲酒代","宿泊料","国内パック旅行費")
# CPI_d_2020 starts from Jan 1. 2019.

if (mdl_number==10){
  hes_var_2019_real <-rbind(hes_var_2019[1,] / CPI_d_2020[1, 1:365], 
                            hes_var_2019[2,] / CPI_d_2020[1, 1:365],
                            hes_var_2019[3,] / CPI_d_2020[1, 1:365],
                            hes_var_2019[4,] / CPI_d_2020[2, 1:365],
                            hes_var_2019[5,] / CPI_d_2020[3, 1:365]) # There is no 被服及び履物, which is on the 6th row. The definition of CPI_d_2020 does not depend on the value of mdl_number.
  
}else{
  hes_var_2019_real <-rbind(hes_var_2019[1,] / CPI_d_2020[1, 1:365], 
                            hes_var_2019[2,] / CPI_d_2020[1, 1:365],
                            hes_var_2019[3,] / CPI_d_2020[1, 1:365],
                            hes_var_2019[4,] / CPI_d_2020[2, 1:365],
                            hes_var_2019[5,] / CPI_d_2020[3, 1:365],
                            hes_var_2019[6,] / CPI_d_2020[4, 1:365])
  
}

hes_var_2019_real <- hes_var_2019_real / 100 # The same treatment as H_expvals: To increase the number of digit of coefficients to avoid the effect of possible rounding error.  


### Convert monthly railways passengers data into daily data from Jan. 2019 to Dec. 2020.

temp_date <- c(ndays_normal,ndays_olympic) # Create a series of date from Jan. 2019 to Dec. 2020.
Rail_Pssgrs_d <- NULL # Initialize a vector to contain daily railways passengers data.
for (i in 1:length(temp_date)){
  Rail_Pssgrs_d <- c(Rail_Pssgrs_d, rep(Rail_Pssgrs_m[i], temp_date[i])) # Fill all the dates in a month by the monthly average.
}

# Plots that validate to compute hypothetical 2019 google mobility data by multiplying 2020 data with the ratio between the numbers of railways passengers in 2019 and 2020.
if (ENG==1){
  temp_ylab <- "Average for Jan. 2020 (or 2020/1/3-2020/2/6) = 1"
  if (plot_paper==1){
    temp_main_label <- ""
    temp_xlab <- ""
  }else{
    temp_main_label <- "Comparison of mobility data"
    temp_xlab <- "Daily"
  }
}else{
  temp_xlab <- "日次"
  temp_ylab <- "2020年1月（もしくは2020/1/3-2020/2/6)平均＝1"
  temp_main_label <- "モビリティデータの比較"
}
#plot.ts(cbind(Rail_Pssgrs_d[(366+31+14):length(Rail_Pssgrs_d)]/Rail_Pssgrs_d[366+31+14],1+mob_var4[1:(length(Rail_Pssgrs_d)-(366+31+14)+1),1]/100),xaxt="n",xlab=temp_xlab,ylab=temp_ylab,col=1:2,main=temp_main_label)
#temp_dates <- conv_date_format(mob_date[29-14+1:(sum(ndays_normal)-(31+28))]) # Transform the format from yyyy-mm-dd to yyyy/m/d.
temp <- cbind(Rail_Pssgrs_d[(365+31+14+1):length(Rail_Pssgrs_d)]/Rail_Pssgrs_m[13],filter(1+mob_var4[1:(length(Rail_Pssgrs_d)-(365+31+14)),1]/100,rep(1/7,7))) # Take 7-day moving average of daily 2020 data and consruct a data matrix for a plot.
matplot(temp, type="l",xaxt="n",xlab=temp_xlab,ylab=temp_ylab,lty=1,col=1:2,main=temp_main_label) # Compare the two series from February 15, 2020, which is the starting date of Google Community Mobility Report. Railway passengers are normalized by the Jan. 2020 average.
temp_dates <- conv_date_format(mob_date[1:(sum(ndays_olympic)-(31+14))]) # Transform the format from yyyy-mm-dd to yyyy/m/d.
axis(side=1, at=c(1,round(length(temp_dates)*c(1:5)/5)), labels=temp_dates[c(1,round(length(temp_dates)*c(1:5)/5))])
if (ENG==1){
  legend("bottomright",legend=c("Monthly index of railway pessengers","transit_stations from Google (7-day mov. ave.)"),lty=1,col=1:2)
}else{
  legend("bottomright",legend=c("2020年鉄道旅客人数","2020年グーグル交通モビリティ"),lty=1,col=1:2)
}

if (ENG==1){
  temp_xlab <- "Month"
  temp_ylab <- "Thousand persons"
  temp_main_label <- "Railway passengers"
}else{
  temp_xlab <- "月"
  temp_ylab <- "千人"
  temp_main_label <- "鉄道旅客人数"
}
matplot(cbind(Rail_Pssgrs_m[1:12],Rail_Pssgrs_m[13:24]),type="l",lty=1,col=1:2,xlab=temp_xlab,ylab=temp_ylab,main=temp_main_label)
legend("bottomright",legend=c("2019","2020"),lty=1,col=1:2)


### Compute hypothetical 2019 google mobility data.

# Compute the ratio between the numbers of railways passengers in 2019 and the January 2020 value, because google mobility report uses the median of each day in a week between 2020 Jan. 3 and 2020 Feb. 6. as the baseline values.
Rail_Pssgrs_m_ratio_2019 <-  Rail_Pssgrs_m[1:12] / Rail_Pssgrs_m[13]

# Covert the ratio into daily data,
temp_date <- c(ndays_normal) # Create a series of date from Jan. 2019 to Dec. 2019.
temp_d <- NULL # Initialize the vector to contain daily mobility index for 2019.
for (i in 1:length(temp_date)){
  temp_d <- c(temp_d, rep(Rail_Pssgrs_m_ratio_2019[i], temp_date[i])) # Fill all the dates in a month by the month's figure in the monthly data.
}

# Convert the index into percentage changes from 1, which is the baseline level.
mob_var4_2019 <- (temp_d - 1) * 100

# Erase the element corresponding to 2019 Feb. 29.
#mob_var4_2019 <- mob_var4_2019[-(31+29)]

# Compare the mobility data created for 2019 and gooble mobility data from 2020 March 1.
if (ENG==1){
  #temp_xlab <- "Daily"
  temp_xlab <- "Number of days from Feb. 15, 2020 or Feb. 14, 2019"
  temp_ylab <- "Average for Jan. 2020 (or 2020/1/3-2020/2/6) = 1"
  if (plot_paper){
    temp_main_label <- ""
  }else{
    temp_main_label <- "Comparison of mobility data"
  }
}else{
  temp_xlab <- "日次"
  temp_ylab <- "2020年1月（もしくは2020/1/3-2020/2/6)平均＝1"
  temp_main_label <- "モビリティデータの比較"
}
#plot.ts(cbind(1+mob_var4_2019[(31+28+1):sum(ndays_normal)]/100,1+mob_var4[1:(sum(ndays_normal)-(31+28)),1]/100),xaxt="n",xlab=temp_xlab,ylab=temp_ylab,col=1:2,main=temp_main_label) 
#temp_dates <- mob_date[29-14+1:(sum(ndays_normal)-(31+28))]
temp_2019 <- 1+c(mob_var4_2019[(31+13+1):sum(ndays_normal)],mob_var4_2019[1:(31+13)])/100 # Construct 2019 mobility data from February 14, 2019, and connect the year-end to the beginning of 2019.
temp <- cbind(filter(1+mob_var4[1:length(temp_2019),1]/100, rep(1/7,7)),temp_2019) # Construct a data matrix. Take 7-day centered moving average of daily 2020 data.
matplot(temp, type="l", xlab=temp_xlab, ylab=temp_ylab, lty=1, col=1:2, main=temp_main_label) # Plot data from February 15, 2020, or February 14, 2019, because of the additional day in February 2020. 
#temp_dates <- mob_date[1:(sum(ndays_normal)-(31+13))]
#axis(side=1, at=c(1,round(length(temp_dates)*c(1:5)/5)), labels=temp_dates[c(1,round(length(temp_dates)*c(1:5)/5))])
if (ENG==1){
  legend("bottomright",legend=c("transit_stations from Google for 2020-21 (7-day mov. ave.)","Hypothetical index based on 2019 railway-passenger data"),lty=1,col=1:2)
}else{
  legend("bottomright",legend=c("2020-21年グーグル交通モビリティ","2019年鉄道旅客人数"),lty=1,col=1:2)
}


### Compute the nationwide weighted averages of temperature in 2019.
temper_var_ave_2019 <- c(temper_var_2019 %*% popu_share) 

### Compute the nationwide weighted averages of humidity in 2019.
hum_var_ave_2019 <- c(hum_var_2019 %*% popu_share) 

### Compute absolute humidity in 2019.
abs_hum_var_2019 <- calc_abs_hum(temper_var_2019,hum_var_2019)

### Create a dummy for the period when absolute humidity < ah_ub g/m^3. Then take the nationwide average of the indicator. 
ind_abs_hum_var_2019 <- c((abs_hum_var_2019<ah_ub) %*% popu_share) 


############## Endogeneize mobility data. ##############################
# Drop household expenditure for cafe from the explanatory variables, as cafe is likely to be a function of mobility.

# Apply OLS to model mobility in transportation as a function of household expenditure, temperature, and the states of emergency.
# mobility data is available from 2020 Feb 15.
# household expenditure data are available from 2020 Jan 1.
# temperature data is available from 2020 Jan. 1. 
# Dummy variables for states of emergencies start from 2020 Feb. 15.
mob_OLS_H_val <- H_expvals[,(31+15):dim(H_expvals)[2]] # Extract the data from 2020 Feb. 15 onward to set the same start date as mobility data.
nobs_mob_OLS <- dim(mob_OLS_H_val)[2] # The number of dates for OLS estimation. 
D_DEC <- rep(0, nobs_mob_OLS) # Initialize a dummy for December to control for year-end parties in a normal year.
D_DEC[29-14+sum(ndays_olympic[3:11])+1:31] <- 1 # Create a dummy for December to control for year-end parties in a normal year.
D_HD <- c(rep(c(1,1,rep(0,5)), (nobs_mob_OLS-2)/7),1,1) # Initialize a dummy for holidays, including weekends. Feb. 15, 2020, is Saturday. Jan. 31, 2020 is Sunday.
D_HD[-14+23:24] <- 1 # 2020/2/23-24
D_HD[29-14+20] <- 1 # 2020/3/20
D_HD[29-14+sum(ndays_olympic[3])+29] <- 1 # 2020/4/29
D_HD[29-14+sum(ndays_olympic[3:4])+3:6] <- 1 # 2020/5/3-6
D_HD[29-14+sum(ndays_olympic[3:6])+23:24] <- 1 # 2020/7/23-24
D_HD[29-14+sum(ndays_olympic[3:7])+10] <- 1 # 2020/8/10
D_HD[29-14+sum(ndays_olympic[3:8])+21:22] <- 1 # 2020/9/21-22
D_HD[29-14+sum(ndays_olympic[3:10])+3] <- 1 # 2020/11/3
D_HD[29-14+sum(ndays_olympic[3:10])+23] <- 1 # 2020/11/23
D_HD[29-14+sum(ndays_olympic[3:11])+29:31] <- 1 # 2020/12/29-31
D_HD[29-14+sum(ndays_olympic[3:12])+1:3] <- 1 # 2021/1/1-3
D_HD[29-14+sum(ndays_olympic[3:12])+11] <- 1 # 2021/1/11
D_SE1 <- rep(0, nobs_mob_OLS) # Initialize a dummy for the first state of emergency.
D_SE1[29-14+sum(ndays_olympic[3])+7:(ndays_olympic[4]+25)] <- 1 # 2020/4/7-2020/5/25
D_SE2 <- rep(0, nobs_mob_OLS) # Initialize a dummy for the second state of emergency.
D_SE2[(29-14+sum(ndays_olympic[3:12])+7):length(D_SE2)] <- 1 # 2021/1/7-2021/1/31

if (mdl_number==10){
  # Define data table for the lm object for prediction.
  # No household expenditures for clothing and footwear. There is no H6.
  mob_OLS_dep <- data.frame(H1=c(mob_OLS_H_val[1,]), H2=c(mob_OLS_H_val[2,]), H3=c(mob_OLS_H_val[3,]), H4=c(mob_OLS_H_val[4,]), H5=c(mob_OLS_H_val[5,]), TEMP=temper_var_ave[31+14+1:nobs_mob_OLS], HUM=hum_var_ave[31+14+1:nobs_mob_OLS], DEC=D_DEC, HD=D_HD, SE1=D_SE1, SE2=D_SE2)
  
  mob_OLS_rslt_1 <- lm(1+mob_var4[1:nobs_mob_OLS,1]/100 ~ H1+H2+H3*DEC+H4+H5+HD+SE1+SE2, data=mob_OLS_dep)
  
  # Record the parameter estimates.
  sink(paste0("mob_OLS_rslt_1_model",mdl_number,".txt"))
  print(summary(mob_OLS_rslt_1))
  sink()
  
  # Drop explanatory variables with insignificant coefficients and wrong signs, and re-run the regression for mobility in transportation.
  mob_OLS_rslt_2 <- lm(1+mob_var4[1:nobs_mob_OLS,1]/100 ~ H2+H3*DEC+H4+H5+HD+SE1+SE2, data=mob_OLS_dep) 
  
  # Record the parameter estimates
  sink(paste0("mob_OLS_rslt_2_model",mdl_number,".txt"))
  print(summary(mob_OLS_rslt_2))
  sink()
  
  # Drop the dummy variable for December and re-run the regression for mobility in transportation.
  #mob_OLS_rslt_3 <- lm(1+mob_var4[1:nobs_mob_OLS,1]/100 ~ H1+H2+H3+H5+TEMP, data=mob_OLS_dep) 
  mob_OLS_rslt_3 <- lm(1+mob_var4[1:nobs_mob_OLS,1]/100 ~ H1+H2+H3+H4+H5+DEC+HD+SE1+SE2, data=mob_OLS_dep)
  
  # Record the parameter estimates
  sink(paste0("mob_OLS_rslt_3_model",mdl_number,".txt"))
  print(summary(mob_OLS_rslt_3))
  sink()
  
}else{
  # Define data table for the lm object for prediction.
  mob_OLS_dep <- data.frame(H1=c(mob_OLS_H_val[1,]), H2=c(mob_OLS_H_val[2,]), H3=c(mob_OLS_H_val[3,]), H4=c(mob_OLS_H_val[4,]), H5=c(mob_OLS_H_val[5,]), H6=c(mob_OLS_H_val[6,]), TEMP=temper_var_ave[31+14+1:nobs_mob_OLS], HUM=hum_var_ave[31+14+1:nobs_mob_OLS], DEC=D_DEC, HD=D_HD, SE1=D_SE1, SE2=D_SE2)
  
  #mob_OLS_rslt_1 <- lm(1+mob_var4[1:nobs_mob_OLS,1]/100 ~ H1+H2+H3*DEC+H4+H5+H6+TEMP, data=mob_OLS_dep)
  mob_OLS_rslt_1 <- lm(1+mob_var4[1:nobs_mob_OLS,1]/100 ~ H1+H2+H3*DEC+H4+H5+H6+HD+SE1+SE2, data=mob_OLS_dep)
  
  # Record the parameter estimates.
  sink(paste0("mob_OLS_rslt_1_model",mdl_number,".txt"))
  print(summary(mob_OLS_rslt_1))
  sink()
  
  # Drop explanatory variables with insignificant coefficients and wrong signs and re-run the regression for mobility in transportation.
  #mob_OLS_rslt_2 <- lm(1+mob_var4[1:nobs_mob_OLS,1]/100 ~ H1+H2+H3*DEC+H5+TEMP, data=mob_OLS_dep) 
  mob_OLS_rslt_2 <- lm(1+mob_var4[1:nobs_mob_OLS,1]/100 ~ H2+H3*DEC+H4+H5+H6+HD+SE1+SE2, data=mob_OLS_dep) 
  
  # Record the parameter estimates
  sink(paste0("mob_OLS_rslt_2_model",mdl_number,".txt"))
  print(summary(mob_OLS_rslt_2))
  sink()
  
  # Drop the dummy variable for December and re-run the regression for mobility in transportation.
  #mob_OLS_rslt_3 <- lm(1+mob_var4[1:nobs_mob_OLS,1]/100 ~ H1+H2+H3+H5+TEMP, data=mob_OLS_dep) 
  mob_OLS_rslt_3 <- lm(1+mob_var4[1:nobs_mob_OLS,1]/100 ~ H1+H2+H3+H4+H5+H6+DEC+HD+SE1+SE2, data=mob_OLS_dep) 
  
  # Record the parameter estimates
  sink(paste0("mob_OLS_rslt_3_model",mdl_number,".txt"))
  print(summary(mob_OLS_rslt_3))
  sink()
}




# Compare predicted mobility in 2019 and computed mobility from railways passenger data in 2019 across different prediction models.
# Sample period: 2019 Jan. 01 - 2019 Dec. 31.
# Create a holiday dummy for 2019.
D_HD_2019 <- c(rep(c(rep(0,4),1,1,0), (365-1)/7),1) # Initialize a dummy for holidays, including weekends, from Jan. 01, 2019. Jan. 01, 2019, is Tuesday.
D_HD_2019[1:3] <- 1 #2019/1/1-3
D_HD_2019[sum(ndays_normal[1])+11] <- 1 #2019/2/11
D_HD_2019[sum(ndays_normal[1:2])+21] <- 1 #2019/3/21
D_HD_2019[sum(ndays_normal[1:3])+29:30] <- 1 #2019/4/29-30
D_HD_2019[sum(ndays_normal[1:4])+1:6] <- 1 #2019/5/1-6
D_HD_2019[sum(ndays_normal[1:6])+15] <- 1 #2019/7/15
D_HD_2019[sum(ndays_normal[1:7])+11:12] <- 1 #2019/8/11-12
D_HD_2019[sum(ndays_normal[1:8])+16] <- 1 #2019/9/16
D_HD_2019[sum(ndays_normal[1:8])+23] <- 1 #2019/9/23
D_HD_2019[sum(ndays_normal[1:9])+14] <- 1 #2019/10/14
D_HD_2019[sum(ndays_normal[1:9])+22] <- 1 #2019/10/22
D_HD_2019[sum(ndays_normal[1:10])+3:4] <- 1 #2019/11/3-4
D_HD_2019[sum(ndays_normal[1:10])+23] <- 1 #2019/11/23
D_HD_2019[sum(ndays_normal[1:11])+29:31] <- 1 #2019/12/29-31

if (mdl_number==10){
  # No household expenditures for clothing and footwear. There is no H6.
  mob_OLS_dep_2019 <- data.frame(H1=hes_var_2019_real[1,], H2=hes_var_2019_real[2,], H3=hes_var_2019_real[3,], H4=hes_var_2019_real[4,], H5=hes_var_2019_real[5,], TEMP=temper_var_ave_2019, HUM=hum_var_ave_2019, DEC=c(rep(0,365-31),rep(1,31)), HD=D_HD_2019, SE1=rep(0,365), SE2=rep(0,365)) # Create a matrix of explanatory variables.
}else{
  mob_OLS_dep_2019 <- data.frame(H1=hes_var_2019_real[1,], H2=hes_var_2019_real[2,], H3=hes_var_2019_real[3,], H4=hes_var_2019_real[4,], H5=hes_var_2019_real[5,], H6=hes_var_2019_real[6,], TEMP=temper_var_ave_2019, HUM=hum_var_ave_2019, DEC=c(rep(0,365-31),rep(1,31)), HD=D_HD_2019, SE1=rep(0,365), SE2=rep(0,365)) # Create a matrix of explanatory variables.
}
for (j in 1:3){
  eval(parse(text=paste0("mob_var4_2019_fitted_",j,"<- predict(mob_OLS_rslt_",j,", mob_OLS_dep_2019, interval = \"confidence\", level=0.95)")))
}

# Convert predicted daily data into monthly data.
for (j in 1:3){
  eval(parse(text=paste0("mob_var4_2019_fitted_",j,"_m <-matrix(NA,nr=12,nc=3)"))) # Initialize the vector to contain monthly data.
  eval(parse(text=paste0("mob_var4_2019_fitted_",j,"_m[1,] <- apply(mob_var4_2019_fitted_",j,"[1:ndays_normal[1],],2,mean)"))) # 2019 Jan average.
  for (i in 2:12){
    eval(parse(text=paste0("mob_var4_2019_fitted_",j,"_m[i,] <- apply(mob_var4_2019_fitted_",j,"[sum(ndays_normal[1:(i-1)])+1:ndays_normal[i],],2,mean)"))) # The average of i-th month in 2019.
  }
}


# Plot predicted mobility and computed mobility in 2019 across different prediction models.
temp <- Rail_Pssgrs_m_ratio_2019 # Initialize the data matrix to plot.
for (j in 1:3){
  eval(parse(text=paste0("temp <- cbind(temp, mob_var4_2019_fitted_",j,"_m)")))
}
if (ENG==1){
  temp_xlab <- "Month in 2019"
  temp_ylab <- "Average for Jan. 2020 (or 2020/1/3-2020/2/6) = 1"
  temp_main_label <- "Prediction of mobility in transportation in 2019"
}else{
  temp_xlab <- "月"
  temp_ylab <- "2020年1月（もしくは2020/1/3-2020/2/6)平均＝1"
  temp_main_label <- "2019年の交通モビリティデータの推計値（月平均）"
}
  matplot(temp, type="l", ylab=temp_ylab, xlab=temp_xlab, col=c(1,2,2,2,3,3,3,4,4,4),lty=c(1,1,2,2,1,2,2,1,2,2),main=temp_main_label)
if (ENG==1){
  legend("bottomleft",legend=c("Railway passengers in 2019","Prediction of regression 1","Prediction of regression 2","Prediction of regression 3"),lty=1,col=1:4)
}else{
  legend("bottomleft",legend=c("2019年鉄道旅客人数","グーグル交通モビリティデータ（2020年2月～）による回帰予測1","グーグル交通モビリティデータ（2020年2月～）による回帰予測2","グーグル交通モビリティデータ（2020年2月～）による回帰予測3"),lty=1,col=1:4)
}


# Plot predicted mobility and computed mobility in 2019 with the regression formula for simulations.
temp <- Rail_Pssgrs_m_ratio_2019 # Initialize the data matrix to plot.
for (j in 2){ #For simulation, model2 is used.
  eval(parse(text=paste0("temp <- cbind(temp, mob_var4_2019_fitted_",j,"_m)")))
}
if (ENG==1){
  temp_xlab <- "Month in 2019"
  temp_ylab <- "Average for Jan. 2020 (or 2020/1/3-2020/2/6) = 1"
  if (plot_paper){
    temp_main_label <- ""
  }else{
    temp_main_label <- "Prediction of mobility in transportation in 2019"
  }
}else{
  temp_xlab <- "月"
  temp_ylab <- "2020年1月（もしくは2020/1/3-2020/2/6)平均＝1"
  temp_main_label <- "2019年の交通モビリティデータの推計値（月平均）"
}
matplot(temp, type="l", ylab=temp_ylab, xlab=temp_xlab, col=c(1,2,2,2,3,3,3,4,4,4),lty=c(1,1,2,2,1,2,2,1,2,2),main=temp_main_label)
if (ENG==1){
  legend("bottomleft",legend=c("Index of railway passengers in 2019","Prediction by OLS regression"),lty=1,col=1:2)
}else{
  legend("bottomleft",legend=c("2019年鉄道旅客人数","人流内生化のための回帰式の予測"),lty=1,col=1:2)
}
  

############## Compute a hypothetical time series of the reproduction number without interventions using 2019 data. ##############################

if (ENG==1){
  if (plot_paper==1){
    R_label <- ""
    time_label <- ""
  }else{
    R_label <- "R"
    time_label <- "Daily (Dotted lines are the first and last dates of each state of emergency)"
  }
  hes_var_nm_2019_ENG <- c("Eating out for meals", "Cafe", "Bar", "Lodging", "Domestic travel packages", "Clothing and footwear") # Translation of six household expenditures in the explanatory variables.
}else{
  R_label <- "実効再生産数"
  time_label <- "日次（破線は緊急事態宣言の開始と解除の日付）"
}

# Extract mcmc samples.
ms <- rstan::extract(fit)

# Drop burnins.
niter <- length(ms[[1]]) # Number of mcmc samples. The first element of mc (gama) is a vector.
for (i in 1:(length(ms)-1)){ # The last element of ms is the log likelihood time (-1). 
  if (length(dim(ms[[i]]))==1){
    ms[[i]] <-ms[[i]][(round(niter/2)+1):niter] # Drop the burn-ins.
  }else if(length(dim(ms[[i]]))==2){
    ms[[i]] <-ms[[i]][(round(niter/2)+1):niter,] # Drop the burn-ins.
  }else{
    ms[[i]] <-ms[[i]][(round(niter/2)+1):niter,,] # Drop the burn-ins.
  }
}

# Construct explanatory variables for simulation from 2019 February 14, so that the first reproduction number to simulate is on 2019 March 6.
# hes_var_2019_real and mob_var4_2019 start from 2019 Jan 1.
H_expvals_simu_2019 <- cbind(hes_var_2019_real[,(31+14):365], hes_var_2019_real[,1:(31+28+4)]) # Move data in the early part of 2019 up to March 4 to the end, so that March 5 reproduction number can be computed.
M_trans_simu_2019 <- c(mob_var4_2019[(31+14):365], mob_var4_2019[1:(31+28+4)]) # Move data in the early part of 2019 up to March 4 to the end, so that March 5 reproduction number can be computed.

# Construct baseline explanatory variables for comparison from 2020 February 15, so that the first reproduction number to simulate is on 2020 March 6.
H_expvals_base <- H_expvals[,(31+15):dim(H_expvals)[2]] # H_expvals defined for the regression starts from 2020 Jan. 1.
M_trans_base <- mob_var4[1:dim(H_expvals_base)[2], 1] # mob_var4 starts from 2020 February 15.

# Construct weather data for simulation. W_abs_hum starts from 2020 Jan. 1. abs_hum_simu is set to start from 2020 February 15, i.e., 14 days before 2020 Feb. 29.
if (length(weath_date) < sum(ndays_olympic)+31+28+3){
  # If the sample period of weather data does not include 2021 March 4.
  # The year-end is combined with the beginning of the year up to 2021 March 3 to the end.
  # 2021 Feb 15 is not dropped when the year-end is connected to the beginning of 2020 data as the next-year data. Because 2020 Feb. 29 is included in the connected next-year data.
  # 2020 March 3 is regarded as March 4 in a normal year.
  abs_hum_simu <- c(W_abs_hum[(31+15):length(W_abs_hum)],W_abs_hum[(length(W_abs_hum)-366):(31+28+3)])
}else{
  # If the sample period of weather data includes 2021 March 4.
  abs_hum_simu <- W_abs_hum[(31+15):which(Weath_date==2021/3/3)]
}

# Construct weather data for a new-year dummy. D_NY is set to start from 2020 February 15. It is extended to March 4 in the next year.
D_NY_simu <- c(rep(0,15+sum(ndays_olympic[3:11])+28),rep(1,6),rep(0,31-3+28+4))

# Compute fitted reproduction numbers for a set of mcmc samples.
# The first date of fitted values of R is to be compared to R on 2020 March 6.
temp <- R_simu(ms_R = ms, H_expvals = H_expvals_simu_2019, H_expvals_base = H_expvals_base, M_trans = M_trans_simu_2019, M_trans_base = M_trans_base, W_abs_hum = abs_hum_simu, dist_incub = dist_incub, D_NY = D_NY_simu)

# Extract the mcmc samples of the fitted reproduction number.
R_fitted_2019 <- temp$R

# Comparing the effects of two sets of explanatory variables.
comp_effect_2020_2019 <- temp$comp_effect

# Compute the mean and the percentile values of time-varying parameters in vectors.

name_list<-c("R_fitted_2019") # Variable name in the stan file.

for (i in 1:length(name_list)){
  
  eval(parse(text=paste0("temp <- Extract_mean_ptl(",name_list[i],", ptl=c(0.025,0.975))")))
  eval(parse(text=paste0(name_list[i],"_pm <- temp[[1]]"))) # posterior mean
  eval(parse(text=paste0(name_list[i],"_025 <- temp[[2]]"))) # 5% percentitle
  eval(parse(text=paste0(name_list[i],"_975 <- temp[[3]]"))) # 97.5% percentitle
}

name_list<-c("Fitted_R") # Variable name in the stan file.

for (i in 1:length(name_list)){
  
  eval(parse(text=paste0("temp <- Extract_mean_ptl(ms$",name_list[i],", ptl=c(0.025,0.975))")))
  eval(parse(text=paste0(name_list[i],"_pm <- temp[[1]]"))) # posterior mean
  eval(parse(text=paste0(name_list[i],"_025 <- temp[[2]]"))) # 2.5% percentitle
  eval(parse(text=paste0(name_list[i],"_975 <- temp[[3]]"))) # 97.5% percentitle
}


# Define the labels for dates starting from 2020 March 6, exclusive, to 365 days later, 2021 March 5.
date_label_simu <- mob_date[(29-14+6):which(mob_date=="2021-03-05")]

# Find the location of the period of the state of emergency.
SE_simu <- c(which(date_label_simu=="2020-04-07"),which(date_label_simu=="2020-05-25"),which(date_label_simu=="2021-01-07"),which(date_label_simu=="2021-03-21")) 

# Define the dependent variable. 
DEP <- R[6:which(R_date=="2021/3/5")] # R_var starts from 2020 March 1. Set the start date to 2020 March 6.

#  Fitted-R based on 2020 data starts from March 6. Make it a series of 365 days.
if (length(Fitted_R_pm) >= 365){
  # Limit the time series to 365 days.
  Fitted_R_2020_pm <- Fitted_R_pm[1:365]
  Fitted_R_2020_025 <- Fitted_R_025[1:365]
  Fitted_R_2020_975 <- Fitted_R_975[1:365]
}else{
  # Fulfill the periods beyond the estimation periods by NAs.
  Fitted_R_2020_pm <- c(Fitted_R_pm, rep(NA,365-length(Fitted_R_pm)))
  Fitted_R_2020_025 <- c(Fitted_R_025, rep(NA,365-length(Fitted_R_pm)))
  Fitted_R_2020_975 <- c(Fitted_R_975, rep(NA,365-length(Fitted_R_pm)))
} 

# Plot the fitted values of the reproduction number.
temp_nl <- c("R_fitted_2019")
temp_nl2 <- c("Fitted_R_2020")
for (i in 1:(length(temp_nl))){
  eval(parse(text=paste0("temp_nl_with_CI2 <- c(\"",temp_nl2[i],"_pm, ", temp_nl2[i],"_025, ",temp_nl2[i],"_975\")"))) # Define character names for the posterior mean and 2.5% and 97.5% percentiles.
  eval(parse(text=paste0("temp <- cbind(",temp_nl_with_CI2,")"))) # Create a data table for a plot of observed data and estimated parameters.
  eval(parse(text=paste0("temp_nl_with_CI <- c(\"",temp_nl[i],"_pm, ", temp_nl[i],"_025, ",temp_nl[i],"_975\")"))) # Define character names for the posterior mean and 2.5% and 97.5% percentiles.
  eval(parse(text=paste0("temp <- cbind(temp,",temp_nl_with_CI,")"))) # Create a data table for a plot of observed data and estimated parameters.
}
if (log_R == 1){
  temp_ylab <- "Log" # R is the log of the reproduction number.
}else{
  temp_ylab <- "Level" # R is the level of the reproduction number.
}
matplot(cbind(DEP, temp), type="l", col=c(1,2,2,2,3,3,3), lty=c(1,1,2,2,1,2,2), xaxt="n", xlab=time_label, ylab=temp_ylab, main=R_label)
axis(side=1, at=c(1,round(nrow(temp)*c(1:5)/5)), labels=conv_date_format(date_label_simu[c(1,round(nrow(temp)*c(1:5)/5))]))
if (log_R == 1){
  abline(h=0,lty=3) # h=0 because R is the log of the reproduction number.
}else{
  abline(h=1,lty=3) # h=1 because R is the level of the reproduction number.
}
for (j in 1:length(SE_simu)){
  abline(v=SE_simu[j],lty=5)
}  
if (ENG==1){
  legend("topright",legend=c("Observed R", "Fitted R with 2020-2021 data", "Simulated R with hypothetical 2019 data"), lty=c(1,1,1), col=c(1,2,3))
}else{
  legend("topright",legend=c("観測値", "回帰式のfitted valueの事後平均", "2019年データ外挿シミュレーション事後平均"), lty=c(1,1,1), col=c(1,2,3))
}


# Compute the mean, the 2.5% percentile, and 97.5% percentile of the fitted value of R using 2019 data.
# mcmc samples are on the rows.
temp_mean_R <- apply(R_fitted_2019,1,mean) #R_fitted_2019 is the mcmc samples. Compute the mean over the year.
mean_R_pm <- c(mean(temp_mean_R), quantile(temp_mean_R,c(0.025,0.975))) # Compute the distribution of the means.
eval(parse(text=paste0("sink(paste0(\"mean_R_pm0_model",mdl_number,"_",R_sfx,".txt\"))")))
print("The posterior mean, 2.5%, and 97.5% percentiles of the simulated reproduction number using 2019 data.")
names(mean_R_pm) <- c("Posterior mean","2.5%","97.5%")
print(mean_R_pm)
sink()


# Compare household expenditure in 2019 and 2020.
for (i in 1:dim(H_expvals_base)[1]){
  temp <- cbind(c(H_expvals_base[i,],rep(NA,365-dim(H_expvals_base)[2])), H_expvals_simu_2019[i,1:365]) # Combine 2020 data and 2019 data. The latter starts from February 16, 2019, and the year-end is connected with the beginning of the year. Plot non-repeated 2019 data for 365 days.
  temp <- apply(temp,2,filter,rep(1/7,7)) # Take centered 7-day moving average.

  if (ENG==1){
    temp_xlab <- "Number of days from Feb. 15, 2020 or Feb. 14, 2019"
    #temp_ylab <- "100 yen (per household per day, in 2020 real value, 7-day mov. ave.）"
    temp_ylab <- "100 yen (per household per day, in 2020 real value）"
    #temp_main_label=paste0(hes_var_nm_2019_ENG[i],"(7-day moving ave.)")
    temp_main_label=hes_var_nm_2019_ENG[i]
  }else{
    temp_xlab <- "2020年2月16日（もしくは2019年2月15日）からの日数"
    temp_ylab <- "百円（一世帯・一日当たり、2020年水準実質値、7日間移動平均）"
    #temp_main_label=paste0(hes_var_nm_2019[i],"(7日間加重平均）")
    temp_main_label=hes_var_nm_2019[i]
  }
  matplot(temp, type="l",lty=1, col=1:2, xlab=temp_xlab, ylab=temp_ylab, main=temp_main_label, cex.main=cex_val, cex.axis=cex_val_axis, cex.lab=cex_val) # Plot H_expvals for 365 days from 2020 February 16 or 2019 February 15.
  axis(side=1, at=364, cex.axis=cex_val_axis)# Add the label at the end of x-axis to indicate the length of each data series is 365 days. The label is 364 because the first label on the x axis is 0.
  if (ENG==1){
      legend("topright",legend=c("2020-2021 data","Hypothetical 2019 data"),lty=1,col=1:2,cex=cex_val)
    }else{
      legend("topright",legend=c("2020年～2021年データ","2019年データに基づく仮想データ"),lty=1,col=1:2,cex=cex_val)
    }
  
  # The length of the sample period for comp_effect_2020_2019 is that of baseline explanatory variables minus 13 (because of 14 days lags) minus 6 (because the reproduction number is 7-days average of the growth rate of infections.) 
  if (ENG==1){
    temp_xlab <- "Number of days from March 6 2020 or 2019"
    temp_ylab <- "Contribution to the difference in R"
    temp_main_label <- paste0(hes_var_nm_2019_ENG[i],": Contribution to differences in R between 2020 and 2019")
  }else{
    temp_xlab <- "3月6日からの日数"
    temp_ylab <- "実効再生産数への効果"
    temp_main_label <- paste0(hes_var_nm_2019[i],"の実効再生産数への寄与の予測値と2019年仮想値の差")
  }
  plot(-comp_effect_2020_2019[i,1:(dim(H_expvals_base)[2]-13-6)], type="l", xlab=temp_xlab, ylab=temp_ylab, main=temp_main_label) 
  
}

# Compare mobility data in 2019 and 2020.
if (ENG==1){
  temp_xlab <- "Number of days from Feb. 15, 2020 or Feb. 14, 2019"
  temp_ylab <- "Average for 2020/1/3-2020/2/6 = 1"
  if(plot_paper == 1){
    temp_main_label <- ""
  }else{
    temp_main_label <- "Mobility in transportation"
  }
}else{
  temp_xlab <-  "2020年2月16日（もしくは2019年2月15日）からの日数"
  temp_ylab <- "2020/1/3-2020/2/6の平均＝1、7日間移動平均"
  temp_main_label <- "交通モビリティデータ"
}
matplot(cbind(filter(1+M_trans_base/100, rep(1/7,7)), 1+M_trans_simu_2019[1:length(M_trans_base)]/100), type="l",lty=1, col=1:2, xlab=temp_xlab, ylab=temp_ylab, main=temp_main_label) # Plot H_expvals for 365 days from 14 days before 2020 March 1.
axis(side=1, at=364)# Add the label at the end of x-axis to indicate the length of each data series is 365 days. The label is 364 because the first label on the x axis is 0.
if (ENG==1){
  legend("bottomright",legend=c("transit_stations from Google for 2020-2021 (7-day mov. ave.)","Hypothetical 2019 railway-passenger data"),lty=1,col=1:2)
}else{
  legend("bottomright",legend=c("2020～2021年（グーグル、7日間移動平均）","2019年（鉄道旅客人数、月次）"),lty=1,col=1:2)
}

if (ENG==1){
  temp_xlab <- "Number of days from March 6 2020 or 2019"
  temp_ylab <- "Contribution to the difference in R"
  temp_main_label <- "Mobility in transportation: Contribution to differences in R between 2020 and 2019"
}else{
  temp_xlab <- "3月6日からの日数"
  temp_ylab <- "実効再生産数への効果"
  temp_main_label <- "交通モビリティの実効再生産数への寄与の予測値と2019年仮想値の差"
}
plot(-comp_effect_2020_2019[dim(H_expvals_base)[1]+1,1:(length(M_trans_base)-13-6)], type="l",xlab=temp_xlab, ylab=temp_ylab, main=temp_main_label)

### Close the pdf file.
dev.off()


############## Simulate the reproduction number with hypothetical interventions. ##############################

# Compute the average expenditure over various periods.
H_exp_ave_2020_9to11 <- apply(H_expvals[,which(hes_var_date=="2020-9-1"):which(hes_var_date=="2020-11-30")],1,mean) #for 2020 Sep-Nov
H_exp_ave_2020_betweenSEGOTO <- apply(H_expvals[,which(hes_var_date=="2020-5-26"):which(hes_var_date=="2020-7-21")],1,mean) #for the period between the end of the first state of emergeny and the beginning of the GO-TO-TRAVEL period.
H_exp_ave_2019 <- apply(hes_var_2019_real,1,mean) #for the whole year.

# Construct weather data for simulation. W_abs_hum starts from 2020 Jan. 1. abs_hum_simu is set to start from 2020 February 15, i.e., 14 days before 2020 Feb. 29.
if (length(weath_date) < sum(ndays_olympic)+31+28+3){
  # If the sample period of weather data does not include 2021 March 4.
  # The year-end is combined with the beginning of the year up to 2021 March 3 to the end.
  # 2021 Feb 15 is not dropped when the year-end is connected to the beginning of 2020 data as the next-year data. Because 2020 Feb. 29 is included in the connected next-year data.
  # 2020 March 3 is regarded as March 4 in a normal year.
  temper_base <- c(temper_var_ave[(31+15):length(temper_var_ave)],temper_var_ave[(length(temper_var_ave)-366):(31+28+3)])
  hum_base <- c(hum_var_ave[(31+15):length(hum_var_ave)],hum_var_ave[(length(hum_var_ave)-366):(31+28+3)])
}else{
  # If the sample period of weather data includes 2021 March 4.
  temper_base <- temper_var_ave[(31+15):which(Weath_date==2021/3/3)]
  hum_base <- hum_var_ave[(31+15):which(Weath_date==2021/3/3)]
}

# Define the labels for dates starting from 2020 February 15, exclusive, to 366 + 13 (i.e., 14 lags for incubations - 1 lag for R) + 6 (7 days average for R - 1 for the current day) days later.
date_label_simu_mob <- mob_date[1:dim(H_expvals_simu_2019)[2]] # mob_date starts from February 15.

# Find the location of the period of the state of emergency.
SE_simu_mob <- c(which(date_label_simu_mob=="2020-04-07"),which(date_label_simu_mob=="2020-05-25"),which(date_label_simu_mob=="2021-01-07"),which(date_label_simu_mob=="2021-03-21")) 

# Set the upper limit on mobility in terms of percentage points to the benchmark date. 
#sim_mob_perc <- c(Inf,-10,-20,-30,-40,-50) # "Inf" means no restriction.
sim_mob_perc <- c(Inf) # "Inf" means no restriction.

# Set the fraction of revenue for cafe and drink to curb based on 2019 average revenue in 2020 prices for each iteration.
sim_revn_frac <- c(0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1)

# Set the value of the restriction on cafe and drink to draw in a plot.
sim_revn_frac_plot <- list(c(1),c(1),c(1),c(0.85),c(0.9),c(0.9))


#### Specify simulations to run. 
# flag_simu: Specify the type of simulation.
# 1st element: Endogenous (1) or exogenous mobility (0). 
# 2th element: Exogenous restriction on mobility (1) or not (0).
# 3nd element: Special treatment of holidays (golden weeks, Obon, the new year period) (1) or not (0).
# 4rd element: Regular restriction on cafe and drink (0) or eating out in general (1).
# 5th element: Restriction on Domestic travel packages (1) or not (0).

flag_simu <- rbind(c(0,0,0,0,0),
                   c(1,0,0,0,0),
                   c(0,0,0,0,1),
                   c(1,0,0,0,1),
                   c(0,1,0,0,1),
                   c(1,1,0,0,1))
                   #c(1,1,1,0,1),
                   #c(1,1,1,1,1))


# Iterate simulations.

for (s in 1:dim(flag_simu)[1]){

  # Record the result of each simulation in a pdf file.
  eval(parse(text=paste0("pdf(paste0(\"simu",s,"_reformed_data_model",mdl_number,"_ENG",as.logical(ENG),"_",R_sfx,".pdf\"), family=\"Japan1GothicBBB\")")))

  # Initialize a matrix to save the posterior mean, 2.5% percentile, and 97.5% percentile of the annual mean of R.
  if (flag_simu[s,2] == 1){
    mean_R_pm <- matrix(NA, nr=length(sim_mob_perc), nc=length(sim_revn_frac)*3) # Rows: restrictions on mobility.
  }else{
    mean_R_pm <- rep(NA, length(sim_revn_frac)*3)
  }

  # Initialize a list to hypothetical household expenditure.
  temp_hes_intv <- list()
  
  for (k in 1:length(sim_mob_perc)){
    
    # Initialize the matrix to store the simulations of interventions.
    temp_intv_rslt <- NULL
    
    # Initialize the matrix to store the simulations of interventions.
    temp_mob_rslt <- NULL
    
    for (j in 1:length(sim_revn_frac)){
      
      ##### Simulation set-up #####
      
      # Initialize household expenditure and mobility data for simulation.
      H_expvals_simu_intv <- H_expvals_simu_2019
      
      # Insert household expenditure for cafe and drink intervention.
      H_expvals_simu_intv[2,] <- H_expvals_simu_2019[2,] * (1 - sim_revn_frac[j])  
      H_expvals_simu_intv[3,] <- H_expvals_simu_2019[3,] * (1 - sim_revn_frac[j])  
      
      
      if (flag_simu[s,4]==1){
        # Insert household expenditure for eating-out intervention.
        H_expvals_simu_intv[1,] <- H_expvals_simu_2019[1,] * (1 - sim_revn_frac[j])  
      }
      
      
      if (flag_simu[s,5]==1){
        # "Domestic packaged travel" for the period between the end of the first state of emergency and the beginning of GO-TO-TRAVEL was 0.16 of 2019 average in 2020 prices.
        # Household expenditure for domestic packaged travel is fixed to the average in the period between the first state of emergency and GO-TO-TRAVEL in 2020.
        H_expvals_simu_intv[5,] <- mean(H_expvals_base[5,which(date_label_simu_mob=="2020-05-26"):which(date_label_simu_mob=="2020-07-21")])
      }      
      
      
      if (flag_simu[s,3]==1){
        # Set household expenditure for drink in the golden-week period and December to the average in the other period in the year.
        H_expvals_simu_intv[3,c(which(date_label_simu_mob=="2020-04-27"):which(date_label_simu_mob=="2020-05-08"), which(date_label_simu_mob=="2020-12-01"):which(date_label_simu_mob=="2021-01-03"))] <- mean(H_expvals_simu_intv[3,c(1:which(date_label_simu_mob=="2020-04-26"), which(date_label_simu_mob=="2020-05-09"):which(date_label_simu_mob=="2020-11-30"), which(date_label_simu_mob=="2021-01-04"):which(date_label_simu_mob=="2021-02-13"))])
        
        # Set household expenditure for non-drink items in golden week and the new-year period to the average in the other period.
        ind_non_drink_GW_NY <- c(1,2,4) # Non-drink industries to restrain during the golden week period.
        if (mdl_number!=10){
          ind_non_drink_NY <- c(6) # Non-drink industries to restrain during the golden week period. If mdl_number =10, there is no clothing and footwear, which is on the 6th row.
        }
        
        if (length(ind_non_drink_GW_NY)>1){
          temp_GW_NY <- rowMeans(H_expvals_simu_intv[ind_non_drink_GW_NY, c(1:which(date_label_simu_mob=="2020-04-26"), which(date_label_simu_mob=="2020-05-09"):which(date_label_simu_mob=="2020-12-28"), which(date_label_simu_mob=="2021-01-04"):which(date_label_simu_mob=="2021-02-13"))]) # The average in the other period.
        }else{
          temp_GW_NY <- mean(H_expvals_simu_intv[ind_non_drink_GW_NY, c(1:which(date_label_simu_mob=="2020-04-26"), which(date_label_simu_mob=="2020-05-09"):which(date_label_simu_mob=="2020-12-28"), which(date_label_simu_mob=="2021-01-04"):which(date_label_simu_mob=="2021-02-13"))]) # The average in the other period.
        }
        
        for (i in ind_non_drink_GW_NY){
          H_expvals_simu_intv[i,which(date_label_simu_mob=="2020-04-27"):which(date_label_simu_mob=="2020-05-08")] <- temp_GW_NY[which(ind_non_drink_GW_NY==i)] # The golden-week period.
          H_expvals_simu_intv[i,which(date_label_simu_mob=="2020-12-29"):which(date_label_simu_mob=="2021-01-03")] <- temp_GW_NY[which(ind_non_drink_GW_NY==i)] # The new-year period.
        }
        
        if (mdl_number!=10){
          if (length(ind_non_drink_NY)>1){
            temp_NY <- rowMeans(H_expvals_simu_intv[ind_non_drink_NY, c(1:which(date_label_simu_mob=="2020-12-28"), which(date_label_simu_mob=="2021-01-04"):which(date_label_simu_mob=="2021-02-13"))]) # The average in the other period.
          }else{
            temp_NY <- mean(H_expvals_simu_intv[ind_non_drink_NY, c(1:which(date_label_simu_mob=="2020-12-28"), which(date_label_simu_mob=="2021-01-04"):which(date_label_simu_mob=="2021-02-13"))]) # The average in the other period.
          }
          
          for (i in ind_non_drink_NY){
            H_expvals_simu_intv[i,which(date_label_simu_mob=="2020-12-29"):which(date_label_simu_mob=="2021-01-03")] <- temp_NY[which(ind_non_drink_NY==i)] # The new-year period.
          }
        }
      }
      
      
      if (flag_simu[s,1]==1){
        # Compute a change in mobility in transportation. 
        # Use Model 2: only variables with significant coefficients are used with a dummy for December.
        # H_expvals_simu starts from 2019 February 14. The year-end is connected with 2019 Jan. 1 data. It continues up to March 4, so that March 5 reproduction number can be computed.
        if (mdl_number==10){
          # There is no H6, as there is no household expenditures for clothing and footwear in the explanatory variables.
          temp_mob_OLS <- data.frame(H1=H_expvals_simu_intv[1,], H2=H_expvals_simu_intv[2,], H3=H_expvals_simu_intv[3,], H4=H_expvals_simu_intv[4,], H5=H_expvals_simu_intv[5,], TEMP=temper_base, HUM=hum_base, DEC=c(rep(0,365-(31+13)-31),rep(1,31),rep(0,31+28+4)), HD=c(D_HD_2019[(31+14):365],D_HD_2019[1:(31+28+4)]), SE1=rep(0,365-31-13+31+28+4), SE2=rep(0,365-31-13+31+28+4)) # Create a matrix of explanatory variables. H_HD_2019 starts from Feb. 14, 2019, and is connected with the new year in 2019 at the year-end.
        }else{
          temp_mob_OLS <- data.frame(H1=H_expvals_simu_intv[1,], H2=H_expvals_simu_intv[2,], H3=H_expvals_simu_intv[3,], H4=H_expvals_simu_intv[4,], H5=H_expvals_simu_intv[5,], H6=H_expvals_simu_intv[6,], TEMP=temper_base, HUM=hum_base, DEC=c(rep(0,365-(31+13)-31),rep(1,31),rep(0,31+28+4)), HD=c(D_HD_2019[(31+14):365],D_HD_2019[1:(31+28+4)]), SE1=rep(0,365-31-13+31+28+4), SE2=rep(0,365-31-13+31+28+4)) # Create a matrix of explanatory variables. H_HD_2019 starts from Feb. 14, 2019, and is connected with the new year in 2019 at the year-end.
        }  
        
        M_trans_intv <- predict(mob_OLS_rslt_2, temp_mob_OLS, interval = "confidence", level=0.95)
        
        M_trans_intv <- (M_trans_intv[,1]-1)*100 # Keep only the posterior mean and convert it into percentage points.
        
      }else{
        
        # Because the causality between cafe and mobility is likely to be mutual, fix the mobility data to the baseline 2020 value, and the data up to the end of the first state of emergency to the 2020 average for Jun.-Nov.
        M_trans_intv <- mob_var4[1:which(date_label_simu_mob=="2021-03-04"),1] # Set mobility data for simulation to 2020 values.
        M_trans_intv[1:which(date_label_simu_mob=="2020-05-25")] <- mean(mob_var4[which(date_label_simu_mob=="2020-06-01"):which(date_label_simu_mob=="2020-11-30"),1])
      }    
      
      
      if (flag_simu[s,2]==1){
        # Upper limit on mobility. - 60% is the level in the first state of emergency.
        M_trans_intv[M_trans_intv > sim_mob_perc[k]] <- sim_mob_perc[k] 
      }
      
      #####      
      
      # Compute fitted reproduction numbers for a set of mcmc samples.
      # The first date of fitted values of R is to be compared to R on 2020 March 6.
      temp_simu <- R_simu(ms_R = ms, H_expvals = H_expvals_simu_intv, H_expvals_base = H_expvals_base, M_trans = M_trans_intv, M_trans_base = M_trans_base, W_abs_hum = abs_hum_simu, dist_incub = dist_incub, D_NY = D_NY_simu)
      
      # Extract simulated reproduction number.
      temp_R <- temp_simu$R
      
      # Compute the mean and the percentile values of time-varying parameters in vectors.
      
      name_list<-c("temp_R") # Variable name in the stan file.
      
      for (i in 1:length(name_list)){
        
        eval(parse(text=paste0("temp <- Extract_mean_ptl(",name_list[i],", ptl=c(0.025,0.975))")))
        eval(parse(text=paste0(name_list[i],"_pm <- temp[[1]]"))) # posterior mean
        eval(parse(text=paste0(name_list[i],"_025 <- temp[[2]]"))) # 2.5% percentile
        eval(parse(text=paste0(name_list[i],"_975 <- temp[[3]]"))) # 97.5% percentile
      }
      
      # Store the result of simulated reproduction numbers.
      temp_intv_rslt <- cbind(temp_intv_rslt, temp_R_pm, temp_R_025, temp_R_975)
      
      # Store the result of simulated mobility.
      temp_mob_rslt <- cbind(temp_mob_rslt, 1+M_trans_intv/100)
      
      # Store the hypothetical values of household expenditures.
      temp_hes_intv[[j]] <- H_expvals_simu_intv
      
      # Compute the mean, the 2.5% percentile, and 97.5% percentile of the mean of R over the simulation period.
      # mcmc samples are on the rows.
      temp_mean_R <- apply(temp_R,1,mean)
      
      if (flag_simu[s,2] == 1){
        # Store the posterior mean, 33% percentile, and 66% percentile of the annual mean simulated R over the simulation period.
        mean_R_pm[k,(j-1)*3+1:3] <- c(mean(temp_mean_R), quantile(temp_mean_R,c(0.025,0.975)))
      }else{
        # Store the posterior mean, 33% percentile, and 66% percentile of the annual mean simulated R over the simulation period.
        mean_R_pm[(j-1)*3+1:3] <- c(mean(temp_mean_R), quantile(temp_mean_R,c(0.025,0.975)))
      }    
      
    }
    
    
    # Create the mail label for plots.
    if(flag_simu[s,1]==1){
      if (ENG==1){
        temp_ml_1 <- "Endogenous mobility"
      }else{
        temp_ml_1 <- "人流（交通）内生"
      }
    }else{
      if (ENG==1){
        temp_ml_1 <- "Exogenous mobility"
      }else{
        temp_ml_1 <- "人流（交通）固定"
      }
    }
    
    if(flag_simu[s,2]==0 || sim_mob_perc[k]==Inf){
      if (ENG==1){
        temp_ml_2 <- NULL
      }else{
        temp_ml_2 <- NULL
      }
    }else{
      if (ENG==1){
        temp_ml_2 <-  paste0(sim_mob_perc[k], "% mobility")
      }else{
        temp_ml_2 <-  paste0("人流（交通）", sim_mob_perc[k], "%以下")
      }
    }
    
    if(flag_simu[s,3]==1){
      if (ENG==1){
        temp_ml_3 <- "Seas. intvn"
      }else{
        temp_ml_3 <- "季節性制限有"
      }
    }else{
      if (ENG==1){
        temp_ml_3 <- NULL
      }else{
        temp_ml_3 <- NULL
      }
    }
    
    if(flag_simu[s,4]==1){
      if (ENG==1){
          temp_ml_4 <- "All eating out"
        }else{
          temp_ml_4 <- "外食一般制限"
        }
    }else{
      if (ENG==1){
          temp_ml_4 <- "Only cafe & drink"
        }else{
          temp_ml_4 <- "喫茶・飲酒制限"
        }
    }
    
    if(flag_simu[s,5]==1){
      if (ENG==1){
        temp_ml_5 <- "Reduced travel"
      }else{
        temp_ml_5 <- "旅行制限有"
      }
    }else{
      if (ENG==1){
        temp_ml_5 <- NULL
      }else{
        temp_ml_5 <- NULL
      }
    }
    
    # Main label for reproduction number.
    temp_main_label <- paste0(c(temp_ml_1,temp_ml_4),collapse=",") 
    for (i in c(2,3,5)){
      if (eval(parse(text=paste0("is.null(temp_ml_",i,")==0")))){
        eval(parse(text=paste0("temp_main_label <- c(temp_main_label,temp_ml_",i,")")))
      }
    }
    if (ENG==1 & plot_paper == 1){
      temp_main_label <- "Effective reproduction number" # The main title for plots in the paper.
    }else{
      temp_main_label <- paste0(temp_main_label,collapse=", ")
    }

    # Main label for mobility.
    # temp_main_label_mob <- temp_ml_1 
    # if (is.null(temp_ml_2)==0){
    #   temp_main_label_mob <- c(temp_main_label_mob, temp_ml_2)
    # }
    # temp_main_label_mob <- paste0(c(temp_main_label_mob,paste0(" (",temp_ml_4,")")),collapse=",")
    if (ENG==1 & plot_paper == 1){
      temp_main_label_mob <- "Mobility in public transportation" # Only show the data label for plots in the paper.
    }else{
      temp_main_label_mob <- temp_main_label # Show simulation details for slides in Japanese.
    }
    
    # Main label for household expenditures.
    temp_main_label_hes <- temp_ml_4 
    for (i in c(3,5)){
      if (eval(parse(text=paste0("is.null(temp_ml_",i,")==0")))){
        eval(parse(text=paste0("temp_main_label_hes <- c(temp_main_label_hes,temp_ml_",i,")")))
      }
    }
    temp_main_label_hes <- paste0(temp_main_label_hes,collapse=", ")
    
    # Create the common part of legends for plots of observed R and mobility data and simulation results.
    temp_pick_plot <- sim_revn_frac_plot[[s]] # The location of the value of restriction to plot.
    temp_legend_rest_rate <- NULL
    for (i in 1:length(temp_pick_plot)){
      if (ENG==1){
        #temp_legend_rest_rate <- c(temp_legend_rest_rate, ", \"", temp_pick_plot[i]*100, "% reduction of cafe and bar consumption\"")
        temp_legend_rest_rate <- c(temp_legend_rest_rate, ", \"Hypothetical data with ", temp_pick_plot[i]*100, "% reduction of cafe and bar consumption\"")
      }else{
        temp_legend_rest_rate <- c(temp_legend_rest_rate, ", \"タイトル()内項目を対2019年比で", temp_pick_plot[i]*100,"%削減した場合\"")
      }
    }  

    
    # Create a data table to plot the simulated values of the reproduction number.
    temp_nl2 <- c("Fitted_R_2020") # The estimated time series of the mean of R from 2020 March 6.. 
    for (i in 1:(length(temp_nl2))){
      eval(parse(text=paste0("temp_nl_with_CI2 <- c(\"",temp_nl2[i],"_pm, ", temp_nl2[i],"_025, ",temp_nl2[i],"_975\")"))) # Define character names for the posterior mean and 2.5% and 97.5% percentiles.
    }
    eval(parse(text=paste0("temp <- cbind(",temp_nl_with_CI2,")"))) # Create a data table for a plot of observed data and estimated parameters.
    for (i in 1:length(temp_pick_plot)){
      temp <- cbind(temp, temp_intv_rslt[,c((which(sim_revn_frac==temp_pick_plot[i])-1)*3+1:3)])
    }
    # Create a y-axis label for the plot of R.
    if (log_R == 1){
      temp_ylab <- "Log" # R is the log of the reproduction number.
    }else{
      temp_ylab <- "Level" # R is the level of the reproduction number.
    } 
    # Plot the simulated values of the reproduction number.
    matplot(cbind(DEP, temp), type="l", col=c(1,rep(2,3),rep(3,3),rep(4,3),rep(5,3)), lty=c(1,1,2,2,1,2,2,1,2,2,1,2,2), xaxt="n", xlab=time_label, ylab=temp_ylab, main=paste0(R_label,temp_main_label))
    # Add x axis.
    axis(side=1, at=c(1,round(nrow(temp)*c(1:5)/5)), labels=conv_date_format(date_label_simu[c(1,round(nrow(temp)*c(1:5)/5))]))
    # Draw a horizon line for R=1.
    if (log_R == 1){   
      abline(h=0,lty=3) # h=0 because R is the log of the reproduction number. 
    }else{   
      abline(h=1,lty=3) # h=1 because R is the level of the reproduction number. 
    } 
    for (j in 1:length(SE_simu)){
      abline(v=SE_simu[j],lty=5)
    }  
    # Add legends.
    if (ENG==1 & plot_paper==1){
      legend("topright",legend=c("Observed R", "Fitted R with 2020-2021 data", "Simulated R with hypothetical data"), lty=1, col=c(1,2,3,4,5)) # Legends of the plot. 
    }else{
      if (ENG==1){
        temp_legend <- c("legend(\"topright\",legend=c(\"Observed R\", \"Fitted R with 2020-2021 data\"", temp_legend_rest_rate,"), lty=1, col=c(1,2,3,4,5))") # Legends of the plot. 
      }else{
        temp_legend <- c("legend(\"topright\",legend=c(\"観測値\", \"回帰式のfitted valueの事後平均\"", temp_legend_rest_rate,"), lty=1, col=c(1,2,3,4,5))") # Legends of the plot. 
      }
      eval(parse(text=paste0(temp_legend, collapse=""))) # Add legends to the plot.
    }
    
    # Create a data table to plot simulated series of mobility with mobility data in 2020.
    temp <- 1+mob_var4[1:dim(temp_mob_rslt)[1],1]/100 # Create a data table for a plot of observed data and estimated parameters.
    for (i in 1:length(temp_pick_plot)){
      temp <- cbind(temp, temp_mob_rslt[,c(which(sim_revn_frac==temp_pick_plot[i]))])
    }
    temp <- apply(temp,2,filter,rep(1/7,7)) # Take centered 7-day moving average.
    # Create a y-axis label for the plot of R.
    if (ENG==1){
      #temp_ylab <- "Average for Jan. 2020 (or 2020/1/3-2020/2/6) = 1"
      temp_ylab <- "Average for 2020/1/3-2020/2/6 = 1"
    }else{
      #temp_ylab <- "2020年1月（もしくは2020/1/3-2020/2/6)平均＝1"
      temp_ylab <- "2020/1/3-2020/2/6平均＝1、7日間移動平均"
    }
    # Plot the simulated series of mobility with mobility data in 2020.
    if (flag_simu[s,1]==1){
      # Endogenous mobility, which is affected by restrictions on eating out.
      matplot(temp, type="l", col=c(1,3,4,5), lty=c(1,1,1,1), xaxt="n", xlab=time_label, ylab=temp_ylab, main=temp_main_label_mob)
      if (ENG==1){
        temp_legend <- c("legend(\"topright\",legend=c(\"transit_stations from Google for 2020-2021\"", temp_legend_rest_rate, "), lty=1, col=c(1,3,4,5))")
      }else{
        temp_legend <- c("legend(\"topright\",legend=c(\"2020年以降のグーグルデータ\"", temp_legend_rest_rate, "), lty=1, col=c(1,3,4,5))")
      }
    }else{
      # Exdogenous mobility, which is not affected by restrictions on eating out.
      matplot(temp, type="l", col=c(1,2), lty=1, xaxt="n", xlab=time_label, ylab=temp_ylab, main=temp_main_label_mob)
      if (ENG==1){
        temp_legend <- "legend(\"topright\",legend=c(\"transit_stations from Google for 2020-2021\", \"Hypothetical value\"), lty=1, col=c(1,2))"
      }else{
        temp_legend <- "legend(\"topright\",legend=c(\"2020年以降のグーグルデータ\", \"仮想値\"), lty=1, col=c(1,2))"
      }
    }
    eval(parse(text=paste0(temp_legend, collapse=""))) # Add legends to the plot.
    # Add x axis.
    axis(side=1, at=c(1,round(nrow(temp)*c(1:5)/5)), labels=conv_date_format(date_label_simu_mob[c(1,round(nrow(temp)*c(1:5)/5))]))
    for (j in 1:length(SE_simu)){
      abline(v=SE_simu_mob[j],lty=5)
    }  
    

    # Plot the simulated series of household expenditures with the values in 2020.
    if (k ==1){ # The hypothetical values of household expenditures do not depend on restriction on mobility. So only plot data for the first value of mobility restriction.
      for (i in 1:dim(H_expvals_base)[1]){
        temp <- NULL # Initialize the matrix to contain the same type of simulated household expenditure for different restriction level. 
        for (j in 1:length(temp_pick_plot)){
          #temp <- cbind(temp, temp_hes_intv[[which(sim_revn_frac==temp_pick_plot[j])]][i,1:dim(H_expvals_base)[2]])
          temp <- cbind(temp, temp_hes_intv[[which(sim_revn_frac==temp_pick_plot[j])]][i,])
        }
        #temp <- cbind(H_expvals_base[i,], H_expvals_simu_2019[i,1:dim(H_expvals_base)[2]], temp) # Combine 2020 data and 2019 data. The latter starts from February 16, 2019, and the year-end is connected with the beginning of the year.
        temp <- cbind(c(H_expvals_base[i,],rep(NA,dim(temp)[1]-dim(H_expvals_base)[2])), temp) # Add 2020-21 data to the data table. Because the length of 2020-21 data for estimation is shorter than that of hypothetical 2019 data, fulfill NA to extend to a year. The latter starts from February 16, 2019, and the year-end is connected with the beginning of the year.
        temp <- apply(temp,2,filter,rep(1/7,7)) # Take centered 7-day moving average.
        if (ENG==1){
          #temp_xlab <- "Number of days from Feb. 16, 2020 or Feb. 15, 2019"
          temp_ylab <- "100 yen (per household per day, in 2020 real value）"
          if (plot_paper==1){
            temp_main_label <- hes_var_nm_2019_ENG[i] # Only show the data label for plots in the paper.
          }else{
            temp_main_label <- paste0(hes_var_nm_2019_ENG[i]," （",temp_main_label_hes, "）")
          }
        }else{
          #temp_xlab <- "2020年2月16日（もしくは2019年2月15日）からの日数"
          temp_ylab <- "百円（一世帯・一日当たり、2020年水準実質値、7日間移動平均）"
          temp_main_label <- paste0(hes_var_nm_2019[i]," （",temp_main_label_hes, "）")
        }
        matplot(temp, type="l",lty=1, col=c(1,3,4,5),  xaxt="n", xlab=time_label, ylab=temp_ylab, main=temp_main_label) # Plot H_expvals for 365 days from 2020 February 16 or 2019 February 15.
        # Add legends.
        if (ENG==1){
          #temp_legend <- c("legend(\"topright\",legend=c(\"2020-2021 data\",\"Hypothetical case of no restriction based on 2019 data\"", temp_legend_rest_rate,"), lty=1, col=c(1,2,3,4,5))") # Legends of the plot. 
          temp_legend <- c("legend(\"topright\",legend=c(\"2020-2021 data\"", temp_legend_rest_rate,"), lty=1, col=c(1,3,4,5))") # Legends of the plot. 
        }else{
          #temp_legend <- c("legend(\"topright\",legend=c(\"2020年～2021年データ\",\"2019年データに基づく仮想データ\"", temp_legend_rest_rate,"), lty=1, col=c(1,2,3,4,5))") # Legends of the plot. 
          temp_legend <- c("legend(\"topright\",legend=c(\"2020年～2021年データ\"", temp_legend_rest_rate,"), lty=1, col=c(1,3,4,5))") # Legends of the plot. 
        }
        eval(parse(text=paste0(temp_legend, collapse=""))) # Add legends to the plot.
        # Add x axis.
        axis(side=1, at=c(1,round(nrow(temp)*c(1:5)/5)), labels=conv_date_format(date_label_simu_mob[c(1,round(nrow(temp)*c(1:5)/5))]))
        for (j in 1:length(SE_simu)){
          abline(v=SE_simu_mob[j],lty=5)
        }  
      }
    }
    
    if (flag_simu[s,2]==0){
      # For a simulation without no restriction on mobility, end the iteration.
      break 
    }
    
  }
  
  # Record the posterior mean and percentiles of the annual mean simulated R over the simulation period.
  temp <- rep(NA,length(sim_revn_frac)*3) # Initialize the vector of column names.
  for (i in 1:(length(sim_revn_frac)*3)){
    if (i%%3==1){ 
      temp[i] <- paste(- sim_revn_frac[i%/%3+1]*100) # Name the columns of the matrix, which are restrictions on household expenditure for cafe and drink.
    }else{
      temp[i] <- "&" # columns for percentiles.
    }
  }
  if (flag_simu[s,2]==0){
    mean_R_pm <- t(matrix(mean_R_pm,nr=3)) # Convert it to a column vector so that 
    rownames(mean_R_pm) <- temp[seq(1,length(sim_revn_frac)*3,by=3)] # Name rows. For each level of revenue restriction, (mean, 2.5% percentile, 97.5% percentile) is one set.
    colnames(mean_R_pm) <- c("Mean", "2.5%", "97.5%")
  }else{
    colnames(mean_R_pm) <- temp # Name columns. For each level of revenue restriction, (mean, 2.5% percentile, 97.5% percentile) is one set.
    rownames(mean_R_pm) <- sim_mob_perc # Name the rows of the matrix, which are restrictions on mobility.
  }
  eval(parse(text=paste0("sink(paste0(\"mean_R_pm",s,"_model",mdl_number,"_",R_sfx,".txt\"))")))
  print("The posterior mean, 2.5%, and 97.5% percentiles of the mean of the reproduction number over the simulated period.")
  if (flag_simu[s,2]==0){
    print("Rows: restrictions on eating out.")
  }else{
    print("Columns: restrictions on eating out.")
    print("Rows: upper bound on mobility. (Inf means no restriction.)")
  }
  print(mean_R_pm)
  sink()
  
  # Close the pdf file for each simulation.
  dev.off()
  
}



