# Load and reform data for a linear regression of the reproduction number.
# This code must be called from a main code.

########## Set the parameter #####################

hes_end <- c(2021,1) # The end of the sample period available (year, month).

ah_ub <- 9 # The threshold for dummies for absolute humidity.

#hes_end_date_estimation <- "2021-01-31" # Set the last date of household expenditure data for estimation.

########## Load data ###########################

#### Google Mobility report data; Daily; 2020 Feb. 15- 2021 March 13; Unit: %. 

mob_data <- read.csv("./data/2020_JP_Region_Mobility_Report.csv", header=T, stringsAsFactors=FALSE) 
mob_ndays <- length(mob_data[[1]])/48 # Number of days in time series.
mob_loc_nm <- rep(NA,48) # Initialize the vectors of names of locations.
mob_var_nm <- c("retail_and_recreation", "grocery_and_pharmacy", "parks", "transit_stations", "workplaces", "residential") #names(mob_data)[10:15] # Names of variables in the original data set.
# Extract each type of mobility data.
for (j in 1:6){
  # Rows are days; columns are locations. 
  eval(parse(text=paste0("mob_var",j,"<-matrix(mob_data[[9+",j,"]],nr=mob_ndays)")))
}
mob_date <- mob_data[[9]][1:mob_ndays] # Date of each row of mob_varj for j=1,2,3,..,6.
for (i in 1:48){
  # Record the names of locations.
  if (i == 1){
    mob_loc_nm[1] <- "Nationwide"  
  }else{
    mob_loc_nm[i] <- mob_data[[3]][(i-1)*mob_ndays+1] 
  }
}

#### Google Mobility report data; Daily; 2021 Jan. 1 - ; Unit: %. 

mob_data_2021 <- read.csv("./data/2021_JP_Region_Mobility_Report.csv", header=T, stringsAsFactors=FALSE) 
mob_ndays_2021 <- length(mob_data_2021[[1]])/48 # Number of days in time series.
mob_loc_nm_2021 <- rep(NA,48) # Initialize the vectors of names of locations.
#mob_var_nm_2021 <- names(mob_data_2021)[10:15] # Names of variables in the original data set.
if (sum(!(names(mob_data)[10:15] == names(mob_data_2021)[10:15]))>0){
 stop("The order of indicators in google mobility data may have changed.")
}
# Extract each type of mobility data.
for (j in 1:6){
  # Rows are days; columns are locations. 
  eval(parse(text=paste0("mob_var",j,"_2021<-matrix(mob_data_2021[[9+",j,"]],nr=mob_ndays_2021)")))
}
mob_date_2021 <- mob_data_2021[[9]][1:mob_ndays_2021] # Date of each row of mob_varj for j=1,2,3,..,6.
for (i in 1:48){
  # Record the names of locations.
  if (i == 1){
    mob_loc_nm_2021[1] <- "Nationwide"  
  }else{
    mob_loc_nm_2021[i] <- mob_data_2021[[3]][(i-1)*mob_ndays_2021+1] 
  }
}
if (sum(!(mob_loc_nm == mob_loc_nm_2021))>0){
  stop("The order of prefectures in google mobility data may have changed.")
}

# Connect 2021 data to 2020 data.
for (j in 1:6){
  eval(parse(text=paste0("if(!(mob_var",j,"[which(mob_date==\"2021-03-13\"),1]==mob_var",j,"_2021[which(mob_date_2021==\"2021-03-13\"),1])){stop(\"New google mobility report data are not consistent with the existing one.\")}"))) # Extend rows with new data.
  eval(parse(text=paste0("mob_var",j,"<-rbind(mob_var",j,", mob_var",j,"_2021[(which(mob_date_2021==\"2021-03-13\")+1):mob_ndays_2021,])"))) # Extend rows with new data.
}

# Extend the date label.
mob_date <- c(mob_date,mob_date_2021[(which(mob_date_2021=="2021-03-13")+1):mob_ndays_2021])

# Revise the number of periods.
mob_ndays <- length(mob_date)

### Household expenditure survey; Daily; 2020 Jan. 01; Unit: current yen. 

hes_var <- NULL # Initialize a matrix for household expenditure item variables.
hes_var2 <- NULL # Initialize a matrix for detailed household expenditure item variables.
hes_var_nm <- NULL # Initialize a matrix for the names of household expenditure item variables.
hes_var_nm2 <- NULL # Initialize a matrix for the names of detailed household expenditure item variables.
hes_var_date <- NULL # Initialize a matrix for the dates of household expenditure item variables.

for (i in 2020:hes_end[1]){
  if (i == hes_end[1]) {
    # The last year in the sample.
    temp_mt <- c(1,hes_end[2]) # The first and last month of the year available in the sample.
  }else{
    temp_mt <- c(1,12) # The first and last month of the year available in the sample.
  }
  for (j in temp_mt[1]:temp_mt[2]){
    # Extract household expenditure data for month j, year i.
    # For an unknown reason, an empty column is read at the end. The last data column is the fourth to the last column.
    eval(parse(text=paste0("temp_dt <- read.csv(\"./data/household survey/a615_",i,"_",j,".csv\", header=F,stringsAsFactors=FALSE)"))) 
    eval(parse(text=paste0("temp_dt2 <- read.csv(\"./data/household survey/a616_",i,"_",j,".csv\", header=F,stringsAsFactors=FALSE)"))) 
    # Convert the list containing data into a numeric matrix. The format of the table changes from 2020 Nov. 
    if (i==2020 && j<11){
      temp_numeric <- conv_list(temp_dt[14:135,16:(ncol(temp_dt)-2-is.na(temp_dt[1,ncol(temp_dt)]))]) # Sometimes the last columns are NAs. In that case, exclude that column.
      temp_numeric2 <- conv_list(temp_dt2[14:679,11:(ncol(temp_dt2)-2-is.na(temp_dt2[1,ncol(temp_dt2)]))]) # Sometimes the last columns are NAs. In that case, exclude that column.
    }else{
      temp_numeric <- conv_list(temp_dt[10:131,14:(ncol(temp_dt)-2-is.na(temp_dt[1,ncol(temp_dt)]))]) # Sometimes the last columns are NAs. In that case, exclude that column.
      temp_numeric2 <- conv_list(temp_dt2[10:675,14:(ncol(temp_dt2)-2-is.na(temp_dt2[1,ncol(temp_dt2)]))]) # Sometimes the last columns are NAs. In that case, exclude that column.
    }
    # Extend the time series of household expenditure items. Remove commas from the matrix
    hes_var <- cbind(hes_var,temp_numeric)
    hes_var2 <- cbind(hes_var2,temp_numeric2)
    # Extend the names of household expenditure items for each period.
    temp_nm <- rep(NA,135-14+1) # Initialize the vector for variable names for month j, year i.
    for (k in 14:135){ 
      # The format of the table changes from 2020 Nov.
      if (i==2020 && j<11){
        temp_nm[k-13] <- paste(temp_dt[k,11:14], collapse="")
      }else{
        temp_nm[k-13] <- paste(temp_dt[k-4,12], collapse="")
      }
    }
    hes_var_nm <- cbind(hes_var_nm, temp_nm)
    
    # Record the vector for variable names for month j, year i.
    if (i==2020 && j<11){
      # Remove double blanks.
      temp_nm2 <- sub(" ","",temp_dt2[14:679,9])
      temp_nm2 <- sub(" ","",temp_nm2)
    }else{
      temp_nm2 <- temp_dt2[10:675,12]
    }
    hes_var_nm2 <- cbind(hes_var_nm2, temp_nm2)
    
    # Extract the dates of each month from the first row of temp_dt.
    if (i==2020 && j<11){
      temp_dates <- conv_list(temp_dt[1,16:(ncol(temp_dt)-2-is.na(temp_dt[1,ncol(temp_dt)]))]) # Sometimes the last columns are NAs. In that case, exclude that column.
    }else{
      temp_dates <- conv_list(temp_dt[1,14:(ncol(temp_dt)-2-is.na(temp_dt[1,ncol(temp_dt)]))]) # Sometimes the last columns are NAs. In that case, exclude that column.
    }
    # Extend the time series of dates. 
    hes_var_date <- c(hes_var_date, paste0(i,"-",j,"-",temp_dates))
  }
}

# Check if the data labels are the same for all months.
for (i in 2:ncol(hes_var_nm)){
  if (sum(!(hes_var_nm[,i]==hes_var_nm[,1]))>0){
    print((hes_var_nm[,i]==hes_var_nm[,1]))
    print(i)
    stop("Data labels for household expeditures are not the same for all month,")
  }
  if (sum(!(hes_var_nm2[,i]==hes_var_nm2[,1]))>0){
    print((hes_var_nm2[,i]==hes_var_nm2[,1]))
    print(i)
    stop("Data labels for detailed household expeditures are not the same for all month,")
  }
}

### Approximate nationwide reproduction number; Daily; 2020 march. 01-; Unit: ratio; 

R_data <- read.csv("./data/toyokeizai/effective_reproduction_number.csv", header=T, stringsAsFactors=FALSE) 
R_var <- R_data[[2]] # Nationwide Reproduction number.
R_date <- R_data[[1]] # Dates.

if (log_R == 1){
  R <- log(R_var) # Use log of the reproduction number for the dependent variable.
}else{
  R <- R_var # Use the level of the reproduction number for the dependent variable.
}

#### Temperature and humidity data; Daily; 2020 Jan. 1-; Unit: %. 

temper_var <- NULL # Initialize a matrix for temperatures across the capitals of prefectures.
hum_var <- NULL # Initialize a matrix for humidity across the capitals of prefectures.
weath_date <- NULL # Initialize a matrix for the dates of weather data.

# Set the orders of the capitals of prefectures.
weath_pref_nm <- c("札幌", "青森", "盛岡", "秋田", "仙台", "山形", "福島", "水戸", "宇都宮", "さいたま", "千葉", "東京", "新潟", "前橋", "長野", "甲府", "横浜", "静岡", "富山", "岐阜", "名古屋", "金沢", "福井", "大津", "津", "奈良", "和歌山", "大阪", "京都", "神戸", "鳥取", "岡山", "松江", "広島", "山口", "高松", "松山", "徳島", "高知", "福岡", "大分", "宮崎", "佐賀", "熊本", "鹿児島", "長崎", "那覇") 

for (i in 2020:hes_end[1]){
  if (i == hes_end[1]) {
    # The last year in the sample of household expenditure data.
    temp_mt <- c(1,hes_end[2]) # The first and last month of the year available in the sample.
  }else{
    temp_mt <- c(1,12) # The first and last month of the year available in the sample.
  }
  for (j in temp_mt[1]:temp_mt[2]){
    # Extract temperature and humidity data for month j, year i.
    # For an unknown reason, an empty column is read at the end. The last data column is the fourth to the last column.
    eval(parse(text=paste0("temp_dt <- read.csv(\"./data/weather/weather_",i,"_",j,".csv\", header=F,stringsAsFactors=FALSE)"))) 
    
    # Convert the list containing data into a numeric matrix for temperature.
    # Blank rows in the csv file are already excluded by read.csv().
    temp <- conv_list(temp_dt[5:(nrow(temp_dt)-(temp_dt[nrow(temp_dt),1]=="")),seq(2, 1+47*6-3, by=6)]) # Exclude the last row, if it is empty.
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
    temper_var <- rbind(temper_var,temp_temper)
    hum_var <- rbind(hum_var,temp_hum)
    
    # Extract the dates of each month from the first row of temp_dt.
    temp_dates <- temp_dt[5:(nrow(temp_dt)-(temp_dt[nrow(temp_dt),1]=="")),1] # Sometimes the last columns are NAs. In that case, exclude that column.
    # Extend the time series of dates. 
    weath_date <- c(weath_date, temp_dates)
  }   
}




#### Approximate reproduction number for each prefecture; Daily; 2020 February. 08- (Tokyo); Unit: ratio;

# Load data.
TstPstv_data <- read.csv("./data/toyokeizai/prefectures.csv", header=T, stringsAsFactors=FALSE)

# Separate data into each prefecture.
TstPstv_pref_nm <- c("北海道", "青森県", "岩手県", "秋田県", "宮城県", "山形県", "福島県", "茨城県", "栃木県", "埼玉県", "千葉県", "東京都", "新潟県", "群馬県", "長野県", "山梨県", "神奈川県", "静岡県", "富山県", "岐阜県", "愛知県", "石川県", "福井県", "滋賀県", "三重県", "奈良県", "和歌山県", "大阪府", "京都府", "兵庫県", "鳥取県", "岡山県", "島根県", "広島県", "山口県", "香川県", "愛媛県", "徳島県", "高知県", "福岡県", "大分県", "宮崎県", "佐賀県", "熊本県", "鹿児島県", "長崎県", "沖縄県")

# Initialize a list to contain prefecture data.
TstPstv_data_pref <-list() 

# Separate each prefecture's date into a different data table.
for (i in 1:length(TstPstv_pref_nm)){
  TstPstv_data_pref[[i]] <- TstPstv_data[TstPstv_data$prefectureNameJ==TstPstv_pref_nm[i],]
}

# Find the prefecture with the longest sample period.
temp <- rep(NA,47) # A vector to contain the number of columns of the table for each prefecture.
for (i in 1:length(TstPstv_pref_nm)){
  temp[i] <- dim(TstPstv_data_pref[[i]])[1] # Record the number of columns of the table for each prefecture, which equals the length of the sample period.
}
TstPstv_loc_max_ndates <- which.max(temp) # Identify the longest sample period among prefectures.
TstPstv_max_ndates <- max(temp) # Identify the longest sample period among prefectures.

# Create a balanced panel of the number of tested positive at each date in each prefecture.
TstPstv_pref_blncd <- matrix(NA, nr=TstPstv_max_ndates, nc=3+47) # Create a matrix that contain the balanced panel of the number of tested positive at each date in each prefecture. The first three columns are year, month and date.
for (i in 1:length(TstPstv_pref_nm)){
  if (dim(TstPstv_data_pref[[i]])[1] < TstPstv_max_ndates){
    TstPstv_pref_blncd[1:(TstPstv_max_ndates-dim(TstPstv_data_pref[[i]])[1]), 3+i] <- 0 # Put 0 to the dates before the first date of the sample period for the prefecture.
  }  
  TstPstv_pref_blncd[TstPstv_max_ndates-dim(TstPstv_data_pref[[i]])[1]+1:dim(TstPstv_data_pref[[i]])[1], 3+i] <- as.matrix(TstPstv_data_pref[[i]][,which(colnames(TstPstv_data_pref[[i]])=="testedPositive")]) # Put data for the prefecture into the balanced panel.
}

# Substitute NA in TstPstv_pref_blncd with zero.
TstPstv_pref_blncd[is.na(TstPstv_pref_blncd)] <- 0

# Set the prefecture names as the column names.
colnames(TstPstv_pref_blncd)[3+1:47] <- TstPstv_pref_nm

# Set the date labels as the column names.
colnames(TstPstv_pref_blncd)[1:3] <- colnames(TstPstv_data_pref[[1]])[1:3]

# Compute the sum of new cases in the past 7 days.
TstPstv_pref_pst7d <- apply(TstPstv_pref_blncd,2,diff,lag=7)

#Set year, month and date.
TstPstv_pref_pst7d[, 1:3] <- as.matrix(TstPstv_data_pref[[TstPstv_loc_max_ndates]][8:TstPstv_max_ndates,1:3])

# Compute the share of new cases in the past 7 days across prefectures. THe first date: February 15, 2020.
TstPstv_pref_share <- t(apply(TstPstv_pref_pst7d[,3+1:47],1,function(x){x/sum(x)}))

# Set the dates of TstPstv_pref_share data.
TstPstv_pref_share_date <- apply(TstPstv_pref_pst7d[, 1:3],1,paste0,collapse="/")


#### Population data; Annual. October 1 in 2019 (the most recently available). Unit: a thousand persons.

# Set the orders of prefectures, which must be in accordance with weath_pref_nm.
popu_pref_nm <- c("北海道", "青森県", "岩手県", "秋田県", "宮城県", "山形県", "福島県", "茨城県", "栃木県", "埼玉県", "千葉県", "東京都", "新潟県", "群馬県", "長野県", "山梨県", "神奈川県", "静岡県", "富山県", "岐阜県", "愛知県", "石川県", "福井県", "滋賀県", "三重県", "奈良県", "和歌山県", "大阪府", "京都府", "兵庫県", "鳥取県", "岡山県", "島根県", "広島県", "山口県", "香川県", "愛媛県", "徳島県", "高知県", "福岡県", "大分県", "宮崎県", "佐賀県", "熊本県", "鹿児島県", "長崎県", "沖縄県")


# Extract population share across prefectures.
temp_dt <- read.csv("./data/population_share_2019.csv", header=F,stringsAsFactors=FALSE)
# Extract prefectures' populations for all genders, all populations, all residents.
temp <- as.numeric(temp_dt[(temp_dt[,4]=="男女計" & temp_dt[,6]=="総数" & temp_dt[,8]=="総人口"), 14])
# Extract prefectures' names for all genders, all populations, all residents.
temp_nm <- temp_dt[(temp_dt[,4]=="男女計" & temp_dt[,6]=="総数" & temp_dt[,8]=="総人口"), 10]
# Divide each prefecture's population by the nationwide population to compute population shares across prefectures.
temp <- temp[-(temp_nm=="全国")]/temp[temp_nm=="全国"]
# Sort the order of prefectures.
popu_share <- rep(NA,47) # Initialize the vector to contain population shares across prefectures.s
for (i in 1:47){
  popu_share[i] <- temp[temp_nm[-(temp_nm=="全国")]==popu_pref_nm[i]]
}

### Load monthly CPI data to convert 2019 prices into 2020 prices. Monthly; Jan. 1970-; Unit: 2015 average = 100.

# Define the data to extract.
CPI_nm <- c("一般外食", "宿泊料", "被服及び履物")

# Initialize a matrix for CPI for household expenditure item variables. # Rows: household expenditure items. Columns: month.
CPI_m <- NULL 

# Load data.
temp_dt <- read.csv("./data/CPI/zmi2015a.csv", header=F,stringsAsFactors=FALSE)

# Extract data from Jan 2019 onward.
for (i in 1:length(CPI_nm)){
  temp <- temp_dt[which(temp_dt[,1]==201901):nrow(temp_dt),temp_dt[1,]==CPI_nm[i]]
  if (length(dim(temp))>1){
    temp <- temp[,1] # If there are two columns with the same title, use the first one.
  }
  CPI_m <- cbind(CPI_m,temp) 
}

# Turn characters into numeric. Also transpose it to have months for columns.
CPI_m <- t(matrix(as.numeric(CPI_m),nc=3))


########## Reform the data ###########################

# Compute 7-days backward moving averages of detailed household expenditure items.  
hes_var2_ma <- t(apply(hes_var2, 1, function(x){filter(x,rep(1/7,7))})) # Centered moving averages. apply(,1,) transposes the original matrices.
hes_var2_ma <- hes_var2_ma[,4:(ncol(hes_var2)-3)] # Shift rows to create backward moving averages.
hes_var2_ma_date <- hes_var_date[7:ncol(hes_var2)] # Dates of data in hes_var2_ma. 

# Compute 7-days backward moving averages of mobility reports data.  
for (i in 1:6){
  eval(parse(text=paste0("mob_var",i,"_ma <- apply(mob_var",i,", 2, function(x){filter(x,rep(1/7,7))})"))) # Centered moving averages.
  eval(parse(text=paste0("mob_var",i,"_ma <- mob_var",i,"_ma[4:(mob_ndays-3),]"))) # Shift rows to create backward moving averages.
}
mob_var_ma_date <- mob_date[7:mob_ndays]
mob_ma_ndays <- length(mob_var_ma_date) # The number of rows of mob_var_i_ma for i =1,2,3,...,6.

# Substitute missing data in hum_var. Interpolate them with data for neiboring dates.
# 2020/5/6: Kobe.
# 2020/12/10: Kumamoto.
hum_var[weath_date=="2020/5/6", weath_pref_nm=="神戸"] <- (hum_var[weath_date=="2020/5/5", weath_pref_nm=="神戸"] + hum_var[weath_date=="2020/5/7", weath_pref_nm=="神戸"]) / 2
hum_var[weath_date=="2020/12/10", weath_pref_nm=="熊本"] <- (hum_var[weath_date=="2020/12/9", weath_pref_nm=="熊本"] + hum_var[weath_date=="2020/12/11", weath_pref_nm=="熊本"]) / 2

# Convert relative humidity into absolute humidity.
abs_hum_var <- calc_abs_hum(temper_var,hum_var)

# Compute the nationwide weighted averages of temperature and humidity.

if (flag_pref_wgt == 0){
  # Use the number of new cases in the past 7 days for the weights for prefectures. 
  temp <- rbind(matrix(1,nr=31+14,nc=1)%*%TstPstv_pref_share[1,],TstPstv_pref_share) # Fill the first row of the data for the weights between 2020 Jan. 1 and 2020 Feb. 14, one day before the first date of the sample period for the data. 
  # Extract the weights for the duration of weather data.
  temp <- temp[1:(31+14+which(TstPstv_pref_share_date==weath_date[length(weath_date)])),]
  temper_var_ave <- rowSums(temper_var * temp) 
  hum_var_ave <- rowSums(hum_var * temp) 
  abs_hum_var_ave <- rowSums(abs_hum_var * temp) 
  ind_abs_hum_var_ave <- rowSums((abs_hum_var<ah_ub) * temp) # = 1 if absolute humidity < ah_ub g/m^3, which is based on Nottmeyer et al (2020). Then take the nationwide average of the indicator. 
}else{
  # Use population shares for the weights for prefectures. 
  temper_var_ave <- c(temper_var %*% popu_share) 
  hum_var_ave <- c(hum_var %*% popu_share) 
  abs_hum_var_ave <- c(abs_hum_var %*% popu_share) 
  ind_abs_hum_var_ave <- c((abs_hum_var<ah_ub) %*% popu_share) # = 1 if absolute humidity < ah_ub g/m^3, which is based on Nottmeyer et al (2020). Then take the nationwide average of the indicator. 
}



# Compute 7-days backward moving averages of temperature and humidity.
temper_var_ma <- apply(temper_var, 2, function(x){filter(x,rep(1/7,7))}) # Centered moving averages. apply(,1,) transposes the original matrices.
temper_var_ma <- temper_var_ma[4:(nrow(temper_var_ma)-3),] # Shift rows to create backward moving averages.
hum_var_ma <- apply(hum_var, 2, function(x){filter(x,rep(1/7,7))}) # Centered moving averages. apply(,1,) transposes the original matrices.
hum_var_ma <- hum_var_ma[4:(nrow(hum_var_ma)-3),] # Shift rows to create backward moving averages.
abs_hum_var_ma <- apply(abs_hum_var, 2, function(x){filter(x,rep(1/7,7))}) # Centered moving averages. apply(,1,) transposes the original matrices.
abs_hum_var_ma <- abs_hum_var_ma[4:(nrow(abs_hum_var_ma)-3),] # Shift rows to create backward moving averages.
weath_ma_date <- weath_date[7:length(weath_date)] # Dates of data in moving average data. 

# Compute the nationwide weighted averages of 7-day moving averages of temperature and humidity.
if (flag_pref_wgt == 0){
  # Use the number of new cases in the past 7 days for the weights for prefectures. 
  temp <- rbind(matrix(1,nr=31+14,nc=1)%*%TstPstv_pref_share[1,],TstPstv_pref_share) # Fill the first row of the data for the weights between 2020 Jan. 1 and 2020 Feb. 14, one day before the first date of the sample period for the data. 
  # Extract the weights for the duration of weather data. The moving average of weather data starts from 2020 January 7.
  temp <- temp[7:(31+14+which(TstPstv_pref_share_date==weath_ma_date[length(weath_ma_date)])),]
  temper_var_ma_ave <- rowSums(temper_var_ma * temp) 
  hum_var_ma_ave <- rowSums(hum_var_ma * temp) 
  abs_hum_var_ma_ave <- rowSums(abs_hum_var_ma * temp) 
  ind_abs_hum_var_ma_ave <- rowSums((abs_hum_var_ma<ah_ub) * temp) # = 1 if absolute humidity < ah_ub g/m^3, which is based on Nottmeyer et al (2020). Then take the nationwide average of the indicator. 
}else{
  # Use population shares for the weights for prefectures. 
  temper_var_ma_ave <- c(temper_var_ma %*% popu_share) 
  hum_var_ma_ave <- c(hum_var_ma %*% popu_share) 
  abs_hum_var_ma_ave <- c(abs_hum_var_ma %*% popu_share) 
  ind_abs_hum_var_ma_ave <- c((abs_hum_var_ma<ah_ub) %*% popu_share) # = 1 if absolute humidity < ah_ub g/m^3, which is based on Nottmeyer et al (2020). Then take the nationwide average of the indicator. 
}

# Compute the 2020 average price for each item. 13th column is Jan 2020.
temp <- apply(CPI_m[,13:24],1,mean) # Compute the 2020 average.
CPI_m_2020 <- apply(CPI_m,2,function(x){x/temp}) # Set the benchmark of CPI to 2020.


# Interpolate the CPI for hotel services during the GO-TO-TRAVEL period in order to convert "宿泊料" in household expenditure into real term. 
# "一般外食” in CPI is used for "食事代", "喫茶代", "飲酒代"; interporated "宿泊料" in CPI for "宿泊料" in household expenditure; "宿泊料" in CPI for "国内パック旅行" in household expenditure.; "被服及び履物" in CPI for "被服及び履物" in household expenditure.
# GO-TO-TRAVEL period: 2020 July 22 to 2020 Dec 27. 
temp <- CPI_m_2020[2,12+7] + (CPI_m_2020[2,25]-CPI_m_2020[2,12+7])/6 * 1:5 # Linear interporation of CPI for "宿泊料".
CPI_m_2020_rvs <- rbind(CPI_m_2020[1,],CPI_m_2020[2,],CPI_m_2020[2,],CPI_m_2020[3,]) # Insert one more CPI series for "宿泊料".
CPI_m_2020_rvs[2,(12+8):24] <- temp # Insert the interporated CPI series for "宿泊料" in household expenditures
  
# Convert monthly CPI into daily CPI from Jan 2019.
ndays_olympic <- c(31,29,31,30,31,30,31,31,30,31,30,31) # Number of days in an olympic year.
ndays_normal <- c(31,28,31,30,31,30,31,31,30,31,30,31) # Number of days in an non-olympic year.

temp_date <- c(ndays_normal,ndays_olympic,rep(ndays_normal,(hes_end[1]-2021)),ndays_normal[1:hes_end[2]]) # Create a series of date from Jan. 2020 to the end of the last month in the estimation period.
CPI_d_2020 <- matrix(1,nc=temp_date[1]) %x% CPI_m_2020_rvs[,1] # Initialize a matrix to contain daily CPI.
for (i in 2: length(temp_date)){
  CPI_d_2020 <- cbind(CPI_d_2020, matrix(1,nc=temp_date[i]) %x% CPI_m_2020_rvs[,i]) # Fill all the dates in a month by the month's CPI for each item.
}

########## Set the explanatory variables in a linear regression for the reproduction number ###########################

# Create a matrix of household expenditure items for estimation. 

if (nominal_hes == 1){
  # These are nominal term.
  H_expvals <- rbind(hes_var2[hes_var_nm2[,1]=="食事代",],
                     hes_var2[hes_var_nm2[,1]=="喫茶代",],
                     hes_var2[hes_var_nm2[,1]=="飲酒代",],
                     hes_var2[hes_var_nm2[,1]=="宿泊料",],
                     hes_var2[hes_var_nm2[,1]=="国内パック旅行費",],
                     t(apply(hes_var2[hes_var_nm2[,1]=="被服及び履物",],2,mean))) #This label appears multiple times.
}else{
  
  # Compute real household expenditure values in 2020 CPI average for each item.
  # CPI_d_2020 contains "一般外食”, interpolated "宿泊料" to remove the effect of GO-TO-TRAVEL, "宿泊料", "被服及び履物" in CPI.
  # The sample period of CPI_d_2020 starts from Jan. 1, 2019.

    H_expvals <- rbind(hes_var2[hes_var_nm2[,1]=="食事代",] / CPI_d_2020[1, 366:dim(CPI_d_2020)[2]], #From 2020 Jan 1 to three days before the end of the last month in the estimation period.
                     hes_var2[hes_var_nm2[,1]=="喫茶代",] / CPI_d_2020[1, 366:dim(CPI_d_2020)[2]],
                     hes_var2[hes_var_nm2[,1]=="飲酒代",] / CPI_d_2020[1, 366:dim(CPI_d_2020)[2]],
                     hes_var2[hes_var_nm2[,1]=="宿泊料",] / CPI_d_2020[2, 366:dim(CPI_d_2020)[2]],
                     hes_var2[hes_var_nm2[,1]=="国内パック旅行費",] / CPI_d_2020[3, 366:dim(CPI_d_2020)[2]],
                     t(apply(hes_var2[hes_var_nm2[,1]=="被服及び履物",],2,mean)) / CPI_d_2020[4, 366:dim(CPI_d_2020)[2]]) #This label appears multiple times.
  
  H_expvals <- H_expvals / 100 # To increase the number of digit of coefficients to avoid the effect of possible rounding error.  
}

# Set the sample period of household expenditure data, given a possible structural change due to the spread of mutant strains after Feb. 2021.
#H_expvals <- H_expvals[,1:which(hes_var_date==hes_end_date_estimation)]


# Choose the absolute humidity data for the use of the estimation.
if (log_abs_hum == 1){
  W_abs_hum <- log(abs_hum_var_ave) # Take log of absolute humidity.
}else if(log_abs_hum == 2){
  W_abs_hum <- abs_hum_var_ave # Use the level of absolute humidity.
}else{
  W_abs_hum <- 1 - ind_abs_hum_var_ave # Use the dummy that absolute humidity exceeds 9 g/m^3.
}

# Create the distribution of lags between an infection and a symptom for the use of the estimation. 
if (log_dist_incub == 1){
  # Based on Stephen A et al. (2020). They apply a log-normal distribution to data, and estimate that the mean is 5.1 (95% CI: 4.5-5.8) and that the 97.5% percentile is 11.5 (95% CI: 8.2-15.6).
  # Given the mean being 5.1, the 97.5% percentile is about 11.5 if the standard deviation is 0.4.
  dist_incub <- rep(NA,14) # Initialize the vector to contain the distribution.
  for (i in 14:1){
    #The density of an incubation for i days.
    dist_incub[15-i] <- dlnorm(i, log(5.1), 0.4);
  }
}else{
  # The empirical distribution of incubation periods from 1 days to 14 days based on MHLW data, published by Sugishita (2020).
  dist_incub <- c(3,5,19,22,11,21,9,11,7,4,4,1,4,4) 
  dist_incub <- rev(dist_incub/sum(dist_incub)) # Compute sample probabilities and reverse the order.
}

# Construct a new year dummy between 12/29-1/3 for the use of the estimation. The first date of D_NY is 2020 Feb. 15, the same as mobility report data.
D_NY <- c(rep(0, length(R_date) + 29-14)) 
D_NY[which(R_date=="2020/12/28") + 29-14 + 1:6] <- 1 # New year period.

# Construct a dummy variable for the declaration of each state of emergency for the use of the estimation. The first date is 2020 Feb. 15, the same as mobility report data.
D_SE1 <- c(rep(0, length(R_date) + 29-14)) 
D_SE1[which(R_date=="2020/4/7"):which(R_date=="2020/5/25") + 29-14] <- 1 # First declaration: 2020/4/7-2020/5/25. 
D_SE2 <- c(rep(0, length(R_date) + 29-14)) 
if (length(R_date)>=365+21){
  D_SE2[which(R_date=="2021/1/7"):which(R_date=="2021/3/21") + 29-14] <- 1 # Second declaration: 2021/1/7-2021/3/21.
}else{
  D_SE2[(which(R_date=="2021/1/7") + 29-14):length(R_date)] <- 1 # Second declaration: 2021/1/7-2021/3/21. The latest sample date is before March 21.
}

if (mdl_number >= 13){
  # Construct a dummy variable for the period before the first state of emergency. The first date is 2020 Feb. 15, the same as mobility report data.
  D_pre_SE1 <- c(rep(0, length(R_date) + 29-14)) 
  D_pre_SE1[1:(which(R_date=="2020/4/6") + 29-14)] <- 1 # First declaration: The first date to 2020/4/7. 
}