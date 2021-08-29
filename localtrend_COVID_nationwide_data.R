# Load and reform data for a linear regression of the reproduction number.
# This code must be called from a main code.

########## Set the parameter #####################

hes_end_all <- c(2021,6) # The end of the sample period available (year, month).

if (mdl_number == 17){
  if(exists("plot_data")==0){ # If this source file is not called from localtrend_COVID_nationwide.r
      hes_end <- c(2021,1) # The end of the sample period for estimation (year, month).
  }else if(estim==0){ # If this source file is called from localtrend_COVID_nationwide.r and only data are plotted.
    hes_end <- hes_end_all # The end of the available sample period (year, month).
  }else{
    hes_end <- c(2021,1) # The end of the sample period for estimation (year, month).
  }
}else{
  hes_end <- hes_end_all # The end of the available sample period (year, month).
}

ah_ub <- 9 # The threshold for dummies for absolute humidity.

# Number of residents in Japan (so jinko) as of July 31, 2021
# https://cio.go.jp/c19vaccine_dashboard
JPN_popu <- 127128905

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

hes_var_all <- NULL # Initialize a matrix for household expenditure item variables.
hes_var2_all <- NULL # Initialize a matrix for detailed household expenditure item variables.
hes_var_nm <- NULL # Initialize a matrix for the names of household expenditure item variables.
hes_var_nm2 <- NULL # Initialize a matrix for the names of detailed household expenditure item variables.
hes_var_date_all <- NULL # Initialize a matrix for the dates of household expenditure item variables.

for (i in 2020:hes_end_all[1]){
  if (i == hes_end_all[1]) {
    # The last year in the sample.
    temp_mt <- c(1,hes_end_all[2]) # The first and last month of the year available in the sample.
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
    hes_var_all <- cbind(hes_var_all,temp_numeric)
    hes_var2_all <- cbind(hes_var2_all,temp_numeric2)
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
    hes_var_date_all <- c(hes_var_date_all, paste0(i,"-",j,"-",temp_dates))
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

temper_var_all <- NULL # Initialize a matrix for temperatures across the capitals of prefectures.
hum_var_all <- NULL # Initialize a matrix for humidity across the capitals of prefectures.
weath_date_all <- NULL # Initialize a matrix for the dates of weather data.

# Set the orders of the capitals of prefectures.
weath_pref_nm <- c("札幌", "青森", "盛岡", "秋田", "仙台", "山形", "福島", "水戸", "宇都宮", "さいたま", "千葉", "東京", "新潟", "前橋", "長野", "甲府", "横浜", "静岡", "富山", "岐阜", "名古屋", "金沢", "福井", "大津", "津", "奈良", "和歌山", "大阪", "京都", "神戸", "鳥取", "岡山", "松江", "広島", "山口", "高松", "松山", "徳島", "高知", "福岡", "大分", "宮崎", "佐賀", "熊本", "鹿児島", "長崎", "那覇") 
# Translate the names of capitals of prefectures into the names of prefectures.
weath_pref_nm2 <- c("北海道", "青森県", "岩手県", "秋田県", "宮城県", "山形県", "福島県", "茨城県", "栃木県", "埼玉県", "千葉県", "東京都", "新潟県", "群馬県", "長野県", "山梨県", "神奈川県", "静岡県", "富山県", "岐阜県", "愛知県", "石川県", "福井県", "滋賀県", "三重県", "奈良県", "和歌山県", "大阪府", "京都府", "兵庫県", "鳥取県", "岡山県", "島根県", "広島県", "山口県", "香川県", "愛媛県", "徳島県", "高知県", "福岡県", "大分県", "宮崎県", "佐賀県", "熊本県", "鹿児島県", "長崎県", "沖縄県") 

for (i in 2020:(hes_end_all[1]+(hes_end_all[2]==12))){ # Weather data are available for one month after the last month for household expenditure data. 

  if (i == hes_end_all[1]+(hes_end_all[2]==12)) {
    # The last year in the sample of weather data.
    # Weather data are available for one month after the last month for household expenditure data.
    temp_mt <- c(1,(hes_end_all[2]+1)*(hes_end_all[2]<12)+1*(hes_end_all[2]==12)) # The first and last month of the year available in the sample.
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
    temper_var_all <- rbind(temper_var_all,temp_temper)
    hum_var_all <- rbind(hum_var_all,temp_hum)
    
    # Extract the dates of each month from the first row of temp_dt.
    temp_dates <- temp_dt[5:(nrow(temp_dt)-(temp_dt[nrow(temp_dt),1]=="")),1] # Sometimes the last columns are NAs. In that case, exclude that column.
    # Extend the time series of dates. 
    weath_date_all <- c(weath_date_all, temp_dates)
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

# Fulfill the year, month, and date in the balanced panel of the number of tested positive at each date in each prefecture.
# The number of rows of the balanced panel equals the number of rows of the prefecture data with the longest column length.
TstPstv_pref_blncd[ ,1:3] <- matrix(as.numeric(unlist(TstPstv_data_pref[[TstPstv_loc_max_ndates]][ ,1:3])),nc=3)

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
hes_var2_ma_all <- t(apply(hes_var2_all, 1, function(x){filter(x,rep(1/7,7))})) # Centered moving averages. apply(,1,) transposes the original matrices.
hes_var2_ma_all <- hes_var2_ma_all[,4:(ncol(hes_var2_all)-3)] # Shift rows to create backward moving averages.
hes_var2_ma_date_all <- hes_var_date_all[7:ncol(hes_var2_all)] # Dates of data in hes_var2_ma. 

# Compute 7-days backward moving averages of mobility reports data.  
for (i in 1:6){
  eval(parse(text=paste0("mob_var",i,"_ma <- apply(mob_var",i,", 2, function(x){filter(x,rep(1/7,7))})"))) # Centered moving averages.
  eval(parse(text=paste0("mob_var",i,"_ma <- mob_var",i,"_ma[4:(mob_ndays-3),]"))) # Shift rows to create backward moving averages.
}
mob_var_ma_date <- mob_date[7:mob_ndays]
mob_ma_ndays <- length(mob_var_ma_date) # The number of rows of mob_var_i_ma for i =1,2,3,...,6.

# Substitute missing data in hum_var. Interpolate them with data for neighboring dates.
# 2020/5/6: Kobe.
# 2020/12/10: Kumamoto.
# 2021/6/20: Yokohama.
hum_var_all[weath_date_all=="2020/5/6", weath_pref_nm=="神戸"] <- (hum_var_all[weath_date_all=="2020/5/5", weath_pref_nm=="神戸"] + hum_var_all[weath_date_all=="2020/5/7", weath_pref_nm=="神戸"]) / 2
hum_var_all[weath_date_all=="2020/12/10", weath_pref_nm=="熊本"] <- (hum_var_all[weath_date_all=="2020/12/9", weath_pref_nm=="熊本"] + hum_var_all[weath_date_all=="2020/12/11", weath_pref_nm=="熊本"]) / 2
hum_var_all[weath_date_all=="2021/6/20", weath_pref_nm=="横浜"] <- (hum_var_all[weath_date_all=="2021/6/19", weath_pref_nm=="横浜"] + hum_var_all[weath_date_all=="2021/6/21", weath_pref_nm=="横浜"]) / 2

# Convert relative humidity into absolute humidity.
abs_hum_var_all <- calc_abs_hum(temper_var_all,hum_var_all)

# Compute the nationwide weighted averages of temperature and humidity.

if (flag_pref_wgt == 0){
  # Use the number of new cases in the past 7 days for the weights for prefectures. 
  temp <- rbind(matrix(1,nr=31+14,nc=1)%*%TstPstv_pref_share[1,],TstPstv_pref_share) # Fill the first row of the data for the weights between 2020 Jan. 1 and 2020 Feb. 14, one day before the first date of the sample period for the data. 
  # Extract the weights for the duration of weather data.
  temp <- temp[1:(31+14+which(TstPstv_pref_share_date==weath_date[length(weath_date)])),]
  temper_var_ave_all <- rowSums(temper_var_all * temp) 
  hum_var_ave_all <- rowSums(hum_var_all * temp) 
  abs_hum_var_ave_all <- rowSums(abs_hum_var_all * temp) 
  ind_abs_hum_var_ave_all <- rowSums((abs_hum_var_all<ah_ub) * temp) # = 1 if absolute humidity < ah_ub g/m^3, which is based on Nottmeyer et al (2020). Then take the nationwide average of the indicator. 
}else{
  # Use population shares for the weights for prefectures. 
  temper_var_ave_all <- c(temper_var_all %*% popu_share) 
  hum_var_ave_all <- c(hum_var_all %*% popu_share) 
  abs_hum_var_ave_all <- c(abs_hum_var_all %*% popu_share) 
  ind_abs_hum_var_ave_all <- c((abs_hum_var_all<ah_ub) %*% popu_share) # = 1 if absolute humidity < ah_ub g/m^3, which is based on Nottmeyer et al (2020). Then take the nationwide average of the indicator. 
}



# Compute 7-days backward moving averages of temperature and humidity.
temper_var_ma_all <- apply(temper_var_all, 2, function(x){filter(x,rep(1/7,7))}) # Centered moving averages. apply(,1,) transposes the original matrices.
temper_var_ma_all <- temper_var_ma_all[4:(nrow(temper_var_ma_all)-3),] # Shift rows to create backward moving averages.
hum_var_ma_all <- apply(hum_var_all, 2, function(x){filter(x,rep(1/7,7))}) # Centered moving averages. apply(,1,) transposes the original matrices.
hum_var_ma_all <- hum_var_ma_all[4:(nrow(hum_var_ma_all)-3),] # Shift rows to create backward moving averages.
abs_hum_var_ma_all <- apply(abs_hum_var_all, 2, function(x){filter(x,rep(1/7,7))}) # Centered moving averages. apply(,1,) transposes the original matrices.
abs_hum_var_ma_all <- abs_hum_var_ma_all[4:(nrow(abs_hum_var_ma_all)-3),] # Shift rows to create backward moving averages.
weath_ma_date_all <- weath_date_all[7:length(weath_date_all)] # Dates of data in moving average data. 

# Compute the nationwide weighted averages of 7-day moving averages of temperature and humidity.
if (flag_pref_wgt == 0){
  # Use the number of new cases in the past 7 days for the weights for prefectures. 
  temp <- rbind(matrix(1,nr=31+14,nc=1)%*%TstPstv_pref_share[1,],TstPstv_pref_share) # Fill the first row of the data for the weights between 2020 Jan. 1 and 2020 Feb. 14, one day before the first date of the sample period for the data. 
  # Extract the weights for the duration of weather data. The moving average of weather data starts from 2020 January 7.
  temp <- temp[7:(31+14+which(TstPstv_pref_share_date==weath_ma_date_all[length(weath_ma_date_all)])),]
  temper_var_ma_ave_all <- rowSums(temper_var_ma_all * temp) 
  hum_var_ma_ave_all <- rowSums(hum_var_ma_all * temp) 
  abs_hum_var_ma_ave_all <- rowSums(abs_hum_var_ma_all * temp) 
  ind_abs_hum_var_ma_ave_all <- rowSums((abs_hum_var_ma_all<ah_ub) * temp) # = 1 if absolute humidity < ah_ub g/m^3, which is based on Nottmeyer et al (2020). Then take the nationwide average of the indicator. 
}else{
  # Use population shares for the weights for prefectures. 
  temper_var_ma_ave_all <- c(temper_var_ma_all %*% popu_share) 
  hum_var_ma_ave_all <- c(hum_var_ma_all %*% popu_share) 
  abs_hum_var_ma_ave_all <- c(abs_hum_var_ma_all %*% popu_share) 
  ind_abs_hum_var_ma_ave_all <- c((abs_hum_var_ma_all<ah_ub) %*% popu_share) # = 1 if absolute humidity < ah_ub g/m^3, which is based on Nottmeyer et al (2020). Then take the nationwide average of the indicator. 
}

# Compute the 2020 average price for each item. 13th column is Jan 2020.
temp <- apply(CPI_m[,13:24],1,mean) # Compute the 2020 average.
CPI_m_2020 <- apply(CPI_m,2,function(x){x/temp}) # Set the benchmark of CPI to 2020.


# Interpolate the CPI for hotel services during the GO-TO-TRAVEL period in order to convert "宿泊料" in household expenditure into real term. 
# "一般外食” in CPI is used for "食事代", "喫茶代", "飲酒代"; interporated "宿泊料" in CPI for "宿泊料" in household expenditure; "宿泊料" in CPI for "国内パック旅行" in household expenditure.; "被服及び履物" in CPI for "被服及び履物" in household expenditure.
# GO-TO-TRAVEL period: 2020 July 22 to 2020 Dec 27. 
temp <- CPI_m_2020[2,12+7] + (CPI_m_2020[2,25]-CPI_m_2020[2,12+7])/6 * 1:5 # Linear interpolation of CPI for "宿泊料".
CPI_m_2020_rvs <- rbind(CPI_m_2020[1,],CPI_m_2020[2,],CPI_m_2020[2,],CPI_m_2020[3,]) # Insert one more row for "宿泊料".
CPI_m_2020_rvs[2,(12+8):24] <- temp # Insert the interporated CPI series for "宿泊料" in household expenditures
  
# Convert monthly CPI into daily CPI from Jan 2019.
ndays_olympic <- c(31,29,31,30,31,30,31,31,30,31,30,31) # Number of days in an olympic year.
ndays_normal <- c(31,28,31,30,31,30,31,31,30,31,30,31) # Number of days in an non-olympic year.

temp_date <- c(ndays_normal,ndays_olympic,rep(ndays_normal,(hes_end_all[1]-2021)),ndays_normal[1:hes_end_all[2]]) # Create a series of date from Jan. 2020 to the end of the last month in the estimation period.
CPI_d_2020 <- matrix(1,nc=temp_date[1]) %x% CPI_m_2020_rvs[,1] # Initialize a matrix to contain daily CPI.
for (i in 2: length(temp_date)){
  CPI_d_2020 <- cbind(CPI_d_2020, matrix(1,nc=temp_date[i]) %x% CPI_m_2020_rvs[,i]) # Fill all the dates in a month by the month's CPI for each item.
}




########## Set the explanatory variables in a linear regression for the reproduction number ###########################

# Set the last date of the estimation period.
hes_end_date_estimation <- paste0(hes_end[1],"-",hes_end[2],"-",ndays_normal[hes_end[2]]) # For household expenditures.
weath_end_date_estimation <- paste0(hes_end[1],"/",hes_end[2],"/",ndays_normal[hes_end[2]]) # For absolute humidity.

# Define the end of the available sample period in the weather date format.
weath_end_date_allsmpl <- paste0(hes_end_all[1],"/",hes_end_all[2],"/",ndays_normal[hes_end_all[2]])

# Create a matrix of household expenditure items for estimation. 

if (nominal_hes == 1){
  # These are nominal term.
  H_expvals_all <- rbind(hes_var2_all[hes_var_nm2[,1]=="食事代",],
                     hes_var2_all[hes_var_nm2[,1]=="喫茶代",],
                     hes_var2_all[hes_var_nm2[,1]=="飲酒代",],
                     hes_var2_all[hes_var_nm2[,1]=="宿泊料",],
                     hes_var2_all[hes_var_nm2[,1]=="国内パック旅行費",],
                     t(apply(hes_var2_all[hes_var_nm2[,1]=="被服及び履物",],2,mean))) #This label appears multiple times.
  
  
}else{
  
  # Compute real household expenditure values in 2020 CPI average for each item.
  # CPI_d_2020 contains "一般外食”, interpolated "宿泊料" to remove the effect of GO-TO-TRAVEL, "宿泊料", "被服及び履物" in CPI.
  # The sample period of CPI_d_2020 starts from Jan. 1, 2019.

    H_expvals_all <- rbind(hes_var2_all[hes_var_nm2[,1]=="食事代",] / CPI_d_2020[1, 366:dim(CPI_d_2020)[2]], #From 2020 Jan 1.
                     hes_var2_all[hes_var_nm2[,1]=="喫茶代",] / CPI_d_2020[1, 366:dim(CPI_d_2020)[2]],
                     hes_var2_all[hes_var_nm2[,1]=="飲酒代",] / CPI_d_2020[1, 366:dim(CPI_d_2020)[2]],
                     hes_var2_all[hes_var_nm2[,1]=="宿泊料",] / CPI_d_2020[2, 366:dim(CPI_d_2020)[2]],
                     hes_var2_all[hes_var_nm2[,1]=="国内パック旅行費",] / CPI_d_2020[3, 366:dim(CPI_d_2020)[2]],
                     t(apply(hes_var2_all[hes_var_nm2[,1]=="被服及び履物",],2,mean)) / CPI_d_2020[4, 366:dim(CPI_d_2020)[2]]) #This label appears multiple times.
  
  H_expvals_all <- H_expvals_all / 100 # To increase the number of digit of coefficients to avoid the effect of possible rounding error.  
}

# Choose the absolute humidity data for the use of the estimation.
if (log_abs_hum == 1){
  W_abs_hum_all <- log(abs_hum_var_ave_all) # Take log of absolute humidity.
}else if(log_abs_hum == 2){
  W_abs_hum_all <- abs_hum_var_ave_all # Use the level of absolute humidity.
}else{
  W_abs_hum_all <- 1 - ind_abs_hum_var_ave_all # Use the dummy that absolute humidity exceeds 9 g/m^3.
}

# Define household expenditures and weather data for the estimation period.
H_expvals <- H_expvals_all[,1:which(hes_var_date_all==hes_end_date_estimation)] # Household expenditures data up to the date defined by hes_end.
hes_var_date <- hes_var_date_all[1:which(hes_var_date_all==hes_end_date_estimation)] # Dates for household expenditures data up to the date defined by hes_end.

hes_var2_ma <- hes_var2_ma_all[,1:which(hes_var2_ma_date_all==hes_end_date_estimation)] # 7-day backward moving average of household expenditure data up to the date defined by hes_end.
hes_var2_ma_date <- hes_var2_ma_date_all[1:which(hes_var2_ma_date_all==hes_end_date_estimation)] # Dates for 7-day backward moving average of household expenditure data up to the date defined by hes_end.

W_abs_hum <- W_abs_hum_all[1:which(weath_date_all==weath_end_date_estimation)] # Absolute humidity data up to the date defined by hes_end.
weath_date <- weath_date_all[1:which(weath_date_all==weath_end_date_estimation)] # Dates for weather data up to the date defined by hes_end.

temper_var_ave <- temper_var_ave_all[1:which(weath_date_all==weath_end_date_estimation)] # Nation-wide average temperature data up to the date defined by hes_end.  
hum_var_ave <- hum_var_ave_all[1:which(weath_date_all==weath_end_date_estimation)] # Nation-wide average relative humidity data up to the date defined by hes_end.  
ind_abs_hum_var_ave <- ind_abs_hum_var_ave_all[1:which(weath_date_all==weath_end_date_estimation)] # 7-day backward moving averages of nation-wide dummy for absolute humidity data up to the date defined by hes_end.

temper_var_ma_ave <- temper_var_ma_ave_all[1:which(weath_ma_date_all==weath_end_date_estimation)] # 7-day backward moving averages of nation-wide average temperature data up to the date defined by hes_end.  
hum_var_ma_ave <- hum_var_ma_ave_all[1:which(weath_ma_date_all==weath_end_date_estimation)] # 7-day backward moving averages of nation-wide average relative humidity data up to the date defined by hes_end.  
abs_hum_var_ma_ave <- abs_hum_var_ma_ave_all[1:which(weath_ma_date_all==weath_end_date_estimation)] # 7-day backward moving averages of nation-wide average absolute humidity data up to the date defined by hes_end.  
ind_abs_hum_var_ma_ave <- ind_abs_hum_var_ma_ave_all[1:which(weath_ma_date_all==weath_end_date_estimation)] # 7-day backward moving averages of nation-wide dummy for absolute humidity data up to the date defined by hes_end.
weath_ma_date <- weath_ma_date_all[1:which(weath_ma_date_all==weath_end_date_estimation)] # Dates for 7-day backward moving averages of weather data up to the date defined by hes_end.

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

# Second declaration: 2021/1/7-2021/3/21. 
# if (length(R_date)>=365+21){
#   D_SE2[which(R_date=="2021/1/7"):which(R_date=="2021/3/21") + 29-14] <- 1 
# }else{
#   D_SE2[(which(R_date=="2021/1/7") + 29-14):length(R_date)] <- 1 # Second declaration: 2021/1/7-2021/3/21. The latest sample date is before March 21.
# }
# Set time dummies for the second state of emergency only up to Jan. 2021, as the out-of-sample prediction of model estimated with data only up to Jan. 2021 shows a good fit with the realized effective reproduction number.
D_SE2[which(R_date=="2021/1/7"):which(R_date=="2021/1/31") + 29-14] <- 1


if (exists("mdl_number")==1){
  if (mdl_number >= 13){
    # Construct a dummy variable for the period before the first state of emergency. The first date is 2020 Feb. 15, the same as mobility report data.
    D_pre_SE1 <- c(rep(0, length(R_date) + 29-14)) 
    D_pre_SE1[1:(which(R_date=="2020/4/6") + 29-14)] <- 1 # First declaration: The first date to 2020/4/7. 
  }
}



##################### Load additional data for simulation ###########################

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

# Load 2016-2018 data.
# Initialize the vector to contain early data.
Rail_Pssgrs_upto2018_m <- NULL
for (i in 2016:2018){
  eval(parse(text=paste0("temp_dt <- read.csv(\"./data/MILTT Railways/Railways",i,".csv\", header=F,stringsAsFactors=FALSE)")))  
  Rail_Pssgrs_upto2018_m <- c(Rail_Pssgrs_upto2018_m, conv_list(temp_dt[18:29,3]))
}


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



############## Reform data for simulations ####################################

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


if (flag_pref_wgt == 1){ # If = 1, the population of each prefecture is used as a weight to average prefectural data.
  ### Compute the nationwide weighted averages of temperature in 2019.
  temper_var_ave_2019 <- c(temper_var_2019 %*% popu_share) 
  
  ### Compute the nationwide weighted averages of humidity in 2019.
  hum_var_ave_2019 <- c(hum_var_2019 %*% popu_share) 
  
  ### Compute absolute humidity in 2019.
  abs_hum_var_2019 <- calc_abs_hum(temper_var_2019,hum_var_2019)
  
  ### Create a dummy for the period when absolute humidity < ah_ub g/m^3. Then take the nationwide average of the indicator. 
  ind_abs_hum_var_2019 <- c((abs_hum_var_2019<ah_ub) %*% popu_share) 
}



############## Load household expenditure data at prefecture level ####################################
# Household expenditure survey at prefecture level; Monthly; 2019 Jan. 01; Unit: current yen. 

# Initialize a list that contains a matrix for detailed household expenditure item variables at each prefecture.
hes_var_pref <- list()
# Initialize a matrix for each city's share of the number of households in household survey data.
hes_popu_pref <- matrix(NA,nr=47,nc=(hes_end_all[1]-2019)*12+hes_end_all[2])

# Define the names of household expenditure items to extract for each prefecture.
hes_var_nm_pref <- c("食事代", "喫茶代","飲酒代","宿泊料","国内パック旅行費","被服及び履物")
# Define the names of the capital cities of prefectures for which household expenditure data exist.
hes_city_nm_pref <- c("札幌市", "青森市", "盛岡市", "仙台市", "秋田市", "山形市", "福島市", "水戸市", "宇都宮市", "前橋市", "さいたま市", "千葉市", "東京都区部", "横浜市", "新潟市", "富山市", "金沢市",	"福井市", "甲府市", "長野市", "岐阜市", "静岡市", "名古屋市", "津市", "大津市", "京都市", "大阪市", "神戸市", "奈良市", "和歌山市", "鳥取市", "松江市", "岡山市", "広島市", "山口市", "徳島市", "高松市", "松山市", "高知市", "福岡市", "佐賀市", "長崎市", "熊本市",	"大分市", "宮崎市", "鹿児島市", "那覇市")	
# Set the orders of prefectures corresponding the capital cities in hes_city_nm_pref. The names of prefectures are in accordance with weath_pref_nm.
hes_pref_nm_pref <- c("北海道", "青森県", "岩手県",  "宮城県", "秋田県", "山形県", "福島県", "茨城県", "栃木県", "群馬県", "埼玉県", "千葉県", "東京都",  "神奈川県", "新潟県", "富山県", "石川県", "福井県", "山梨県", "長野県", "岐阜県", "静岡県", "愛知県", "三重県", "滋賀県", "京都府", "大阪府", "兵庫県",  "奈良県", "和歌山県", "鳥取県", "島根県", "岡山県","広島県", "山口県", "徳島県",  "香川県", "愛媛県", "高知県", "福岡県", "佐賀県", "長崎県", "熊本県", "大分県", "宮崎県", "鹿児島県", "沖縄県")
# Translate the names of prefectures into English, preserving the order of elements in the vector.
hes_pref_nm_pref_ENG <- c("Hokkaido", "Aomori", "Iwate",  "Miyagi", "Akita", "Yamagata", "Fukushima", "Ibaraki", "Tochigi", "Gunma", "Saitama", "Chiba", "Tokyo",  "Kanagawa", "Niigata", "Toyama", "Ishikawa", "Fukui", "Yamanashi", "Nagano", "Gifu", "Shizuoka", "Aichi", "Mie", "Shiga", "Kyoto", "Osaka", "Hyogo",  "Nara", "Wakayama", "Tottori", "Shimane", "Okayama","Hiroshima", "Yamaguchi", "Tokushima",  "Kagawa", "Ehime", "Kochi", "Fukuoka", "Saga", "Nagasaki", "Kumamoto", "Oita", "Miyazaki", "Kagoshima", "Okinawa")

# Iterations over years.
for (i in 2019:hes_end_all[1]){
  if (i == hes_end_all[1]) {
    # The last year in the sample.
    temp_mt <- c(1,hes_end_all[2]) # The first and last month of the year available in the sample.
  }else{
    temp_mt <- c(1,12) # The first and last month of the year available in the sample.
  }

  # Iterations over months.
  for (j in temp_mt[1]:temp_mt[2]){
    
    # Extract household expenditure data for month j, year i.
    # For an unknown reason, an empty column is read at the end. The last data column is the fourth to the last column.
    eval(parse(text=paste0("temp_dt <- read.csv(\"./data/household survey/pref/a401_",i,"_",j,".csv\", header=F,stringsAsFactors=FALSE)"))) 
    
    # Initialize a vector to record the locations of items to extract.
    temp_ind <- rep(NA,length(hes_var_nm_pref))
    
    # Locate the rows for the household expenditures in explanatory variables.
    # There is a change in the format of the data file from 2020 Nov.
    for (k in 1:length(hes_var_nm_pref)){
      if (i<=2019 || (i==2020 && j<11)){
        temp_ind[k] <- which(temp_dt[,9]==hes_var_nm_pref[k])[1] # The names of household expenditure items are in the 9th column of the original data file loaded above. If there are multiple rows for the same household expenditure item, collect the first row.
      }else{
        temp_ind[k] <- which(temp_dt[,12]==hes_var_nm_pref[k])[1] # The names of household expenditure items are in the 12th column of the original data file loaded above. If there are multiple rows for the same household expenditure item, collect the first row.
      }
    }

    # Extract household expenditure data for the capital of prefecture in month j, year i.
    for (v in 1:length(hes_city_nm_pref)){
      # Locate the column of th data file that contains the data for the capital of the prefecture.
      # For each city, there is a pair of quantities and nominal expenditures placed as consective column vectors in the data table.   
      # There is a change in the format of the data file from 2020 Nov.
      if (i<=2019 || (i==2020 && j<11)){
        # Up to 2020 Oct., the city name is placed only in the column for quantities. Nominal expenditures are placed in the next column. 
        temp <- conv_list(temp_dt[temp_ind, which(temp_dt[8,] == hes_city_nm_pref[v])+1]) # Convert characters in numeric.
        temp_popu <- conv_list(temp_dt[temp_dt[,8]==" 世帯数分布(抽出率調整)", which(temp_dt[8,] == hes_city_nm_pref[v])+1]) # Convert characters in numeric.
      }else{
        # From 2020 Nov., the city name is placed in columns for both quantities and nominal expenditures. 
        temp <- conv_list(temp_dt[temp_ind, grep(paste0("^",hes_city_nm_pref[v]), substring(temp_dt[9,],7,100))[2]]) # Convert characters in numeric. There are 5 digit number and a blank before each name of prefectures' capital. 100 is large enough to contain all characters with the number, a blank, and the name of the city in the cell.
        temp_popu <- conv_list(temp_dt[temp_dt[,12]=="世帯数分布(抽出率調整)", grep(paste0("^",hes_city_nm_pref[v]), substring(temp_dt[9,],7,100))[2]]) # Convert characters in numeric.
      }
      
      # Record household expenditure data for each prefecture and each city's share of the number of households in month j, year i.
      if (i == 2019 && j == 1){
        # The first iteration for the prefecture. No previous data.
        # Turn the monthly total into daily average by dividing the monthly total data by the number of days in the month.
        hes_var_pref[[v]] <- as.matrix(temp,nc=1)/31 # There are 31 days in Jan. 2019.
        hes_popu_pref[v,1] <- temp_popu
      }else{
        # Extend the new month in a new column.
        # Turn the monthly total into daily average by dividing the monthly total data by the number of days in the month.
        if (i==2020){
          hes_var_pref[[v]] <- cbind(hes_var_pref[[v]],as.matrix(temp/ndays_olympic[j],nc=1)) # Divide the monthly total by the number of days during month j in an Olympic years.
        }else{
          hes_var_pref[[v]] <- cbind(hes_var_pref[[v]],as.matrix(temp/ndays_normal[j],nc=1)) # Divide the monthly total by the number of days during month j in a normal years.
        }
        hes_popu_pref[v,(i-2019)*12+j] <- temp_popu
      }
      
    }
  }
}

# Compute each city's share of the number of households in each month.
hes_popu_share_pref <- apply(hes_popu_pref, 2, function(x){x/sum(x)})

# Compute the min and the max of each household expenditure among prefectures.

hes_var_pref_min <- hes_var_pref_max <- rep(NA,6) # Initialize the vectors to contain the min and max of each household expenditure among prefectures.

# Iterations over household expenditure items.
for (k in 1:length(hes_var_nm_pref)){
  # Initialize the matrix to contain the household expenditure in each prefecture.
  temp <- matrix(NA, nr=length(hes_city_nm_pref), nc = ncol(hes_var_pref[[1]]))
  # Iterations over prefectures.
  for (v in 1:length(hes_city_nm_pref)){
    # Record the household expenditure in each prefecture.
    temp[v,] <- hes_var_pref[[v]][k,]
  }
  # Compute the minimum of the maximum of the household expenditure among prefectures.
  hes_var_pref_min[k] <- min(temp)
  hes_var_pref_max[k] <- max(temp) 
}


############## Load the number of hotel guests at prefecture level ####################################
# nobe shukuhakusha suu ; Monthly; 2019 Jan. 01; Unit: persons, shukuhaku ryoko tokei chosa (kokko sho).
# nobe gaikokujin shukuhakusha suu ; Monthly; 2019 Jan. 01; Unit: persons, shukuhaku ryoko tokei chosa (kokko sho).

# Initialize the matrix for the number of hotel guests from inside Japan to each prefecture.
# Initialize the matrix for each prefecture's share of the number of hotel guests from inside Japan.
# The release of hotel guest data lags that of household expenditure data by one month. So reduce the number of columns by one.
guest_var_pref <- guest_var_pref_share <- matrix(NA,nr=47,nc=12*(hes_end_all[1]-2019)+hes_end_all[2]-1)


# Iterations over years.
for (i in 2019:hes_end_all[1]){
  if (i == hes_end_all[1]) {
    # The last year in the sample.
    temp_mt <- c(1,hes_end_all[2]) # The first and last month of the year available in the sample.
  }else{
    temp_mt <- c(1,12) # The first and last month of the year available in the sample.
  }
  for (j in temp_mt[1]:temp_mt[2]){
    
    # Skip the last iteration, because of the release of hotel guest data lags that of household expenditure data by one month.
    if (i == hes_end_all[1] && j ==hes_end_all[2]){
      break
    }
    
    # Extract hotel guest data for month j, year i.
    eval(parse(text=paste0("temp_dt <- read.csv(\"./data/shukuhaku ryoko tokei chosa/",i,"/第2表(",j,"月).csv\", header=F,stringsAsFactors=FALSE)")))
    
    # There are "延べ宿泊者数" and "外国人延べ宿泊者数".
    # Extract "延べ宿泊者数" for the month, whose column is located before the column for "外国人延べ宿泊者数"
    temp_all <- conv_list(temp_dt[8:54,grep("延べ\n宿泊者数",temp_dt[4,])[1]]) # Convert characters in numeric.
    # Extract "外国人延べ宿泊者数" for the month, whose column is located after the column for "延べ宿泊者数"
    temp_foreign <- conv_list(temp_dt[8:54,grep("延べ\n宿泊者数",temp_dt[4,])[2]]) # Convert characters in numeric.
    # Compute hotel guests from inside Japan.
    temp <- temp_all - temp_foreign
    
    # Record the number of hotel guests from inside Japan to each prefecture in the month.
    guest_var_pref[ ,(i-2019)*12+j] <- temp 
    # Record each prefecture's share of the number of hotel guests from inside Japan in the month.
    guest_var_pref_share[ ,(i-2019)*12+j] <- temp/sum(temp)
      
    # Extract the names of prefectures, which are in the first column.
    temp_pref_nm <- temp_dt[8:54,1]
    # Remove the two digit numbering before the name of each prefecture. There is a black before each numbering. So remove the first three characters in each cell.
    #100 is a sufficiently large value that is greater than the number of characters in each element of temp_pref_nm.
    temp_pref_nm <- substring(temp_pref_nm,4,100) 
    
    if (i==2019 && j == 1){
      # Record the order of prefectures for hotel guest data.
      guest_pref_nm_pref <- temp_pref_nm      
    }else{
      # Check if there is any change in the order of prefectures in the data.
      if(sum(guest_pref_nm_pref != temp_pref_nm)>0){
        stop("There is a change in the order of prefectures in hotel guest data.")
      }
    }
  }
}

############## Load the delta-variant share of new reported cases in Tokyo ####################################
# Weekly average. For April 30 - May 2, 2021, and weekly average from May 3, 2021. 

# Load data. First column: Dates (yyyy/m/d); Second column; Number of tested cases; Third column: Number of L452R detected.
temp_dt <- read.csv("./data/vaccination/TokyoDeltaScreening.csv", header=T,stringsAsFactors=FALSE)

# Extract the delta-variant share of new reported cases, including the projected numbers for future dates.
L452R_share_Tokyo_w <- temp_dt[,4]

# Remove rows with NAs.
if (sum(is.na(L452R_share_Tokyo_w))>0){
  L452R_share_Tokyo_w <- L452R_share_Tokyo_w[1:(which(is.na(L452R_share_Tokyo_w))[1]-1)]
}

# Back out the L452R share of infection for each week.
# The weekly average of L452 share of reported new cases is regarded as the value for the mid-date of the week.
L452R_share_Tokyo_w_infctn <- rep(NA, length(L452R_share_Tokyo_w)) # Initialize the vector to contain the L452R share of infection on each day.
for (i in 1:length(L452R_share_Tokyo_w)){
  if (i == 1){
    # For simplicity, assume that the first 14 days of the sample have the same value, so that it equals the L452 share of reported new cases in the next day.
    # L452R_share_Tokyo_w_infctn[1] is the average L452R share of infection for the 7 days before the mid-date of the first week of L452R_share_Tokyo_w.
    L452R_share_Tokyo_w_infctn[1] <- L452R_share_Tokyo_w[1]
  }else{
    # The L452R share of reported new cases = sum(dist_incub[1:7]) * The L452R share of infection for the past 8th to 14th days 
    # +sum(dist_incub[8:14]) * The L452R share of infection for the past 1st to 7th days
    L452R_share_Tokyo_w_infctn[i] <- (L452R_share_Tokyo_w[i] - L452R_share_Tokyo_w_infctn[i-1] * sum(dist_incub[1:7])) / sum(dist_incub[8:14])
  }
  # If the element exceeds one, it is corrected to one.
  L452R_share_Tokyo_w_infctn[i] <- L452R_share_Tokyo_w_infctn[i]*(L452R_share_Tokyo_w_infctn[i] < 1) + (L452R_share_Tokyo_w_infctn[i]>=1)
}

# Distribute the weekly number to each date.
for (i in 1:length(L452R_share_Tokyo_w_infctn)){
  if (i ==1){
    # For simplicity, assume that the first 14 days of the sample have the same value, so that it equals the L452 share of reported new cases in the next day.
    L452R_share_Tokyo_d <- rep(L452R_share_Tokyo_w_infctn[1],14)
  }else{
    # Distribute the weekly average to each date in the week.
    L452R_share_Tokyo_d <- c(L452R_share_Tokyo_d, rep(L452R_share_Tokyo_w_infctn[i],7))
  }
}

# If the element exceeds one, it is corrected to one.
L452R_share_Tokyo_d <- L452R_share_Tokyo_d*(L452R_share_Tokyo_d<1) + (L452R_share_Tokyo_d>=1)

# Fulfill zeros for the previous dates for earlier dates in 2021 up to April 18 (i.e., 14+1 days before the mid-date of the first week of L452R share of reported new cases, May 3rd), 2021.
L452R_share_Tokyo_d <- c(rep(0, sum(ndays_normal[1:3])+18), L452R_share_Tokyo_d)

# Fulfill the last value of L452R share in the sample for the future dates in 2021 after the end of the samples.
L452R_share_Tokyo_d <- c(L452R_share_Tokyo_d, rep(L452R_share_Tokyo_d[length(L452R_share_Tokyo_d)], 365-length(L452R_share_Tokyo_d)))



############## Load the delta-variant share of new reported cases nationwide ####################################
# Weekly average from May 31 - June 6, 2021. 
# Prediction by a fitted 2nd polynomial is fulfilled for future dates.

# Load data. First column: Dates (yyyy/m/d); Second column; Number of the L452R share of new cases tested for variants.
temp_dt <- read.csv("./data/vaccination/NWDeltaScreening.csv", header=T,stringsAsFactors=FALSE)

# Extract the delta-variant share of new reported cases, including the projected numbers for future dates.
L452R_share_NW_w <- temp_dt[,2]

# Remove rows with NAs.
if (sum(is.na(L452R_share_NW_w))>0){
  L452R_share_NW_w <- L452R_share_NW_w[1:(which(is.na(L452R_share_NW_w))[1]-1)]
}

# Back out the L452R share of infection for each week.
# The weekly average of L452 share of reported new cases is regarded as the value for the mid-date of the week.
L452R_share_NW_w_infctn <- rep(NA, length(L452R_share_NW_w)) # Initialize the vector to contain the L452R share of infection on each day.
for (i in 1:length(L452R_share_NW_w)){
  if (i == 1){
    # For simplicity, assume that the first 14 days of the sample have the same value, so that it equals the L452 share of reported new cases in the next day.
    # L452R_share_Tokyo_w_infctn[1] is the average L452R share of infection for the 7 days before the mid-date of the first week of L452R_share_Tokyo_w.
    L452R_share_NW_w_infctn[1] <- L452R_share_NW_w[1]
  }else{
    # The L452R share of reported new cases = sum(dist_incub[1:7]) * The L452R share of infection for the past 8th to 14th days 
    # +sum(dist_incub[8:14]) * The L452R share of infection for the past 1st to 7th days
    L452R_share_NW_w_infctn[i] <- (L452R_share_NW_w[i] - L452R_share_NW_w_infctn[i-1] * sum(dist_incub[1:7])) / sum(dist_incub[8:14])
  }
  # If the element exceeds one, it is corrected to one.
  L452R_share_NW_w_infctn[i] <- L452R_share_NW_w_infctn[i]*(L452R_share_NW_w_infctn[i] < 1) + (L452R_share_NW_w_infctn[i]>=1)
}

# Distribute the weekly number to each date.
for (i in 1:length(L452R_share_NW_w_infctn)){
  if (i ==1){
    # For simplicity, assume that the first 14 days of the sample have the same value, so that it equals the L452 share of reported new cases in the next day.
    L452R_share_NW_d <- rep(L452R_share_NW_w_infctn[1],14)
  }else{
    # Distribute the weekly average to each date in the week.
    L452R_share_NW_d <- c(L452R_share_NW_d, rep(L452R_share_NW_w_infctn[i],7))
  }
}

# If the element exceeds one, it is corrected to one.
L452R_share_NW_d <- L452R_share_NW_d*(L452R_share_NW_d<1) + (L452R_share_NW_d>=1)

# Fulfill zeros for the previous dates for earlier dates in 2021 up to May 19 (i.e., 14+1 days before the mid-date of the first week of L452R share of reported new cases, June 3), 2021.
L452R_share_NW_d <- c(rep(0, sum(ndays_normal[1:4])+19), L452R_share_NW_d)


# # Initialize the vector to contain the daily L452 share of new cases tested for variants nationwide.
# L452R_share_NW_d <- NULL
# # Distribute the weekly number to each date.
# for (i in 1:length(L452R_share_NW_w)){
#   if (i ==1){
#     # Interpolate between the mid dates in consecutive weeks. The middle date in each week takes the weekly average.
#     # Exterpolate for the dates before the mid date in the first week of the sample period.
#     L452R_share_NW_d <- L452R_share_NW_w[1] + (L452R_share_NW_w[2]-L452R_share_NW_w[1]) * c(-3:6)/7
#   }else  if (i ==length(L452R_share_NW_w)){
#     # Interpolate between the mid dates in consecutive weeks. The middle date in each week takes the weekly average.
#     # Exterpolate for the dates after the mid date in the last week of the sample period.
#     L452R_share_NW_d <- c(L452R_share_NW_d, L452R_share_NW_w[i-1]+(L452R_share_NW_w[i]-L452R_share_NW_w[i-1])*c(7:10)/7)
#   }else{
#     # Interpolate between the mid dates in consecutive weeks. The middle date in each week takes the weekly average.
#     L452R_share_NW_d <- c(L452R_share_NW_d, L452R_share_NW_w[i]+(L452R_share_NW_w[i+1]-L452R_share_NW_w[i])*c(0:6)/7)
#   }
# }
# 
# # If the element exceeds one, it is corrected to one.
# # L452R_share_NW_d <- L452R_share_NW_d*(L452R_share_NW_d<1) + (L452R_share_NW_d>=1)
# 
# # Back out the L452R share of infection on each day.
# # For simplicity, assume that the first 14 days in the sample have a constant L452 share of infection.
# # The first date of this share is one day before the first date of L452R share of reported new cases.
# L452R_share_NW_infctn <- rep(NA, length(L452R_share_Tokyo_d)) # Initialize the vector to contain the L452R share of infection on each day.
# for (i in 1:length(L452R_share_NW_d)){
#   if (i == 1){
#     L452R_share_NW_infctn[1] <- L452R_share_NW_d[1]
#   }else if (i>= 2 && i <= 13){
#     L452R_share_NW_infctn[i] <- (L452R_share_NW_d[i] - sum(c(rep(L452R_share_NW_d[1],14-i), L452R_share_NW_infctn[1:(i-1)]) * dist_incub[1:13])) / dist_incub[14]
#   }else{
#     L452R_share_NW_infctn[i] <- (L452R_share_NW_d[i] - sum(L452R_share_NW_infctn[(i-13):(i-1)] * dist_incub[1:13])) / dist_incub[14]
#   }
# }
# 
# # Fulfill zeros for the previous dates for earlier dates in 2021 up to May 29, 2021.
# L452R_share_NW_infctn <- c(rep(0, sum(ndays_normal[1:4])+29), L452R_share_NW_infctn)
# 
# # Fulfill ones for the future dates in 2021 after the end of the samples.
# L452R_share_NW_infctn <- c(L452R_share_NW_infctn, rep(1, 365-length(L452R_share_NW_infctn)))




#################### The number of vaccinated populations in Japan for each type of vaccines ############

# Load the number of each type of vaccinations. From April 12, 2021. First column: Dates (yyyy/m/d); Second column; Day of the week; Third column: Number of vaccinations; Fourth column: Number of first pfizar vaccinations; Fifth column: Number of first moderna vaccinations; Sixth column: Number of second pfizar vaccinations; Seventh column: Number of second moderna vaccinations;
vac_gen <- read.csv("./data/vaccination/vaccination_data5_ippan.csv", header=F,stringsAsFactors=FALSE)

# Convert characters into numerics.
# The data starts from the seventh row, which is the latest data.
# The last two rows are blank and notes.
vac_gen <- conv_list(vac_gen[7:(nrow(vac_gen)-2), 4:7])

# Upside down the rows of the data matrix to put the oldest data in the first row.
vac_gen <- apply(vac_gen,2,rev)


# Load the number of vaccinations for medical staff. From April 12, 2021. First column: Dates (yyyy/m/d); Second column: Number of first pfizar vaccinations; Third column: Number of first moderna vaccinations; Fourth column: Number of second pfizar vaccinations; Fifth column: Number of second moderna vaccinations;
vac_med <- read.csv("./data/vaccination/vaccination_data5_iryo.csv", header=F,stringsAsFactors=FALSE)

# Convert characters into numerics.
vac_med <- conv_list(vac_med[, 2:5])

# Remove rows with NAs.
if (sum(is.na(vac_med[,1]))>0){
  vac_med <- vac_med[1:(which(is.na(vac_med[,1]))[1]-1),]
}


# Load the number of vaccinations before April 9, 2021. From February 17, 2021. First column: Dates (yyyy/m/d); Second column: Number of first pfizar vaccinations; Third column: Number of second pfizar vaccinations;
vac_pre <- read.csv("./data/vaccination/vaccination_Feb_Apr2021.csv", header=F,stringsAsFactors=FALSE)

# Convert characters into numerics.
vac_pre <- conv_list(vac_pre[, 2:3])

# Remove rows with NAs.
if (sum(is.na(vac_pre[,1]))>0){
  vac_pre <- vac_pre[1:(which(is.na(vac_pre[,1]))[1]-1),]
}


# Construct of the number of daily vaccinations for each type of vaccines for 2021.
# Fulfill 0 up to February 16, 2021.
# Contain first and second pfizer and first and second moderna in columns in order
vac_type <- cbind(rbind(matrix(0,nr=31+16,nc=2), vac_pre), matrix(0,nr=31+28+31+9,nc=2)) # Up to April 9, 2021.
vac_type <- rbind(vac_type, matrix(0,nr=2,nc=4)) # Fulfill zeros for April 10 and 11.
vac_type <- rbind(vac_type, vac_med[,c(1,3,2,4)]+vac_gen[1:nrow(vac_med),c(1,3,2,4)]) # Up to July 30, 2021.
vac_type <- rbind(vac_type, vac_gen[(nrow(vac_med)+1):nrow(vac_gen),c(1,3,2,4)]) # Up to the end of the sample period.


# Compute the moving average of the cumulative vaccinated shares of populations.
mv_vac_type_share <- matrix(NA, nr=nrow(vac_type),nc=ncol(vac_type)) # Initialize the matrix. 
# Compute cumulative share for each type of vaccinated share.
for (i in 1:ncol(vac_type)){
  mv_vac_type_share[,i] <- cumsum(vac_type[,i]) / JPN_popu
  # The share must be no greater than one.
  mv_vac_type_share[,i] <- mv_vac_type_share[,i]*(mv_vac_type_share[,i]<1) + 1*(mv_vac_type_share[,i]>=1)

  # Compute average second-vaccination share of population for the past 7 or 14 days from each lag.
  # For second pfizer, it is 7 days. For the other types of vaccines, it is 14 days.
  if (i == 2){
    temp <- 7 # The width of time window for moving average in case of second vaccinations of pfizer.
  }else{
    temp <- 14 # The width of time window for moving average in case of the other types of vaccinations
  }
  # No protection effect on the day of vaccination. For each date, the average share up to the previous date enters as an explanatory variable.
  # Thus, extend 0 for the number of previous days defined by temp from Jan. 1, 2021. 
  temp_mv <- filter(c(rep(0,temp),mv_vac_type_share[,i]), rep(1/temp,temp), sides=1) 
  mv_vac_type_share[,i] <- temp_mv[temp:(length(temp_mv)-1)] # Drop NAs in the first part of elements. Drop the last element, as it is for the first date of 2022.
}



#################### The number of vaccinated populations in Japan ############
# Data: From April, 12, 2021. Daily.

# Load data. First column: Dates (yyyy/m/d); Second column; Number of one vaccinations; Third column: Number of second vaccinations.
vac_popu <- read.csv("./data/vaccination/summary_by_date.csv", header=T,stringsAsFactors=FALSE)

# Remove rows with NAs.
if (sum(is.na(vac_popu[,1]))>0){
  vac_popu <- vac_popu[1:(which(is.na(temp_dt[,1]))[1]-1),]
}

# Keep the date (in yyyy/m/d format).
vac_popu_date <- vac_popu[,1]

### Compute the vaccinated shares of population.

# Fulfill zeros for the previous dates for earlier dates in 2021 up to April 11, 2021.
# Drop the first column, which contain dates.
vccn_persons <- rbind(matrix(0, nr=sum(ndays_normal[1:3])+11,nc=2), as.matrix(vac_popu[,2:3]))

# Add the number of vaccinations during February 17 - April 9, 2021.
vccn_persons[(31+17):(sum(ndays_normal[1:3])+9),] <- vccn_persons[(31+17):(sum(ndays_normal[1:3])+9),] + vac_pre

# Add the number of vaccinations for medical staff for April 12 - July 30, 2021.
vccn_persons[(sum(ndays_normal[1:3])+12):(sum(ndays_normal[1:6])+30),] <- vccn_persons[(sum(ndays_normal[1:3])+12):(sum(ndays_normal[1:6])+30),] + cbind(rowSums(vac_med[,1:2]), rowSums(vac_med[,3:4])) # Sum the first pfizer and moderna, and the second pfizer and moderna separately.


# Compute the first-vaccinated share of populations.
vccn_frst_share <- cumsum(vccn_persons[,1]) / JPN_popu
# The share must be no greater than one.
vccn_frst_share <- vccn_frst_share*(vccn_frst_share<1) + 1*(vccn_frst_share>=1)

# Compute the second-vaccinated share of populations.
vccn_scnd_share <- cumsum(vccn_persons[,2] / JPN_popu)
# The share must be no greater than one.
vccn_scnd_share <- vccn_scnd_share*(vccn_scnd_share<1) + 1*(vccn_scnd_share>=1)

# Compute average second-vaccination share of population for the past 7 days from each lag.
# No protection effect on the day of vaccination. For each date, the average share up to the previous date enters as an explanatory variable.
# Thus, extend 0 for the previous 7 days from Jan. 1, 2021. 
mv_vccn_scnd_share <- filter(c(rep(0,7),vccn_scnd_share), rep(1/7,7), sides=1) 
mv_vccn_scnd_share <- mv_vccn_scnd_share[7:length(mv_vccn_scnd_share)] # Drop NAs in the first 6 elements. 

# Compute the first-vaccination-only share of population for the past 14 days from each lag.
# No protection effect on the day of vaccination. For each date, the average share up to the previous date enters as an explanatory variable.
mv_vccn_frst_share <- filter(c(rep(0,14),vccn_frst_share - vccn_scnd_share), rep(1/14,14), sides=1) # Extend 0 for the previous 14 days from Jan. 1, 2021.
mv_vccn_frst_share <- mv_vccn_frst_share[14:length(mv_vccn_frst_share)] # Drop NAs in the first 13 elements.
