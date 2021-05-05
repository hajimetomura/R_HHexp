# Estimate a linear regression for the reproduction number with mobility report data and household expenditure data.
# Frequency: daily.
# Model 1: No restriction on the signs of the effects of household expenditure items and mobility in transportation.
# Model 2- : Non-negativity restriction on the signs of the effects of household expenditure items and mobility in transportation and Non-positive restriction on the effect of lower absolute humidity.
# Model 2: Error terms are the simple 7-day moving average of white noises.
# Model 3: Add dummy variables for the two states of emergency to Model 2.
# Model 4: Error terms have serial correlation, which is equivalent to AR(1).
# Model 5: Add dummy variables for the two states of emergency to Model 4.
#
# Model 6-: Non-negativity restriction on the signs of the effects of household expenditure items and mobility in transportation and Non-positive restriction on the effect of lower absolute humidity.
# Model 6-: coefficient restrictions so that household expenditure and mobility have always positive effects on the reproduction number.
# Model 6-: Household expenditures are in level, rather than in log, because some items became zero during the estimation period.
# Model 6: Error terms have serial correlation. 
# Model 7: Error terms have serial correlation. Dummy variables for the two states of emergency to Model 4.
# Model 8: Error terms have no serial correlation.
# Model 9: Error terms have no serial correlation. Dummy variables for the two states of emergency to Model 4.
# Model 10: Clothing and footwear are dropped from the explanatory variables for Model 7.
# Model 11: The error term in Model 7 is changed to AR(2) process.
# Model 12: The error term in Model 7 has a serial correlation with shocks in a week ago. (No convergence.)
# Model 13: Add a dummy for the period before the first state of emergency to Model 7.
# Model 14: Add a dummy for the period before the first state of emergency to Model 11.
# Model 15: Add a dummy for the period before the first state of emergency to Model 7 and remove serial correlation in error terms from Model 7.
# Model 16: Apply the distribution of incubation periods to AR(1) error term. Also add white noises as a measurement error of R to Model 13.
# Model 17: Modify the distribution of the initial value of unobserved infectious events to the unconditional distribution.
# Model 18: Set real <lower=0, upper=1> rho in Model 17; // AR(1) term for unobseved infectious events.


########## Clear global workspace and plots. ################ 

if( dev.cur() > 1 ) dev.off() # Clear plots.
rm( list = ls( envir = globalenv() ), envir = globalenv() ) # Clear global workspace.

########## Set parameters ###################################

plot_data <- 1 # = 1, plot detailed household expenditure variables with the reproduction number in the common sample period. 

estim <- 0 # = 1, run a stan model.

flag_pref_wgt <- 1 # Default value: 1. If = 1, use population share among prefectures to compute average absolute humidity across prefectures. If = 0, use new cases in the past 7 days.

log_abs_hum <- 0 # Default value: 0. If = 1, use log of absolute humidity. If = 2, use the level of absolute humidity. Otherwise, use a dummy that absolute humidity exceeds 7. Unit: g/m^3.

log_R <- 1 # Default value: 0. If = 1, use log of the reproduction number. otherwise, use the level.

log_dist_incub <- 0 #  Default value: 0. If = 1, use a log normal distribution based on Chinese data reported by Stephen A et al. (2020). =0, use the empirical distribution of incubation periods based on Sugishita (2020).

nominal_hes <- 0 # Default value: 0. If = 1, use nominal household expenditure data. If = 0, use real household expenditure data denominated by 2020 average CPI for each item.

mdl_number <- 17 # Numbering of the stan model to be estimated.

ENG <- 1 # If = 1, write labels in English.

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
library(xtable)

# Call hand-made functions. 
source("./def_func_data_COVID.R",encoding="utf-8")

# Load and reform data. 
source("./localtrend_COVID_nationwide_data.R",encoding="utf-8")


########### Plot data #################################

if (plot_data == 1){

  ### The names of variables that are used to set the minimum values of correlation coefficients to extract contributors to R or mob.
  if (ENG==1){
    mincc_label <- c("Eating out for meals","Cafe","Bar")
  }else{
    mincc_label <- c("食事代","喫茶代","飲酒代")
  }
  
  ### Define the labels of R, temperature, relative humidity, and absolute humidity.
  if (ENG==1){
    R_label <- "R"
    temper_label <- "Temperature"
    rh_label <- "Relative humidity"
    ah_label <- "Absolute humidity"
  }else{
    R_label <- "実効再生産数"
    temper_label <- "温度"
    rh_label <- "相対湿度"
    ah_label <- "絶対湿度"
  }

  ### Define labels for household expenditures.
  hes_label <- hes_var_nm2[,1] 
  if(ENG==1){
    # Write "食事代", "喫茶代", "飲酒代", "宿泊料", "国内パック旅行費", "被服及び履物" in English.
    hes_label[hes_label=="食事代"] <- "Eating out for meals"
    hes_label[hes_label=="喫茶代"] <- "Cafe"
    hes_label[hes_label=="飲酒代"] <- "Bar"
    hes_label[hes_label=="宿泊料"] <- "Lodging"
    hes_label[hes_label=="国内パック旅行費"] <- "Domestic travel packages"
    hes_label[hes_label=="被服及び履物"] <- "Clothing and footwear"
  }
  
  
  ### Plot each expenditure item against the reproduction number.

  # Record the figures in a pdf file.
  eval(parse(text=paste0("pdf(file=\"plot_R_hes_ENG",as.logical(ENG),".pdf\", family=\"Japan1GothicBBB\")")))

  # Extract the data for the common sample period.
  temp_hes <- hes_var2_ma[,(31+29+1-6):ncol(hes_var2_ma)] # hes_var2_ma starts from 2020 January 7. Extract data from 2020 March 1 on.
  temp_R <- R_var[1:ncol(temp_hes)] # Reproduction number from 2020 March 1 on.

  # Check if the dates of the two data coincide.
  print(paste("R:",c(R_date[1],R_date[ncol(temp_hes)]), "; hes:", c(hes_var2_ma_date[(31+29+1-6)],hes_var2_ma_date[ncol(hes_var2_ma)])))

  # Find the location of the period of the state of emergency.
  date_label <- R_date[1:ncol(temp_hes)] # Define the date label to use.
  SE <- c(which(date_label=="2020/4/7"),which(date_label=="2020/5/25"),which(date_label=="2021/1/7")) # Add 3/21 after the sample period exceeds this date.
  if (length(date_label)>=which(date_label=="2021/1/7")+31-7+28+21){
    SE <- c(SE, which(date_label=="2021/3/21")) # Add the end of the second emergency.
  }
  
  # Plot data.  
  rslt_ccf_R_hes<-plot_ts_ccf(temp_R,R_label,temp_hes,hes_label,date_label,h_val=1,SE=SE)
  
  # Close the pdf file.
  dev.off()
  
  # Print ccf of all household expenditure items.
  # Define items to extract from the list of ccf.
  temp_nm <- c("食料","住居","光熱・水道","家具・家事用品","被服及び履物","保健医療","交通・通信","教育","教養娯楽","その他の消費支出","飲酒代")
  temp_nm_ENG <- c("Food","Housing","Fuel, light and water charges","Furniture and household utensils","Clothing and footwear","Medical care","Transportation and communication","Education","Culture and recreation","Other consumption expenditures","Bar")
  temp_1 <- rep(NA, length(temp_nm)) # Data label.
  temp_2 <- rep(NA, length(temp_nm)) # Largest correlation coefficient with R.
  temp_3 <- rep(NA, length(temp_nm)) # Location of the lag for the largest correlation coefficient.
  for (i in 1:length(temp_nm)){
    if (ENG==1){
      temp_1[i] <- temp_nm_ENG[i]
    }else{
      temp_1[i] <- temp_nm[i]
    }
    temp_2[i] <- mean(rslt_ccf_R_hes[which(hes_var_nm2[,1]==temp_nm[i]),1]) # mean() is applied as some labels appear twice.
    temp_3[i] <- mean(rslt_ccf_R_hes[which(hes_var_nm2[,1]==temp_nm[i]),2]) # mean() is applied as some labels appear twice.
  }
  temp <- cbind(temp_2,temp_3)
  colnames(temp)<-c("Largest correlation coefficient","lag")
  rownames(temp)<-temp_1
  temp_fn <- file(paste0("rslt_ccf_R_hes_model",mdl_number,"_ENG",as.logical(ENG),".txt"), open="w", encoding="UTF-8")
  sink(temp_fn)
  print(xtable(temp,digits=c(0,2,0))) # Print the table in the latex form.
  sink()
  close(temp_fn)

  # Extract and print items that have a higher correlation coefficient in positive lags.
  temp_mincc <- rep(NA, 3) # Initialize the vector to record the value of the correlation coefficient for the variable in mincc_label.
  for (j in 1:3){
    temp_mincc[j] <- rslt_ccf_R_hes[(hes_label==mincc_label[j]),1]
  }
  mincc_R_hes <- min(temp_mincc) # Cut off value for correlation coefficients to extract variables of interest.
  print(paste("ccf: hes to R. Cut-off:",mincc_R_hes))
  print(cbind(hes_var_nm2[(rslt_ccf_R_hes[,1] >= mincc_R_hes),1], rslt_ccf_R_hes[(rslt_ccf_R_hes[,1] >= mincc_R_hes),])) # Print the variables of interest.

  
  
  
  ### Plot each expenditure item against nationwide mobility report on recreation, grocery, and workplace.
  
  # Extract the data for the common sample period.
  temp_hes <- hes_var2_ma[,(31+21-6):ncol(hes_var2_ma)] # hes_var2_ma starts from 2020 January 7. Extract data from 2020 Feb 21 on.

  # Store the results of ccf.
  rslt_ccf_mob_hes <- list()
  mincc_mob_hes <- list()

    
  for (i in c(1,2,4,5)){
    # Record the figures in a pdf file.
    eval(parse(text=paste0("pdf(file=\"plot_mob",i,"_hes_ENG",as.logical(ENG),".pdf\", family=\"Japan1GothicBBB\")")))
    
    # Extract the nationwide reproduction number of each type for the common sample period.
    eval(parse(text=paste0("temp_mob <- mob_var",i,"_ma[1:ncol(temp_hes),1]"))) 

    if (i==1){
      # Check if the dates of the two data coincide.
      print(paste("mob:",c(mob_var_ma_date[1],mob_var_ma_date[ncol(temp_hes)]), "; hes:", c(hes_var2_ma_date[31+21-6],hes_var2_ma_date[ncol(hes_var2_ma)]))) # hes_var2_ma starts from 2020 January 7. mob_vari_ma starts from 2020 February 21.
    }

    # Find the location of the period of the state of emergency.
    date_label <- mob_var_ma_date[1:ncol(temp_hes)] # Define the date label to use.
    SE <- c(which(date_label=="2020-04-07"),which(date_label=="2020-05-25"),which(date_label=="2021-01-07")) # Add 3/21 after the sample period exceeds this date.
    if (length(date_label)>=which(date_label=="2021-01-07")+31-7+28+21){
      SE <- c(SE, which(date_label=="2021-03-21")) # Add the end of the second emergency.
    }
    
    # Plot data.
    rslt_ccf_mob_hes[[i]] <- plot_ts_ccf(temp_mob,mob_var_nm[i],temp_hes,hes_label,date_label,h_val=NULL,SE=SE)
    
    # Close the pdf file.
    dev.off()
    
    # Extract and print items that have a higher correlation coefficient in positive lags.
    temp_mincc <- rep(NA, 3) # Initialize the vector to record the value of the correlation coefficient for the variable in mincc_label.
    for (j in 1:3){
      temp_mincc[j] <- rslt_ccf_mob_hes[[i]][(hes_label==mincc_label[j]),1]
    }
    mincc_mob_hes[[i]] <- min(temp_mincc) # Cut off value for correlation coefficients to extract variables of interest.
    print(paste("ccf: hes to mob", i, ". Cut-off:",mincc_mob_hes[[i]]))
    print(cbind(hes_var_nm2[(rslt_ccf_mob_hes[[i]][,1] >= mincc_mob_hes[[i]]),1], rslt_ccf_mob_hes[[i]][(rslt_ccf_mob_hes[[i]][,1] >= mincc_mob_hes[[i]]),])) # Print the variables of interest.
    
  }
  
  
   
  ### Plot each expenditure item against nationwide weather data.

  # Extract the data for the common sample period.
  temp_hes <- hes_var2_ma # The moving averages of both weather data and household expenditure data start from 2020 Jan. 7 on.
  
  # Store the results of ccf.
  rslt_ccf_W_hes <- list()
  mincc_W_hes <- list()

  # Record the figures in a pdf file.
  eval(parse(text=paste0("pdf(file=\"plot_W_hes_ENG",as.logical(ENG),".pdf\", family=\"Japan1GothicBBB\")")))
  
  for (i in 1:3){
    
    if (i == 1){
      temp_W <- temper_var_ma_ave
      temp_nm <- temper_label

      # Check if the dates of the two data coincide.
      print(paste("W:",c(weath_ma_date[1],weath_ma_date[length(temp_W)]), "; hes:", c(hes_var2_ma_date[1],hes_var2_ma_date[length(temp_W)])))
      
    }else if (i==2){
      temp_W <- hum_var_ma_ave
      temp_nm <- rh_label
    }else{
      temp_W <- abs_hum_var_ma_ave
      temp_nm <- ah_label
    }
    
    # Find the location of the period of the state of emergency.
    date_label <- weath_ma_date[1:ncol(temp_hes)] # Define the date label to use.
    SE <- c(which(date_label=="2020/4/7"),which(date_label=="2020/5/25"),which(date_label=="2021/1/7")) # Add 3/21 after the sample period exceeds this date.
    if (length(date_label)>=which(date_label=="2021/1/7")+31-7+28+21){
      SE <- c(SE, which(date_label=="2021/3/21")) # Add the end of the second emergency.
    }
    
    # Plot data.
    rslt_ccf_W_hes[[i]] <- plot_ts_ccf(temp_W,temp_nm,temp_hes,hes_label,date_label,h_val=mean(temp_W),SE=SE)
    
  }

  # Close the pdf file.
  dev.off()


  
    
  ### Plot nationwide mobility report on recreation, grocery, and workplace against nationwide reproduction number.


  # Record the figures in a pdf file.
  eval(parse(text=paste0("pdf(file=\"plot_R_mob_ENG",as.logical(ENG),".pdf\", family=\"Japan1GothicBBB\")")))

  for (i in 1:6){
    
    # Extract the data for the common sample period. mobility report data is shorter.
    eval(parse(text=paste0("temp_mob <- mob_var",i,"_ma[(29-14+1-6):mob_ma_ndays,1]"))) # mob_vari_ma starts from 2020 February 21. Extract the nationwide reproduction number of each type for the common sample period.
    # Extract reproduction number from 2020 March 1 on. 
    temp_R <- R_var[1:length(temp_mob)] 
  
    if (i ==1){
     # Check if the dates of the two data coincide.
     print(paste("mob:",c(mob_var_ma_date[29-14+1-6],mob_var_ma_date[mob_ma_ndays]), "; R:", c(R_date[1],R_date[length(temp_mob)]))) #mob_vari_ma starts from 2020 January 7.
    }
    
    # Find the location of the period of the state of emergency.
    date_label <- R_date[1:length(temp_mob)] # Define the date label to use.
    SE <- c(which(date_label=="2020/4/7"),which(date_label=="2020/5/25"),which(date_label=="2021/1/7")) # Add 3/21 after the sample period exceeds this date.
    if (length(date_label)>=which(date_label=="2021/1/7")+31-7+28+21){
      SE <- c(SE, which(date_label=="2021/3/21")) # Add the end of the second emergency.
    }
    
    # Plot data.
    rslt_ccf<-plot_ts_ccf(temp_R,R_label,matrix(temp_mob,nr=1),mob_var_nm[i],date_label,h_val=1,SE=SE)
    
  }

  # Close the pdf file.
  dev.off()


  
  ### Plot weather data against nationwide mobility report on recreation, grocery, and workplace.
  
  
  # Record the figures in a pdf file.
  eval(parse(text=paste0("pdf(file=\"plot_W_mob_ENG",as.logical(ENG),".pdf\", family=\"Japan1GothicBBB\")")))
  
  for (k in 1:3){
    
    # Extract the data for the common sample period from 2020 Feb. 18. weather data is shorter.
    if (k == 1){
      temp_W <- temper_var_ma_ave[(31+21-6):length(temper_var_ma_ave)] # The backward moving average of weather data starts from 2020 January 7. The moving average of mobility data starts from February 21, 2020.
      temp_nm <- temper_label
      
    }else if(i==2){
      temp_W <- hum_var_ma_ave[(31+21-6):length(hum_var_ma_ave)] # The backward moving average of weather data starts from 2020 January 7. The moving average of mobility data starts from February 21, 2020.
      temp_nm <- rh_label
    }else{
      temp_W <- abs_hum_var_ma_ave[(31+21-6):length(abs_hum_var_ma_ave)] # The backward moving average of weather data starts from 2020 January 7. The moving average of mobility data starts from February 21, 2020.
      temp_nm <- ah_label
    }
    
    for (i in 1:6){
      
      # Extract the data for the common sample period. Weather data is shorter.
      eval(parse(text=paste0("temp_mob <- mob_var",i,"_ma[1:length(temp_W),1]"))) # Extract the nationwide reproduction number of each type for the common sample period.
      
      if (i ==1){
        # Check if the dates of the two data coincide.
        print(paste("mob:",c(mob_var_ma_date[1],mob_var_ma_date[length(temp_W)]), "; W:", c(weath_ma_date[31+21-6],weath_ma_date[length(temper_var_ma_ave)]))) # The backward moving average of weather data starts from 2020 January 7. The moving average of mobility data starts from February 21, 2020.
      }
      
      # Find the location of the period of the state of emergency.
      date_label <- mob_var_ma_date[1:length(temp_W)] # Define the date label to use.
      SE <- c(which(date_label=="2020/4/7"),which(date_label=="2020/5/25"),which(date_label=="2021/1/7")) # Add 3/21 after the sample period exceeds this date.
      if (length(date_label)>=which(date_label=="2021-01-07")+31-7+28+21){
        SE <- c(SE, which(date_label=="2021-03-21")) # Add the end of the second emergency.
      }
      
      # Plot data.
      rslt_ccf<-plot_ts_ccf(temp_mob,mob_var_nm[i],matrix(temp_W,nr=1),temp_nm,date_label,h_val=0,SE=SE)
      
    }
  }
  
  # Close the pdf file.
  dev.off()
  
  
    
  
  ### Plot weather data against nationwide reproduction number.
  
  # Record the figures in a pdf file.
  eval(parse(text=paste0("pdf(file=\"plot_R_W_ENG",as.logical(ENG),".pdf\", family=\"Japan1GothicBBB\")")))
  
  for (i in 1:3){
    
    if (i == 1){
      temp_W <- temper_var_ma_ave[(31+29+1-6):length(temper_var_ma_ave)] # The backward moving average of weather data starts from 2020 January 7. The effective reproduction number starts from 2020 March 1.
      temp_nm <- temper_label

    }else if(i==2){
      temp_W <- hum_var_ma_ave[(31+29+1-6):length(hum_var_ma_ave)] # The backward moving average of weather data starts from 2020 January 7. The effective reproduction number starts from 2020 March 1.
      temp_nm <- rh_label
    }else {
      temp_W <- abs_hum_var_ma_ave[(31+29+1-6):length(abs_hum_var_ma_ave)] # The backward moving average of weather data starts from 2020 January 7. The effective reproduction number starts from 2020 March 1.
      temp_nm <- ah_label
    }
    
    # Extract reproduction number from 2020 March 1 on. Weather data is shorter.
    temp_R <- R_var[1:length(temp_W)] 

    if (i == 1){
      # Check if the dates of the two data coincide.
      print(paste("W:",c(weath_ma_date[31+29+1-6],weath_ma_date[length(temper_var_ma_ave)]), "; R:", c(R_date[1],R_date[length(temp_W)]))) # The backward moving average of weather data starts from 2020 January 7.
    }
    
    # Find the location of the period of the state of emergency.
    date_label <- R_date[1:length(temp_W)] # Define the date label to use.
    SE <- c(which(date_label=="2020/4/7"),which(date_label=="2020/5/25"),which(date_label=="2021/1/7")) # Add 3/21 after the sample period exceeds this date.
    if (length(date_label)>=which(date_label=="2021/1/7")+31-7+28+21){
      SE <- c(SE, which(date_label=="2021/3/21")) # Add the end of the second emergency.
    }
    
    # Plot data.
    rslt_ccf<-plot_ts_ccf(temp_R,R_label,matrix(temp_W,nr=1),temp_nm,date_label,h_val=1,SE=SE)
    
  }
  
  # Close the pdf file.
  dev.off()

  ### Plot the sample distribution of incubation periods ###
  
  eval(parse(text=paste0("pdf(file=\"plot_dist_incub_ENG",as.logical(ENG),".pdf\", family=\"Japan1GothicBBB\")")))
  if (ENG==1){
    #temp_main_label <- "Incubation periods"
    temp_main_label <- ""
    temp_x_lab <- "Days"  
    temp_y_lab <- "Share of 125 cases"
  }else{
    temp_main_label <- "潜伏期間"
    temp_x_lab <- "日"
    temp_y_lab <- "125例中の割合" 
  }
  barplot(rev(dist_incub),names.arg=1:14,xlab=temp_x_lab,ylab=temp_y_lab,main=temp_main_label,cex.main=cex_val, cex.lab=cex_val, cex.names=cex_val, cex.axis=cex_val)
  
  dev.off()
  
  ### Plot the absolute humidity dummy. ###
  
  eval(parse(text=paste0("pdf(file=\"plot_ind_abs_hum_ave_ENG",as.logical(ENG),".pdf\", family=\"Japan1GothicBBB\")")))
  temp <- 1 - ind_abs_hum_var_ave # Dummy variable for the weighted average of absolute humidity across prefectures.  
  # Find the location of the period of the state of emergency.
  temp_SE <- c(which(weath_date=="2020/4/7"),which(weath_date=="2020/5/25"),which(weath_date=="2021/1/7")) # Add 3/21 after the sample period exceeds this date.
  if (length(weath_date)>=which(weath_date=="2021/1/7")+31-7+28+21){
    temp_SE <- c(temp_SE, which(weath_date=="2021/3/21")) # Add the end of the second emergency.
  }
  if (ENG==1){
    #temp_x_lab <- "Daily (Dotted lines are the first and last dates of each state of emergency)"
    temp_x_lab <- ""
    temp_main <- ""
  }else{
    temp_x_lab <- "日次（破線は緊急事態宣言の開始と解除の日付）"
    temp_main <- expression(D[paste("AH,t")])
  }
  plot(temp, type="l", xaxt="n",main=temp_main,xlab=temp_x_lab,ylab="")
  # Set the labels of the x axis.
  axis(side=1, at=c(1,round(length(temp)*c(1:5)/5)), labels=weath_date[c(1,round(length(temp)*c(1:5)/5))])
  # Draw a line to indicate the declaration of the state of emergency.
  for (j in 1:length(temp_SE)){
    abline(v=temp_SE[j],lty=5)
  }  
  
  dev.off()
  
  
  
}



########## Run a regression model ###################################

# Ensure the sample period stops at 2021 Jan, due to a concern on mutations.
if(hes_end[1] !=2021  || hes_end[2] !=1){
  stop("Set the end of the sample period for estimation to 2021 Jan.")
}

# Drop household expenditure for closes and shoes from explanatory variables for model 10.
if (mdl_number == 10){

  H_expvals <- H_expvals[1:5,] # Drop household expenditures for closes and shoes, which is on the 6th row.
      
}

if (mdl_number <= 12){ 
  # Construct the data set for the stan file.
  dat <- list(R_TT = length(R_var), # From 2020 March 1.
              R = R, # Unit: ratio.
              H_TT = ncol(hes_var2), # From 2020 Jan. 1.
              H_expvals = H_expvals,
              M_TT = mob_ndays, # From 2020 Feb. 15.
              M_trans = mob_var4[,1], # Unit: Percentage points.
              W_TT = length(temper_var_ave), # From 2020 Jan. 1.
              W_abs_hum = W_abs_hum, # Unit: g/m^3 or log of g/m^3 or dummy.
              TT_diff_M_H = 31+14, # Number of days between the first sample dates of M_series and H_series. 
              TT_diff_M_W = 31+14, # Number of days between the first sample dates of M_series and W_series. 
              dist_incub =dist_incub,
              D_NY = D_NY, # From 2020 Feb. 15, the same as M_series.
              D_SE1 = D_SE1, # Dummy variable for the declaration of a state of emergency.
              D_SE2 = D_SE2 # Dummy variable for the declaration of a state of emergency.
  )
}else{
  dat <- list(R_TT = length(R_var), # From 2020 March 1.
              R = R, # Unit: ratio.
              H_TT = ncol(hes_var2), # From 2020 Jan. 1.
              H_expvals = H_expvals,
              M_TT = mob_ndays, # From 2020 Feb. 15.
              M_trans = mob_var4[,1], # Unit: Percentage points.
              W_TT = length(temper_var_ave), # From 2020 Jan. 1.
              W_abs_hum = W_abs_hum, # Unit: g/m^3 or log of g/m^3 or dummy.
              TT_diff_M_H = 31+14, # Number of days between the first sample dates of M_series and H_series. 
              TT_diff_M_W = 31+14, # Number of days between the first sample dates of M_series and W_series. 
              dist_incub =dist_incub,
              D_NY = D_NY, # From 2020 Feb. 15, the same as M_series.
              D_SE1 = D_SE1, # Dummy variable for the declaration of the first state of emergency.
              D_SE2 = D_SE2, # Dummy variable for the declaration of the second state of emergency.
              D_pre_SE1 = D_pre_SE1 # Dummy variable for the period before the first state of emergency.
  )
}
  
# Activate parallel computing. See Matuura (2016), page 46.
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores()) 

# Suffix to file names
if (log_abs_hum == 1){
  W_sfx <- "log_abs_hum" # Use the log of absolute humidity.
}else if(log_abs_hum == 2){
  W_sfx <- "lv_abs_hum" # Use the level of absolute humidity.
}else{
  W_sfx <- "ind_abs_hum" # Use a dummy that absolute humidity exceeds 7 g/m^3.
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
mdl_nm <- eval(parse(text=paste0("\"",thm,mdl_number,".stan\""))) # Name of the model to estimate.
file_summary_nm <- eval(parse(text=paste0("\"",thm,mdl_number,"_fit-summary_",incub_sfx,"_",R_sfx,"_",W_sfx,"_",nominal_sfx,"_",weath_sfx,".csv\""))) # Name of the file for the summary of estimates.
if (ENG == 1){
  file_results_nm <- eval(parse(text=paste0("\"",thm,mdl_number,"_results_",incub_sfx,"_",R_sfx,"_",W_sfx,"_",nominal_sfx,"_",weath_sfx,"_ENG.pdf\""))) # Name of the file for estimation results.
}else{
  file_results_nm <- eval(parse(text=paste0("\"",thm,mdl_number,"_results_",incub_sfx,"_",R_sfx,"_",W_sfx,"_",nominal_sfx,"_",weath_sfx,"_JPN.pdf\""))) # Name of the file for estimation results.
}
file_mcmc_nm <- eval(parse(text=paste0("\"",thm,mdl_number,"_mcmc_",incub_sfx,"_",R_sfx,"_",W_sfx,"_",nominal_sfx,"_",weath_sfx,".data\""))) # Name of the file for the mcmc samples.

# Run a stan model.
if (estim ==1){
  
  # Define a stan model.
  stanmodel <- stan_model(file=mdl_nm)
  
  fit <- sampling(
    stanmodel,
    data=dat,
    seed=18,
    #control = list(max_treedepth = 20)
    #chains=4, iter=20000, warmup=10000, thin=5,
    control = list(adapt_delta=0.99, max_treedepth = 20)
    #control = list(adapt_delta=0.9, stepsize=1e-05, max_treedepth = 20)
  )
  
  # Save the mcmc sample.
  save(fit, file=file_mcmc_nm)
  
  # Record the parameter estimates
  write.csv(data.frame(summary(fit)$summary),file=file_summary_nm)
  
  
}else{
  
  # Load a saved mcmc sample.
  load(file=file_mcmc_nm)
  
}

# Write a table of the parameter estimates in a tex file.
sink(paste0("para_estim_model",mdl_number,"_",incub_sfx,"_",R_sfx,"_",W_sfx,"_",nominal_sfx,".txt"))
# Define the variable names in the model, which are the same order as in the stan file except six R_err_0 before parameters contained in conv_para_vn2.
conv_para_vn1 <- c("\\sigma_{\\eta}","\\alpha_0","\\alpha_1","\\alpha_2","\\beta_1","\\beta_2","\\beta_0","\\gamma_1","\\gamma_2","\\gamma_3","\\gamma_4","\\gamma_5","\\gamma_6","\\gamma_7","\\delta_1","\\delta_2","\\delta_3","\\delta_4","\\delta_5","\\delta_6","\\delta_7","\\phi_11","\\phi_12","\\phi_13","\\phi_14","\\phi_15","\\phi_16","\\phi_17","\\phi_21","\\phi_22","\\phi_23","\\phi_24","\\phi_25","\\phi_26","\\phi_27","\\phi_01","\\phi_02","\\phi_03","\\phi_04","\\phi_05","\\phi_06","\\phi_07")
conv_para_vn2 <- c("\\rho","\\sigma_{\\epsilon}")
# Extract the posterior mean, 2.5% point, and 97.5% point of time-invariant parameters.
temp <- data.frame(summary(fit)$summary)[c(1:length(conv_para_vn1),length(conv_para_vn1)+6+1:length(conv_para_vn2)),c(1,4,8)]
# Replace the data labels in the stan file into those in the paper.
rownames(temp) <- c(conv_para_vn1,conv_para_vn2) 
# Print the table in the latex form.
print(xtable(temp,digits=c(0,rep(3,3))))
sink()

# Extract mcmc samples.
ms <- rstan::extract(fit)

# Drop burnins.
niter <- length(ms[[1]]) # Number of mcmc samples. The first element of mc (gama) is a vector.
for (i in 1:(length(ms)-1)){ # The last element of ms is the log likelihood time (-1). 
  if (length(dim(ms[[i]]))==1){
    ms[[i]] <-ms[[i]][(round(niter/2)+1):niter] # Drop the burn-ins.
  } else if(length(dim(ms[[i]]))==2){
    ms[[i]] <-ms[[i]][(round(niter/2)+1):niter,] # Drop the burn-ins.
  }else{
    ms[[i]] <-ms[[i]][(round(niter/2)+1):niter,,] # Drop the burn-ins.
  }
}

# Compute the mean and the percentile values of time-varying parameters in vectors.

name_list<-c("R_obs_err","R_err","Fitted_R") # Variable name in the stan file.

for (i in 1:length(name_list)){
  
  eval(parse(text=paste0("temp <- Extract_mean_ptl(ms$",name_list[i],", ptl=c(0.025,0.975))")))
  eval(parse(text=paste0(name_list[i],"_pm <- temp[[1]]"))) # posterior mean
  eval(parse(text=paste0(name_list[i],"_025 <- temp[[2]]"))) # 2.5% percentile
  eval(parse(text=paste0(name_list[i],"_975 <- temp[[3]]"))) # 97.5% percentile
}

# Compute the mean and the percentile values of shocks to unobserved infectious events.

if (mdl_number>=16){
  XF_hat_err <- ms$XF_err * 0 # Initialize shocks to unobserved infectious events.
  for (i in 1:length(ms$sd_XF)){
    # Multiply simulated white noises following the standard normal distribution by the standard deviation of shocks to unobserved infectious events. 
    XF_hat_err[i,] <- ms$sd_XF[i] * ms$XF_err[i,]   
  } 
  temp <- Extract_mean_ptl(XF_hat_err, ptl=c(0.025,0.975)) # Compute percentiles of the shocks to unobserved infectious events in the mcmc samples.
  XF_hat_err_pm <- temp[[1]] # posterior mean
  XF_hat_err_025 <- temp[[2]] # 2.5% percentile
  XF_hat_err_975 <- temp[[3]] # 97.5% percentile
}  

# Compute the mean and the percentile values of time-varying parameters in matrices.
# rows: variables
# columns: time.

if (ENG == 1){
  
  # Create English labels.
  if (mdl_number== 10){
    exp_var_name <- c("New year dummy", "Abs. hum. dummy", "Eating out for meals", "Cafe", "Bar", "Lodging", "Domestic travel packages", "Mobility in transportation") # Names of variables for graphs.
  }else{
    exp_var_name <- c("New year dummy", "Abs. hum. dummy", "Eating out for meals", "Cafe", "Bar", "Lodging", "Domestic travel packages", "Clothing and footwear", "Mobility in transportation") # Names of variables for graphs.
  }
  
  if (sum(mdl_number == c(3,5,7))>0 || mdl_number >= 9){
    exp_var_name <- c(exp_var_name, "1st state of emergency", "2nd state of emergency") # Include the label of additional dummy variables.
  }
  
  if (mdl_number >= 13){
    exp_var_name <- c(exp_var_name, "Before 1st state of emergency") # Include the label of additional dummy variables.
  }

  #time_label <- "Daily (Dotted lines are the first and last dates of each state of emergency)"
  time_label <- ""
  
}else{
  
  # Create Japanese labels.
  if (mdl_number== 10){
    exp_var_name <- c("年末年始", "絶対湿度", "食事代", "喫茶代", "飲酒代", "宿泊料", "国内パック旅行費", "人出（交通）") # Names of variables for graphs.
  }else{
    exp_var_name <- c("年末年始", "絶対湿度", "食事代", "喫茶代", "飲酒代", "宿泊料", "国内パック旅行費", "被服及び履物", "人出（交通）") # Names of variables for graphs.
  }
  
  if (sum(mdl_number == c(3,5,7))>0 || mdl_number >= 9){
    exp_var_name <- c(exp_var_name, "緊急事態宣言1", "緊急事態宣言2") # Include the label of additional dummy variables.
  }
  
  if (mdl_number >= 13){
    exp_var_name <- c(exp_var_name, "緊急事態宣言1以前") # Include the label of additional dummy variables.
  }
  
  time_label <- "日次（破線は緊急事態宣言の開始と解除の日付）"
  
}

# Set the number of dummy variables for the two states of emergency and the period before the first state of emergency, if any.
if (mdl_number >= 13){
  n_SE_dummy <- 3 
}else{
  n_SE_dummy <- 2 
}


# A matrix that contains the contribution of explanatory variables to changes in the dependent variable.
name_list<-c("ef_d_X") # Variable name in the stan file.

# Set the total number of variables. 2+n_SE_dummy = the total number of dummy variables.
if (sum(mdl_number== c(1,2,4,6,8))>0){
  temp_ind <- (2+(length(exp_var_name)-2)*2) # No dummy for declarations of two states of emergency.
}else{
  temp_ind <- (2+n_SE_dummy+(length(exp_var_name)-2-n_SE_dummy)*(2+n_SE_dummy)) # With dummy for declarations of two states of emergency.
}

for (i in 1:length(name_list)){

  for (k in 1:temp_ind){
    eval(parse(text=paste0("temp <- Extract_mean_ptl(ms$",name_list[i],"[,",k,",], ptl=c(0.025,0.975))")))
    eval(parse(text=paste0(name_list[i],"_",k,"_pm <- temp[[1]]"))) # posterior mean
    eval(parse(text=paste0(name_list[i],"_",k,"_025 <- temp[[2]]"))) # 5% percentile
    eval(parse(text=paste0(name_list[i],"_",k,"_975 <- temp[[3]]"))) # 95% percentile
  }
}

# Plot the estimation results.

pdf(file=file_results_nm, family="Japan1GothicBBB")


# Define the labels for dates starting from 2020 March 6, exclusive, to the end of the estimation period. mob_date starts from February 15, 2020.
date_label0 <- mob_date[29-14+6 + 0:(length(hes_var_date)+1-(31+29+6))]

# Define the labels for dates starting from one day after 2020 March 6 to the end of the estimation period. mob_date starts from February 15, 2020.
date_label1 <- mob_date[29-14+6+1 + 0:(length(hes_var_date)+1-(31+29+6))]

# Define the labels for dates starting from 13+6 days before 2020 March 6, i.e., 2020 February 16, to the end of the estimation period (2021/2/1). mob_date starts from February 15, 2020.
date_label2 <- mob_date[(16-14):which(mob_date=="2021-02-01")]

# Define the labels for dates starting from 6 days before 2020 March 6, i.e., 2020 February 29, to the end of the estimation period  (2021/2/1). mob_date starts from February 15, 2020.
date_label3 <- mob_date[(29-14):which(mob_date=="2021-02-01")]

# Find the location of the period of the state of emergency.
for (i in 0:3){
  if (length(mob_date)>=366 - (31+14) + 31 + 28 + 21){ # mob_date starts from 2020 Feb. 15. 2020 has 366 days.
    eval(parse(text=paste0("SE",i,"<- c(which(date_label",i,"==\"2020-04-07\"),which(date_label",i,"==\"2020-05-25\"),which(date_label",i,"==\"2021-01-07\"),which(date_label",i,"==\"2021-03-21\"))"))) 
  }else{
    eval(parse(text=paste0("SE",i,"<- c(which(date_label",i,"==\"2020-04-07\"),which(date_label",i,"==\"2020-05-25\"),which(date_label",i,"==\"2021-01-07\"))"))) # The sample period ends before 3/21.
  }
}

# Fix the range of the y axis for the components of changes in the reproduction number.
y_max <- NULL; # The maximum value of the y axis.
y_min <- NULL; # The minimum value of the y axis.
for (k in 1:temp_ind){
  # Components
  eval(parse(text=paste0("y_min <- min(y_min, ef_d_X_",k,"_pm)"))) # 5% percentile 
  eval(parse(text=paste0("y_max <- max(y_max, ef_d_X_",k,"_pm)"))) # 95% percentile 
}
y_min <- min(y_min, diff(Fitted_R_pm)) # Fitted values
y_max <- max(y_max, diff(Fitted_R_pm)) # Fitted values

if(sum(mdl_number==c(7,10,11,13,14,15,16))>0){
  y_min <- -0.2 #For models 7 and 9, shrink the range of y axis manually.
  y_max <- 0.2 #For models 7 and 9, shrink the range of y axis manually.
}

# Define the dependent variable.
DEP <- R[5+1:length(date_label0)] # R_var starts from 2020 March 1. The fitted value of R is available from 2020 March 6.

# Set the y-axis label for the plot of the reproducion number.
if (log_R == 1){
  R_label <- "Log" # R is log.
}else{
  R_label <- "Level" # R is level.
}

# Fitted values.
temp_nl <- c("Fitted_R")
for (i in 1:(length(temp_nl))){
  eval(parse(text=paste0("temp_nl_with_CI <- c(\"",temp_nl[i],"_pm, ", temp_nl[i],"_025, ",temp_nl[i],"_975\")"))) # Define character names for the posterior mean (_pm).
  eval(parse(text=paste0("temp <- cbind(",temp_nl_with_CI,")"))) # Create a data table for a plot of observed data and estimated parameters.
  if (ENG==1){
    temp_main_label <- "Effective reproduction number"
    #temp_main_label <- ""
  }else{
    temp_main_label <- "実効再生産数"
  }
  matplot(cbind(DEP, temp), type="l", col=c(1,2,2,2), lty=c(1,1,2,2), xaxt="n", xlab=time_label, ylab=R_label, main=temp_main_label,cex.main=cex_val,cex.axis=cex_val_axis,cex.lab=cex_val)
  axis(side=1, at=c(1,round(nrow(temp)*c(1:5)/5)), labels=conv_date_format(date_label0[c(1,round(nrow(temp)*c(1:5)/5))]),cex.axis=cex_val_axis)
  abline(h=0*log_R+1*(1-log_R),lty=3) # h=1 if R is the level of the reproduction number and h=0 if R is the log.
  for (j in 1:length(SE0)){
    abline(v=SE0[j],lty=5)
  }  
  if (ENG==1){
    #legend("topright",legend=c("Observed R", "Fitted R", "95% CI"), lty=c(1,1,2), col=c(1,3,2))
    legend("topright",legend=c("Observed R", "Fitted R"), lty=c(1,1), col=c(1,2),cex=cex_val)
  }else{
    legend("topright",legend=c("観測値", "平均値の事後平均", "95%予測区間"), lty=c(1,1,2), col=c(1,3,2))
  }
}

# Observed error terms.

if (mdl_number>=16){
  temp_nl <- c("R_err","XF_hat_err")
  temp_var_name_ENG <- c("Measurement error","Shocks to unobserved infectious events")
  temp_var_name_JPN <- c("測定誤差","観察されない感染性活動のショック")
}else{
  temp_nl <- c("R_err")
  temp_var_name_ENG <- c("Measurement error")
  temp_var_name_JPN <- c("測定誤差")
}
for (i in 1:(length(temp_nl))){
  eval(parse(text=paste0("temp_nl_with_CI <- c(\"",temp_nl[i],"_pm, ", temp_nl[i],"_025, ",temp_nl[i],"_975\")"))) # Define character names for the posterior mean (_pm).
  eval(parse(text=paste0("temp <- cbind(",temp_nl_with_CI,")"))) # Create a data table for a plot of observed data and estimated parameters.
  if (ENG==1){
    temp_main_label <- temp_var_name_ENG[i]
  }else{
    temp_main_label <- temp_var_name_JPN[i]
  }
  matplot(temp, type="l", col=c(1,2,2), lty=c(1,2,2), xaxt="n", xlab=time_label, ylab=R_label, main=temp_main_label,cex.main=cex_val,cex.axis=cex_val_axis,cex.lab=cex_val)
  if(temp_nl[i] == "R_err"){
    temp_date_label <- date_label3 # From February 29, 2020.
    temp_SE <- SE3 
  }else{
    #temp_nl[i] == "XF_hat_err"
    temp_date_label <- date_label2 # From February 16, 2020.
    temp_SE <- SE2
  }
  axis(side=1, at=c(1,round(nrow(temp)*c(1:5)/5)), labels=conv_date_format(temp_date_label[c(1,round(nrow(temp)*c(1:5)/5))]), cex.axis=cex_val_axis)
  abline(h=0,lty=3)
  for (j in 1:length(temp_SE)){
    abline(v=temp_SE[j],lty=5)
  }  
  if (ENG==1){
    legend("topright",legend=c("Posterior mean", "95% credible interval"), lty=c(1,2), col=1:2, cex=cex_val)
  }else{
    legend("topright",legend=c("事後平均", "95%予測区間"), lty=c(1,2), col=1:2)
  }
  
  # ACF of the posterior means of error terms.
  eval(parse(text=paste0("acf(",temp_nl[i],"_pm)")))
  
  # ACF of mcmc samples of error terms.
  if (temp_nl[i] == "XF_hat_err"){
    temp <- acf_mcmc(XF_hat_err)
  }else{
    eval(parse(text=paste0("temp <- acf_mcmc(ms$",temp_nl[i],")")))
  }
  if (ENG==1){
    #eval(parse(text=paste0("temp_main_label <- \"Box plot of auto correlations of ",temp_var_name_ENG[i],"\"")))
    temp_main_label <- temp_var_name_ENG[i]
  }else{
    eval(parse(text=paste0("temp_main_label <- \"",temp_var_name_JPN[i],"の自己相関関数の箱ひげ図\"")))
  }
  boxplot(temp$acfs, names=as.character(temp$lags), xlab="Lag", main=temp_main_label, staplewex=0.95, cex.main=cex_val, cex.lab=cex_val, cex.names=cex_val, cex.axis=cex_val)
  eval(parse(text=paste0("temp_nobs <- length(",temp_nl[i],"_pm)")))
  abline(h = 2 / sqrt(temp_nobs), lty=2, col=2) # 2.S.E. of correlation coefficients between independent white noises.
  abline(h = -2 / sqrt(temp_nobs), lty=2, col=2) # 2.S.E. of correlation coefficients between independent white noises.
}



# Contributions from each series except cross terms.

temp_nl_with_CI0 <- paste0("diff(Fitted_R","_pm)") # Define character names for the posterior mean (_pm).

# Set the number of household expenditure and mobility data that have cross terms.
if (sum(mdl_number==c(1,2,4,6,8))>0){
  temp_ind <- length(exp_var_name) 
}else{
  temp_ind <- length(exp_var_name)-n_SE_dummy # Exclude dummy variables related to states of emergency, as they are placed after cross terms in the order of explanatory variables.
}

# Initialize the container for an area plot.
ef_d_X_areaplot <- matrix(NA,nr=length(diff(Fitted_R_pm)),nc=length(exp_var_name)) 


for (k in 1:temp_ind){
  eval(parse(text=paste0("temp_nl_with_CI1 <- paste0(\"ef_d_X_\", \"",k,"_pm\")"))) # Define character names for the posterior mean (_pm).
  eval(parse(text=paste0("temp <- cbind(",paste0(c(temp_nl_with_CI0, temp_nl_with_CI1),collapse=","),")"))) # Create a data table for a plot.
  #eval(parse(text=paste0("matplot(temp, type=\"l\", col=c(1,2), lty=1, xaxt=\"n\", xlab=time_label, ylab=\"\", ylim=c(y_min,y_max), main=\"",exp_var_name[k],"\")")))
  matplot(temp, type="l", col=c(1,2), lty=1, xaxt="n", xlab=time_label, ylab="", ylim=c(y_min,y_max), main=exp_var_name[k])
  axis(side=1, at=c(1,round(nrow(temp)*c(1:5)/5)), labels=date_label1[c(1,round(nrow(temp)*c(1:5)/5))])
  abline(h=0,lty=3)
  for (j in 1:length(SE1)){
    abline(v=SE1[j],lty=5)
  }  
  if (ENG==1){
    legend("topright",legend=c("Change in fitted R","Change in the component of R in the title"), lty=1, col=c(1,2))
  }else{
    legend("topright",legend=c("実効再生産数平均値の差分","タイトル項の効果の予測値の差分"), lty=1, col=c(1,2))
  }

  # Record the contribution of first two dummies for an area plot.
  if (k<3){
    eval(parse(text=paste0("ef_d_X_areaplot[,k] <- ",paste0(temp_nl_with_CI1,collapse=","))))
  }
    
}

# Contributions from cross terms with absolute humidity.

for (k in 3:temp_ind){
  eval(parse(text=paste0("temp_nl_with_CI1 <- paste0(\"ef_d_X_\", \"",temp_ind+k-2,"_pm\")"))) # Define character names for the posterior mean (_pm).
  eval(parse(text=paste0("temp <- cbind(",paste0(c(temp_nl_with_CI0, temp_nl_with_CI1),collapse=","),")"))) # Create a data table for a plot.
  if (ENG==1){
    temp_main_label <- paste0(exp_var_name[k],"*abs. hum. dummy")
  }else{
    temp_main_label <- paste0(exp_var_name[k],"*絶対湿度ダミー")
  }
  #eval(parse(text=paste0("matplot(temp, type=\"l\", col=c(1,2), lty=1, xaxt=\"n\", xlab=\"日次（破線は緊急事態宣言の開始と解除の日付）\", ylab=\"\", ylim=c(y_min,y_max), main=\"(",exp_var_name[k],"*絶対湿度)\")")))
  matplot(temp, type="l", col=c(1,2), lty=1, xaxt="n", xlab=time_label, ylab="", ylim=c(y_min,y_max), main=temp_main_label)
  axis(side=1, at=c(1,round(nrow(temp)*c(1:5)/5)), labels=date_label1[c(1,round(nrow(temp)*c(1:5)/5))])
  abline(h=0,lty=3)
  for (j in 1:length(SE1)){
    abline(v=SE1[j],lty=5)
  }  
  if (ENG==1){
    legend("topright",legend=c("Change in fitted R","Change in the component of R in the title"), lty=1, col=c(1,2))
  }else{
    legend("topright",legend=c("実効再生産数平均値の差分","タイトル項の効果の予測値の差分"), lty=1, col=c(1,2))
  } 
}

# total contributions from household expenditure items and mobility in transportation.

for (k in 3:temp_ind){
  eval(parse(text=paste0("temp_nl_with_CI1 <- paste0(\"ef_d_X_\", \"",temp_ind+k-2,"_pm\")"))) # Define character names for the posterior mean (_pm).
  eval(parse(text=paste0("temp_nl_with_CI1 <- paste0(paste0(\"ef_d_X_\", \"",k,"_pm + \"), temp_nl_with_CI1)"))) # Add each variable's level effect and cross effect.
  eval(parse(text=paste0("temp <- cbind(",paste0(c(temp_nl_with_CI0, temp_nl_with_CI1),collapse=","),")"))) # Create a data table for a plot.
  if (ENG==1){
    temp_main_label <- paste0(exp_var_name[k],"*(1+abs. hum. dummy)")
  }else{
    temp_main_label <- paste0(exp_var_name[k],"*(1+絶対湿度ダミー)")
  }
  #eval(parse(text=paste0("matplot(temp, type=\"l\", col=c(1,2), lty=1, xaxt=\"n\", xlab=\"日次（破線は緊急事態宣言の開始と解除の日付）\", ylab=\"\", ylim=c(y_min,y_max), main=\"",exp_var_name[k],"*(1+絶対湿度)の差分\")")))
  matplot(temp, type="l", col=c(1,2), lty=1, xaxt="n", xlab=time_label, ylab="", ylim=c(y_min,y_max), main=temp_main_label)
  axis(side=1, at=c(1,round(nrow(temp)*c(1:5)/5)), labels=date_label1[c(1,round(nrow(temp)*c(1:5)/5))])
  abline(h=0,lty=3)
  for (j in 1:length(SE1)){
    abline(v=SE1[j],lty=5)
  }  
  if (ENG==1){
    legend("topright",legend=c("Change in fitted R","Change in the component of R in the title"), lty=1, col=c(1,2))
  }else{
    legend("topright",legend=c("実効再生産数平均値の差分","タイトル項の効果の予測値の差分"), lty=1, col=c(1,2))
  } 
  

  # Record the contribution of first two dummies for an area plot.
  eval(parse(text=paste0("ef_d_X_areaplot[,k] <- ",paste0(temp_nl_with_CI1,collapse=","))))
  
}



if (sum(mdl_number == c(3,5,7))>0 || mdl_number >= 9){
  
  # The level effects of additional dummy variables related to the states of emergency.
  for (k in 1:n_SE_dummy){
    eval(parse(text=paste0("temp_nl_with_CI1 <- paste0(\"ef_d_X_\", \"",temp_ind*2-2+k,"_pm\")"))) # Define character names for the posterior mean (_pm).
    eval(parse(text=paste0("temp <- cbind(",paste0(c(temp_nl_with_CI0, temp_nl_with_CI1),collapse=","),")"))) # Create a data table for a plot.
    #eval(parse(text=paste0("matplot(temp, type=\"l\", col=c(1,2), lty=1, xaxt=\"n\", xlab=\"日次（破線は緊急事態宣言の開始と解除の日付）\", ylab=\"\", ylim=c(y_min,y_max), main=\"",exp_var_name[temp_ind+k],"の差分\")")))
    matplot(temp, type="l", col=c(1,2), lty=1, xaxt="n", xlab=time_label, ylab="", ylim=c(y_min,y_max), main=exp_var_name[temp_ind+k])
    axis(side=1, at=c(1,round(nrow(temp)*c(1:5)/5)), labels=date_label1[c(1,round(nrow(temp)*c(1:5)/5))])
    abline(h=0,lty=3)
    for (j in 1:length(SE1)){
      abline(v=SE1[j],lty=5)
    }  
    if (ENG==1){
      legend("topright",legend=c("Change in fitted R","Change in the component of R in the title"), lty=1, col=c(1,2))
    }else{
      legend("topright",legend=c("実効再生産数平均値の差分","タイトル項の効果の予測値の差分"), lty=1, col=c(1,2))
    } 
    

    # Record the contribution of first two dummies for an area plot.
    eval(parse(text=paste0("ef_d_X_areaplot[,temp_ind+k] <- ",paste0(temp_nl_with_CI1,collapse=","))))
  
  }

  
  # cross terms. household expenditure and mobility data are from the 3rd to the (temp_ind)-th variables in the list of explanatory variables. There are dummies for the two states of emergency following.
  
  for (k in 3:temp_ind){
    # Add each household and mobility variable's cross effect with each dummy variable related to states of emergency. Define character names for the posterior mean (_pm).
    eval(parse(text=paste0("temp_nl_with_CI1 <- paste0(\"ef_d_X_\", \"",temp_ind*(2+n_SE_dummy-1)-2-2*(n_SE_dummy-1) + n_SE_dummy +k-2,"_pm\")"))) 
    for (d in (1:n_SE_dummy-1)){
      eval(parse(text=paste0("temp_nl_with_CI1 <- paste0(paste0(\"ef_d_X_\", \"",temp_ind*(2+d-1)-2-2*(d-1) + n_SE_dummy +k-2,"_pm + \"), temp_nl_with_CI1)"))) 
    }  
    # Add each household and mobility variable's cross effect with absolute humidity. Define character names for the posterior mean (_pm).
    eval(parse(text=paste0("temp_nl_with_CI1 <- paste0(paste0(\"ef_d_X_\", \"",temp_ind+k-2,"_pm + \"), temp_nl_with_CI1)")))
    # Add each household and mobility variable's level effect.
    eval(parse(text=paste0("temp_nl_with_CI1 <- paste0(paste0(\"ef_d_X_\", \"",k,"_pm + \"), temp_nl_with_CI1)")))
    # Create a data table for a plot.
    eval(parse(text=paste0("temp <- cbind(",paste0(c(temp_nl_with_CI0, temp_nl_with_CI1),collapse=","),")"))) 
    # Draw the plot.
    if (ENG==1){
      temp_main_label <- paste0("Total effect from ",exp_var_name[k])
    }else{
      temp_main_label <- paste0(exp_var_name[k],"の全効果")
    }
    #eval(parse(text=paste0("matplot(temp, type=\"l\", col=c(1,2), lty=1, xaxt=\"n\", xlab=\"日次（破線は緊急事態宣言の開始と解除の日付）\", ylab=\"\", ylim=c(y_min,y_max), main=\"",exp_var_name[k],"の全効果の差分\")")))
    matplot(temp, type="l", col=c(1,2), lty=1, xaxt="n", xlab=time_label, ylab="", ylim=c(y_min,y_max), main=temp_main_label)
    axis(side=1, at=c(1,round(nrow(temp)*c(1:5)/5)), labels=date_label1[c(1,round(nrow(temp)*c(1:5)/5))])
    abline(h=0,lty=3)
    for (j in 1:length(SE1)){
      abline(v=SE1[j],lty=5)
    }  
    if (ENG==1){
      legend("topright",legend=c("Change in fitted R","Change in the component of R in the title"), lty=1, col=c(1,2))
    }else{
      legend("topright",legend=c("実効再生産数平均値の差分","タイトル項の効果の予測値の差分"), lty=1, col=c(1,2))
    } 
    
    # Record the contribution of first two dummies for an area plot.
    eval(parse(text=paste0("ef_d_X_areaplot[,k] <- ",paste0(temp_nl_with_CI1,collapse=","))))
    
  }

}


# Draw each variable's contribution to changes in the reproduction number in an area plot.
temp <- abs(ef_d_X_areaplot) # Normalize the absolute value of each contribution by the total change in the reproduction number.
if (ENG==1){
  temp_main_label <- "Absolute value of the change in each component of R"
}else{
  temp_main_label <- "各要素の差分の絶対値の比較"
}
area_plot(temp, var_name=exp_var_name, xlab=time_label, xaxt="n", x_axis_label=date_label1, n_label=5, main=temp_main_label)

# Close the pdf file.
dev.off()



