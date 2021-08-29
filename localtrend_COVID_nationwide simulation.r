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
# Model 17: Modify the distribution of the initial value of residual infectious events to the unconditional distribution.
# Model 18: Set real <lower=0, upper=1> rho; // AR(1) term for residual infectious events.
# Model 19: Same as Model 17. This copy is used when the estimation uses all the available data for explanatory variables, rather than using data only up to Jan. 2021.
# Model 20: Add explanatory variables on L452R and vaccination to Model 17.
# Model 21: Add linear explanatory variables on L452R and vaccination to Model 17.

# The saved data and estimates of either model are loaded. 

# Data to update for factor decomposition: household expenditures, weather, CPI.

########## Clear global workspace and plots. ################

if( dev.cur() > 1 ) dev.off() # Clear plots.
rm( list = ls( envir = globalenv() ), envir = globalenv() ) # Clear global workspace.

########## Set parameters ###################################

flag_pref_wgt <- 1 # Default value: 1. If = 1, use population share among prefectures to compute average absolute humidity across prefectures. If = 0, use new cases in the past 7 days.

log_abs_hum <- 0 # Default value: 0. If = 1, use log of absolute humidity. If = 2, use the level of absolute humidity. Otherwise, use a dummy that absolute humidity exceeds 7. Unit: g/m^3.

log_R <- 1 # Default value: 0. If = 1, use log of the reproduction number. otherwise, use the level.

log_dist_incub <- 0 #  Default value: 0. If = 1, use a log normal distribution based on Chinese data reported by Stephen A et al. (2020). =0, use the empirical distribution of incubation periods based on Sugishita (2020).

nominal_hes <- 0 # Default value: 0. If = 1, use nominal household expenditure data. If = 0, use real household expenditure data denominated by 2020 average CPI for each item.

mdl_number <- 17 # Numbering of the stan model whose result is loaded.

ENG <- 1 # If = 1, write labels in English.

plot_paper <- 1 # If =1 and ENG=1, the main titles of some plots are set for the paper, rather than slides.

onlyplot_simu0 <- 1 # If = 1, only plot the first set of simulations in simu0 (i.e., no simulation of interventions in household consumption and mobility.)

if (mdl_number >=20){
  # For model 20 or above, there is no simulation of retrictions on household expenditures. 
  onlyplot_simu0 <- 1
  }

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

if (mdl_number <20){
  
  # Ensure the sample period stops at 2021 Jan, due to a concern on mutations.
  if(hes_end[1] !=2021  || hes_end[2] !=1){
    stop("Set the end of the sample period for estimation to 2021 Jan.")
  }
  # Drop household expenditure for closes and shoes from explanatory variables for model 10.
  if (mdl_number == 10){
    
    H_expvals <- H_expvals[1:5,] # Drop household expenditures for closes and shoes, which is on the 6th row.
    
  }
  
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

# Extract mcmc samples.
ms <- rstan::extract(fit)

# Drop burnins.
niter <- length(ms[[1]]) # Number of mcmc samples. The first element of ms (gama) is a vector.
for (i in 1:(length(ms)-1)){ # The last element of ms is the log likelihood times (-1). 
  if (length(dim(ms[[i]]))==1){
    ms[[i]] <-ms[[i]][(round(niter/2)+1):niter] # Drop the burn-ins.
  }else if(length(dim(ms[[i]]))==2){
    ms[[i]] <-ms[[i]][(round(niter/2)+1):niter,] # Drop the burn-ins.
  }else{
    ms[[i]] <-ms[[i]][(round(niter/2)+1):niter,,] # Drop the burn-ins.
  }
}


############## Plot data and simulation results using 2019 data. ##############################

# Define a common label for x axis when the axis shows dates.
if (ENG==1){
  if (plot_paper==1){
    time_xlab <- ""
  }else{
    time_xlab <- "Daily (Dotted lines are the first and last dates of states of emergency)"
  }
}else{
  time_xlab <- "日次（破線は緊急事態宣言の開始と解除の日付）"
}

# Save plots of data in a pdf file.
eval(parse(text=paste0("pdf(paste0(\"simu0_reformed_data_model",mdl_number,"_ENG",as.logical(ENG),"_",R_sfx,".pdf\"), family=\"Japan1GothicBBB\")")))

# The following figures are plotted only for models without mutant strains and vaccinations.

if (mdl_number < 20){
  
  # Plot railways passengers and google mobility data for Feb. 15, 2020, to the year end.
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
  temp <- cbind(Rail_Pssgrs_d[(365+31+14+1):length(Rail_Pssgrs_d)]/Rail_Pssgrs_m[13],filter(1+mob_var4[1:(length(Rail_Pssgrs_d)-(365+31+14)),1]/100,rep(1/7,7))) # Take 7-day moving average of daily 2020 data and construct a data matrix for a plot.
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
    }else{
      R_label <- "R"
    }
    hes_var_nm_2019_ENG <- c("Eating out for meals", "Cafe", "Bar", "Lodging", "Domestic travel packages", "Clothing and footwear") # Translation of six household expenditures in the explanatory variables.
  }else{
    R_label <- "実効再生産数"
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
  matplot(cbind(DEP, temp), type="l", col=c(1,2,2,2,3,3,3), lty=c(1,1,2,2,1,2,2), xaxt="n", xlab=time_xlab, ylab=temp_ylab, main=R_label)
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
  
}

  
############### Compute fitted reproduction numbers for the sample period beyond the estimation period. ###############################3

# Check if there is a sample period beyond the estimation period.
if (hes_end[1]+(hes_end[2]-1)/12 < hes_end_all[1]+(hes_end_all[2]-1)/12){
  
  # Define the length of the sample period beyond the estimation period for dependent variables.
  # Because the weekly moving average includes the current data, only six is subtracted. 14 is the number of lags for explanatory variables.
  prdctn_end <- dim(H_expvals_all)[2] - (which(hes_var_date_all==hes_end_date_estimation)+2-6-14) + 1 

  # Define time dummies. This part must be updated manually.
  D_NY_pred <- rep(0,prdctn_end)
  D_SE1_pred <- rep(0,prdctn_end)
  #D_SE2_pred <- c(rep(1,6+14+27+21),rep(0,prdctn_end-(6+14+27+21))) # The estimation period ends during the second state of emergency, which will end at March 21, 2021. 6 + 14 days up to February 1, 27 is the remaining days in February 2021.
  D_SE2_pred <- rep(0,prdctn_end)
  D_pre_SE1_pred <- rep(0,prdctn_end)
  
  # The fitted values of R within the estimation period end at one day after the sample period for dependent variables in the estimation.
  # Construct explanatory variables for out-of-sample forecasts from 19 days before the end of the last month of the estimation period, so that the first reproduction number to simulate is on the 2nd date of the next month.
  # Convert the format of mob_date to make it consistent with the format of hes_var_date_all.
  # Set zeros for D_NY, as the available sample period does not include the new year period.
  if (mdl_number < 20){
    temp <- R_simu(ms_R = ms, H_expvals = H_expvals_all[,(which(hes_var_date_all==hes_end_date_estimation)+2-6-14)+0:(prdctn_end-1)], M_trans = mob_var4[(which(conv_date_format(mob_date)==weath_end_date_estimation)+2-6-14)+0:(prdctn_end-1), 1], W_abs_hum = W_abs_hum_all[(which(weath_date_all==weath_end_date_estimation)+2-6-14)+0:(prdctn_end-1)], dist_incub = dist_incub, D_NY = D_NY_pred, D_SE1 = D_SE1_pred, D_SE2 = D_SE2_pred, D_pre_SE1 = D_pre_SE1_pred)
  }else{
    temp <- R_simu(ms_R = ms, H_expvals = H_expvals_all[,(which(hes_var_date_all==hes_end_date_estimation)+2-6-14)+0:(prdctn_end-1)], M_trans = mob_var4[(which(conv_date_format(mob_date)==weath_end_date_estimation)+2-6-14)+0:(prdctn_end-1), 1], W_abs_hum = W_abs_hum_all[(which(weath_date_all==weath_end_date_estimation)+2-6-14)+0:(prdctn_end-1)], dist_incub = dist_incub, D_NY = D_NY_pred, D_SE1 = D_SE1_pred, D_SE2 = D_SE2_pred, D_pre_SE1 = D_pre_SE1_pred, EXP_L452R=L452R_share_NW_d[sum(ndays_normal[1:hes_end[2]])+2-6-14+0:(prdctn_end-1)], EXP_MV_1st_VP=mv_vccn_frst_share[sum(ndays_normal[1:hes_end[2]])+2-6-14+0:(prdctn_end-1)], EXP_MV_2nd_VP=mv_vccn_scnd_share[sum(ndays_normal[1:hes_end[2]])+2-6-14+0:(prdctn_end-1)],mdl_number=mdl_number)
  }
  
  # Extract the mcmc samples of the fitted reproduction number for the sample period beyond the estimation period.
  R_fitted_pred <- temp$R
  
  # Extract the mcmc samples of the decomposition of fitted reproduction number for the sample period beyond the estimation period.
  R_fitted_pred_decomp <- temp$comp_effect
  
  # Compute the mean and the percentile values of time-varying parameters in vectors.
  
  name_list<-c("R_fitted_pred") # Variable name in the stan file.
  
  for (i in 1:length(name_list)){
    
    eval(parse(text=paste0("temp <- Extract_mean_ptl(",name_list[i],", ptl=c(0.025,0.975))")))
    eval(parse(text=paste0(name_list[i],"_pm <- temp[[1]]"))) # posterior mean
    eval(parse(text=paste0(name_list[i],"_025 <- temp[[2]]"))) # 5% percentile
    eval(parse(text=paste0(name_list[i],"_975 <- temp[[3]]"))) # 97.5% percentile
  }
  
  # Define the labels for dates starting from 2020 March 6, exclusive, to the end of the available sample period. The last date of fitted values of R is one day after the end of the available sample period.
  date_label_pred <- mob_date[(29-14+6):(which(conv_date_format(mob_date)==weath_end_date_allsmpl)+1)]
  
  # Find the location of the period of the state of emergency.
  SE_pred <- c(which(date_label_pred=="2020-04-07"),which(date_label_pred=="2020-05-25"),which(date_label_pred=="2021-01-07"),which(date_label_pred=="2021-03-21"),which(date_label_pred=="2021-04-25"),which(date_label_pred=="2021-06-20"),which(date_label_pred=="2021-07-12"),which(date_label_pred=="2021-08-31"))
  
  # Define the dependent variable. 
  DEP_pred <- R[6:(which(R_date==weath_end_date_allsmpl)+1)] # R_var starts from 2020 March 1. Set the start date to 2020 March 6. R_date has the same format as weath_date. The last date of fitted values of R is one day after the end of the available sample period.
  
  # Extend R_fitted_pred and Fitted_R to make their lengths identical. Fulfill NAs into the estimation period and the sample period after the estimation period, respectively.
  # The posterior mean and 95% credible interval for the fitted reproduction number for the estimation period.
  Fitted_R_ext_pm <- c(Fitted_R_pm, R_fitted_pred_pm*NA)
  Fitted_R_ext_025 <- c(Fitted_R_025, R_fitted_pred_025*NA) 
  Fitted_R_ext_975 <- c(Fitted_R_975, R_fitted_pred_975*NA)
  # The posterior mean and 95% credible interval for the fitted reproduction number after the estimation period.
  R_fitted_pred_pm <- c(Fitted_R_pm*NA, R_fitted_pred_pm)
  R_fitted_pred_025 <- c(Fitted_R_025*NA, R_fitted_pred_025) 
  R_fitted_pred_975 <- c(Fitted_R_975*NA, R_fitted_pred_975)
  
  
  # Plot the fitted values of the reproduction number.
  temp_nl2 <- c("Fitted_R_ext")
  temp_nl <- c("R_fitted_pred")
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
  if (ENG==1){
    if (plot_paper==1){
      temp_main_label<- ""
    }else{
      temp_main_label<- "Effective reproduction number (R)"
    }
  }else{
    temp_main_label<- "実効再生産数 (R)"
  }
  
  matplot(cbind(DEP_pred, temp), type="l", col=c(1,2,2,2,3,3,3), lty=c(1,1,2,2,1,2,2), xaxt="n", xlab=time_xlab, ylab=temp_ylab, main=temp_main_label)
  axis(side=1, at=c(1,round(nrow(temp)*c(1:5)/5)), labels=conv_date_format(date_label_pred[c(1,round(nrow(temp)*c(1:5)/5))]))
  if (log_R == 1){
    abline(h=0,lty=3) # h=0 because R is the log of the reproduction number.
  }else{
    abline(h=1,lty=3) # h=1 because R is the level of the reproduction number.
  }
  for (j in 1:length(SE_pred)){
    abline(v=SE_pred[j],lty=5)
  }  
  if (ENG==1){
    legend("topright",legend=c("Observed R", "Fitted R", "Out-of-sample forecasts for R"), lty=c(1,1,1), col=c(1,2,3))
  }else{
    legend("topright",legend=c("観測値", "回帰式のfitted valueの事後平均", "推計期間以降のRの予測値"), lty=c(1,1,1), col=c(1,2,3))
  }
  

}else if (mdl_number >= 20){

  ##### Compute fitted reproduction numbers for a part of the estimation period for models with L452R share of reported new cases. #########

  # Define the length of the sample period to compute the effective reproduction number from May 1, 2021, to include the beginning of the sample of L452R share of reported new cases nationwide.
  L452R_decomp_end <- dim(H_expvals_all)[2] - (which(hes_var_date_all=="2021-5-1")-6-14) + 1 
  
  # Define time dummies. This part must be updated manually.
  D_NY_L452R <- rep(0,L452R_decomp_end)
  D_SE1_L452R <- rep(0,L452R_decomp_end)
  D_SE2_L452R <- rep(0,L452R_decomp_end)
  D_pre_SE1_L452R <- rep(0,L452R_decomp_end)
  
  temp <- R_simu(ms_R = ms, H_expvals = H_expvals_all[,which(hes_var_date_all=="2021-5-1")-6-14+0:(L452R_decomp_end-1)], M_trans = mob_var4[which(mob_date=="2021-05-01")-6-14+0:(L452R_decomp_end-1), 1], W_abs_hum = W_abs_hum_all[which(weath_date_all=="2021/5/1")-6-14+0:(L452R_decomp_end-1)], dist_incub = dist_incub, D_NY = D_NY_L452R, D_SE1 = D_SE1_L452R, D_SE2 = D_SE2_L452R, D_pre_SE1 = D_pre_SE1_L452R, EXP_L452R=L452R_share_NW_d[sum(ndays_normal[1:4])+1-6-14+0:(L452R_decomp_end-1)], EXP_MV_1st_VP=mv_vccn_frst_share[sum(ndays_normal[1:4])+1-6-14+0:(L452R_decomp_end-1)], EXP_MV_2nd_VP=mv_vccn_scnd_share[sum(ndays_normal[1:4])+1-6-14+0:(L452R_decomp_end-1)],mdl_number=mdl_number)
  
  # Extract the mcmc samples of the fitted reproduction number for the sample period beyond the estimation period.
  R_fitted_pred <- temp$R
  
  # Extract the mcmc samples of the decomposition of fitted reproduction number for the sample period beyond the estimation period.
  R_fitted_pred_decomp <- temp$comp_effect
  
  # Compute the mean and the percentile values of time-varying parameters in vectors.
  
  name_list<-c("R_fitted_pred") # Variable name in the stan file.
  
  for (i in 1:length(name_list)){
    
    eval(parse(text=paste0("temp <- Extract_mean_ptl(",name_list[i],", ptl=c(0.025,0.975))")))
    eval(parse(text=paste0(name_list[i],"_pm <- temp[[1]]"))) # posterior mean
    eval(parse(text=paste0(name_list[i],"_025 <- temp[[2]]"))) # 5% percentile
    eval(parse(text=paste0(name_list[i],"_975 <- temp[[3]]"))) # 97.5% percentile
  }

}

######## For plotting the level decomposition of the fitted value of the reproduction number for the prediction period. ######

# Define the labels for dates starting from one day after the end of the estimation period to the end of the available sample period. The last date of fitted values of R is one day after the end of the available sample period.
date_label_pred_decomp <- mob_date[(which(conv_date_format(mob_date)==weath_end_date_estimation)+2):(which(conv_date_format(mob_date)==weath_end_date_allsmpl)+1)]

######## Common block for plotting the level decomposition of the fitted value of the reproduction number for the prediction period. ######

# Find the location of the period of the state of emergency.
SE_pred_decomp <- c(which(date_label_pred_decomp=="2020-04-07"),which(date_label_pred_decomp=="2020-05-25"),which(date_label_pred_decomp=="2021-01-07"),which(date_label_pred_decomp=="2021-03-21"),which(date_label_pred_decomp=="2021-04-25"),which(date_label_pred_decomp=="2021-06-20"),which(date_label_pred_decomp=="2021-07-12"),which(date_label_pred_decomp=="2021-08-31")) 

# Plot each component of the decomposition, including both effects via no coefficient dummies and the coefficient dummies of absolute humidity, in one diagram.
# Define the name of each component of decomposition.
if (ENG==1){
  temp_nl <- c("Constant", "Absolute humidity", "Eating out for meal", "Cafe", "Bar", "Lodging", "Domestic travel packages", "Clothing and footwear", "Mobility in transportation") 
}else{
  temp_nl <- c("定数項", "絶対湿度", "食事代", "喫茶代", "飲酒代", "宿泊料", "国内パック旅行費", "被服及び履物", "人出（公共交通）")
}  

if (mdl_number == 21){
  # Define the names of additional explanatory variables related to L452R and vaccinations.
    if (ENG==1){
      temp_nl_add <- c("L452R share", "1st-time vaccination", "2nd-time vaccination")
    }else{
      temp_nl_add <- c("デルタ株陽性率", "1回接種", "2回接種")
    }
}else if (mdl_number == 20){
  # Define the names of additional explanatory variables related to L452R and vaccinations.
  if (ENG==1){
    temp_nl_add <- c("L452R and vaccine factor")
  }else{
    temp_nl_add <- c("デルタ株・ワクチン係数")
  }
}else{
  # Set NULL for the names of additional explanatory variables related to L452R and vaccinations.
  temp_nl_add <- NULL
}
  
# Constant and absolute humidity dummies, which are in the second row of the matrix on the right-hand side.
temp <- R_fitted_pred_decomp[1:2,]
# Terms that include the absolute humidity coefficient dummies. 
for (i in 1:(length(temp_nl)-2)){
  # Sum the three components for each variable: basic coefficients without coefficient dummies, absolute humidity coefficient dummies, state of emergency coefficient dummies.
  temp <- rbind(temp, R_fitted_pred_decomp[2+i,] + R_fitted_pred_decomp[2+i+(length(temp_nl)-2),] + R_fitted_pred_decomp[2+i+(length(temp_nl)-2)*2,])
}

if (mdl_number >= 20){
  # Add contributions from additional explanatory variables related to L452R and vaccinations.
  temp <- rbind(temp, R_fitted_pred_decomp[2+(length(temp_nl)-2)*3+1:length(temp_nl_add),])
}


# Plot the figures.
for (i in 1:2){
  # First plot: level of each component.
  # Second plot: Index of each component which is normalized to 0 for the first value.  

  temp <- t(temp) # Transpose the matrix to plot time series in matplot for the first iteration. Retranspose in the second iteration, which works fine.

  # Set the title of the diagram.
  if (ENG==1){
    if (i ==1){
      if (log_R == 1){
        temp_ylab <- "Level" #"Components of log R"
        temp_main_title <- "Decomposition of predicted ln R"
      }else{
        temp_ylab <- "Level" #"Components of R"
        temp_main_title <- "Decomposition of predicted R"
      }
    }else{
      if (hes_end[1]+(hes_end[2]-1)/12 < hes_end_all[1]+(hes_end_all[2]-1)/12){
        temp_month_name <- c("Jan.","Feb.","Mar.","Apr.","May","Jun.","Jul.","Aug.","Sep.","Oct.","Nov.","Dec.")
        
        if (log_R == 1){
          temp_ylab <- paste0("Contribution to a change in ln R from 2021 ", temp_month_name[hes_end[2]+1] ," 2 (2021 ", temp_month_name[hes_end[2]+1], "2 = 0)")
          temp_main_title <- "Decomposition of predicted ln R"
        }else{
          temp_ylab <- paste0("Contribution to a change in R from 2021 ", temp_month_name[hes_end[2]+1] ," 2 (2021 ", temp_month_name[hes_end[2]+1], "2 = 0)")
          temp_main_title <- "Decomposition of predicted R"
        }
      }else{
        if (log_R == 1){
          temp_ylab <- "Contribution to a change in ln R from 2021 May 1 (2021 May 1 = 0)"
          temp_main_title <- "Decomposition of predicted ln R"
        }else{
          temp_ylab <- "Contribution to a change in R from 2021 May 1 (2021 May 1 = 0)"
          temp_main_title <- "Decomposition of predicted R"
        }
      }
      temp <- apply(temp,1,function(x){x-x[1]}) 
    }
  }else{
    if (i ==1){
      if (log_R == 1){
        temp_ylab <- "ln Rの構成要素"
        temp_main_title <- "ln Rの予測値の要因分解"
      }else{
        temp_ylab <- "Rの構成要素"
        temp_main_title <- "Rの予測値の要因分解"
      }
    }else{
      if (log_R == 1){
        temp_ylab <- paste0("2021年",hes_end[2]+1,"月2日からのln Rの変化分（2021年",hes_end[2]+1,"月2日=0）")
        temp_main_title <- "ln Rの予測値の要因分解"
      }else{
        temp_ylab <- paste0("2021年",hes_end[2]+1,"月2日からのRの変化分（2021年",hes_end[2]+1,"月2日=0）")
        temp_main_title <- "Rの予測値の要因分解"
      }
      temp <- apply(temp,1,function(x){x-x[1]}) 
    }
    
  }
  
  # Plot the components of decomposition in one diagram.
  # The first element of temp and temp_nl corresponds to the constant term.
  # For i = 2, exclude the constant term from the figure, because the figure plots changes in the components of temp, and a change in the constant term is zero.
  if (i == 1){
    # Define the palette of colors.
    temp_col <- 1:(length(temp_nl))
    # Define the line types.
    temp_lty <- c(6, rep(1,length(temp_nl)-2),2)
    # Define line width.
    temp_lwd <- c(1.05,rep(1,length(temp_nl)-1))
    # Define line width for legend.
    temp_lwd_legend <- c(1.1,rep(1,length(temp_nl)-1))
  }else{
    # Define the palette of colors.
    temp_col <- 1+1:(length(temp_nl)-1)
    # Define the line types.
    temp_lty <- c(rep(1,length(temp_nl)-2),2)
    # Define line width.
    temp_lwd <- rep(1,length(temp_nl)-1)
    # Define line width for legend.
    temp_lwd_legend <- temp_lwd
  }
  if (mdl_number >= 20){
    # Define the line types for explanatory variables related to L452R and vaccinations.
    temp_col <- c(temp_col, temp_col[length(temp_col)]+1:length(temp_nl_add))
    temp_lty <- c(temp_lty, rep(1,length(temp_nl_add)))
    temp_lwd <- c(temp_lwd, rep(1,length(temp_nl_add)))
  }
    
  matplot(temp[,i:length(c(temp_nl,temp_nl_add))], type="l", col=temp_col, lty=temp_lty, lwd=temp_lwd, xaxt="n", xlab=time_xlab, ylab=temp_ylab, main=temp_main_title, ylim=c(min(temp[,i:length(c(temp_nl,temp_nl_add))])-0.2*(i==1)-0.1*(i==2), max(temp[,i:length(c(temp_nl,temp_nl_add))])+0.01))
  axis(side=1, at=c(1,round(nrow(temp)*c(1:5)/5)), labels=conv_date_format(date_label_pred_decomp[c(1,round(nrow(temp)*c(1:5)/5))]))
  if (log_R == 1){
    abline(h=0,lty=3) # h=0 because R is the log of the reproduction number.
  }else{
    abline(h=1,lty=3) # h=1 because R is the level of the reproduction number.
  }
  for (j in 1:length(SE_pred_decomp)){
    abline(v=SE_pred_decomp[j],lty=5)
  }  

  # Define the location of legend in the figure.
  if (i==1){
    legend_loc <- "bottomright"
  }else{
    legend_loc <- "bottomleft"
  }
  # Add legend to the figure.
  legend(legend_loc,legend=c(temp_nl[i:length(temp_nl)],temp_nl_add),col=temp_col,lty=temp_lty, lwd=temp_lwd_legend)

}


# Separate the effects of absolute humidity from the decomposition if there is a sample period beyond the estimation period.
if (hes_end[1]+(hes_end[2]-1)/12 < hes_end_all[1]+(hes_end_all[2]-1)/12){

  # Define the name of each component of decomposition.
  if (ENG==1){
    temp_nl <- c("Eating out for meal", "Cafe", "Bar", "Lodging", "Domestic travel packages", "Clothing and footwear", "Mobility in transportation") 
    temp_nl2 <- c("benchmark coef.", "absolute humidity coef. dummies","state of emercency coef. dummies")
  }else{
    temp_nl <- c("食事代", "喫茶代", "飲酒代", "宿泊料", "国内パック旅行費", "被服及び履物", "人出（公共交通）")
    temp_nl2 <- c("基本係数","絶対湿度係数ダミー","緊急事態宣言係数ダミー")
  }  

  # Compute the difference from the value in the first period for each variable.
  temp <- apply(R_fitted_pred_decomp,1,function(x){x-x[1]}) # Output is transposed because each row vector is recognized as a column vector after the operation, which is the default format of a vector.
  
  # Plot the effects via no-dummy coefficients and absolute humidity coefficient dummies separately.
  for (i in 1:3){
    # First plot: No-dummy coefficients.
    # Second plot: Absolute humidity coefficient dummies.
    # Third plot: State of emergencies coefficient dummies (which are set to zeros).
    
    # The label for the vertical axis and the main title of plots.
    if (ENG==1){
      temp_month_name <- c("Jan.","Feb.","Mar.","Apr.","May","Jun.","Jul.","Aug.","Sep.","Oct.","Nov.","Dec.")
      
      if (log_R == 1){
        temp_ylab <- paste0("Contribution to a change in ln R from 2021 ", temp_month_name[hes_end[2]+1] ," 2 (2021 ", temp_month_name[hes_end[2]+1], "2 = 0)")
        temp_main_title <- paste0("Decomposition of predicted ln R (", temp_nl2[i],")")
      }else{
        temp_ylab <- paste0("Contribution to a change in R from 2021 ", temp_month_name[hes_end[2]+1] ," 2 (2021 ", temp_month_name[hes_end[2]+1], "2 = 0)")
        temp_main_title <- paste0("Decomposition of predicted R (", temp_nl2[i],")")
      }
    }else{
      if (log_R == 1){
        temp_ylab <- paste0("2021年",hes_end[2]+1,"月2日からのln Rの変化分（2021年",hes_end[2]+1,"月2日=0）")
        temp_main_title <- paste0("Log Rの予測値の要因分解（", temp_nl2[i],"）")
      }else{
        temp_ylab <- paste0("2021年",hes_end[2]+1,"月2日からのRの変化分（2021年",hes_end[2]+1,"月2日=0）")
        temp_main_title <- paste0("Rの予測値の要因分解（", temp_nl2[i],"）")
      }
    }
    
    # Plot each of the effects via no-dummy coefficients and absolute humidity coefficient dummies separately. By  col=1+1:length(temp_nl), use the same color for each series as the previous figure on the total contribution from each explanatory variable.  
    matplot(temp[,2+length(temp_nl)*(i-1)+1:length(temp_nl)], type="l", col=2+1:(length(temp_nl)), lty=c(rep(1,length(temp_nl)-1),2), xaxt="n", xlab=time_xlab, ylab=temp_ylab, main=temp_main_title, ylim=c(min(temp)-0.01, max(temp)+0.01))
    axis(side=1, at=c(1,round(nrow(temp)*c(1:5)/5)), labels=conv_date_format(date_label_pred_decomp[c(1,round(nrow(temp)*c(1:5)/5))]))
    if (log_R == 1){
      abline(h=0,lty=3) # h=0 because R is the log of the reproduction number.
    }else{
      abline(h=1,lty=3) # h=1 because R is the level of the reproduction number.
    }
    for (j in 1:length(SE_pred_decomp)){
      abline(v=SE_pred_decomp[j],lty=5)
    }  
    legend("bottomleft",legend=temp_nl,col=2+1:length(temp_nl),lty=c(rep(1,length(temp_nl)-1),2))
    
    # Save the data table as .csv
    temp_save <- temp[,2+length(temp_nl)*(i-1)+1:length(temp_nl)] # Data table.
    rownames(temp_save) <- conv_date_format(date_label_pred_decomp) # Define dates as row labels.
    colnames(temp_save) <- temp_nl # Define variable names as column labels.
    eval(parse(text=paste0("write.csv(temp_save,file=\"fourthwavedecomp_",i,".csv\")")))
    
  }
  
}

### Close the pdf file.
dev.off()


if (onlyplot_simu0 == 1 || mdl_number >= 20){
  
  # Stop here.
  print("Plot only simu0.")
  
}else{
  
  ############## Set common variables and parameters for the simulation of the reproduction number with hypothetical interventions. ##############################
  
  # Compute the average expenditure over various periods.
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
  
  # Define the labels for dates starting from 2020 February 15, exclusive, to 366 + 13 (i.e., 14 lags for incubation - 1 lag for R) + 6 (7 days average for R - 1 for the current day) days later.
  date_label_simu_mob <- mob_date[1:dim(H_expvals_simu_2019)[2]] # mob_date starts from February 15.
  
  # Find the location of the period of the state of emergency.
  SE_simu_mob <- c(which(date_label_simu_mob=="2020-04-07"),which(date_label_simu_mob=="2020-05-25"),which(date_label_simu_mob=="2021-01-07"),which(date_label_simu_mob=="2021-03-21")) 
  
  # Set the upper limit on mobility in terms of percentage points to the benchmark date. 
  #sim_mob_perc <- c(Inf,-10,-20,-30,-40,-50) # "Inf" means no restriction.
  sim_mob_perc <- c(Inf) # "Inf" means no restriction.
  
  # Set the fraction of revenue for cafe and drink to curb based on 2019 average revenue in 2020 prices for each iteration.
  sim_revn_frac <- c(0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1)
  
  # Set the value of the restriction on cafe and drink to draw in a plot.
  sim_revn_frac_plot <- list(c(1),c(1),c(0.8),c(0.7),c(0.9),c(0.9),c(0.9),c(0.9))
  
  
  # Set the first and last date of each simulation period to compute the average R.
  def_sim_dates <- rbind(c("2020-07-01","2020-10-31"),c("2020-07-01","2020-11-30"),c("2020-07-01","2020-12-31"))
  
  
  ############## Simulate the reproduction number with hypothetical interventions for the year. ##############################
  
  #### Specify simulations to run. 
  # flag_simu: Specify the type of simulation.
  # 1st element: Endogenous (1) or exogenous mobility (0). 
  # 2th element: Exogenous restriction on mobility (1) or not (0).
  # 3nd element: Special treatment of holidays (golden weeks, Obon, the new year period) (1) or not (0).
  # 4rd element: Regular restriction on drink (0) or cafe and drink (1).
  # 5th element: Restriction on Domestic travel packages (1) or not (0).
  
  flag_simu <- rbind(c(0,0,0,0,0),
                     c(1,0,0,0,0),
                     c(0,0,0,0,1),
                     c(1,0,0,0,1),
                     c(0,0,0,1,0),
                     c(1,0,0,1,0),
                     c(0,0,0,1,1),
                     c(1,0,0,1,1))
  #c(0,1,0,0,1),
  #c(1,1,0,0,1))
  #c(1,1,1,0,1),
  #c(1,1,1,1,1))
  
  
  # Iterate simulations.
  
  for (s in 1:dim(flag_simu)[1]){
    
    # Record the result of each simulation in a pdf file.
    eval(parse(text=paste0("pdf(paste0(\"simu",s,"_reformed_data_model",mdl_number,"_ENG",as.logical(ENG),"_",R_sfx,".pdf\"), family=\"Japan1GothicBBB\")")))
    
    # Initialize a matrix to save the posterior mean, 2.5% percentile, and 97.5% percentile of the annual mean of R.
    if (flag_simu[s,2]==1){
      mean_R_pm <- matrix(NA, nr=length(sim_mob_perc)*nrow(def_sim_dates), nc=length(sim_revn_frac)*3) # Rows: restrictions on mobility.
    }else{
      mean_R_pm <- matrix(NA, nr=nrow(def_sim_dates), nc=length(sim_revn_frac)*3) # Rows: restrictions on mobility.
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
        
        # Insert household expenditure for drink intervention.
        #H_expvals_simu_intv[2,] <- H_expvals_simu_2019[2,] * (1 - sim_revn_frac[j])  #cafe
        H_expvals_simu_intv[3,] <- H_expvals_simu_2019[3,] * (1 - sim_revn_frac[j])  # drink
        
        
        if (flag_simu[s,4]==1){
          # Insert household expenditure for eating-out intervention.
          # H_expvals_simu_intv[1,] <- H_expvals_simu_2019[1,] * (1 - sim_revn_frac[j]) # Eating out for meals 
          # Insert household expenditure for cafe intervention.
          H_expvals_simu_intv[2,] <- H_expvals_simu_2019[2,] * (1 - sim_revn_frac[j])  #cafe
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
        # Iterate for different periods to compute the average R.
        for (v in 1:nrow(def_sim_dates)){
          
          # Set the locations of elements for the simulation period.
          sim_period <- which(date_label_simu==def_sim_dates[v,1]):which(date_label_simu==def_sim_dates[v,2])
          
          # mcmc samples are on the rows.
          temp_mean_R <- apply(temp_R[,sim_period],1,mean)
          
          # Store the posterior mean, 33% percentile, and 66% percentile of the annual mean simulated R over the simulation period.
          mean_R_pm[(k-1)*length(sim_mob_perc)+v,(j-1)*3+1:3] <- c(mean(temp_mean_R), quantile(temp_mean_R,c(0.025,0.975)))
          
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
          #temp_ml_4 <- "All eating out"
          temp_ml_4 <- "Only cafe & drink"
        }else{
          #temp_ml_4 <- "外食一般制限"
          temp_ml_4 <- "喫茶・飲酒制限"
        }
      }else{
        if (ENG==1){
          #temp_ml_4 <- "Only cafe & drink"
          temp_ml_4 <- "Only drink"
        }else{
          #temp_ml_4 <- "喫茶・飲酒制限"
          temp_ml_4 <- "飲酒制限"
        }
      }
      
      if(flag_simu[s,5]==1){
        if (ENG==1){
          temp_ml_5 <- "Reduced travel"
        }else{
          temp_ml_5 <- "国内パック旅行自粛有"
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
        temp_main_label <- "Effective reproduction number (R)" # The main title for plots in the paper.
      }else{
        temp_main_label <- paste0(temp_main_label,collapse=", ")
      }
      
      # Main label for mobility.
      # temp_main_label_mob <- temp_ml_1 
      # if (is.null(temp_ml_2)==0){
      #   temp_main_label_mob <- c(temp_main_label_mob, temp_ml_2)
      # }
      # temp_main_label_mob <- paste0(c(temp_main_label_mob,paste0(" (",temp_ml_4,")")),collapse=",")
      if (ENG==1){
        if (plot_paper == 1 && flag_simu[s,1]==0){
          temp_main_label_mob <- "Mobility in public transportation" # Only show the data label for plots in the paper.
        }else{
          temp_main_label_mob <- "Mobility in public transportation" # Only show the data label for plots in the paper.
        }
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
          if (plot_paper==1){
            temp_legend_rest_rate <- c(temp_legend_rest_rate, ", \"Hypothetical values\"")
          }else{
            #temp_legend_rest_rate <- c(temp_legend_rest_rate, ", \"", temp_pick_plot[i]*100, "% reduction of cafe and bar consumption\"")
            temp_legend_rest_rate <- c(temp_legend_rest_rate, ", \"Hypothetical data with ", temp_pick_plot[i]*100, "% reduction of cafe and bar consumption\"")
          }
        }else{
          temp_legend_rest_rate <- c(temp_legend_rest_rate, ", \"介入対象を対2019年比で", temp_pick_plot[i]*100,"%削減した場合\"")
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
      matplot(cbind(DEP, temp), type="l", col=c(1,rep(2,3),rep(3,3),rep(4,3),rep(5,3)), lty=c(1,1,2,2,1,2,2,1,2,2,1,2,2), xaxt="n", xlab=time_xlab, ylab=temp_ylab, main=paste0(R_label,temp_main_label))
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
      
      # Create a data table to plot simulated series of mobility with mobility data in 2020 and 2019.
      temp <- 1+mob_var4[1:dim(temp_mob_rslt)[1],1]/100 # Create a data table for a plot of observed data and estimated parameters. The first column is 2020-2021 data.
      for (i in 1:length(temp_pick_plot)){
        temp <- cbind(temp, temp_mob_rslt[,c(which(sim_revn_frac==temp_pick_plot[i]))]) # Add data of restricted mobility.
      }
      if (ENG!=1 || plot_paper!=1){
        temp <- cbind(temp, 1+M_trans_simu_2019[1:dim(temp_mob_rslt)[1]]/100) # Create a data table for hypothetical data based on 2019 data.
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
        matplot(temp, type="l", col=c(1,3,2,5), lty=c(1,1,1,1), xaxt="n", xlab=time_xlab, ylab=temp_ylab, main=temp_main_label_mob)
        if (ENG==1){
          if (plot_paper==1){
            temp_legend <- c("legend(\"topright\",legend=c(\"transit_stations from Google for 2020-2021\"", temp_legend_rest_rate, "), lty=1, col=c(1,3,2,5))")
          }else{
            temp_legend <- c("legend(\"topright\",legend=c(\"transit_stations from Google for 2020-2021\"", temp_legend_rest_rate, ",\"Hypothetical 2019 railway-passenger data\"), lty=1, col=c(1,3,2,5))")
          }
        }else{
          temp_legend <- c("legend(\"topright\",legend=c(\"2020年以降のグーグルデータ\"", temp_legend_rest_rate, ",\"2019年鉄道旅客人数\"), lty=1, col=c(1,3,2,5))")
        }
      }else{
        # Exogenous mobility, which is not affected by restrictions on eating out.
        matplot(temp, type="l", col=c(1,3,2), lty=1, xaxt="n", xlab=time_xlab, ylab=temp_ylab, main=temp_main_label_mob)
        if (ENG==1){
          if (plot_paper==1){
            temp_legend <- "legend(\"topright\",legend=c(\"2020-2021 data\", \"Hypothetical values\"), lty=1, col=c(1,3))"
          }else{
            temp_legend <- c("legend(\"topright\",legend=c(\"transit_stations from Google for 2020-2021\", \"Hypothetical values\",\"Hypothetical 2019 railway-passenger data\"), lty=1, col=c(1,3,2))")
          }
        }else{
          temp_legend <- "legend(\"topright\",legend=c(\"2020年以降のグーグルデータ\", \"仮想値\",\"2019年鉄道旅客人数\"), lty=1, col=c(1,2,3))"
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
          temp <- c(H_expvals_base[i,],rep(NA,dim(temp)[1]-dim(H_expvals_base)[2])) # Plot 2020-2021 data. 
          for (j in 1:length(temp_pick_plot)){
            temp <- cbind(temp, temp_hes_intv[[which(sim_revn_frac==temp_pick_plot[j])]][i,]) # Plot hypothestical data with consumption interventions.
          }
          if (ENG!=1 || plot_paper!=1){
            # Plot 2019 data, 2020-2021 data and hypothetical data with interventions.
            temp <- cbind(temp, H_expvals_simu_2019[i,]) # Combine 2020 data and 2019 data. The latter starts from February 16, 2019, and the year-end is connected with the beginning of the year.
          }
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
          matplot(temp, type="l",lty=1, col=c(1,3,2,5),  xaxt="n", xlab=time_xlab, ylab=temp_ylab, main=temp_main_label) # Plot H_expvals for 365 days from 2020 February 16 or 2019 February 15.
          # Add legends.
          if (ENG==1){
            if (plot_paper==1){
              temp_legend <- c("legend(\"topright\",legend=c(\"2020-2021 data\"", temp_legend_rest_rate,"), lty=1, col=c(1,3,2,5))") # Legends of the plot. 
            }else{
              temp_legend <- c("legend(\"topright\",legend=c(\"2020-2021 data\"",temp_legend_rest_rate,",\"Hypothetical 2019 data\"), lty=1, col=c(1,3,2,5))") # Legends of the plot. 
            }
          }else{
            temp_legend <- c("legend(\"topright\",legend=c(\"2020年～2021年データ\"", temp_legend_rest_rate,",\"2019年データに基づく仮想データ\"), lty=1, col=c(1,3,2,5))") # Legends of the plot. 
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
    
    # Record the posterior mean and percentiles of the annual mean simulated R over each simulation period to compute the average R.
    # For now, only do so when there is no exogenous restriction on mobility.
    if (flag_simu[s,2]==0){
      for (v in 1:nrow(def_sim_dates)){
        temp <- rep(NA,length(sim_revn_frac)*3) # Initialize the vector of column names.
        for (i in 1:(length(sim_revn_frac)*3)){
          if (i%%3==1){
            temp[i] <- paste(- sim_revn_frac[i%/%3+1]*100) # Name the columns of the matrix, which are restrictions on household expenditure for cafe and drink.
          }else{
            temp[i] <- "&" # columns for percentiles.
          }
        }
        temp_mean_R_pm <- t(matrix(mean_R_pm[v,],nr=3)) # Convert it to a column vector so that
        rownames(temp_mean_R_pm) <- temp[seq(1,length(sim_revn_frac)*3,by=3)] # Name rows. For each level of revenue restriction, (mean, 2.5% percentile, 97.5% percentile) is one set.
        colnames(temp_mean_R_pm) <- c("Mean", "2.5%", "97.5%")
        eval(parse(text=paste0("sink(paste0(\"mean_R_pm",s,"_model",mdl_number,"_",R_sfx,"_",def_sim_dates[v,1],"_",def_sim_dates[v,2],".txt\"))")))
        print("The posterior mean, 2.5%, and 97.5% percentiles of the mean of the reproduction number over the simulated period.")
        print("Rows: restrictions on eating out.")
        print(xtable(temp_mean_R_pm,digits=c(0,rep(2,3))))
        sink()
      }
    }
    
    # temp <- rep(NA,length(sim_revn_frac)*3) # Initialize the vector of column names.
    # for (i in 1:(length(sim_revn_frac)*3)){
    #   if (i%%3==1){ 
    #     temp[i] <- paste(- sim_revn_frac[i%/%3+1]*100) # Name the columns of the matrix, which are restrictions on household expenditure for cafe and drink.
    #   }else{
    #     temp[i] <- "&" # columns for percentiles.
    #   }
    # }
    # if (flag_simu[s,2]==0){
    #    mean_R_pm <- t(matrix(mean_R_pm[],nr=3)) # Convert it to a column vector so that 
    #   rownames(mean_R_pm) <- temp[seq(1,length(sim_revn_frac)*3,by=3)] # Name rows. For each level of revenue restriction, (mean, 2.5% percentile, 97.5% percentile) is one set.
    #   colnames(mean_R_pm) <- c("Mean", "2.5%", "97.5%")
    # }else{
    #   colnames(mean_R_pm) <- temp # Name columns. For each level of revenue restriction, (mean, 2.5% percentile, 97.5% percentile) is one set.
    #   rownames(mean_R_pm) <- sim_mob_perc # Name the rows of the matrix, which are restrictions on mobility.
    # }
    # eval(parse(text=paste0("sink(paste0(\"mean_R_pm",s,"_model",mdl_number,"_",R_sfx,".txt\"))")))
    # print("The posterior mean, 2.5%, and 97.5% percentiles of the mean of the reproduction number over the simulated period.")
    # if (flag_simu[s,2]==0){
    #   print("Rows: restrictions on eating out.")
    # }else{
    #   print("Columns: restrictions on eating out.")
    #   print("Rows: upper bound on mobility. (Inf means no restriction.)")
    # }
    # print(mean_R_pm)
    # sink()
    
    # Close the pdf file for each simulation.
    dev.off()
    
  }
  
}






