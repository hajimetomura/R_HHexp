# Define functions for data handling 

########## Define a function to remove non-data labels and convert data in the file into a numeric character. #########

conv_list <- function(m){
  # m must be a list containing vectors of only numbers.
  if (is.null(dim(m))==1){
    m <- matrix(m,nc=1)
  }
  y <- matrix(NA,nr=dim(m)[1],nc=dim(m)[2]) # The matrix containing the numbers in m.
  for (i in 1: dim(m)[2]){ 
    x <- unlist(m[,i]) # Convert the i-th list into a character vector.
    x <- sub("・","",x) # Remove dot.
    x <- sub("・","",x) # Remove dot.
    x <- sub("・","",x) # Remove dot.
    x <- sub(",","",x) # Remove comma.
    x <- sub(",","",x) # Remove comma.
    x <- sub(",","",x) # Remove comma.
    x <- sub("<","",x) # Remove parentyes.
    x <- sub(">","",x) # Remove parentyes.
    y[,i] <- as.numeric(x) # Convert a character vector into a numeric vector and store it into y.
  }
  
  if (dim(y)[1] == 1 || dim(y)[2] == 1){
    y <- c(y) # If y is one-dimensional, return a vector, rather than a matrix.
  }
  return(y)
}

############# Nishimura's function to write barplot labels in tategaki.
tategaki = function(x) {
  x = chartr("ー", "丨", x) # 長音符の処理
  x = strsplit(split="", x)
  sapply(x, paste, collapse="\n")
}

######## Convert relative humidity into absolute humidity. ###########

calc_abs_hum <- function(temper_var,hum_var){
  # temper_var: temperature.
  # hum_var: humidity.
  
  GofGra <- function(t){ ## 入力：℃　出力：hPa 飽和水蒸気圧
    10^
      (10.79574 * (1 - 273.16/(t + 273.15)) -
         5.02800 * log((t + 273.15)/273.16, 10) +
         1.50475 * 10^{-4} * {1-10^(-8.2969 * ((t + 273.15)/273.16 - 1))} +
         0.42873 * 10^{-3} * {10^(4.76955*(1 - 273.16/(t + 273.15))) - 1} +
         0.78614)
  }
  abs_hum_var <- 2.166740 * 10^2 * hum_var * GofGra(temper_var)/(100 * (temper_var + 273.15)) #絶対湿度
  #temp <- 6.11 * 10 ^(7.5*temper_var /(temper_var+237.3)) # 飽和水蒸気圧
  #temp <- 217 * temp / (temper_var+273.15) #飽和水蒸気量
  #abs_hum_var <- temp * hum_var / 100 #絶対湿度
  
  return(abs_hum_var)
}

############## Substitute yyyy-mm-dd to yyyy/m/d ##################

conv_date_format <- function(x){
  # x: A character vector containing date labels in the format of yyyy-mm-dd.
  for (k in 1:length(x)){
    temp <- x[k] # Extract each date.
    for (j in 1:2){
      for (i in 1:9){
        eval(parse(text=paste0("temp_sub <- sub(\"0",i,"\",\"",i,"\",substr(temp,5, nchar(temp)))"))) # Do not change the year.
        temp <- paste0(substr(temp,1,4),temp_sub) # Combine the year and the month and the date.
      }
      temp <- sub("-","/",temp) 
    }
    x[k] <- temp # Return the newly formatted date.
  }    
  return(x)
}

############### Plot a matrix of time series with a common time series. Also plot ccf between each pair. ###########

plot_ts_ccf <- function(x,x_name,y,y_name,date_label,h_val,SE){
  # x: A vector of 7-day moving average of a time series for the same sample period as y. In ccf, x is fixed at t=0. 
  # x_name: The name of the variable in x.
  # y: A matrix of time series. Rows: items; Columns: time.
  # y_name: The vector containing the names of variables in y.
  # date_label: The character vector of dates.
  # h_val: An important level of x, such as 1 for reproduction number.
  # SE: Location of the period of the state of emergency.
  #
  
  flag_main <- 0 # Set this value to 1 to print plots with the main title for slides. 
  
  # Standardize x.
  adj_x <- (x-mean(x))/sd(x)

  # Initialize the matrix to record the results of ccf.
  rslt_ccf <- matrix(-Inf, nr=nrow(y),nc=2)
  
  # Convert the format of the date_label into a common format.
  date_label <- conv_date_format(date_label)

  # Draw data.
  for (i in 1:nrow(y)){
    
    # Standardize each series in y.
    if (sd(y[i,])>0){
      adj_y <- (y[i,]-mean(y[i,]))/sd(y[i,])
    }else{
      adj_y <- y[i,];
    }
    
    # Draw the plot.
    if (flag_main == 1){
      if (is.null(h_val)==0){
        # If h_val is given, indicate what it means.
        eval(parse(text=paste0("matplot(cbind(adj_y,adj_x), type=\"l\", lty=1, col=1:2, xaxt=\"n\", xlab=\"Dash lines are the first and last dates of states of emergency \", ylab=\"Standardized index (the dotted line: ",x_name,"=",h_val,")\", main=\"",y_name[i],"\")")))
      }else{
        eval(parse(text=paste0("matplot(cbind(adj_y,adj_x), type=\"l\", lty=1, col=1:2, xaxt=\"n\", xlab=\"Dash lines are the first and last dates of states of emergency \", ylab=\"Standardized index\")")))
        }
    }else{
      if (is.null(h_val)==0){
        # If h_val is given, indicate what it means.
        eval(parse(text=paste0("matplot(cbind(adj_y,adj_x), type=\"l\", lty=1, col=1:2, cex.axis=1.5, cex.lab=1.5, xaxt=\"n\", ylab=\"Standardized index (the dotted line: ",x_name,"=",h_val,")\")"))) # For the paper, in which notes can explain the meanings of axes.
      }else{
        eval(parse(text=paste0("matplot(cbind(adj_y,adj_x), type=\"l\", lty=1, col=1:2, cex.axis=1.5, cex.lab=1.5, xaxt=\"n\", ylab=\"Standardized index\")"))) # For the paper, in which notes can explain the meanings of axes.
      }
    }
    
    # Set the labels of the x axis.
    axis(side=1, at=c(1,round(length(x)*c(1:5)/5)), labels=date_label[c(1,round(length(x)*c(1:5)/5))],cex.lab=1.5, cex.axis=1.2)
    # Draw a line to indicate an important level of x, if h_val is given.
    if (is.null(h_val)==0){
      abline(h=(h_val-mean(x))/sd(x),lty=3)
    }
    # Draw a line to indicate the declaration of the state of emergency.
    for (j in 1:length(SE)){
      abline(v=SE[j],lty=5)
    }  

    # Add legends.
    if (flag_main == 1){
      eval(parse(text=paste0("legend(\"topright\", legend=c(\"",y_name[i],"\",\"",x_name,"\"), lty=1, col=1:2)")))
    }else{
      eval(parse(text=paste0("legend(\"topright\", legend=c(\"",y_name[i],"\",\"",x_name,"\"), lty=1, col=1:2, cex=1.5)"))) # For the paper, in which notes can explain the meanings of axes.
    }
    
    # Draw the ccf.
    if (sd(y[i,])>0){
      if (flag_main == 1){
        eval(parse(text=paste0("temp_ccf<-ccf(x, y[i,], lag.max=28, ylab=\"Correlation coefficients between ",x_name," (at t=0) and lagged variables\", main=\"",y_name[i],"\")")))
      }else{
        eval(parse(text=paste0("temp_ccf<-ccf(x, y[i,], lag.max=28, cex.axis=1.5, cex.lab=1.5, ylab=\"\",main=\"\")"))) # For the paper, in which notes can explain the meanings of axes.
      }
      
      # Extract the highest correlation coefficients for positive lags.
      temp_ccf_loc <- which.max(temp_ccf[[1]][30:57,1,1]) # Location of the highest correlation coefficient in the output of ccf().
      rslt_ccf[i,1] <- temp_ccf[[1]][29+temp_ccf_loc,1,1] # The value of the highest correlation coefficient.
      rslt_ccf[i,2] <- temp_ccf_loc # The corresponding lag for the highest correlation coefficient.
    }
  }
  
  return(rslt_ccf) # The returns of this function are plots.
}

###### Extract the postmean and the percentile values from a mcmc sample. #########

Extract_mean_ptl<- function(x, ptl){
  # x: the mcmc sample of the variable. Samples are in rows; variables (including the time-varying values of a variable) are in columns.
  # ptl: c(low percentile, high percentile)
  
  if (length(dim(x))>1){
    # Compute the mean of a time series.
    x_postmean <- apply(x,2,mean)
    
    # Compute the percentiles of a time series.
    x_q <- apply(x, 2, quantile, ptl)
    x_low <- x_q[1,]
    x_high <- x_q[2,]
  }else{
    # Compute the mean of a scaler parameter.
    x_postmean <- mean(x)
    
    # Compute the percentiles of a scaler parameter.
    x_q <- quantile(x, ptl)
    x_low <- x_q[1]
    x_high <- x_q[2]
  }

  return(list(x_postmean,x_low,x_high))
}


########## Area plot of posterior mean. ################

area_plot <- function(x,var_name,ylim_scale=1,xlab="",ylab="",xaxt="s",x_axis_label=NULL,n_label=NULL,main=""){

  # x: the data table. All elements are positive. Rows: x axis. Columns: Categories.
  # var_name: Name of variables for categories.
  # The other inputs: graphic parameters.

  # Add a zero column to apply rowSums to the last explanatory variable.
  temp <- cbind(numeric(nrow(x)),x)

  # Horizontal line.
  temp_h_line <- 1:nrow(x)

  # Number of lines separating the areas above the horizontal line in the area plot.
  temp_num_lines_pstv <- ncol(x)

  # Colors for each area.
  temp_palette <- rev(rainbow(temp_num_lines_pstv)) 

  # Draw polygon.
  temp_total <- rowSums(temp) # Compute the height of stacked area above the x axis.
  plot.ts(temp_total, type = "n", xaxt=xaxt, xlab=xlab, ylab=ylab, ylim = c(0, max(temp_total)*ylim_scale),main=main) # Draw the line for the sum of positive components above the x axis.
  for(kk in 1:temp_num_lines_pstv){
    temp_subtotal <- rowSums(temp[,1:(temp_num_lines_pstv + 2 - kk)]) # Compute each line separating area above the x axis from the top to the bottom.
    polygon(c(temp_h_line[1], temp_h_line, temp_h_line[length(temp_h_line)]), c(0, temp_subtotal, 0), col = temp_palette[kk], lty=0) # Draw a polygon over the existing polygons. kk-temp_strt+1 starts from 2 if the intercept is negative.
    lines(temp_subtotal, lwd=0.5)    # 境界線を引きたければ
  }
  if (xaxt == "n"){
   # Draw the x axis.
   axis(side=1, at=c(1,round(nrow(temp)*c(1:n_label)/n_label)), labels=x_axis_label[c(1,round(nrow(temp)*c(1:n_label)/n_label))]) 
  }
  legend("topleft", legend=rev(var_name), col=temp_palette, pch=15)
  
  return(NULL) # Define the return of the function.
}

########## Compute acf for each mcmc sample. ################

acf_mcmc <- function(x, lag.max=NULL){
  # x: mcmc samples. A time series after the burnin period.
  # burnin: the number of draws in the burnin period.
  # lag.max: the maximum number of lags to compute acf.
  # y: A matrix to store the autocorrelation function for each sample. Rows: mcmc samples. Columns: lag lengths.
  # z: The number of lags.
  
  if (length(dim(x))>2){
    stop("The input must be mcmc samples of a time series.")
  }

  # Initialize y.
  y <- NULL
  
  # Stack mcmc samples of acfs.
  for (i in 1:(dim(x)[1])){
      temp <- acf(x[i,], lag.max=lag.max, plot=FALSE)
      y <- rbind(y,temp[[1]])
  }
  
  # Record the number of lags for each columns
  z <- temp[[4]]
    
  return(list(acfs=y,lags=z))
    
}

############## Compute reproduction numbers for a set of mcmc samples ##################

R_simu <- function(ms_R, H_expvals, M_trans, H_expvals_base=NULL, M_trans_base=NULL, W_abs_hum, dist_incub, D_NY, D_SE1=NULL, D_SE2=NULL, D_pre_SE1=NULL, EXP_L452R=NULL, EXP_MV_1st_VP=NULL, EXP_MV_2nd_VP=NULL, mdl_number=0){
  
  # ms_R: a mcmc sample after the burnin period created by a stan file. 
  # H_expvals: hypothetical values of real household expenditures. Rows: items; Columns: dates.
  # W_abs_hum: hypothetical values of absolute humidity.
  # M_trans: mobility.
  # dist_incub: Distribution of incubation periods.
  # D_NY: Dummy variables for the new year period.
  # D_SE1: Dummy for the first state of emergency.
  # D_SE2: Dummy for the second state of emergency.
  # D_pre_SE1: Dummy for the period before the first state of emergency.
  # EXP_L452R: the delta-variant share of new cases.
  # EXP_MV_1st_VP: 14-day backward moving average of the first-vaccination-only share of population.
  # EXP_MV_2nd_VP: 7-day backward moving average of the twice-vaccinated share of population.
  # H_expvals_base: Baseline series of H_expvals for comparison
  # M_trans_base: Baseline series of H_expvals for comparison
  # mdl_number: Model number. Relevant for model 20 or above.
  
  # Set the number of the mcmc samples after burnins. The first element of ms_R is a vector of mcmc samples.
  n_mcmc <- length(ms_R[[1]])
  
  # Specify the length of the sample period.
  nobs <- dim(H_expvals)[2]
  
  # Number of household expenditure items in the explanatory variables.
  n_hes <- dim(H_expvals)[1]
  
  # Initialize the matrix of explanatory variables.
  EXPVAR <- matrix(NA, nr = 2 + (n_hes+1)*2 + 3 + (n_hes+1)*3, nc = nobs-13)

  # Initialize the matrix of baseline explanatory variables for comparison.
  EXPVAR_base <- matrix(NA, nr = 2 + (n_hes+1)*2, nc = nobs-13)
  
  # Initialize the matrix of explanatory variables related to L452R and vaccinations.
  if (is.null(EXP_L452R)!=1){
    EXPVAR_L452R_VP <- matrix(NA, nr = 5, nc = nobs-13)
  }
  
  # Construct a matrix of explanatory variables, except dummy variables for the states of emergency.
  # Incubation periods are distributed between 1 day to 14 days. 
  # t is one day before the date of the reproduction number to simulate on the left-hand side.
  # t+13 is one day before the date of the reproduction number to simulate on the right-hand side.
  for (t in 1:(nobs-13)){
    
    EXPVAR[1,t] = D_NY[t:(t+13)] %*% dist_incub
    EXPVAR[2,t] = W_abs_hum[t:(t+13)] %*% dist_incub
    EXPVAR[2 + 1:n_hes,t] = H_expvals[,t:(t+13)] %*% dist_incub
    EXPVAR[2+n_hes+1,t] = M_trans[t:(t+13)] %*% dist_incub
    EXPVAR[2+n_hes+1 + 1:n_hes,t] = H_expvals[,t:(t+13)] %*% (dist_incub * W_abs_hum[t:(t+13)])
    EXPVAR[2+n_hes*2+1 + 1,t] = M_trans[t:(t+13)] %*% (dist_incub * W_abs_hum[t:(t+13)])
    
    if (is.null(D_SE1)){
      # If D_SE1 is set to NULL, insert zeros into D_SE1.  
      EXPVAR[2 + (n_hes+1)*2 + 1, t] = 0
      EXPVAR[2 + (n_hes+1)*2 + 3 + 1:n_hes, t] = 0
      EXPVAR[2 + (n_hes+1)*2 + 3 + (n_hes+1), t] = 0
    }else{
      # Define the explanatory variables including D_SE1.
      EXPVAR[2 + (n_hes+1)*2 + 1, t] = D_SE1[t:(t+13)] %*% dist_incub
      EXPVAR[2 + (n_hes+1)*2 + 3 + 1:n_hes, t] = H_expvals[,t:(t+13)] %*% (dist_incub * D_SE1[t:(t+13)])
      EXPVAR[2 + (n_hes+1)*2 + 3 + (n_hes+1), t] = M_trans[t:(t+13)] %*% (dist_incub * D_SE1[t:(t+13)])
    }

    if (is.null(D_SE2)){
      # If D_SE2 is set to NULL, insert zeros into D_SE2.  
      EXPVAR[2 + (n_hes+1)*2 + 2, t] = 0
      EXPVAR[2 + (n_hes+1)*2 + 3 + (n_hes+1) + 1:n_hes, t] = 0
      EXPVAR[2 + (n_hes+1)*2 + 3 + (n_hes+1)*2, t] = 0
    }else{
      # Define the explanatory variables including D_SE2.
      EXPVAR[2 + (n_hes+1)*2 + 2, t] = D_SE2[t:(t+13)] %*% dist_incub
      EXPVAR[2 + (n_hes+1)*2 + 3 + (n_hes+1) + 1:n_hes, t] = H_expvals[,t:(t+13)] %*% (dist_incub * D_SE2[t:(t+13)])
      EXPVAR[2 + (n_hes+1)*2 + 3 + (n_hes+1)*2, t] = M_trans[t:(t+13)] %*% (dist_incub * D_SE2[t:(t+13)])
    }

    if (is.null(D_pre_SE1)){
      # If D_SE2 is set to NULL, insert zeros into D_SE2.  
      EXPVAR[2 + (n_hes+1)*2 + 3, t] = 0
      EXPVAR[2 + (n_hes+1)*2 + 3 + (n_hes+1)*2 + 1:n_hes, t] = 0
      EXPVAR[2 + (n_hes+1)*2 + 3 + (n_hes+1)*3, t] = 0
    }else{
      # Define the explanatory variables including D_SE2.
      EXPVAR[2 + (n_hes+1)*2 + 3, t] = D_pre_SE1[t:(t+13)] %*% dist_incub
      EXPVAR[2 + (n_hes+1)*2 + 3 + (n_hes+1)*2 + 1:n_hes, t] = H_expvals[,t:(t+13)] %*% (dist_incub * D_pre_SE1[t:(t+13)])
      EXPVAR[2 + (n_hes+1)*2 + 3 + (n_hes+1)*3, t] = M_trans[t:(t+13)] %*% (dist_incub * D_pre_SE1[t:(t+13)])
    }
    
        
    # Construct baseline explanatory variables.
    if (is.null(H_expvals_base)==0 && t+13 <= dim(H_expvals_base)[2]){
      EXPVAR_base[2 + 1:n_hes,t] = H_expvals_base[,t:(t+13)] %*% dist_incub
      EXPVAR_base[2+n_hes+1,t] = M_trans_base[t:(t+13)] %*% dist_incub
      EXPVAR_base[2+n_hes+1 + 1:n_hes,t] = H_expvals_base[,t:(t+13)] %*% (dist_incub * W_abs_hum[t:(t+13)])
      EXPVAR_base[2+n_hes*2+1 + 1,t] = M_trans_base[t:(t+13)] %*% (dist_incub * W_abs_hum[t:(t+13)])
    }
    
    # Construct a matrix of explanatory variables related to L452R and vaccinations.
    if (mdl_number==21){
      EXPVAR_L452R_VP[1,t] <- EXP_L452R[t:(t+13)] %*% dist_incub
      EXPVAR_L452R_VP[2,t] <- (EXP_MV_1st_VP[t:(t+13)] * EXP_L452R[t:(t+13)]) %*% dist_incub
      EXPVAR_L452R_VP[3,t] <- (EXP_MV_2nd_VP[t:(t+13)] * EXP_L452R[t:(t+13)]) %*% dist_incub
      EXPVAR_L452R_VP[4,t] <- (EXP_MV_1st_VP[t:(t+13)] * (1-EXP_L452R[t:(t+13)])) %*% dist_incub
      EXPVAR_L452R_VP[5,t] <- (EXP_MV_2nd_VP[t:(t+13)] * (1-EXP_L452R[t:(t+13)])) %*% dist_incub
    }
  }
  

  # R: The fitted reproduction number.
  # Initialize a matrix for R in each mcmc sample.
  R <- matrix(NA, nr=n_mcmc, nc=nobs-13-6)

  # Apply each mcmc sample to the explanatory variables.
  for (i in 1:n_mcmc){
    
    # Extract coefficients from a mcmc sample.
    gama <- ms_R$gama[i]
    coef_NY <- ms_R$coef_NY[i]
    coef_AH <- ms_R$coef_AH[i]
    scl <- ms_R$scl[i,]
    scl_AH <- ms_R$scl_AH[i,]
    coef_SE1 <- ms_R$coef_SE1[i]
    coef_SE2 <- ms_R$coef_SE2[i]
    coef_pre_SE1 <- ms_R$coef_pre_SE1[i]
    scl_AH_SE1 <- ms_R$scl_AH_SE1[i,]
    scl_AH_SE2 <- ms_R$scl_AH_SE2[i,]
    scl_AH_pre_SE1 <- ms_R$scl_AH_pre_SE1[i,]
    
    if (is.null(EXP_L452R)!=1){
      # Extract coefficients related to L452R and vaccination from a mcmc sample.
      coef_L452R <- ms_R$coef_L452R[i]
      coef_1st_VP_L452R <- ms_R$coef_1st_VP_L452R[i]
      coef_2nd_VP_L452R <- ms_R$coef_2nd_VP_L452R[i]
      coef_1st_VP_orgn <- ms_R$coef_1st_VP_orgn[i]
      coef_2nd_VP_orgn <- ms_R$coef_2nd_VP_orgn[i]
      
      if(mdl_number == 20){
        coef_total_L452R_VP <- ms_R$coef_total_L452R_VP[i]
      }
    }
    
    # mean_R: The mean of the normal distribution of R in each period.
    # Initialize a vector for mean_R.
    mean_R <- rep(NA, nobs-13)
    

    # t is the current period for mean_R.
    # t + 6 + 1 is the current period for R.
    for (t in 1:(nobs-13)){
      mean_R[t] <- gama +  coef_NY * EXPVAR[1,t] + coef_AH * EXPVAR[2,t] + t(scl) %*% EXPVAR[2 + 1:(n_hes+1),t] + t(scl_AH) %*% EXPVAR[2+n_hes+1 + 1:(n_hes+1),t] +
        + coef_SE1 * EXPVAR[2 + (n_hes+1)*2 + 1,t] + coef_SE2 * EXPVAR[2 + (n_hes+1)*2 + 2,t] +  coef_pre_SE1 * EXPVAR[2 + (n_hes+1)*2 + 3,t] +
        + t(scl_AH_SE1) %*% EXPVAR[2 + (n_hes+1)*2 + 3 + 1:(n_hes+1),t] +
        + t(scl_AH_SE2) %*% EXPVAR[2 + (n_hes+1)*2 + 3 + (n_hes+1) + 1:(n_hes+1),t] + 
        + t(scl_AH_pre_SE1) %*% EXPVAR[2 + (n_hes+1)*2 + 3 + (n_hes+1)*2 + 1:(n_hes+1),t]
      
      if (mdl_number==20){
        # Add the effect of L452R and vaccination.
        mean_R[t] <- mean_R[t] + sum(dist_incub * coef_total_L452R_VP * log(coef_L452R * (1 - coef_1st_VP_L452R * EXP_MV_1st_VP[t:(t+13)] - coef_2nd_VP_L452R * EXP_MV_2nd_VP[t:(t+13)]) * EXP_L452R[t:(t+13)] + (1 - coef_1st_VP_orgn * EXP_MV_1st_VP[t:(t+13)] - coef_2nd_VP_orgn * EXP_MV_2nd_VP[t:(t+13)]) * (1-EXP_L452R[t:(t+13)])))
      }

      if (mdl_number==21){
        # Add the effect of L452R and vaccination.
        mean_R[t] <- mean_R[t] + coef_L452R * EXPVAR_L452R_VP[1,t] - coef_1st_VP_L452R * EXPVAR_L452R_VP[2,t] - coef_2nd_VP_L452R * EXPVAR_L452R_VP[3,t] - coef_1st_VP_orgn * EXPVAR_L452R_VP[4,t] - coef_2nd_VP_orgn * EXPVAR_L452R_VP[5,t]
      }

      if (t>6){
        R[i, t-6] <- sum(mean_R[(t-6):t]) # R is the ratio between the numbers of infections one day after today and a week before, raised to the power of 5/7.
      }
    }
  }

  
  # Compute posterior means of coefficients.
  
  gama_pm <- mean(ms_R$gama)
  coef_NY_pm <- mean(ms_R$coef_NY)
  coef_AH_pm <- mean(ms_R$coef_AH)
  coef_SE1_pm <- mean(ms_R$coef_SE1)
  coef_SE2_pm <- mean(ms_R$coef_SE2)
  coef_pre_SE1_pm <- mean(ms_R$coef_pre_SE1)
  
  scl_pm <- apply(ms_R$scl, 2, mean)
  scl_AH_pm <- apply(ms_R$scl_AH, 2, mean)
  scl_AH_SE1_pm <- apply(ms_R$scl_AH_SE1, 2, mean)
  scl_AH_SE2_pm <- apply(ms_R$scl_AH_SE2, 2, mean)
  scl_AH_pre_SE1_pm <- apply(ms_R$scl_AH_pre_SE1, 2, mean)
  
  if (is.null(H_expvals_base)){
    # If there is no alternative explanatory variable, record the level contribution from each explanatory variable.
    
    if (is.null(EXP_L452R)){
      # Initialize a matrix to record the level effects of explanatory variables on R evaluated by posterior mean coefficients.
      comp_effect <- matrix(NA, nr=2+(n_hes+1)*3, nc=nobs-13-6)
    }else{
      
      # Compute posterior means of additional coefficients.
      
      coef_L452R_pm <- mean(ms_R$coef_L452R)
      coef_1st_VP_L452R_pm <- mean(ms_R$coef_1st_VP_L452R)
      coef_2nd_VP_L452R_pm <- mean(ms_R$coef_2nd_VP_L452R)
      coef_1st_VP_orgn_pm <- mean(ms_R$coef_1st_VP_orgn)
      coef_2nd_VP_orgn_pm <- mean(ms_R$coef_2nd_VP_orgn)

      if (mdl_number==20){
        # Initialize a matrix to record the level effects of explanatory variables on R evaluated by posterior mean coefficients.
        comp_effect <- matrix(NA, nr=2+(n_hes+1)*3+1, nc=nobs-13-6)
        # Initialize a vector to record the daily effects of explanatory variables related to L452R and vaccinations on R evaluated by posterior mean coefficients.
        daily_comp_effect <- rep(NA, nobs-13)
        
        # Extract the level coefficient for the log of the net effect of L452R and vaccinations.
        coef_total_L452R_VP_pm <- mean(ms_R$coef_total_L452R_VP)
        
      }else if (mdl_number==21){
        # Initialize a matrix to record the level effects of explanatory variables on R evaluated by posterior mean coefficients.
        comp_effect <- matrix(NA, nr=2+(n_hes+1)*3+3, nc=nobs-13-6)
      }
      
    }
      
    # Record the level contribution from each explanatory variable.
    for (t in 7:(nobs-13)){
      
      comp_effect[1,t-6] <- sum(gama_pm + coef_NY_pm * EXPVAR[1,(t-6):t] + coef_SE1_pm * EXPVAR[2 + (n_hes+1)*2 + 1,(t-6):t] + coef_SE2_pm * EXPVAR[2 + (n_hes+1)*2 + 2,(t-6):t] +  coef_pre_SE1_pm * EXPVAR[2 + (n_hes+1)*2 + 3,(t-6):t])
      comp_effect[2,t-6] <- sum(coef_AH_pm * EXPVAR[2,(t-6):t])
      comp_effect[2+1:(n_hes+1),t-6] <- rowSums((t(rep(1,7)) %x% scl_pm) * EXPVAR[2 + 1:(n_hes+1),(t-6):t])
      comp_effect[2+(n_hes+1)+1:(n_hes+1), t-6] <- rowSums((t(rep(1,7)) %x% scl_AH_pm) * EXPVAR[2 + n_hes+1 + 1:(n_hes+1),(t-6):t])
      comp_effect[2+(n_hes+1)*2+1:(n_hes+1), t-6] <- rowSums((t(rep(1,7)) %x% scl_AH_SE1) * EXPVAR[2 + (n_hes+1)*2 + 3 + 1:(n_hes+1),(t-6):t] +
                                                               + (t(rep(1,7)) %x% scl_AH_SE2) * EXPVAR[2 + (n_hes+1)*2 + 3 + (n_hes+1) + 1:(n_hes+1),(t-6):t] +
                                                               + (t(rep(1,7)) %x% scl_AH_pre_SE1) * EXPVAR[2 + (n_hes+1)*2 + 3 + (n_hes+1)*2 + 1:(n_hes+1),(t-6):t])
      if (mdl_number==20){
        
        if (t==7){
          # Compute the daily effects of explanatory variables related to L452R and vaccinations for the first six days of the sample period.
          for (s in 1:7){
            daily_comp_effect[s] <- sum(dist_incub * coef_total_L452R_VP_pm * log(coef_L452R_pm * (1 - coef_1st_VP_L452R_pm * EXP_MV_1st_VP[s:(s+13)] - coef_2nd_VP_L452R_pm * EXP_MV_2nd_VP[s:(s+13)]) * EXP_L452R[s:(s+13)] + (1 - coef_1st_VP_orgn_pm * EXP_MV_1st_VP[s:(s+13)] - coef_2nd_VP_orgn_pm * EXP_MV_2nd_VP[s:(s+13)]) * (1-EXP_L452R[s:(s+13)])))
          }
        }else{
          # Compute the daily effects of explanatory variables related to L452R and vaccinations for each day.
          daily_comp_effect[t] <- sum(dist_incub * coef_total_L452R_VP_pm * log(coef_L452R_pm * (1 - coef_1st_VP_L452R_pm * EXP_MV_1st_VP[t:(t+13)] - coef_2nd_VP_L452R_pm * EXP_MV_2nd_VP[t:(t+13)]) * EXP_L452R[t:(t+13)] + (1 - coef_1st_VP_orgn_pm * EXP_MV_1st_VP[t:(t+13)] - coef_2nd_VP_orgn_pm * EXP_MV_2nd_VP[t:(t+13)]) * (1-EXP_L452R[t:(t+13)])))
        }
        
        # Add the effect of L452R and vaccination.
        comp_effect[2+(n_hes+1)*3+1,t-6] <- sum(daily_comp_effect[(t-6):t])
        
      } else if (mdl_number==21){
        
        # Add the effect of L452R and vaccination.
        comp_effect[2+(n_hes+1)*3+1,t-6] <- sum(coef_L452R_pm * EXPVAR_L452R_VP[1,(t-6):t])
        comp_effect[2+(n_hes+1)*3+2,t-6] <- - sum(coef_1st_VP_L452R_pm * EXPVAR_L452R_VP[2,(t-6):t] + coef_1st_VP_orgn_pm * EXPVAR_L452R_VP[4,(t-6):t])
        comp_effect[2+(n_hes+1)*3+3,t-6] <- - sum(coef_2nd_VP_L452R_pm * EXPVAR_L452R_VP[3,(t-6):t] + coef_2nd_VP_orgn_pm * EXPVAR_L452R_VP[5,(t-6):t])
      }
      
    }
  }else{
    # Initialize a matrix to record the difference in the posterior mean of R between the two sets of explanatory variables.
    comp_effect <- matrix(NA, nr=n_hes+1, nc=nobs-13-6)
    
    # Record the difference in the posterior mean of R between the two sets of explanatory variables.
    for (t in 7:(nobs-13)){
      
      if (t+13 <= dim(EXPVAR_base)[2]){
        comp_effect[,t-6] <- rowSums((t(rep(1,7)) %x% scl_pm) * (EXPVAR[2 + 1:(n_hes+1),(t-6):t]-EXPVAR_base[2 + 1:(n_hes+1),(t-6):t]) + (t(rep(1,7)) %x% scl_AH_pm) * (EXPVAR[2+n_hes+1 + 1:(n_hes+1),(t-6):t] - EXPVAR_base[2+n_hes+1 + 1:(n_hes+1),(t-6):t]))
      }
    }
    
  }

  return(list(R=R, comp_effect=comp_effect)) 
  
}

