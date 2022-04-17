#'  CoFESNARXdata
#'
#'  \code{CoFESNARXdata} returns the lead/lag series necessary to run the CoFESNARX
#'
#'  This function transforms the target and exegenous series variables for the
#'  CoFESNARX model.  variables series, and the number of desired lags for each
#'  group respectively and then returns the requested lag series for each in a
#'  single  data set.  Ydelay/Xdelay are vectors that holds the consecutive
#'  delays required for the target variable series (ex 1:3 or 4:5).
#'
#' @param yy numeric vectors containing target data
#' @param xx numeric vectors containing feature data
#' @param ydelay integer representing the desired AR order for y
#' @param xdelay integer representing the desired AR order for x
#' @param delay integers representing n, the size of the prediction window into the future
#' @param date boolean value to indicate if you data includes dates
#' @return returns the target combined with the original and lagged exegenous variables in one database.
#'
#'
#' @export

CoFESNARXdata <-function(yy, xx, ydelay, xdelay, delay=1, date=T){
  # create a sequence up to the number of delays for the target
  lags <- seq(ydelay)
  # create column names for the lags create in the previous line.
  lag_names <- paste("lag", formatC(lags, width = nchar(max(lags)), flag = "0")
                     , sep = "_")
  # A function to create target.
  lag_functions <- stats::setNames(paste("dplyr::lag(., ", lags, ")"), lag_names)
  # A function to create lead series for target.
  lead_functions <- stats::setNames(paste("dplyr::lead(., ", delay, ")"), 'target')

  ifelse(date,
         # Apply lag function to the target series and create lags
         y<-yy %>% dplyr::mutate_at(vars( colnames(yy)[2] )
                                    , funs_(c(lead_functions,lag_functions))),
         y<-yy %>% dplyr::mutate_at(vars( colnames(yy)[1] )
                                    , funs_(c(lead_functions,lag_functions))))

  # Remove NA's
  y<-stats::na.omit(y)
  # create a sequence up to the number of delays for the exegenous series
  lagsx <- seq(xdelay+1)
  # create column names for the lags create in the previous line.
  lag_namesx <- paste("lag", formatC(lagsx, width = nchar(max(lagsx))
                                     , flag = "0"), sep = "_")
  # A function to create lags and adds their names.
  lag_functionsx <- stats::setNames(paste("dplyr::lag(., ", lagsx, ")"), lag_namesx)
  # Apply lag function to the target series and create lags
  x<-xx %>% dplyr::mutate_at(vars(colnames(xx)[2:ncol(xx)] ), funs_(lag_functionsx))
  # Remove NA's
  x<-stats::na.omit(x)
  # Merge X and Y series
  abc<-merge(y, x)
  # remove NA's
  abc <- stats::na.omit(abc)
  # return boty X and Y
  return(abc)
}

#'  CoFESSNARXdata
#'
#'  \code{CoFESSNARXdata} returns the lead/lag series necessary to run the CoFESSNARX
#'
#'  This function transforms the target and exegenous series variables for the
#'  CoFESSNARX model.  Variables series, and the number of desired lags for each
#'  group respectively and then returns the requested lag series arlong with the
#'  seasonally lagged verson of teh original series for each in a single  data set.
#'
#' @param yy numeric vectors containing target data
#' @param xx numeric vectors containing feature data
#' @param ydelay integer representing the desired AR order for y
#' @param xdelay integer representing the desired AR order for x
#' @param delay integers representing n, the size of the prediction window into the future
#' @param s integers representing the seasonality desired.  Can be expressed as a
#' list for P>1.
#' @param date boolean value to indicate if you data includes dates
#' @return returns the target combined with the seasonallly lagged target variable,and the original
#' and lagged exegenous variables in one database.
#'
#' @export
CoFESSNARXdata <-function(yy, xx, ydelay, xdelay, delay=1, s=NULL, date=T){
  # If s=Null, then ignore the seasonal portion and boils down to prepareNARXdata.
  if(is.null(s)){
    # Traditional series lags.
    lags <- seq(ydelay)
  }
  else {
    # Add S to the end of the traditional series.
    lags <- c(seq(ydelay),s)
  }
  # create column names for the lags create in the previous line.
  lag_names <- paste("lag", formatC(lags, width = nchar(max(lags)), flag = "0")
                     , sep = "_")
  # A function to create target.
  lag_functions <- stats::setNames(paste("dplyr::lag(., ", lags, ")"), lag_names)
  # A function to create lead series for target.
  lead_functions <- stats::setNames(paste("dplyr::lead(., ", delay, ")"), 'target')

  ifelse(date,
  # Apply lag function to the target series and create lags
  y<-yy %>% dplyr::mutate_at(vars( colnames(yy)[2] )
                             , funs_(c(lead_functions,lag_functions))),
  y<-yy %>% dplyr::mutate_at(vars( colnames(yy)[1] )
                             , funs_(c(lead_functions,lag_functions))))
  # Remove NA's
  y<-stats::na.omit(y)
  # create a sequence up to the number of delays for the exegenous series
  lagsx <- seq(xdelay+1)
  # create column names for the lags create in the previous line.
  lag_namesx <- paste("lag", formatC(lagsx, width = nchar(max(lagsx))
                                     , flag = "0"),
                      sep = "_")
  # A function to create lags and adds their names.
  lag_functionsx <- stats::setNames(paste("dplyr::lag(., ", lagsx, ")"), lag_namesx)
  # Apply lag function to the target series and create lags
  x<-xx %>% dplyr::mutate_at(vars(colnames(xx)[2:ncol(xx)] ), funs_(lag_functionsx))
  # Remove NA's
  x <- stats::na.omit(x)
  # Merge X and Y series
  abc <- merge(y, x)
  # remove NA's
  abc <- stats::na.omit(abc)
  # return both X and Y
  return(abc)
}


#' returnTARGET
#'
#' \code{returnTARGET} returns TARGET created by CoFESNARXdata/CoFESSNARXdata
#'
#' This function returns the target series created by CoFESNARXdata and CoFESSNARXdata.
#'
#' @param data numeric vectors
#' @return returns the target
#'
#' @export
returnTARGET <-function(data){
  y <- data %>%  dplyr::select(target)
  #y <- as.matrix(y)
  return(y)
}



#' returnX
#'
#' \code{returnX} returns exogenous series
#'
#' This function returns the exogenous series created by CoFESNARXdata and CoFESSNARXdata.
#'
#' @param data numeric vectors
#' @param date Boolen if date is included
#' @return returns the exogenous series
#'
#' @export
returnX <-function(data, date=T){
  ifelse(date,
  x<- data %>% dplyr::select(c(-date,-target)),
  x<- data %>% dplyr::select(-target)
  )
  return(x)
}
