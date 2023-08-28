#'Cointegration Based Support Vector Regression Model
#'
#' @param data  A cointegated time series data
#' @param type Type of cointegration test to be used. Either "trace" or "eigen" value based
#' @param t  Partition value for spliting the data set into training and testing
#' @param l Number of lags used for the support vector machine model fitting
#' @param ker.funct The available choices of kernel functions like radial basis, linear,   polynomial and sigmoid for fitting support vector regression. By default radial basis function works
#' @param svm.type SVM can be used as a regression machine. User can apply eps-regression or nu-regression. By default the ECTSVR uses eps-regression
#' @param verbose It is a logical parameter, represented by Boolean variables. This argument accepts either TRUE or FALSE values.
#'
#' @author Pankaj Das
#'
#' @description
#' The cointegration based support vector regression model is a combination of error correction model and support vector regression (http://krishi.icar.gov.in/jspui/handle/123456789/72361). This hybrid model allows the researcher to make use of the information extracted by the cointegrating vector as an input in the support vector regression model
#'
#' @references
#'
#' 1. Das, P. (2019). Study On Machine Learning Techniques Based Hybrid Model for Forecasting in Agriculture. Unpublished Ph.D. Thesis. (http://krishikosh.egranth.ac.in/handle/1/5810147805).
#'
#' 2. Das, P., Jha,G. K. and Lama, A. (2021). An Improved Cointegration based Time Delay Neural Network Model for Price Forecasting. Journal of the Indian Society of Agricultural Statistics 75(3) 2021 187–192 (http://krishi.icar.gov.in/jspui/handle/123456789/72361)
#'
#' @details
#' ECTSVR uses the concept of cointegration based Timedelay Neural network model proposed by Das (2019). First the cointegration of the data series is identified by Johansen cointegration test. Then error correction model is fitted for the estimation of parameters i.e. Beta and error coreection term (ECT). The estimated ECT is used as a auxiliary information in support vector regression model fitting. Then the support vector regression model is used foresting of the data series.
#'
#' @return Prediction performance of the ECTSVR model with outsample predition values
#' @export
#' @import vars
#' @import WeightSVM
#' @import urca
#' @import stats
#' @importFrom vars VARselect
#' @importFrom urca ca.jo
#' @importFrom WeightSVM wsvm
#' @note
#' The variables in data should be cointegrated and the dependent variable should in first in data. Otherwise result will be statistically validated.
#' @examples
#' data(finland)
#' data_example<-finland[,4:3]
#' ECTSVR(data_example,"trace",0.8,2, "radial","eps-regression",verbose = FALSE)
#'
ECTSVR <- function(data,type="", t,l, ker.funct="",svm.type="",verbose = FALSE)
{
  coin=data
  length_vec<-nrow(coin)
  #fit a VAR model with appropiate lag
  lag_no=vars::VARselect(coin,lag.max = 12,type = "const")$selection
  k=as.numeric(lag_no[1])
  # conduct cointegration test (Eigen test)
  cointest=urca::ca.jo(coin,K=k,type =type, ecdet = "const", spec = "transitory")
  # make a ca.jo object to convert in VECM and VAR (lags K should be minimum 2)
  summary(cointest)
  #Run VECM
  vecm=urca::cajorls(cointest) # convert in vecm
  # Extract error correction term coefficients (ECT)
  vecm$rlm$coefficients[1,1] # for first variable
  vecm$rlm$coefficients[1,2] # for second variable
  if(verbose){print(vecm$beta)}
  #print(vecm$beta)
  aux_var<-0
  ##vector of vecm$rlm$coefficients[1,2] # for second variable
  for(i in 1:length_vec)
  {
    aux_var[i]<-vecm$rlm$coefficients[1,2]
  }
  #cointegration
  #ANN
  #setting
  #ann forecasting for ORIGNAL DATA SET
  yts<-as.matrix(coin[,1])
  lent_data=length(yts)
  y <- yts[1:(lent_data-l),]
  x <- yts[(1+l):lent_data,]
  dt <- data.frame(y,x)
  len_data=length(dt$y)
  split_train=ceiling(t*len_data)
  r_train=(split_train)
  traindata=dt[1:r_train,]
  testdata=dt[(r_train+1):len_data,]
  #data_X <- as.matrix(aux_var)
  ft<- vecm$rlm$coefficients[1,2]
  traindatar=rep(ft,length(traindata$y))

  #fit.y0 <- svm(y~., data=traindata, kernel=ker.funct,type=svm.type)
  fit.y1 <- WeightSVM::wsvm(y ~ ., data=traindata, kernel=ker.funct, type=svm.type, weight = abs(traindatar))
  #summary(fit.y0)
  summary(fit.y1)
  predicted_out1<- stats::predict(fit.y1,traindata)
  predicted_out2<- stats::predict(fit.y1,testdata)
  if(verbose){print(predicted_out1)}
  if(verbose){print(predicted_out1)}
  #print(predicted_out1)
  #print(predicted_out2)
  # summarize accuracy
  MSE_In_ECTSVR <- mean((traindata$y - predicted_out1)^2)
  RMSE_In_ECTSVR <- sqrt(MSE_In_ECTSVR)
  MSE_out_ECTSVR <- mean((testdata$y - predicted_out2)^2)
  RMSE_out_ECTSVR <- sqrt(MSE_out_ECTSVR)
  #mean absolute deviation (MAD)
  MAD_In_ECTSVR <- mean(abs(traindata$y - predicted_out1))
  MAD_out_ECTSVR <- mean(abs(testdata$y - predicted_out2))
  #Mean absolute percent error (MAPE)
  MAPE_In_ECTSVR <- mean(abs((traindata$y - predicted_out1)/traindata$y))
  MAPE_out_ECTSVR <- mean(abs((testdata$y - predicted_out2)/testdata$y))
  ECTSVR_Predict <- predicted_out2
  outsample_accuracy <- cbind(RMSE_In_ECTSVR,RMSE_out_ECTSVR,MAD_In_ECTSVR,MAD_out_ECTSVR,MAPE_In_ECTSVR,MAPE_out_ECTSVR)
  output_f=list(outsample_accuracy,ECTSVR_Predict)
  return(output_f)
}
