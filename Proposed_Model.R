##################### LIBRARY ###########################################################
library(tseries)
library(forecast)
library(Metrics)
library(nonlinearTseries)
library(lmtest)
####################### SanJuan Data ########################

data = read.csv("Sanjuan_data_weekly.csv", stringsAsFactors = F)
df=data[,c("Cases","Rain")]
con_tr = df$Cases[37:1128]
xreg_tr = df$Rain[37:1128]
con_tst = df$Cases[1129:1180]

##################### Iquitos data ###########################

# data = read.csv("Iquitos_data_weekly.csv", stringsAsFactors = F)
# df=data[,c("Cases","Rain")]
# con_tr = df$Cases[1:520]
# xreg_tr = df$Rain[1:520]
# con_tst = df$Cases[521:572]

################# Statistical Tests ############################

kpss.test(con_tr, null = "Trend") 
nonlinearityTest(con_tr, verbose = TRUE)

################# Granger Causality test ############################
cases_series=diff(con_tr)
rain_series=diff(xreg_tr)

grangertest(cases_series~rain_series, order=2)
grangertest(rain_series~cases_series, order=2)

####################### Proposed WARNN ###########################

source("warnn.R")
fit_warnn = WaveletFittingnar(ts(con_tr), Waveletlevels = floor(log(length(con_tr))), boundary = "periodic", 
            FastFlag = TRUE, MaxARParam = 10, NForecast = 52)
fore_warnn = as.data.frame(fit_warnn$Finalforecast, h = 52)
forecast::accuracy(fore_warnn$`fit_warnn$Finalforecast`, con_tst)
smape(con_tst, fore_warnn$`fit_warnn$Finalforecast`)*100
mase(con_tst, fore_warnn$`fit_warnn$Finalforecast`)

####################### Proposed WARNNX ##########################

source("warnnx.R")
fit_warnnx = WaveletFittingnar(ts(con_tr), Waveletlevels = floor(log(length(con_tr))), 
             boundary = "periodic", FastFlag = TRUE, MaxARParam = 10, NForecast = 52)
fore_warnnx = as.data.frame(fit_warnnx$Finalforecast, h = 52)
forecast::accuracy(fore_warnnx$`fit_warnnx$Finalforecast`, con_tst)
smape(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)*100
mase(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)

