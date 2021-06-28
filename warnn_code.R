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

source("wnnar.R")
fit_wnnar = WaveletFittingnar(ts(con_tr), Waveletlevels = floor(log(length(con_tr))), boundary = "periodic", 
            FastFlag = TRUE, MaxARParam = 10, NForecast = 52)
fore_wnnar = as.data.frame(fit_wnnar$Finalforecast, h = 52)
forecast::accuracy(fore_wnnar$`fit_wnnar$Finalforecast`, con_tst)
smape(con_tst, fore_wnnar$`fit_wnnar$Finalforecast`)*100
mase(con_tst, fore_wnnar$`fit_wnnar$Finalforecast`)

####################### Proposed WARNNX ##########################

source("wavelet_nar.R")
fit_wnnarx = WaveletFittingnar(ts(con_tr), Waveletlevels = floor(log(length(con_tr))), 
             boundary = "periodic", FastFlag = TRUE, MaxARParam = 10, NForecast = 52)
fore_wnnarx = as.data.frame(fit_wnnarx$Finalforecast, h = 52)
forecast::accuracy(fore_wnnarx$`fit_wnnarx$Finalforecast`, con_tst)
smape(con_tst, fore_wnnarx$`fit_wnnarx$Finalforecast`)*100
mase(con_tst, fore_wnnarx$`fit_wnnarx$Finalforecast`)

