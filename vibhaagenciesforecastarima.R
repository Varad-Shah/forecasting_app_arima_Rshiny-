library(readxl)
library(dplyr)
library(fpp2)
library(purrr)
library(data.table)
library(openxlsx)

##################################################

jul<-read_xlsx(".\\monthly Vibha Agencies Forecast july-2018.xlsx")
aug<-read_xlsx(".\\monthly Vibha Agencies Forecast aug-2018.xlsx")
sep<-read_xlsx(".\\monthly Vibha Agencies Forecast sep-2018.xlsx")
octnov<-read_xlsx(".\\monthly Vibha Agencies Forecast oct-nov-2018.xlsx")
decjanfeb<-read_xlsx(".\\monthly Vibha Agencies Forecast dec-jan-feb-2018-19.xlsx")

fj1<-full_join(jul,aug)
fj2<-full_join(sep,octnov)
fj3<-full_join(fj1,fj2)
finaljoin<-full_join(fj3,decjanfeb) #join all data
finaljoin[is.na(finaljoin)]<-0      #remove NA values

##################################################

only_numbers<-finaljoin[,-c(1:6)]   #remove columns which contain characters
transpose<-t(only_numbers)          # dates of each part are arranged vertially for converting to time series

ts_1<-ts(transpose,start = c(2018,7),frequency = 12)  #convert to timeseries
ts_2<-t(ts_1)                                         #transpose back to original position

the_function <- function(x){
  modelingarima <- auto.arima(x)
  predictingarima <- forecast(modelingarima,h=1)
  dataframe1=as.data.frame(predictingarima)
  return(dataframe1)
}                                            #function to predict data for next month using ARIMA

resulta1<-apply(ts_2,1,the_function)         #apply the function to each row
resulta2<-reduce(resulta1,bind_rows)         

resulta2$`Point Forecast`<-round(resulta2$`Point Forecast`)
colnames(resulta2)[1] <- "2019 mar"

final=cbind(finaljoin,resulta2)

write.xlsx(final, file = "final.xlsx",row.names=FALSE)

final<-read_xlsx(".\\final.xlsx")

##################################################

final_no_lowhigh<-final[,-c(16:19)]             #remove the LO HI of ARIMA
dtforhist<-final_no_lowhigh
dtforhist$Part.No.<-paste0(dtforhist$`Part No.`,sep="-",dtforhist$`Plant Code`,sep="-",dtforhist$`Plant Buyer`)
dtforhist<-dtforhist[,-c(1:6)]                  #remove columns which contain unnecessary characters
rownames(dtforhist) <- dtforhist$Part.No.       #set the combined column as rowname to transpose
dtforhist<-as.data.frame(dtforhist)             
dtforhist <- dtforhist[,-10]                    #remove the combined column
dtforhist<-t(dtforhist)

write.xlsx(dtforhist, file = "dtforhist.xlsx",row.names=TRUE)

dtforhist<-read_xlsx(".\\dtforhist.xlsx")
