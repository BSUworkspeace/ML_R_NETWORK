library(fpp2)
library(xlsx)
library(tseries)


alldata.month.volts = ts(alldata[,"vol"],frequency = 12,start = c(2004,7))
autoplot(alldata.month.volts)
alldata.season.volts = ts(alldata.month.volts,frequency = 4,start = c(2004,7))
autoplot(alldata.season.volts)
alldata.year.volts =  ts(
  alldata%>% group_by(year) %>%  
    summarise(vol=sum(vol)),frequency = 1,start = c(2004))
autoplot(alldata.year.volts)

## process preice data

alldata.month.prts = ts(alldata[,"price"],frequency = 12,start = c(2004,7))
autoplot(alldata.month.prts)
alldata.season.prts = ts(alldata[,"price"],frequency = 4,start = c(2004,7))
autoplot(alldata.season.prts)
alldata.year.prts =  ts(
  alldata%>% group_by(year) %>%  
    summarise(price=sum(price)),frequency = 1,start = c(2004))
autoplot(alldata.year.prts)


mass = read.xlsx(xlsx,sheetIndex = 1,startRow = 2)
food = read.xlsx(xlsx,sheetIndex = 2,startRow = 2)
drug = read.xlsx(xlsx,sheetIndex = 3,startRow = 2)

proc_data <- function(xlsx,foodnames,indexs) {
datas = read.xlsx(xlsx,sheetIndex = indexs,startRow = 2)
alldata = data.frame(
  vol  = datas[,c(paste("VOL_FRESH." , foodnames,sep = ""))],
  pr = datas[,c(paste("PR_FRESH.",foodnames,sep = ""))],
  date = as.Date(time(ts(mass$PR_FRESH.DOUGHNUTS,frequency = 12 ,start = c(2004,7))))
)%>%
  separate(date ,into=c("year","month","day"),sep = "-",remove = F)
alldata$turnover = alldata$vol*alldata$pr
alldata$quarters = quarters(alldata$date)
return(alldata)
}
xlsx = './Data_for_test.xlsx'
datas = proc_data(xlsx = './Data_for_test.xlsx',"DOUGHNUTS",1)
food_data = proc_data(xlsx = './Data_for_test.xlsx',"DOUGHNUTS",2)

alldata.month.turnts = ts(alldata[,"turnover"],frequency = 12,start = c(2004,7))
autoplot(alldata.month.turnts)
alldata.season.turnts = ts((alldata%>% group_by(year,quarters) %>%  
                              summarise(turnover=sum(turnover),.groups = 'drop'))[,'turnover'],frequency = 4,start = c(2004,7))
autoplot(alldata.season.turnts)
alldata.year.turnts =  ts(
  (alldata%>% group_by(year) %>%  
     summarise(turnover=sum(turnover)))[,'year'],frequency = 1,start = c(2004))
autoplot(alldata.year.turnts)



proc_ts_data <- function(datas) {
  month_vol = ts(datas[,"vol"],frequency = 12,start = c(2004,7))
  month_turn=ts(datas[,"turnover"],frequency = 12,start = c(2004,7))
  month_pr = ts(datas[,"pr"],frequency = 12,start = c(2004,7))
  
  
  quarters_turn = ts((datas%>% group_by(year,quarters) %>%  
                             summarise(turnover=sum(turnover),.groups = 'drop'))[,'turnover'],frequency = 4,start = c(2004,7))
  quarters_vol = ts((datas%>% group_by(year,quarters) %>%  
                            summarise(vol=sum(vol),.groups = 'drop'))[,'vol'],frequency = 4,start = c(2004,7))
  quarters_pr = ts((datas%>% group_by(year,quarters) %>%  
                           summarise(pr=sum(pr),.groups = 'drop'))[,'pr'],frequency = 4,start = c(2004,7))
  
  years_turn = ts(
    (datas%>% group_by(year) %>%  
       summarise(turnover=sum(turnover)))[,'year'],frequency = 1,start = c(2004))
  years_pr = ts(
    (datas%>% group_by(year) %>%  
       summarise(pr=sum(pr)))[,'pr'],frequency = 1,start = c(2004))
  years_vol = ts(
    (datas%>% group_by(year) %>%  
       summarise(vol=sum(vol)))[,'vol'],frequency = 1,start = c(2004))
  
  out = list(
    month_vol,month_pr,month_turn,
    years_vol,years_pr,years_turn,
    quarters_vol,quarters_pr,quarters_turn
  )
  
  return(out)
}