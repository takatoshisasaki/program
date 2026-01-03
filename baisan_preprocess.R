
# baisan preprocess

rm(list = ls()); gc()
library(dplyr)
library(stringr)
library(lubridate)
setwd("C:/Users/takat/Desktop/test/rawdata")
outputfoldername <- "C:/Users/takat/Desktop/test"
selectedcolumns  <- 0 # NSSで使う列のみ

colnames_alldata  <- c("releasedate", # 発表日付は1営業日後
                       "issuetype", "meigaracode", "meigaraissues", "duedate", "couponrate", "avgcompoundyield", "avgpriceyen", "dailychangeavgprice",
                       "interestpaymentmonth", "interestpaymentday", "info1", "info2", "info3", 
                       "avgsimpleyield", "highpriceyen", "highsimpleyield", "lowpriceyen", "lowsimpleyield", "invalid", "numreportingfirms", 
                       "highcompoundyield", "dailychangehighprice", "lowcompoundyield", "dailychangelowprice", 
                       "medcompoundyield", "medsimpleyield", "medpriceyen", "dailychangemedprice")

# check
rawdatalist <- list.files()
filelist_year <- as.data.frame(table(str_sub(rawdatalist, 2, 3)))
# write.csv(filelist_year, "filelist_year.csv", row.names = F)

for(i in 1:length(rawdatalist)){
  alldata <- read.csv(rawdatalist[i], 
                      fileEncoding = "cp932", # necessary
                      header = F) # necessary
  
  if(ncol(alldata) != length(colnames_alldata)){
    empty <- as.data.frame(matrix(NA_real_, nrow(alldata), length(colnames_alldata)-ncol(alldata)))
    alldata <- cbind(alldata, empty)
    print(paste0(rawdatalist[i], " columns added"))
  }
  colnames(alldata) <- colnames_alldata
  
  
  # 国債に限定, WI含む
  # WI（When-Issued）取引（発行日前取引）、国債の入札アナウンスメントが行われた日
  # から発行日前日までに行われる取引, 入札前における新発債の需要動向を反映するため、
  # 落札価格の予測値として機能
  # 短期国債, 物価連動国債, CT債, 海外の国債除く
  data <- alldata %>% 
    filter(str_detect(meigaraissues, "中期国債|長期国債|超長期国債"))
  
  data <- data %>%
    select(-issuetype, -interestpaymentmonth, -interestpaymentday, -info1, -info2, -info3, -invalid, -numreportingfirms)
  
  if(i == 1){
    mergedata <- data
  }else{
    mergedata <- rbind(mergedata, data) 
  }
  print(rawdatalist[i])
}

# check <- as.data.frame(unique(mergedata$meigaraissues))
# mergedata <- mergedata %>%
#   filter(str_detect(meigaraissues, "中期国債|長期国債|超長期国債"))

# 売買参考統計値は、当日の午後3時における気配に基づいて作成・発表していますが、
# 翌営業日の公社債店頭売買を行う際の参考となる価格・利回りであるため、発表日付は翌営業日の日付。
# (例) 3月31日午後3時の気配を基に当日夕刻に発表した売買参考統計値の発表日付は4月1日
# 発表日付を抽出
reldate <- as.data.frame(unique(mergedata$releasedate))
colnames(reldate) <- "releasedate" 
# 取引日付は前営業日を参照させる
reldate <- reldate %>% mutate(tradedate = lag(releasedate, 1))
reldate$tradedate[1] <- "20020801" # 売買参考統計の最初の発表日(2002/8/2)の前営業日で埋める
# merge back
mergedata <- left_join(mergedata, reldate, by = "releasedate")
# sort columns
mergedata <- mergedata %>%
  select(releasedate, tradedate, duedate, meigaracode, meigaraissues, couponrate,
         avgcompoundyield, highcompoundyield, lowcompoundyield, medcompoundyield, # 複利
         avgsimpleyield, highsimpleyield, lowsimpleyield, medsimpleyield, # 単利
         avgpriceyen, highpriceyen, lowpriceyen, medpriceyen, # 単価(yen)
         dailychangeavgprice, dailychangehighprice, dailychangelowprice, dailychangemedprice) # 単価前日差

if(selectedcolumns == 1){
  mergedata <- mergedata %>%
    select(tradedate, duedate, meigaracode, meigaraissues, 
           avgcompoundyield) # 平均複利
}

# Date preprocess
mergedata$tradedate <- ymd(mergedata$tradedate, tz = "Asia/Tokyo")
mergedata$duedate   <- ymd(mergedata$duedate,   tz = "Asia/Tokyo")
# Residual maturity
mergedata <- mergedata %>%
  mutate(residmatu = as.numeric(difftime(duedate, tradedate, "days")) / 365.25)

# Missing values
apply(mergedata[,6:ncol(mergedata)], 2, max, na.rm = T)
mergedata[mergedata == 999.99000] <- NA
mergedata[mergedata == 999.99900] <- NA
mergedata[mergedata ==  99.99900] <- NA
apply(mergedata[,6:ncol(mergedata)], 2, max, na.rm = T)

# Save output
today <- Sys.Date()
setwd(outputfoldername)
saveRDS(mergedata, paste0("mergedata_", selectedcolumns, "_", today, ".RDS"))

