
rm(list = ls()); gc()
library(dplyr)
library(stringr)
setwd("C:/Users/takat/Desktop/test/rawdata")

# utility
rawdatalist <- list.files()
filelist_year <- as.data.frame(table(str_sub(rawdatalist, 2, 3)))
yearseq <- unique(filelist_year$Var1)[]
monthseq1 <- c("01", "02", "03")
monthseq2 <- c("04", "05", "06")
monthseq3 <- c("07", "08", "09")
monthseq4 <- c("10", "11", "12")

#### zip ####
specific_ym1 <- rawdatalist[str_sub(rawdatalist, 2, 3) == "02"]
zip("b02.zip", specific_ym1)

for(i in 2:length(yearseq)){
  specific_ym1 <- rawdatalist[str_sub(rawdatalist, 2, 3) == yearseq[i] & str_sub(rawdatalist, 4, 5) %in% monthseq1]
  specific_ym2 <- rawdatalist[str_sub(rawdatalist, 2, 3) == yearseq[i] & str_sub(rawdatalist, 4, 5) %in% monthseq2]
  specific_ym3 <- rawdatalist[str_sub(rawdatalist, 2, 3) == yearseq[i] & str_sub(rawdatalist, 4, 5) %in% monthseq3]
  specific_ym4 <- rawdatalist[str_sub(rawdatalist, 2, 3) == yearseq[i] & str_sub(rawdatalist, 4, 5) %in% monthseq4]
  
  name1 <- paste0("b", yearseq[i], "_1Q.zip")
  name2 <- paste0("b", yearseq[i], "_2Q.zip")
  name3 <- paste0("b", yearseq[i], "_3Q.zip")
  name4 <- paste0("b", yearseq[i], "_4Q.zip")
  
  zip(name1, specific_ym1)
  print(name1)
  zip(name2, specific_ym2)
  print(name2)
  zip(name3, specific_ym3)
  print(name3)
  zip(name4, specific_ym4)
  print(name4)
}

monthseq1 <- c("01", "02")
monthseq2 <- c("03", "04")
monthseq3 <- c("05", "06")
monthseq4 <- c("07", "08")
monthseq5 <- c("09", "10")
monthseq6 <- c("11", "12")

yearseq <- c("21", "22", "23", "24", "25")

for(i in 1:length(yearseq)){
  specific_ym1 <- rawdatalist[str_sub(rawdatalist, 2, 3) == yearseq[i] & str_sub(rawdatalist, 4, 5) %in% monthseq1]
  specific_ym2 <- rawdatalist[str_sub(rawdatalist, 2, 3) == yearseq[i] & str_sub(rawdatalist, 4, 5) %in% monthseq2]
  specific_ym3 <- rawdatalist[str_sub(rawdatalist, 2, 3) == yearseq[i] & str_sub(rawdatalist, 4, 5) %in% monthseq3]
  specific_ym4 <- rawdatalist[str_sub(rawdatalist, 2, 3) == yearseq[i] & str_sub(rawdatalist, 4, 5) %in% monthseq4]
  specific_ym5 <- rawdatalist[str_sub(rawdatalist, 2, 3) == yearseq[i] & str_sub(rawdatalist, 4, 5) %in% monthseq5]
  specific_ym6 <- rawdatalist[str_sub(rawdatalist, 2, 3) == yearseq[i] & str_sub(rawdatalist, 4, 5) %in% monthseq6]
  
  name1 <- paste0("b", yearseq[i], "_1.zip")
  name2 <- paste0("b", yearseq[i], "_2.zip")
  name3 <- paste0("b", yearseq[i], "_3.zip")
  name4 <- paste0("b", yearseq[i], "_4.zip")
  name5 <- paste0("b", yearseq[i], "_5.zip")
  name6 <- paste0("b", yearseq[i], "_6.zip")
  
  zip(name1, specific_ym1)
  print(name1)
  zip(name2, specific_ym2)
  print(name2)
  zip(name3, specific_ym3)
  print(name3)
  zip(name4, specific_ym4)
  print(name4)
  zip(name5, specific_ym5)
  print(name5)
  zip(name6, specific_ym6)
  print(name6)
}


#### Unzip ####
setwd("C:/Users/takat/Desktop/test2")
taishofiles <- list.files(pattern = "\\.zip$")
for(i in 1:taishofiles[i]){
  unzip(taishofiles[i]); print(taishofiles[i])
}

