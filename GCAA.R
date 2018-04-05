options(java.parameters = "-Xmx8000m")
library(rJava)
#library(xlsx)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(MASS)
library(broom)

setwd("/media/torrent/Database/Users/Torrent/Google 雲端硬碟/stat/R/GCAA") #for Ubuntu
setwd("D:/Users/Torrent/Google 雲端硬碟/stat/R/GCAA")

foldername <- list.files("2016") #讀取雲林2016年資料夾

all.cems <- list() #all.cems的list

for (m in 1:12) { #進入1至12月的資料夾
  csvfolder <- paste('2016/', foldername[m], sep = '')
  csvname <- list.files(csvfolder)
  csvpath <- paste(csvfolder, '/', csvname, sep = '')
  
  for (d in 1:length(csvpath)) {
    cems <- read.csv(csvpath[d]) #讀取各月資料夾的csv
    
    if (nrow(cems) == 0) { #過濾出部份csv檔無資料問題
      
    } else {
      
      #日期標定
      date <- names(cems)[1]
      date <- substr(date, 2, nchar(date))
      date <- gsub('\\.', '\\-', date)
      date <- as.Date(date)
      cems$date <- date #增加date variable
      names(cems) <- c('registration_no', 'facilities_no', 'typecode', 'time', 'value', 'date') #變數名稱變更
      
      #時間標定
      cems$time <- as.character(cems$time)
      cems$time[nchar(cems$time) == 1] <- paste ('000', cems$time[nchar(cems$time) == 1], sep = '')
      cems$time[nchar(cems$time) == 2] <- paste ('00', cems$time[nchar(cems$time) == 2], sep = '')
      cems$time[nchar(cems$time) == 3] <- paste ('0', cems$time[nchar(cems$time) == 3], sep = '')
      
      hour <- substr(cems$time, 1, 2)
      min <- substr(cems$time, 3, 4)
      cems$time <- paste(hour, ':', min, sep = '')
      
      all.cems <- rbind(all.cems, cems)
    }
  }
  
}

#write.csv(all.cems, file = "cems.csv")

epa.filename <- list.files("EPA") #EPA資料夾檔名抓取

all.epa <- list()
min.loop <- list()
min.vars <- read.csv('mins.csv', header = F)
min.vars <- as.list(min.vars[1,])

for (minloop in 1:240) { #讀取六分鐘迴圈
  min.loop[minloop] <- as.character(min.vars[[minloop]])
}

for (epa.file in 1:length(epa.filename)) { #按照EPA檔案夾長度設定迴圈終點
  
  epacsv <- paste('EPA/', epa.filename[epa.file], sep = '')
  
  sheets <- excel_sheets(epacsv) #讀取xls檔案內sheet個數
  #sheets <- excel_sheets(sheet.num)
  
  for (sheetindex in 1:length(sheets)) { #按照sheet個數設定迴圈終點
    
    epa.emission <- read_xls(epacsv, sheet = sheetindex, skip = 5)
    epa.faci <- read_xls(epacsv, sheet = sheetindex, n_max = 4, col_names = FALSE)
    
    if (ncol(epa.emission) > 100) {
      
      names(epa.emission) <- c('month', 'date', 1:240)
      
      epa.emission <- epa.emission[names(epa.emission) %in% c('date', 1:240)]
      epa.emission <- filter(epa.emission, date != '')
      
      epa.emission <- gather(epa.emission, key = "time", value = "value", 2:241)
      names(epa.emission) <- c('date', 'time', 'value')
      
      epa.emission$time <- min.loop[as.numeric(epa.emission$time)]
      
      epa.emission$registration_no <- epa.faci$X__2[1]
      epa.emission$facilityname <- epa.faci$X__2[2]
      epa.emission$facilities_no <- epa.faci$X__2[3]
      
      type.find <- regexpr('[A-Z]+|[A-Z]+[0-9]', epa.faci$X__2[4])
      
      type <- substr(epa.faci$X__2[4], type.find, type.find + attr(type.find, "match.length") - 1)
      
      epa.emission$type <- type
      
      all.epa <- rbind(all.epa, epa.emission)
      
    } else {
      
      names(epa.emission) <- c('month', 'date', 0:23)
      
      epa.emission <- epa.emission[names(epa.emission) %in% c('date', 0:23)]
      epa.emission <- filter(epa.emission, date != '')
      
      epa.emission <- gather(epa.emission, time, 'value', 2:25)
      names(epa.emission) <- c('date', 'time', 'value')
      
      epa.emission$time[nchar(epa.emission$time) == 1] <- paste('0', epa.emission$time[nchar(epa.emission$time) == 1], ':00', sep = '')
      epa.emission$time[nchar(epa.emission$time) == 2] <- paste(epa.emission$time[nchar(epa.emission$time) == 2], ':00', sep = '')
      
      epa.emission$registration_no <- epa.faci$X__2[1]
      epa.emission$facilityname <- epa.faci$X__2[2]
      epa.emission$facilities_no <- epa.faci$X__2[3]
      
      type.find <- regexpr('[A-Z]+|[A-Z]+[0-9]', epa.faci$X__2[4])
      
      type <- substr(epa.faci$X__2[4], type.find, type.find + attr(type.find, "match.length") - 1)
      
      epa.emission$type <- type
      
      all.epa <- rbind(all.epa, epa.emission)
    }
  }
}

#時間格式修正

all.epa$time <- as.character(all.epa$time)

all.epa$date <- gsub('月', '-', all.epa$date)
all.epa$date <- gsub('日', '', all.epa$date)
all.epa$date <- paste('2016-', all.epa$date, sep = '')
all.epa$date <- as.Date(all.epa$date)

#write.csv(all.epa, file = "epa.csv")

formosa <- as.character(unique(all.epa$registration_no)) #六輕廠編號過濾

all.epa.oct <- all.epa[all.epa$date < '2016-10-03',]

cems.formosa <- filter(all.cems, registration_no == "P5801719" | registration_no == "P5801728" | registration_no == "P5802074" | registration_no == "P5802092" | registration_no == "P5802421" | registration_no == "P5802430")
cems.formosa$typecode <- as.character(cems.formosa$typecode)

typecode <- read.csv('https://raw.githubusercontent.com/ks-opendata-community/chimney/gh-pages/data/%E9%A0%85%E7%9B%AE%E4%BB%A3%E7%A2%BC.csv')

cems.formosa$type <- typecode$ABBR[match(cems.formosa$typecode, typecode$ITEM)]
#names(cems.formosa) <- c("X", "registration_no", "facilities_no", "typecode", "time", "value", "date", "type")

threshold <- read.csv('threshold.csv')
names(threshold)[3] <- c('typecode')
threshold$type <- typecode$ABBR[match(threshold$typecode, typecode$ITEM)]

#names(threshold) <- c("registration_no", "facilities_no", "typecode", "value", "type")

threshold$typecode <- as.character(threshold$typecode)

cems.formosa.thre <- left_join(cems.formosa, threshold, 
                     by = c('registration_no' = 'registration_no', 
                            'facilities_no' = 'facilities_no',
                            'typecode' = 'typecode'))

names(cems.formosa.thre) <- c("registration_no", "facilities_no", "typecode", "time", "value", "date", "type.x", "thres", "type.y")

cems.formosa.thre$value <- as.numeric(as.character(cems.formosa.thre$value))

cems.formosa.thre$standard <- cems.formosa.thre$value - cems.formosa.thre$thres

cems.formosa.thre$date <- as.Date(cems.formosa.thre$date)

#cems.formosa.thre <- cems.formosa.thre[grep('^2|911', cems.formosa.thre$typecode),]

cems.formosa.thre.epa <- full_join(cems.formosa.thre, all.epa.oct, 
                         by = c('registration_no' = 'registration_no', 
                                'facilities_no' = 'facilities_no', 
                                'date' = 'date', 
                                'time' = 'time', 
                                'type.x' = 'type'))

names(cems.formosa.thre.epa)[5] <- c('value.cems')
names(cems.formosa.thre.epa)[11] <- c('value.epa')

cems.formosa.thre.epa$date.time <- paste(cems.formosa.thre.epa$date, cems.formosa.thre.epa$time, sep = ' ')
cems.formosa.thre.epa$date.time <- as.POSIXct(cems.formosa.thre.epa$date.time)


cems.formosa.thre.epa <- cems.formosa.thre.epa[cems.formosa.thre.epa$date < '2016-10-03',]

cems.formosa.thre.epa <- cems.formosa.thre.epa %>% group_by(registration_no, facilities_no, typecode, date, substr(time, 1, 2)) %>%
  mutate(check = grepl('[\u4e00-\u9fa5]+', first(value.epa)), check.911 = grepl('911', typecode)) %>%
  #filter(check == TRUE) %>%
  mutate(value.epa.amend = ifelse(check == TRUE, first(value.epa), value.epa), 
         value.epa.avg = ifelse(check == TRUE, first(value.epa), ifelse(check.911 == FALSE, first(value.epa), value.epa)), 
         value.cems.avg = ifelse(check.911 == FALSE, mean(value.cems, na.rm = T), value.cems)) %>%
  arrange(registration_no, facilities_no, typecode, date.time)
    #select(-check)

test <- cems.formosa.thre.epa[cems.formosa.thre.epa$standard > 0 & is.na(cems.formosa.thre.epa$standard) == F & is.na(cems.formosa.thre.epa$value.epa.amend) == T,]

#CEMS即時值缺失
cems.lost <- cems.formosa.thre.epa[is.na(cems.formosa.thre.epa$typecode) == T & is.na(cems.formosa.thre.epa$value.cems) == T,]
nrow(cems.lost[cems.lost$value.epa == '',])
nrow(cems.lost[grep('[\u4e00-\u9fa5]+', cems.lost$value.epa),])
nrow(cems.lost[grep('[0-9]+', cems.lost$value.epa),])

#不過濾五條特殊煙道

overrate <- cems.formosa.thre.epa[cems.formosa.thre.epa$standard > 0 & is.na(cems.formosa.thre.epa$standard) != T, ] #抽取超標

#P5802421五條特殊煙道過濾
P5802421.5flue <- filter(cems.formosa.thre.epa, registration_no == 'P5802421') %>% filter(facilities_no == 'P101' | facilities_no == 'P201' | facilities_no == 'P301' | facilities_no == 'P701' | facilities_no == 'P801')

overrate <- setdiff(cems.formosa.thre.epa, P5802421.5flue) #排除P5802421五條煙道的資料
overrate <- overrate[overrate$standard > 0 & is.na(overrate$standard) != T, ] #抽取超標

#超標無超標計算

cleaning.filter <- setdiff(cems.formosa.thre.epa, P5802421.5flue)

#未超標
nrow(cleaning.filter[cleaning.filter$standard <= 0 & is.na(cleaning.filter$standard) == F,])

#超標
nrow(cleaning.filter[cleaning.filter$standard > 0 & is.na(cleaning.filter$standard) == F,])

#超標無更改
cleaning.filter[cleaning.filter$standard > 0 & is.na(cleaning.filter$standard) == F,] %>%
  setdiff(cleaning.filter[grep('[\u4e00-\u9fa5]+', cleaning.filter$value.epa.amend),]) %>% 
  nrow()

#超標無更改而all.epa有資料
cleaning.filter[cleaning.filter$standard > 0 & is.na(cleaning.filter$standard) == F,] %>%
  setdiff(cleaning.filter[grep('[\u4e00-\u9fa5]+', cleaning.filter$value.epa.amend),]) %>% 
  filter(is.na(value.epa) == F) %>%
  nrow()

#超標無更改、all.epa有資料、all.epa均值無超標

cleaning.filter[cleaning.filter$standard > 0 & is.na(cleaning.filter$standard) == F,] %>%
  setdiff(cleaning.filter[grep('[\u4e00-\u9fa5]+', cleaning.filter$value.epa.amend),]) %>% 
  filter(is.na(value.epa) == F)  %>% 
  filter(value.epa.avg < thres) %>%
  nrow()

#超標無更改、all.epa有資料、all.epa均值有超標

cleaning.filter[cleaning.filter$standard > 0 & is.na(cleaning.filter$standard) == F,] %>%
  setdiff(cleaning.filter[grep('[\u4e00-\u9fa5]+', cleaning.filter$value.epa.amend),]) %>% 
  filter(is.na(value.epa) == F)  %>% 
  filter(value.epa.avg > thres) %>%
  nrow()


#超標無更改而all.epa無資料
cleaning.filter[cleaning.filter$standard > 0 & is.na(cleaning.filter$standard) == F,] %>%
  setdiff(cleaning.filter[grep('[\u4e00-\u9fa5]+', cleaning.filter$value.epa.amend),]) %>% 
  filter(is.na(value.epa) == T) %>%
  nrow()

#超標無更改、all.epa無資料、all.cems均值無超標

cleaning.filter[cleaning.filter$standard > 0 & is.na(cleaning.filter$standard) == F,] %>%
  setdiff(cleaning.filter[grep('[\u4e00-\u9fa5]+', cleaning.filter$value.epa.amend),]) %>% 
  filter(is.na(value.epa) == T)  %>% 
  filter(value.cems.avg < thres) %>%
  nrow()

#超標無更改、all.epa無資料、all.cems均值有超標

cleaning.filter[cleaning.filter$standard > 0 & is.na(cleaning.filter$standard) == F,] %>%
  setdiff(cleaning.filter[grep('[\u4e00-\u9fa5]+', cleaning.filter$value.epa.amend),]) %>% 
  filter(is.na(value.epa) == T)  %>% 
  filter(value.cems.avg > thres) %>%
  nrow()

#超標有更改
cleaning.filter[cleaning.filter$standard > 0 & is.na(cleaning.filter$standard) == F,] %>%
  intersect(cleaning.filter[grep('[\u4e00-\u9fa5]+', cleaning.filter$value.epa.amend),]) %>% 
  nrow()


#超標值為缺失
nrow(cleaning.filter[is.na(cleaning.filter$standard) == T,])

#超標值為缺失、cems有資料、無標準值
nrow(cleaning.filter[is.na(cleaning.filter$standard) == T & is.na(cleaning.filter$thres) == T & is.na(cleaning.filter$value.cems) == F,])

#超標值為缺失、cems有資料、無標準值、無更改
cleaning.filter[is.na(cleaning.filter$standard) == T & is.na(cleaning.filter$thres) == T & is.na(cleaning.filter$value.cems) == F,] %>%
  setdiff(cleaning.filter[grep('[\u4e00-\u9fa5]+', cleaning.filter$value.epa.amend),]) %>%
  nrow()

#超標值為缺失、cems有資料、無標準值、無更改、all.epa有資料
cleaning.filter[is.na(cleaning.filter$standard) == T & is.na(cleaning.filter$thres) == T & is.na(cleaning.filter$value.cems) == F,] %>%
  setdiff(cleaning.filter[grep('[\u4e00-\u9fa5]+', cleaning.filter$value.epa.amend),]) %>%
  filter(is.na(value.epa) == F) %>%
  nrow()

#超標值為缺失、cems有資料、無標準值、無更改、all.epa無資料
cleaning.filter[is.na(cleaning.filter$standard) == T & is.na(cleaning.filter$thres) == T & is.na(cleaning.filter$value.cems) == F,] %>%
  setdiff(cleaning.filter[grep('[\u4e00-\u9fa5]+', cleaning.filter$value.epa.amend),]) %>%
  filter(is.na(value.epa) == T) %>%
  nrow()

#超標值為缺失、cems有資料、無標準值、有更改
cleaning.filter[is.na(cleaning.filter$standard) == T & is.na(cleaning.filter$thres) == T & is.na(cleaning.filter$value.cems) == F,] %>%
  intersect(cleaning.filter[grep('[\u4e00-\u9fa5]+', cleaning.filter$value.epa.amend),]) %>%
  nrow()

#超標值為缺失、cems無資料、無標準值
nrow(cleaning.filter[is.na(cleaning.filter$standard) == T & is.na(cleaning.filter$thres) == T & is.na(cleaning.filter$value.cems) == T,])

#超標值為缺失、cems無資料、無標準值、無更改
cleaning.filter[is.na(cleaning.filter$standard) == T & is.na(cleaning.filter$thres) == T & is.na(cleaning.filter$value.cems) == T,] %>%
  setdiff(cleaning.filter[grep('[\u4e00-\u9fa5]+', cleaning.filter$value.epa.amend),]) %>%
  nrow()

#超標值為缺失、cems無資料、無標準值、無更改、all.epa有資料
cleaning.filter[is.na(cleaning.filter$standard) == T & is.na(cleaning.filter$thres) == T & is.na(cleaning.filter$value.cems) == T,] %>%
  setdiff(cleaning.filter[grep('[\u4e00-\u9fa5]+', cleaning.filter$value.epa.amend),]) %>%
  filter(is.na(value.epa) == F) %>%
  nrow()

#超標值為缺失、cems無資料、無標準值、無更改、all.epa無資料
cleaning.filter[is.na(cleaning.filter$standard) == T & is.na(cleaning.filter$thres) == T & is.na(cleaning.filter$value.cems) == T,] %>%
  setdiff(cleaning.filter[grep('[\u4e00-\u9fa5]+', cleaning.filter$value.epa.amend),]) %>%
  filter(is.na(value.epa) == T) %>%
  nrow()

#超標值為缺失、cems無資料、有標準值、有更改
cleaning.filter[is.na(cleaning.filter$standard) == T & is.na(cleaning.filter$thres) == T & is.na(cleaning.filter$value.cems) == T,] %>%
  intersect(cleaning.filter[grep('[\u4e00-\u9fa5]+', cleaning.filter$value.epa.amend),]) %>%
  nrow()

#超標值為缺失、cems無資料、有標準值
nrow(cleaning.filter[is.na(cleaning.filter$standard) == T & is.na(cleaning.filter$thres) == F & is.na(cleaning.filter$value.cems) == T,])

#超標值為缺失、cems無資料、有標準值、無更改
cleaning.filter[is.na(cleaning.filter$standard) == T & is.na(cleaning.filter$thres) == F & is.na(cleaning.filter$value.cems) == T,] %>%
  setdiff(cleaning.filter[grep('[\u4e00-\u9fa5]+', cleaning.filter$value.epa.amend),]) %>%
  nrow()

#超標值為缺失、cems無資料、有標準值、無更改、all.epa有資料
cleaning.filter[is.na(cleaning.filter$standard) == T & is.na(cleaning.filter$thres) == F & is.na(cleaning.filter$value.cems) == T,] %>%
  setdiff(cleaning.filter[grep('[\u4e00-\u9fa5]+', cleaning.filter$value.epa.amend),]) %>%
  filter(is.na(value.epa) == F) %>%
  nrow()

#超標值為缺失、cems無資料、有標準值、無更改、all.epa無資料
cleaning.filter[is.na(cleaning.filter$standard) == T & is.na(cleaning.filter$thres) == F & is.na(cleaning.filter$value.cems) == T,] %>%
  setdiff(cleaning.filter[grep('[\u4e00-\u9fa5]+', cleaning.filter$value.epa.amend),]) %>%
  filter(is.na(value.epa) == T) %>%
  nrow()

#超標值為缺失、cems無資料、有標準值、有更改
cleaning.filter[is.na(cleaning.filter$standard) == T & is.na(cleaning.filter$thres) == F & is.na(cleaning.filter$value.cems) == T,] %>%
  intersect(cleaning.filter[grep('[\u4e00-\u9fa5]+', cleaning.filter$value.epa.amend),]) %>%
  nrow()

#綠盟所提需求要將即時值平均值改掉的筆數輸出

rate.for.gcaa <- cleaning.filter[cleaning.filter$standard > 0 & is.na(cleaning.filter$standard) == F,] %>%
  setdiff(cleaning.filter[grep('[\u4e00-\u9fa5]+', cleaning.filter$value.epa.amend),]) %>% 
  filter(is.na(value.epa) == F)  %>% 
  filter(value.cems.avg > value.epa)

#write.csv(rate.for.gcaa, file = "avgamend.csv")

#t-test

t.test(cleaning.filter$value.cems[grep('[\u4e00-\u9fa5]+', cleaning.filter$value.epa)], cleaning.filter$value.cems[grep('[0-9]+', cleaning.filter$value.epa)], alternative = c('greater'))

t.test(test$value.cems[nchar(test$value.epa) > 3], test$value.cems[nchar(test$value.epa) < 3], alternative = c('greater'))

#進行以天為單位的數據整理

#OPC特別處理

OPC.filter <- filter(cleaning.filter, typecode == '911' | typecode == '211') %>% filter(thres != 'NA')

OPC.filter$num.overrate <- c('0') #生成超標dummy variable
OPC.filter$num.overrate[OPC.filter$standard > 1] <- c('1')
OPC.filter$num.overrate <- as.numeric(OPC.filter$num.overrate)

OPC.filter$num.na <- c('0') #生成cems缺失值dummy variable
OPC.filter$num.na[is.na(OPC.filter$value.cems) == TRUE] <- c('1')
OPC.filter$num.na <- as.numeric(OPC.filter$num.na)

OPC.filter$nun.zero <- c('1') #生成cems為0 dummy variable
OPC.filter$nun.zero[OPC.filter$value.cems == 0] <- c('0')
OPC.filter$nun.zero <- as.numeric(OPC.filter$nun.zero)

OPC.filter$void <- c('0') #生成環保署備查資料為註記 dummy variable
OPC.filter$void[grep('[\u4e00-\u9fa5]+', OPC.filter$value.epa.amend)] <- c('1')
OPC.filter$void <- as.numeric(OPC.filter$void)

OPC.daily <- OPC.filter %>% 
  group_by(registration_no, facilities_no, typecode, date) %>%
  summarise(overrate = sum(num.overrate, na.rm = T), 
     cems.na = sum(num.na, na.rm = T),
     amend = sum(void, na.rm = T),
     zero = sum(nun.zero, na.rm = T))

OPC.lm <- lm(amend ~ overrate + cems.na + zero, data = OPC.daily)
summary(OPC.lm)

#OLS

facility.OPC.lm <- OPC.daily %>% 
  group_by(registration_no, facilities_no) %>%
  do(fit.facility = lm(amend ~ overrate + cems.na +zero, data = .))

OPC.lm.result <- tidy(facility.OPC.lm, fit.facility)
summary(OPC.lm.result)

write.csv(OPC.lm.result, file = 'OPC.csv')

#t-test

facility.OPC.ttest <- OPC.filter %>% 
  group_by(registration_no, facilities_no) %>%
  do(ttest.facility = t.test(OPC.filter$value.cems[grep('[\u4e00-\u9fa5]+', OPC.filter$value.epa)], OPC.filter$value.cems[grep('[0-9]+', OPC.filter$value.epa)], alternative = c('greater')))

OPC.ttest.result <- tidy(facility.OPC.ttest, ttest.facility)
summary(OPC.ttest.result)

t.test(OPC.filter$value.cems[grep('[\u4e00-\u9fa5]+', OPC.filter$value.epa)], OPC.filter$value.cems[grep('[0-9]+', OPC.filter$value.epa)], alternative = c('greater'))

#其他種類污染

other.filter <- filter(cleaning.filter, typecode != '911') %>% filter(thres != 'NA')

other.filter$num.overrate <- c('0') #生成超標dummy variable
other.filter$num.overrate[other.filter$standard > 1] <- c('1')
other.filter$num.overrate <- as.numeric(other.filter$num.overrate)

other.filter$num.na <- c('0') #生成cems缺失值dummy variable
other.filter$num.na[is.na(other.filter$value.cems) == TRUE] <- c('1')
other.filter$num.na <- as.numeric(other.filter$num.na)

other.filter$nun.zero <- c('1') #生成cems為0 dummy variable
other.filter$nun.zero[other.filter$value.cems == 0] <- c('0')
other.filter$nun.zero <- as.numeric(other.filter$nun.zero)

other.filter$void <- c('0') #生成環保署備查資料為註記 dummy variable
other.filter$void[grep('[\u4e00-\u9fa5]+', other.filter$value.epa)] <- c('1')
other.filter$void <- as.numeric(other.filter$void)

other.daily <- other.filter %>% 
  group_by(registration_no, facilities_no, typecode, date) %>%
  summarise(overrate = sum(num.overrate, na.rm = T), 
            cems.na = sum(num.na, na.rm = T),
            amend = sum(void, na.rm = T),
            zero = sum(nun.zero, na.rm = T))

SOX.lm <- lm(amend ~ overrate + cems.na + zero, data = other.daily[other.daily$typecode == '923',])
summary(SOX.lm)

facility.other.lm <- other.daily %>% 
  group_by(registration_no, facilities_no, typecode) %>%
  do(fit.other.facility = lm(amend ~ overrate + cems.na + zero, data = .))

other.lm.result <- tidy(facility.other.lm, fit.other.facility)

write.csv(other.lm.result, file = 'other.csv')


SOX.nb <- negbin(amend ~ overrate + cems.na + zero, -1, data = other.daily[other.daily$typecode == '922',])
summary(SOX.nb)

NOX.lm <- lm(amend ~ overrate + cems.na + zero, data = other.daily[other.daily$typecode == '923',])
summary(NOX.lm)
ggplot(other.daily, aes(date, overrate)) +
  geom_line() +  #would display all the non-smoothed lines
  geom_line(data = test, aes(date, amend, color = 'red')) +
  geom_line(data = test, aes(date, overrate, color = 'blue')) +
  geom_line(data = test, aes(date, zero, color = 'yellow'))

ggplot(OPC.daily, aes(date, overrate)) +
  geom_line(aes(color = 'red'))

#epa資料分析

all.epa.analysis <- all.epa

all.epa.analysis$void <- c('0')
all.epa.analysis$void[grep('[\u4e00-\u9fa5]+', all.epa.analysis$value)] <- c('1')
all.epa.analysis$void <- as.numeric(all.epa.analysis$void)

all.epa.analysis$nodata <- ('1')
all.epa.analysis$nodata[all.epa.analysis$value == ''] <- c('0')
all.epa.analysis$nodata <- as.numeric(all.epa.analysis$nodata)

length(all.epa.analysis$nodata[all.epa.analysis$nodata == '0'])

sum(all.epa.analysis$void[all.epa.analysis$void == '1' & all.epa.analysis$type == 'SOX'])
sum(all.epa.analysis$void)

nrow(all.epa.analysis[all.epa.analysis$type == 'SOX' & all.epa.analysis$nodata == '1',])
sum(all.epa.analysis$void[all.epa.analysis$type == 'SOX' & all.epa.analysis$nodata == '1'])

nrow(all.epa.analysis[all.epa.analysis$nodata == '1',])
sum(all.epa.analysis$void[all.epa.analysis$nodata == '1'])

nrow(all.epa.analysis[all.epa.analysis$type == 'SOX' & all.epa.analysis$nodata == '1',])
  
sum(OPC.daily$overrate)
sum(other.daily$amend[other.daily$typecode == '922'])

sum(OPC.daily$amend)

sum(other.daily$amend)
