library(purrr)
library(rvest)
library(dplyr)
library(tibble)
library(matlab)
library(pracma)
library(stringr)

# Start -> 2012
url        <- "https://www.federalreserve.gov/monetarypolicy/fomchistorical"
timevalues <- seq(as.Date("1940/01/01"), as.Date("2012/12/31"), by = "year")
timevalues <- format(timevalues, format="%Y")
unitedata  <- function(x){
  full_url <- paste0(url, x,".htm")
  full_url
  }
finalurl   <- c(unitedata(timevalues))
FOMCscrape <- function(x){
  page  <- x
  date  <- page %>% 
           read_html() %>%
           html_nodes('div.panel-heading') %>%
           html_text()
  date         <-  as.data.frame(date,stringsAsFactors=FALSE)
  chart        <- cbind(date)
  names(chart) <- c("Date")
  chart        <- as.tibble(chart)
  return(chart)
}
dates   <- map_df(finalurl[!(timevalues %in% c(2011,2012) )], FOMCscrape)
dates_f <- read_html(finalurl[timevalues==2012]) %>% 
           html_nodes("div.panel.panel-default.panel-padded") %>%
           html_nodes("h5.panel-heading.panel-heading--shaded") %>%
           html_text() %>% as.data.frame()
dates_n <- read_html(finalurl[timevalues==2011]) %>% 
           html_nodes('div.panel-heading') %>% html_nodes('h5') %>%
           html_text() %>% as.data.frame()
dates_f <- rbind(dates_n,dates_f)
names(dates_f) <- c("Date")
n       <- c(1:dim(dates)[1])
for (ii in n) {
  date  <- unlist(strsplit(dates$Date[ii] ,split="\n        ", fixed = TRUE))[2]
  date1 <- unlist(strsplit(date,split="Meeting - ", fixed = TRUE))[1]
  date2 <- unlist(strsplit(date,split="Meeting - ", fixed = TRUE))[2]
  date2 <- unlist(strsplit(date2,split="\n    ", fixed = TRUE))[1]
  dates$Date[ii] <- paste0(date1,date2)
}
s       <- c(1:dim(dates_f)[1])
dates_t <- as.data.frame(s)
for (ii in s) {
  date_f1 <- unlist(strsplit(toString(dates_f$Date[ii]),split=" Meeting - ", fixed = TRUE))[1]
  date_f2 <- unlist(strsplit(toString(dates_f$Date[ii]),split="Meeting - ", fixed = TRUE))[2]
  dates_t[ii,1] <- paste(date_f1,date_f2)
}
names(dates_t) <- c("Date")
dates_t <- as.tibble(dates_t)
dates   <- rbind(dates,dates_t)
n <- c(1:dim(dates)[1])
ind <- n*0
for (ii in n) {
    test <- unlist(strsplit(dates$Date[ii],split="Conference", fixed = TRUE))
    if(length(test)>1){ next }
    ind[ii] <- ii
    }
calendar_1 <- dates[ind,1]
for (ii in c(1:dim(calendar_1)[1])) {
  list <- unlist(strsplit(toString(calendar_1[ii,]),split=" ", fixed = TRUE))
  dt   <- unlist(strsplit(list[2],split="-", fixed = TRUE))
  if("TRUE" %in% (month.name %in% dt[2])){
    list_temp   <- unlist(strsplit(list[2],split="-", fixed = TRUE))
    dt[2]       <- list[3]
    year        <- tail(list,n=1)
    monthnum[1] <- match(list[1],month.name)
    monthnum[2] <- match(list_temp[2],month.name)
  }else{
    monthnum <- repmat(match(list[1],month.name),2,1)
    year     <- list[3] 
  }
  if(length(dt) == 1){
    start_day <- dt
    end_day   <- start_day
  }else{
    start_day <- dt[1]
    end_day   <- dt[2]
  }

  start_date <- str_replace(toString(c(start_day,monthnum[1])),", ","/")
  end_date   <- str_replace(toString(c(end_day,monthnum[2]))  ,", ","/")
  if(ii == 1){
    start_f   <- str_replace(toString(c(start_date,year)) ,", ","/")
    end_f     <- str_replace(toString(c(end_date,year))     ,", ","/")
  }else{
    start_f   <- c(start_f,str_replace(toString(c(start_date,year)) ,", ","/"))
    end_f     <- c(end_f,str_replace(toString(c(end_date,year))     ,", ","/"))
  }
}
sched <- ones(dim(calendar_1)[1],1)   # Since I removed all non-scheduled dates
calendar_1     <- as.data.frame(cbind(start_f, end_f,sched))
names(calendar_1)   <- c("Start","End","Scheduled")

# From 2013 ->
url     <- "https://www.federalreserve.gov/monetarypolicy/fomccalendars.htm"
webpage <- read_html(url)
n <- length(webpage %>% html_nodes("div.panel.panel-default"))
n <- c(1:n)
for (ii in n) {
  year    <- webpage %>% html_nodes("div.panel.panel-default") %>% 
             .[[ii]] %>% html_nodes("div.panel-heading") %>% html_text()
  year    <- unlist(strsplit(year,split=" FOMC", fixed = TRUE))[1]
  months  <- webpage %>% html_nodes("div.panel.panel-default") %>% 
             .[[ii]] %>% html_nodes("div.fomc-meeting__month.col-xs-5.col-sm-3.col-md-2") %>% 
              html_text() 
  days    <- webpage %>% html_nodes("div.panel.panel-default") %>% 
             .[[ii]] %>% html_nodes("div.fomc-meeting__date.col-xs-4.col-sm-9.col-md-10.col-lg-1") %>% 
             html_text() %>% as.matrix()
  days_un <- webpage %>% html_nodes("div.panel.panel-default") %>% 
             .[[ii]] %>% html_nodes("div.fomc-meeting__date.col-xs-4.col-sm-9.col-md-10.col-lg-2") %>% 
             html_text()
    if(length(days_un)[1] > 0){
      m       <-  gsub(" ", "", months, fixed = TRUE)
      l       <- length(m)
      c       <- c(FALSE, m[-1] == m[1:l-1])
      sched   <- as.matrix((!c)*1)
      indT    <- which(c)
      days1   <- days[1:indT-1,1] 
      days2   <- days[(indT+1):l-1,1]
      days_un <- unlist(strsplit(days_un,split=" (unscheduled)", fixed = TRUE))[1]
      days    <- as.data.frame(c(days1,days_un,days2)) 
      }
      else{
      sched <- ones(dim(days)[1],1)
      }
  months <-  gsub(" ", "", months, fixed = TRUE)
  # Find start/end dates
  for (jj in c(1:dim(days)[1])) {
    dt     <- unlist(strsplit(as.matrix(months)[jj] ,split="/", fixed = TRUE))
    if(length(dt) == 1){
      start_month <- match(dt,month.name)
      end_month   <- start_month
    }else{
      if("TRUE" %in% (dt[1] %in% month.abb)){
      start_month <- match(dt[1], month.abb)
      end_month   <- match(dt[2], month.abb)
      }else{
      start_month <- match(dt[1], month.name)
      end_month   <- match(dt[2], month.name)
      }
    }
    td <- unlist(strsplit(as.matrix(days)[jj] ,split="-", fixed = TRUE))
    if(length(td) == 1){
      start_day <- td
      end_day   <- td
    }else{
      start_day <- td[1]
      end_day   <- gsub("*", "", td[2], fixed = TRUE)
    }
    
    start_t <- str_replace(toString(c(start_day,start_month)),", ","/")
    end_t   <- str_replace(toString(c(end_day,end_month))    ,", ","/")
    if(jj == 1){
      start_f   <- str_replace(toString(c(start_t,year)) ,", ","/")
      end_f     <- str_replace(toString(c(end_t,year))     ,", ","/")
    }else{
      start_f   <- c(start_f,str_replace(toString(c(start_t,year)) ,", ","/"))
      end_f     <- c(end_f,str_replace(toString(c(end_t,year))     ,", ","/"))
    }
  }
  sched   <- as.data.frame(as.character(sched),stringsAsFactors=FALSE)
  start_f <- as.data.frame(start_f)
  end_f   <- as.data.frame(end_f)
  calendar_temp          <- cbind(start_f, end_f,sched)
  names(calendar_temp)   <- c("Start","End","Scheduled")
    if(ii == 1){
      calendar_2 <- calendar_temp
      next
    }
  calendar_2 <- rbind(calendar_temp,calendar_2)
}
calendar_comp <- rbind(calendar_1,calendar_2)

write.csv(calendar_comp,file="FOMC_dates.csv")
  




