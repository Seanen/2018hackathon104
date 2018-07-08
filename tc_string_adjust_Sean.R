wideScreen <- function(howWide=Sys.getenv("COLUMNS")) {
  options(width=as.integer(howWide))
}

library(DBI)
library(RPostgreSQL)
con <- dbConnect(dbDriver("PostgreSQL"),
                 dbname = "postgres",
                 host = "140.112.153.64",
                 port = 5432,
                 user = "admin", password = "12345")

train_click <- dbGetQuery(con, "select * from train_click limit 10")

library(stringr)
library(dplyr)
tc_string <- train_click[,c(7)]
tc_string <- tc_string[1:10]

go <-function(x) {
  a<-unlist(strsplit(unlist(strsplit(as.character(gsub('&',' ',x)), split=" ")),split="="))
  a[seq(a) %% 2 == 1]
  a[seq(a) %% 2 == 0]
  b<<-rbind(a[seq(a) %% 2 == 0], data.frame())
  colnames(b)<<-a[seq(a) %% 2 == 1]
  b <- b[ , order(names(b))]
  #colnames(b)
  x <- c("area","asc","jobcat","jobsource","keyword","kwop","mode","order","ro", "s9")
  mis <- x[!x %in% colnames(b)]
  
  if (identical(mis,character(0))!=TRUE) {
    datalist_value = list()

    for (i in mis) {
      datalist_value[[i]] <- data.frame(x='NA')
    }
    missing_datas = do.call(cbind, datalist_value)
    colnames(missing_datas) <- mis
    adj_x <- cbind(missing_datas,b)
    adj_x <- adj_x[ , order(names(adj_x))]
  } else {
    b
  }
}
lapply(tc_string, go)
adjust_tc_string <- do.call("rbind",lapply(tc_string, go))

tc_string
adjust_tc_string

