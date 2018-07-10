library(RPostgreSQL)
library(plyr)
library(urltools)
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "postgres",
                 host = "", port = ,
                 user = "", password = )


dbSendQuery(con,'SET CLIENT_ENCODING TO BIG5')
train_click <- dbGetQuery(con, "select * from train_click limit 100000") # all rows are 738,000
handle <- function(x){
  b <- unlist(strsplit(x,"&"))
  t <- str_conv(url_decode(sub(".*=","\\1",b)),encoding = "UTF-8")
  
  bb <- data.frame(matrix(t,ncol=length(b)))
  
  colnames(bb) <- sub("=.*","\\1",b)
  return(bb)
}
ptm <- proc.time()
t <- do.call("rbind.fill",lapply(train_click$querystring,handle))
proc.time() - ptm
#114 seconds 100000