rm(list = ls())
library(RPostgreSQL)
setwd("...\\2018_104hackathon\\data")
con <- dbConnect(dbDriver("PostgreSQL"), 
                dbname = "", 
                host = "",
                port = 5432,
                user = "", password = "")

dbSendQuery(con,'SET CLIENT_ENCODING TO BIG5')
train_click <- dbGetQuery(con, "select * from train_click limit 100") # all rows are 738,000

# deal with the query string in train_click
tc_string <- train_click[,c(1,7)]
library(stringr)

## step1. 將字串依照"&"的數量分割成"&+1"欄
maxstr <- max(str_count(tc_string$querystring, "&")) + 1
test <- str_split_fixed(tc_string$querystring, "&", maxstr)
#View(test)

## step2. 將空白補滿，以便後續以matrix切割時方能正確計算
test2 <- ifelse(test == "", "blank=blank", test)
#View(test2)

## step3. 以matrix將"="切開，得出每個train_click的參數與值的欄位
test3 <- matrix(apply(test2, 1, function(x) str_split(x, "=")) %>% unlist(), byrow = T, ncol = maxstr * 2)
#View(test3)

## step4. decode the string.
library(urltools)
test4 <- apply(test3, 2, function(x) url_decode(x) %>% str_conv(encoding = "UTF-8"))
test4 <- cbind(tc_string$row.names, test4)
#View(test4)

## step5. 創造物件，準備mapping
library(dplyr)
arg_list <- c("area", "asc", "dep", "dis_role", "edu", "excludeCompanyKeyword", "excludeIndustryCat", "excludeJobKeyword", "expcat", "indcat", "isnew", "jobcat / cat", "jobexp", "jobsource", "keyword", "kwop", "lang", "m", "mode", "order", "ro", "rostatus", "s5", "s9", "sctp", "sr", "scmax", "scmin", "wf", "wt", "wktm", "zone")

index <- 1
map_fun <- function(x) {
  for (i in 1:length(arg_list)) {
    arg <- arg_list[i]
    arg_loc <- which(grepl(paste0("\\<", arg, "\\>"), x)) # \\<'string'\\> for full match, return the location in the matrix(calculated by row)
    if (identical(arg_loc, integer(0)) == T) next # 如果querystring沒有出現在arg_list上，則略過此次迴圈
    rownames_index <- ifelse(arg_loc %% nrow(x) == 0, nrow(x), arg_loc %% nrow(x)) # 因為若是最後一筆，使用餘數判斷法會找不到[0,1]的row.names，因此用個ifelse判斷，若餘數為0，則回傳第[nrow,1]筆
    rownames <- x[rownames_index,1]
    arg_value <- x[arg_loc + nrow(x)]
    arg_table <- data.frame("row_names" = rownames, "arg_value" = arg_value)
    colnames(arg_table)[2] <- arg # change the column names into arg name.
    
    if (index == 1) {
      final <- arg_table
      index <- 0
    } else {
      final <- full_join(final, arg_table, by = "row_names") %>% arrange(., row_names)
    }
  }
  return(final)
}
tc_clean <- map_fun(test4)
View(tc_clean)
