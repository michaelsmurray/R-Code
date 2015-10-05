
library(dplyr)
nic <- read.csv("NICtool.csv")

nic$UPC.number <- as.character(nic$UPC.number)

nic$nchar.UPC <- nchar(nic$UPC.number)




nic6 <- filter(nic, nchar.UPC == 6)
nic7 <- filter(nic, nchar.UPC == 7)
nic8 <- filter(nic, nchar.UPC == 8)
nic9 <- filter(nic, nchar.UPC == 9)
nic10 <- filter(nic, nchar.UPC == 10)
nic11 <- filter(nic, nchar.UPC == 11)
nic12 <- filter(nic, nchar.UPC == 12)

nic6$new.upc <- paste("000000",nic6$UPC.number,sep = '')
nic7$new.upc <- paste("00000",nic7$UPC.number,sep = '')
nic8$new.upc <- paste("0000",nic8$UPC.number,sep = '')
nic9$new.upc <- paste("000",nic9$UPC.number,sep = '')
nic10$new.upc <- paste("00",nic10$UPC.number,sep = '')
nic11$new.upc <- paste("0",nic11$UPC.number,sep = '')
# nic12$new.upc <- paste("000000",nic12$UPC.number,sep = '')

join1 <- bind_rows(nic6,nic7)
join2 <- bind_rows(join1,nic7)
join3 <- bind_rows(join2,nic9)
join4 <- bind_rows(join3,nic10)
join5 <- bind_rows(join4,nic11)
join6 <- bind_rows(join5, nic12)
nic <- join6 

x <- 3
old <- nic$new.upc
nic$UPC <- paste(substr(old, 1, x-1), "-", substr(old, x, nchar(old)), sep = "")

y <- 9
old <- nic$UPC
nic$UPC <- paste(substr(old, 1, y-1), "-", substr(old, y, nchar(old)), sep = "")

write.csv(nic, "NICtoolfixed.csv")