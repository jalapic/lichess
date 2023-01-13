### Initial Processing of data

ldata <- readRDS("rdata/lichess.Rdata")

length(ldata)

str(ldata[[1]]) #data.frame':	68823 obs. of  17 variables:
str(ldata[[2]])  #'data.frame':	4478 obs. of  23 variables:

colnames(ldata[[1]])
colnames(ldata[[2]])

str(ldata[[3]])  #data.frame':	417 obs. of  23 variables:
str(ldata[[4]])  #'data.frame':	124 obs. of  19 variables:
str(ldata[[5]])  #'data.frame':	4563 obs. of  22 variables:
str(ldata[[6]])  #'data.frame':	1051 obs. of  22 variables:
str(ldata[[7]])  #'data.frame':	1534 obs. of  23 variables:
str(ldata[[8]])  #'data.frame':	3608 obs. of  23 variables:
str(ldata[[9]])  #'data.frame':	17580 obs. of  23 variables:
str(ldata[[10]])  #'data.frame':	10376 obs. of  23 variables:
str(ldata[[11]])  #'data.frame':	2862 obs. of  23 variables:
str(ldata[[12]]) # long list
str(ldata[[13]]) # shorter list
str(ldata[[14]]) # long list


# drop empty elements
ldata[[13]] <- ldata[[13]][lapply(ldata[[13]],length)>0]
lapply(ldata[[13]],nrow) # 4 elements left, different 
str(ldata[[13]][[1]]) #data.frame':	3132 obs. of  23 variables:
str(ldata[[13]][[2]]) #'data.frame':	144 obs. of  20 variables:
str(ldata[[13]][[3]]) #'data.frame':	9 obs. of  19 variables:
str(ldata[[13]][[4]]) #'data.frame':	6650 obs. of  23 variables:


ldata[[14]] <- ldata[[14]][lapply(ldata[[14]],length)>0]
lapply(ldata[[14]],nrow) # 25 elements left, different 
range(unlist(lapply(ldata[[14]],nrow))) # 5-21039 rows
lapply(ldata[[14]],ncol) # 19-23 cols

ldata[[12]] <- ldata[[12]][lapply(ldata[[12]],length)>0]
lapply(ldata[[12]],nrow) # 19 elements left, different 
range(unlist(lapply(ldata[[12]],nrow))) # 21-14017 rows
lapply(ldata[[12]],ncol) # 19-23 cols


### But into better list:

ll <-list(ldata[[1]],ldata[[2]],ldata[[3]],ldata[[4]],ldata[[5]],ldata[[6]],ldata[[7]],ldata[[8]],ldata[[9]],ldata[[10]],ldata[[11]],
     ldata[[12]][[1]],ldata[[12]][[2]],ldata[[12]][[3]],ldata[[12]][[4]],ldata[[12]][[5]],ldata[[12]][[6]],ldata[[12]][[7]],
     ldata[[12]][[8]],ldata[[12]][[9]],ldata[[12]][[10]],ldata[[12]][[11]],ldata[[12]][[12]],ldata[[12]][[13]],ldata[[12]][[14]],
     ldata[[12]][[15]],ldata[[12]][[16]],ldata[[12]][[17]],ldata[[12]][[18]],ldata[[12]][[19]],
     ldata[[13]][[1]],ldata[[13]][[2]],ldata[[13]][[3]],ldata[[13]][[4]],
     ldata[[14]][[1]],ldata[[14]][[2]],ldata[[14]][[3]],ldata[[14]][[4]],ldata[[14]][[5]],ldata[[14]][[6]],ldata[[14]][[7]],
     ldata[[14]][[8]],ldata[[14]][[9]],ldata[[14]][[10]],ldata[[14]][[11]],ldata[[14]][[12]],ldata[[14]][[13]],ldata[[14]][[14]],
     ldata[[14]][[15]],ldata[[14]][[16]],ldata[[14]][[17]],ldata[[14]][[18]],ldata[[14]][[19]],ldata[[14]][[20]],ldata[[14]][[21]],
     ldata[[14]][[22]],ldata[[14]][[23]],ldata[[14]][[24]],ldata[[14]][[25]])

ll
colnames(ll[[3]])
head(ll[[3]])
# Keep just columns by name
ll1 <- lapply(ll, function(x) x[(names(x) %in% c("Username", "Date","White","Black","Result","UTCDate","UTCTime","WhiteElo",
                                          "BlackElo", "WhiteRatingDiff", "BlackRatingDiff","Variant",
                                          "TimeControl", "ECO", "Opening","WhiteTitle","BlackTitle","Event", "FEN","SetUp"))])


## Note that in first element of ll1, didn't include usernames...
ll1[[1]]$Username <- 'penguingim1'


#into one dataframe
dt <- data.table::rbindlist(ll1,fill=TRUE)
saveRDS(dt, "rdata/lichess_pilot.RData")

table(dt$Username)
