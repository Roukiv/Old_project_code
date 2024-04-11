require(tidyverse)

# setwd(" ")

load("January_2013.RData")
load("January_2014.RData")
load("January_2015.RData")
Characteristics <- read.csv("Characteristics.csv", stringsAsFactors=FALSE)

head(Characteristics)

############################################
#                    1                     #
############################################
Characteristics <- mutate(Characteristics,
                           TRANSFORMER_TYPE = as.factor(TRANSFORMER_TYPE))
summary(Characteristics[,2:6])
cor(Characteristics[,3:6])


ggplot(Characteristics, aes(x = Percentage_IC, fill = factor(TRANSFORMER_TYPE))) +
  geom_histogram(aes(y = ..count.. / (sum(..count..))), binwidth = 0.02,colour="black") +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Histogram plot of industrial and commercial customers") +
  labs(x="Percentage of industrial and commercial customers",y="Density")

ggplot(Characteristics, aes(x = Transformer_RATING, fill = factor(TRANSFORMER_TYPE))) +
  geom_histogram(aes(y = ..count.. / (sum(..count..))), binwidth = 50,colour="black") +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Histogram plot of transformer ratings") +
  labs(x="Transformer ratings",y="Density")

ggplot(Characteristics, aes(x = TRANSFORMER_TYPE  )) +
  geom_bar(aes(y = ..count.. / (sum(..count..))),fill="cyan",colour="black") +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Bar chart of transformer type") +
  labs(x="Transformer type",y="Density")

ggplot(Characteristics, aes(x = TOTAL_CUSTOMERS, fill = factor(TRANSFORMER_TYPE))) +
  geom_histogram(aes(y = ..count.. / (sum(..count..))), binwidth = 50,colour="black") +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Histogram plot of total customers") +
  labs(x="Total customers",y="Density")

ggplot(Characteristics, aes(x = LV_FEEDER_COUNT, fill = factor(TRANSFORMER_TYPE))) +
  geom_histogram(aes(y = ..count.. / (sum(..count..))), binwidth = 1,colour="black") +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Histogram plot of number of feeders coming from substation") +
  labs(x="The number of feeders coming from substation",y="Density")


ggplot(Characteristics, aes(x = Transformer_RATING, y = Percentage_IC, 
                            color = factor(TRANSFORMER_TYPE))) +
  geom_point(alpha = 1/4) +
  ggtitle("Histogram plot of transformer ratings") +
  scale_y_continuous(labels = scales::percent) +
  labs(x="Transformer ratings",
       y="Percentage of industrial and commercial customers",
       color = "Transformer type")


ggplot(Characteristics, aes(x = TOTAL_CUSTOMERS, y = LV_FEEDER_COUNT, 
                            color = factor(TRANSFORMER_TYPE))) +
  geom_point(alpha = 1/4) +
  labs(x="Total customer",
       y="Number of feeders from substation",
       color = "Transformer type")

############################################
#                    2                     #
############################################


############################################
#                part 1 & 2                #
############################################

# wrangling the data to a dataset that holds all data of substation of each day of week
set.seed(1234)

January_2013$day <- weekdays(as.Date(January_2013$Date))

subs <- sort(unique(January_2013$Substation))
days <- sort(unique(January_2013$day))
January_2013 <- na.omit(January_2013)

# rearrange the data to reduce the size (my laptop can't handle the original one)
c2013 <- rbind(data.frame(),c("substation","day",1:144))
e <- c()
for (i in 1:length(subs)){
  for (j in 1:length(days)){
    e <- c(e, subs[i], days[j])
    test <- filter(January_2013,Substation == subs[i] & day == days[j])[,3:146]
    for (k in 1:144){
      e <- c(e, mean(unlist(test[,k])))
    }
    c2013 <- rbind(c2013,e)
    e <- c()
  }
}
c2013 <- c2013[-1,]

c2013v2 <- data.frame(sapply(c2013[,1:146], function(x) as.double(as.character(x))))
c2013v3 <- cbind(c2013[,1:2],c2013v2[,3:146])
c2013v3 <- na.omit(c2013v3)


# distance matrix
x <- dist(as.matrix(c2013v3[,3:146]))


require(cluster)
require(factoextra)
require(rdist)
require(gridExtra)


# decide no. of cluster

wss <- function(k) {
  kmeans(c2013v3, k, nstart = 10 )$tot.withinss
}
fviz_nbclust(c2013v3[,3:146], kmeans, method = "wss", k.max=20)

# so 5 to 11 cluster


for (i in 3:11){
  p <- fviz_cluster(kmeans(c2013v3[,3:146], centers = i, nstart = 25), 
             data = c2013v3[,3:146], geom="point")
  print(p)
}
set.seed(1234)
cutcluster6 <- kmeans(c2013v3[,3:146], centers = 6, nstart = 25)
fviz_cluster(cutcluster6, data = c2013v3[,3:146], geom="point")

# dendrogram
hc <- hclust(x)
plot(hc, hang=-1)
rect.hclust(hc, k = 6)



# label each substation according to its cluster membership
c2013v3 <- cbind(c2013v3,cutcluster6$cluster) %>%
  rename(c("cluster" = "cutcluster6$cluster")) %>%
  rename(c("day" = "X.day.")) %>%
  rename(c("substation" = "X.substation."))
  
head(c2013v3[,c(1,2,147)])


############################################
#                   part 3                 #
############################################

time_inmins <- format(as.POSIXct((c(0:143)/144) * 86400, 
                          origin = "1970-01-01", tz = "UTC"), "%H:%M")
time_ <- time_inmins[seq(1, length(time_inmins), 6)]
tit <- c("Daily average demand for cluster ","Weekday average demand for cluster ",
         "Saturday average demand for cluster ","Sunday average demand for cluster ")
df <- as.data.frame(c2013v3)
dfs1 <- filter(df,day != "Sunday" & day !="Saturday")
dfs2 <- filter(df,day == "Saturday")
dfs3 <- filter(df,day == "Sunday")
for (i in 1:6){
  for (k in 1:4){
    if (k == 1) {df2 <- df} 
    else if (k == 2) {df2 <- dfs1} 
    else if (k == 3) {df2 <- dfs2} 
    else {df2 <- dfs3}
    df2 <- filter(df2,cluster == i)
    dad <- c()
    for (j in 1:144){
      dad <- c(dad,mean(df2[,j+2]))
    }
    df2 <- cbind(as.data.frame(time_inmins), as.data.frame(dad))
    tit2 <- paste(c(tit[k],i),collapse="")
    p1 <- ggplot(df2,aes(x=time_inmins,y=dad)) +
      labs(title = tit2,x = "Time in 10 minutes", 
           y = "Average substation measurements") +
      geom_line(color="red",group = 1)+
      geom_point() +
      scale_x_discrete(breaks=time_)
    print(p1)
  }
}


############################################
#                part 4 & 5                #
############################################

# remove duplicated element
dup <- Characteristics$SUBSTATION_NUMBER
dup[duplicated(dup)] # 563225
which(grepl(563225, Characteristics$SUBSTATION_NUMBER))
Characteristics <- Characteristics[-909,]

# remove data without cluster
share<-match(unique(Characteristics$SUBSTATION_NUMBER),unique(January_2013$Substation))
Characteristics <- Characteristics[-c(which(is.na(share))),]

# check is there any multiple cluster Substation
c2013v3 <- arrange(c2013v3,substation)
double_cluster <- c()
for (i in 1:535){
  dc <- length(unique(c2013v3[which(grepl(Characteristics[i,1], 
                                          c2013v3$substation)),147]))
  if (dc > 1) {double_cluster <- c(double_cluster,Characteristics[i,1])}
}

# cluster situation
cluster_list <- list()
for (i in 1:535){
  cluster_numb <- list(as.character(unique(c2013v3[which(grepl(Characteristics[i,1],
                                                            c2013v3$substation)),147])))
  cluster_list <- append(cluster_list,cluster_numb)
}

# Summary

c <- as.character(c(1:6))
Characteristics2 <- Characteristics
Characteristics2 <- mutate(Characteristics2,
                           TRANSFORMER_TYPE = as.factor(TRANSFORMER_TYPE))

for (i in 1:6){
  print(paste(i,"th cluster summary"))
  print(summary(Characteristics2[which(grepl(c[i], Characteristics2[,1])),2:6]))
}


############################################
#                    3                     #
############################################

############################################
#                  part 1                  #
############################################

# reusing the code from previous part
nsub <- read.csv("NewSubstations.csv", stringsAsFactors=FALSE)


nsub$day <- weekdays(as.Date(nsub$Date))
subs <- sort(unique(nsub$Substation))
days <- sort(unique(nsub$day))
nsub <- na.omit(nsub)

# rearrange the data to reduce the size (my laptop can't handle the original one)
cnsub <- rbind(data.frame(),c("substation","day",1:144))
e <- c()
for (i in 1:length(subs)){
  for (j in 1:length(days)){
    e <- c(e, subs[i], days[j])
    test <- filter(nsub,Substation == subs[i] & day == days[j])[,3:146]
    for (k in 1:144){
      e <- c(e, mean(unlist(test[,k])))
    }
    cnsub <- rbind(cnsub,e)
    e <- c()
  }
}
cnsub <- cnsub[-1,]
cnsubv2 <- data.frame(sapply(cnsub[,1:146], function(x) as.double(as.character(x))))
cnsubv2 <- cbind(cnsub[,1:2],cnsubv2[,3:146])

# Plots
time_inmins <- format(as.POSIXct((c(0:143)/144) * 86400, 
                                 origin = "1970-01-01", tz = "UTC"), "%H:%M")
time_ <- time_inmins[seq(1, length(time_inmins), 6)]
tit <- c("Daily average demand for Substation ","Weekday average demand for Substation ",
         "Saturday average demand for Substation ","Sunday average demand for Substation ")
sta <- unique(cnsub[,1])

df <- as.data.frame(cnsubv2) 

dfs1 <- filter(df,X.day. != "Sunday" & X.day. !="Saturday")
dfs2 <- filter(df,X.day. == "Saturday")
dfs3 <- filter(df,X.day. == "Sunday")
for (i in 1:5){
  for (k in 1:4){
    if (k == 1) {df2 <- df} 
    else if (k == 2) {df2 <- dfs1} 
    else if (k == 3) {df2 <- dfs2} 
    else {df2 <- dfs3}
    df2 <- filter(df2,X.substation. == sta[i])
    dad <- c()
    for (j in 1:144){
      dad <- c(dad,mean(df2[,j+2]))
    }
    df2 <- cbind(as.data.frame(time_inmins), as.data.frame(dad))
    
    tit2 <- paste(c(tit[k],sta[i]),collapse="")
    p1 <- ggplot(df2,aes(x=time_inmins,y=dad)) +
      labs(title = tit2,x = "Time in 10 minutes", 
           y = "Substation measurements") +
      geom_line(color="red",group = 1)+
      geom_point() +
      scale_x_discrete(breaks=time_)
    print(p1)
  }
}

############################################
#               part 2 & 3                 #
############################################

# recalculate cluster with 5 new substation

cnsubv2 <- cnsubv2 %>%
  rename(c("day" = "X.day.")) %>%
  rename(c("substation" = "X.substation."))
rc <- rbind(c2013v3[,1:146],cnsubv2)
set.seed(1234)
cutcluster6 <- kmeans(rc[,3:146], centers = 6, nstart = 25)

# cluster for the new ones in ascending substation number
for (i in 1:5){
  s <- 3733+7*(i-1)
  e <- 3739+7*(i-1)
  print(sta[i])
  print(unique(cutcluster6$cluster[s:e]))
}

# get the data for the new substation
Characteristics <- read.csv("Characteristics.csv", stringsAsFactors=FALSE)
e <- c()
for (i in 1:5) {
  e <- c(e,which(grepl(sta[i], Characteristics$SUBSTATION_NUMBER)))
}

Characteristics[e,]


############################################
#                    4                     #
############################################

# Assume 2014 and 2015 have similar situation
# So same process as before for 2014, 2015 data

January_2014$day <- weekdays(as.Date(January_2014$Date))

subs <- sort(unique(January_2014$Substation))
days <- sort(unique(January_2014$day))
January_2014 <- na.omit(January_2014)

# rearrange the data to reduce the size (my laptop can't handle the original one)
c2014 <- rbind(data.frame(),c("substation","day",1:144))
e <- c()
for (i in 1:length(subs)){
  for (j in 1:length(days)){
    e <- c(e, subs[i], days[j])
    test <- filter(January_2014,Substation == subs[i] & day == days[j])[,3:146]
    for (k in 1:144){
      e <- c(e, mean(unlist(test[,k])))
    }
    c2014 <- rbind(c2014,e)
    e <- c()
  }
}
c2014 <- c2014[-1,]
c2014v2 <- data.frame(sapply(c2014[,1:146], function(x) as.double(as.character(x))))
c2014 <- cbind(c2014[,1:2],c2014v2[,3:146])
c2014 <- na.omit(c2014)


# decide no. of cluster

wss <- function(k) {
  kmeans(c2014, k, nstart = 10 )$tot.withinss
}
fviz_nbclust(c2014[,3:146], kmeans, method = "wss", k.max=20)

for (i in 3:11){
  p <- fviz_cluster(kmeans(c2014[,3:146], centers = i, nstart = 25), 
                    data = c2014[,3:146], geom="point")
  print(p)
}


set.seed(1234)
cutcluster6_14 <- kmeans(c2014[,3:146], centers = 6, nstart = 25)

# label each substation according to its cluster membership
c2014 <- cbind(c2014,cutcluster6_14$cluster) %>%
  rename(c("cluster" = "cutcluster6_14$cluster")) %>%
  rename(c("day" = "X.day.")) %>%
  rename(c("substation" = "X.substation."))

# check same substation or not
match(unique(c2014$substation),unique(c2013v3$substation))

# cluster situation
cluster_list_2014 <- list()
for (i in 1:535){
  cluster_numb <- list(as.character(unique(c2014[which(grepl(Characteristics2[i,1],
                                                             c2014$substation)),147])))
  cluster_list_2014 <- append(cluster_list_2014,cluster_numb)
}


fviz_cluster(cutcluster6_14, data = c2014[,3:146], geom="point")


df <- as.data.frame(c2014)
for (i in 1:6){
  df2 <- df
  df2 <- filter(df2,cluster == i)
  dad <- c()
  for (j in 1:144){
    dad <- c(dad,mean(df2[,j+2]))
  }
  df2 <- cbind(as.data.frame(time_inmins), as.data.frame(dad))
  tit2 <- paste(c("2014 ",tit[1],i),collapse="")
  p1 <- ggplot(df2,aes(x=time_inmins,y=dad)) +
    labs(title = tit2,x = "Time in 10 minutes", 
         y = "Average substation measurements") +
    geom_line(color="red",group = 1)+
    geom_point() +
    scale_x_discrete(breaks=time_)
  print(p1)
}




#----------------------------------------------------------------------

# 2015

January_2015$day <- weekdays(as.Date(January_2015$Date))

subs <- sort(unique(January_2015$Substation))
days <- sort(unique(January_2015$day))
January_2015 <- na.omit(January_2015)

# rearrange the data to reduce the size (my laptop can't handle the original one)
c2015 <- rbind(data.frame(),c("substation","day",1:144))
e <- c()
for (i in 1:length(subs)){
  for (j in 1:length(days)){
    e <- c(e, subs[i], days[j])
    test <- filter(January_2015,Substation == subs[i] & day == days[j])[,3:146]
    for (k in 1:144){
      e <- c(e, mean(unlist(test[,k])))
    }
    c2015 <- rbind(c2015,e)
    e <- c()
  }
}
c2015 <- c2015[-1,]
c2015v2 <- data.frame(sapply(c2015[,1:146], function(x) as.double(as.character(x))))
c2015 <- cbind(c2015[,1:2],c2015v2[,3:146])
c2015 <- na.omit(c2015)


# decide no. of cluster

wss <- function(k) {
  kmeans(c2015, k, nstart = 10 )$tot.withinss
}
fviz_nbclust(c2015[,3:146], kmeans, method = "wss", k.max=20)

for (i in 3:11){
  p <- fviz_cluster(kmeans(c2015[,3:146], centers = i, nstart = 25), 
                    data = c2015[,3:146], geom="point")
  print(p)
}


set.seed(1234)
cutcluster6_15 <- kmeans(c2015[,3:146], centers = 6, nstart = 25)

# label each substation according to its cluster membership
c2015 <- cbind(c2015,cutcluster6_15$cluster) %>%
  rename(c("cluster" = "cutcluster6_15$cluster")) %>%
  rename(c("day" = "X.day.")) %>%
  rename(c("substation" = "X.substation."))

# check same substation or not
match(unique(c2015$substation),unique(c2013v3$substation))

# cluster situation
cluster_list_2015 <- list()
for (i in 1:535){
  cluster_numb <- list(as.character(unique(c2015[which(grepl(Characteristics2[i,1],
                                                             c2015$substation)),147])))
  cluster_list_2015 <- append(cluster_list_2015,cluster_numb)
}


fviz_cluster(cutcluster6_15, data = c2015[,3:146], geom="point")


df <- as.data.frame(c2015)
for (i in 1:6){
  df2 <- df
  df2 <- filter(df2,cluster == i)
  dad <- c()
  for (j in 1:144){
    dad <- c(dad,mean(df2[,j+2]))
  }
  df2 <- cbind(as.data.frame(time_inmins), as.data.frame(dad))
  tit2 <- paste(c("2015 ",tit[1],i),collapse="")
  p1 <- ggplot(df2,aes(x=time_inmins,y=dad)) +
    labs(title = tit2,x = "Time in 10 minutes", 
         y = "Average substation measurements") +
    geom_line(color="red",group = 1)+
    geom_point() +
    scale_x_discrete(breaks=time_)
  print(p1)
}


#--------------------------------------

# I will use 2013 as a standard
# change everything to match 2013


cl13 <- cluster_list
cl14 <- cluster_list_2014
cl15 <- cluster_list_2015


#cl14 <- rapply(cl14,function(x) ifelse(x=="1","A",x), how = "replace")
cl14 <- rapply(cl14,function(x) ifelse(x=="2","B",x), how = "replace")
cl14 <- rapply(cl14,function(x) ifelse(x=="3","C",x), how = "replace")
cl14 <- rapply(cl14,function(x) ifelse(x=="4","D",x), how = "replace")
cl14 <- rapply(cl14,function(x) ifelse(x=="5","E",x), how = "replace") 
cl14 <- rapply(cl14,function(x) ifelse(x=="6","F",x), how = "replace") 
#cl14 <- rapply(cl14,function(x) ifelse(x=="A","5",x), how = "replace") 
cl14 <- rapply(cl14,function(x) ifelse(x=="B","6",x), how = "replace")
cl14 <- rapply(cl14,function(x) ifelse(x=="C","4",x), how = "replace") 
cl14 <- rapply(cl14,function(x) ifelse(x=="D","3",x), how = "replace")
cl14 <- rapply(cl14,function(x) ifelse(x=="E","2",x), how = "replace")
cl14 <- rapply(cl14,function(x) ifelse(x=="F","5",x), how = "replace")

cl15 <- rapply(cl15,function(x) ifelse(x=="1","A",x), how = "replace")
cl15 <- rapply(cl15,function(x) ifelse(x=="2","B",x), how = "replace")
cl15 <- rapply(cl15,function(x) ifelse(x=="3","C",x), how = "replace")
cl15 <- rapply(cl15,function(x) ifelse(x=="4","D",x), how = "replace")
cl15 <- rapply(cl15,function(x) ifelse(x=="5","E",x), how = "replace") 
cl15 <- rapply(cl15,function(x) ifelse(x=="6","F",x), how = "replace")
cl15 <- rapply(cl15,function(x) ifelse(x=="A","5",x), how = "replace")
cl15 <- rapply(cl15,function(x) ifelse(x=="B","1",x), how = "replace")
cl15 <- rapply(cl15,function(x) ifelse(x=="C","2",x), how = "replace")
cl15 <- rapply(cl15,function(x) ifelse(x=="D","6",x), how = "replace")
cl15 <- rapply(cl15,function(x) ifelse(x=="E","4",x), how = "replace") 
cl15 <- rapply(cl15,function(x) ifelse(x=="F","3",x), how = "replace")

#--------------------------------------
clf <- cbind(cl13,cl14,cl15)

# sort element in the list of cluster
for (i in 1:3){
  for (j in 1:535){
    clf[j,i] <- list(sort(as.character(unlist(clf[j,i]))))
  }
}

clf <- cbind(clf,rep(0,535),rep(0,535),rep(0,535),rep(0,535))
for (j in 1:535){
  c34 <- as.character(unlist(clf[j,1]))==as.character(unlist(clf[j,2]))
  c45 <- as.character(unlist(clf[j,2]))==as.character(unlist(clf[j,3]))
  c35 <- as.character(unlist(clf[j,1]))==as.character(unlist(clf[j,3]))
  if (sum(c34 == FALSE) > 0){clf[j,4]<-1}
  if (sum(c45 == FALSE) > 0){clf[j,5]<-1}
  if (sum(c35 == FALSE) == 0 & clf[j,5] == 1){clf[j,6]<-1}
  if (sum(c34 == FALSE) == 1 & sum(c45 == FALSE) == 1 & clf[j,6] == 0){clf[j,7]<-1}
}

# change in 13 to 14
sum(as.data.frame(clf[,4])) # 126

# change in 14 to 15
sum(as.data.frame(clf[,5])) # 138

# change back to 13 cluster
# data variability check
sum(as.data.frame(clf[,6])) # 72

# continuously change in years and not back to 13 value
sum(as.data.frame(clf[,7])) # 11

# no change
sum(clf[,4] == 0 & clf[,5] == 0) # 359

# only change 13 to 14
sum(clf[,4] == 1 & clf[,5] == 0) # 38

# only change 14 to 15
sum(clf[,4] == 0 & clf[,5] == 1) # 50




