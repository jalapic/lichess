### Example Clustering Script

gen_Seq1 <- function(){
# Generate the first number between 1000 and 2000
first_number <- sample(1000:2000, 1)

# Generate random changes between -15 and 15
random_changes <- sample(-25:25, 19, replace = TRUE)

# Calculate the cumulative sum of random changes and adjust the range to stay between 1000 and 2000
cumulative_changes <- cumsum(random_changes)
num_vector <- pmax(pmin(first_number + c(0, cumulative_changes), 2000), 1000)

return(num_vector)
}

gen_Seq2 <- function(){
  # Generate the first number between 1000 and 2000
  first_number <- sample(1000:2000, 1)
  
  # Generate random changes between -15 and 15
  random_changes <- sample(-25:1, 19, replace = TRUE)
  
  # Calculate the cumulative sum of random changes and adjust the range to stay between 1000 and 2000
  cumulative_changes <- cumsum(random_changes)
  num_vector <- pmax(pmin(first_number + c(0, cumulative_changes), 2000), 1000)
  
  return(num_vector)
}


gen_Seq3 <- function(){
  # Generate the first number between 1000 and 2000
  first_number <- sample(1000:2000, 1)
  
  # Generate random changes between -15 and 15
  random_changes <- sample(-1:25, 19, replace = TRUE)
  
  # Calculate the cumulative sum of random changes and adjust the range to stay between 1000 and 2000
  cumulative_changes <- cumsum(random_changes)
  num_vector <- pmax(pmin(first_number + c(0, cumulative_changes), 2000), 1000)
  
  return(num_vector)
}



xx <- t(replicate(20,gen_Seq()))
xx2 <- t(replicate(20,gen_Seq2()))
xx3 <- t(replicate(20,gen_Seq3()))

xxx <- rbind(xx,xx2,xx3)
xxx

## PCA to look at data
pca <- princomp(xxx)
plot(pca)

## Clustering k-means

res.k <- kmeans(xxx, 3)
table(res.k$cluster)

res.k$cluster



library(tidyverse)
data.frame(xxx) %>%
  mutate(id=paste0("id",row_number())) %>%
  pivot_longer(cols=1:20, names_to = "time") ->dfxxx

dfxxx$time <- factor(dfxxx$time, levels=paste0("X",1:20))


dfxxx %>%
  ggplot(., aes(x=time,y=value,group=id))+
  geom_line()+
  theme_minimal()
