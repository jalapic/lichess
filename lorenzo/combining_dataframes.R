### combining dataframes

# rule 1:   all dataframes you are combining must have the same column names in the same order.


df1 <- data.frame(names = c("Steve", "Jose", "Marlon"), ages = c(16,24,63))

df2 <- data.frame(names = c("Marianne", "Melanie", "Myra"), ages = c(33,34,73))

df1

df2


rbind(df1,df2)


l <- list(df1,df2)

l

data.table::rbindlist(l)

