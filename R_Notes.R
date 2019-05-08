library(rvest)
df <- url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[1]') %>%
  html_table() %>%
  flatten_df()

df2 <- c("Year\tTitle\tWorldwide gross\tBudget\tReference(s)", "1915\tThe Birth of a Nation\t$50,000,000–100,000,000", "$20,000,000+R ($5,200,000)R\t$110,000\t[# 87][# 88][# 89]", "1916\tIntolerance\t$1,000,000*R IN\t$489,653\t[# 90][# 91]", "1917\tCleopatra\t$500,000*R\t$300,000\t[# 90]", "1918\tMickey\t$8,000,000\t$250,000\t[# 92]", "1919\tThe Miracle Man\t$3,000,000R\t$120,000\t[# 93]", "1920\tWay Down East\t$5,000,000R ($4,000,000)R\t$800,000\t[# 94][# 95]", "1921\tThe Four Horsemen of the Apocalypse\t$5,000,000R ($4,000,000)R\t$600,000–800,000\t[# 96]", "1922\tRobin Hood\t$2,500,000R\t$930,042.78\t[# 97][# 98]", "1923\tThe Covered Wagon\t$5,000,000R\t$800,000\t[# 99][# 100]", "1924\tThe Sea Hawk\t$3,000,000R\t$700,000\t[# 99]", "1925\tThe Big Parade\t$18,000,000–22,000,000R", "($6,131,000)R\t$382,000\t[# 101][# 102][# 103]")
#____________________________________________________

# 1. Doing class( ) allows you to see what type of data something I assigned is. Options are: Numeric, Character, or Logical
# 2. Vector (used to store data): create it by using c(). A vector just stores data.
# 3. Names() allows me to name combine 2 vectors like days of week and winning.
# 4. Matrix. matrix(1:9, byrow = TRUE, nrow = 3). To organize by column, byrow = FALSE
  # rowSums sums the row
  # rownames(my_vector) <- 
# 5. cbind(). Combine R Objects By Rows Or Columns. Take a sequence of vector, matrix or data-frame arguments and combine by columns or rows, respectively
# 6. head() enables you to show the first observations of a data frame. Also tail()
# 7. data.frame
# 8. str() shows the structure of a data set. In a data frame, that means: The total number of observations (e.g. 32 car types)
      #The total number of variables (e.g. 11 car features)
      #A full list of the variables names (e.g. mpg, cyl ... )
      #The data type of each variable (e.g. num)
      #The first observations
#9. $. If I have a column called "Rings", to call it up, just use planets_df$Rings
#10. subset(). subset(planets_df, subset = rings). The first argument of subset() specifies the data set for which you want a subset. By adding the second argument, you give R the necessary information and conditions to select the correct subset.
#11. order() is a function that gives you the ranked position of each element when it is applied on a variable, such as a vector for example:
#12. iif (condition1) {
expr1
} else if (condition2) {
  expr2
} else if (condition3) {
  expr3
} else {
  expr4
}
#13. ctr <- 1 means counter of 1
#14. nchar = number of characters. So i can search for something w 4 characters by nchar(x) == 4
#15. print means print. next means skip.
#16. For Loops. primes <- c(2, 3, 5, 7, 11, 13)

# loop version 1
#for (p in primes) {
  #print(p)
}

# loop version 2
#for (i in 1:length(primes)) {
  #print(primes[i])
}
#17. args(FUNCTION). input a funcion (like sd (standard deviation)) and it will expalin what it does
#18. lapply(x, question). x = vector (say, of cities). question = what you're trying to find out (nchar, class, etc.) 
#19. To turn a data frame (df) into a tibble, us as_tibble(df)