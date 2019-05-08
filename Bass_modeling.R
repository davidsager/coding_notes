2019: x
2018: 245,240
2017: 103,097
2016: 76,295
2015: 50,580
2014: 31,655
2013: 22,477
2012:  2,650
2011:     0

tmp <- tibble(year = seq(2011,2019,1),
              value = c(NA_integer_, 2650, 22477, 31655, 50580,
                        76295,103097,245240, 394490),
              new = value - lag(value))

###
T <- 1:6 # time, year = 1979 + T
Sales <- c(19827, 9178, 18925, 25715, 26802, 142143)

Bass.nls<-nls(Sales ~ M*(((P+Q)^2/P)*exp(-(P+Q)*T))/
                (1+(Q/P)*exp(-(P+Q)*T))^2,
              # add P and Q below
              start=c(list(M=sum(Sales),P=...,Q=...)))
summary(Bass.nls)


