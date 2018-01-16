library(dplyr)
library(tidyr)

# https://stackoverflow.com/questions/30592094/r-spreading-multiple-columns-with-tidyr
# https://rpubs.com/bradleyboehmke/data_wrangling
# https://github.com/tclavelle/dplyr-tidyr-tutorial

# create dummy data
df <- data.frame(month = rep(1:3,2),
                 student = rep(c("Amy", "Bob"), each = 3),
                 A = c(9, 7, 6, 8, 6, 9),
                 B = c(6, 7, 8, 5, 6, 7))
df

# convert to long data, then convert to wide
df %>% gather(key = variable, value = value, -c(month, student)) %>%
        unite(col = placeholder, student, variable) %>% spread(key = placeholder, value = value)


#############################################################


# get difference of two variables by first pivoting long data to wide, 
# then getting diff, then pivot back to semi-long for ggplot
location <- c(rep(c("asia", "africa", "north america", "europe", "australia", "south america"), 2))
time <- c(rep(c(2000, 2015), each = 6))
millions <- c(rnorm(12, mean = 50, sd = 5))
data <- data.frame(location = location, time = time, millions = millions)
data <- data %>% mutate(log_millions = log(millions))

data %>% gather(key = variable, value = value, c(millions, log_millions)) %>% 
        spread(key = time, value = value) %>% 
        mutate(difference = `2000` - `2015`) %>% 
        gather(key = time, value = value, c(`2000`, `2015`), -c(location, difference)) %>%
        gather(key = variable2, value = value2, c(difference, value), -c(location, variable, time)) %>%
        unite(col = variable_type, variable, variable2) %>%
        spread(key = variable_type, value = value2) %>%
        data.frame()


###################################################################


setwd("C:/Users/Stephen/Desktop/R/shiny/grants/data")
list.files()

df2 <- read_csv("shiny_data_fake.csv")
df2 <- df2 %>% select(FY, Appl.State.Abbr, Best.EDA..) %>% mutate(Best.EDA.. = as.numeric(Best.EDA..))
glimpse(df2)
unique(df2$Appl.State.Abbr)
unique(df2$FY)

df2 %>% group_by(FY, Appl.State.Abbr) %>%
        summarize(amount = sum(Best.EDA..), count = n()) %>% gather(variable, value, amount, count) %>%
        unite(fy_variable, FY, variable) %>% spread(fy_variable, value)


#################################################


df <- data.frame(x = c("a", "b"), y = c(3, 4), z = c(5, 6))
df
df %>% spread(key = x, value = y) %>% gather(key = x, value = y, a:b, na.rm = TRUE)


#################################################


asdf <- data.frame(Col1 = rep(letters[1:5], each=2), Col2 = rep(c("Day1", "Day7")), 
                   Col3 = runif(10), Col4 = runif(10), Col5 = runif(10))
asdf

asdf %>% gather(key, val, Col3:Col5) %>% 
        unite(col = key2, Col2, key) %>% 
        spread(key = key2, value = val) 


