# For 2016
## Load Libraries
library(dplyr) ; library(tidyr) ; library(stringr)

## Import Data
food <- read.csv()  # Insert data directory
names(food) <- gsub( ".",  "", names(food), fixed = TRUE) ##Remove . from column names

food$n <- 1:nrow(food)  ##Give each row unique number
#a <- a %>% select(RowLabels, n)

## Filter out rows with specific labels
food_week <- filter(food, RowLabels == "1ST" | RowLabels == "2ND" | 
                   RowLabels == "3RD" | RowLabels == "4TH" | RowLabels == "5TH")
food_week$date <- seq(as.Date("2016/1/2"), as.Date("2017/1/1"), by = "week") #Give specific labels dates
food_week <- food_week %>% select(date, n) 
food <- left_join(food, food_week, by = "n") #Join both dataframes by unique numbers

food <- food %>% fill(date, .direction = "down") #Fill lower row with previous row value if current row NA

food <- food %>% select(date, RowLabels, contains("rice"), contains("maize"), contains("Millet"), 
                  contains("egg"), contains("oranges"), 
                  contains("herrings"), contains("palm"), contains("onion"), 
                  contains("tomatoes"), contains("red"), contains("plan"), 
                  contains("yam"), contains("sor"), contains("beef"), 
                  contains("pork"), contains("bird"), contains("c"))
food <- food %>% rename(town = RowLabels)
#a <- filter(a, town != "1ST" | town != "2ND" | town != "3RD" | town != "4TH" | town != "5TH") #Interestingly didn't work
food <- filter(food, !town %in% c("1ST", "2ND", "3RD", "4TH", "5TH")) #But this did
food_week <- filter(food, !town %in% c("JANUARY", "FEBRUARY", "MARCH", 
                            "APRIL", "MAY", "JUNE", "JULY", 
                            "AUGUST", "SEPTEMBER", "OCTOBER", 
                            "NOVEMBER", "DECEMBER", "(blank)"))
food_week$n <- 1:746
food_week <- filter(food_week, !n %in% 391:405)
food_week <- select(food_week, -n)
colnames(food_week)<- str_to_lower(colnames(food_week))
food_2016 <- food_week


# For 2015
## Load Libraries
library(dplyr) ; library(tidyr) ; library(stringr)

food <- read.csv()  # Insert data directory
names(food) <- gsub( ".",  "", names(food), fixed = TRUE) ##Remove . from column names

food$n <- 1:nrow(food)  ##Give each row unique number
#a <- a %>% select(RowLabels, n)

## Filter out rows with specific labels
food_week <- filter(food, RowLabels == "1ST" | RowLabels == "2ND" | RowLabels == "3RD" | RowLabels == "4TH" | RowLabels == "5TH")
food_week$date <- seq(as.Date("2015/1/3"), as.Date("2016/1/1"), by = "week") #Give specific labels dates
food_week <- food_week %>% select(date, n) 
food <- left_join(food, food_week, by = "n") #Join both dataframes by unique numbers

food <- food %>% fill(date, .direction = "down") #Fill lower row with previous row value if current row NA

food <- food %>% select(date, RowLabels, contains("rice"), contains("maize"), contains("Millet"), 
                  contains("egg"), contains("oranges"), 
                  contains("herrings"), contains("palm"), contains("onion"), 
                  contains("tomatoes"), contains("red"), contains("plan"), 
                  contains("yam"), contains("sor"), contains("beef"), 
                  contains("pork"), contains("bird"), contains("c"))
food <- food %>% rename(town = RowLabels)
#a <- filter(a, town != "1ST" | town != "2ND" | town != "3RD" | town != "4TH" | town != "5TH") #Interestingly didn't work
food <- filter(food, !town %in% c("1ST", "2ND", "3RD", "4TH", "5TH")) #But this did
food <- filter(food, !town %in% c("JANUARY", "FEBRUARY", "MARCH", 
                            "APRIL", "MAY", "JUNE", "JULY", 
                            "AUGUST", "SEPTEMBER", "OCTOBER", 
                            "NOVEMBER", "DECEMBER", "(blank)"))
colnames(food)<- str_to_lower(colnames(food))
food_2015 <- food
View(food_2015)

# Combine both dataframes
food <- rbind(food_2015, food_2016)
#food %>% select(date) 
#b <- colnames(select(food, -date, -town))
#p <- sapply(food[b], as.character, as.numeric)


View(food)

#Change data into numeric
food[,3] <- as.numeric(as.character(food[,3]))
food[,4] <- as.numeric(as.character(food[,4]))
food[,5] <- as.numeric(as.character(food[,5]))
food[,6] <- as.numeric(as.character(food[,6]))
food[,7] <- as.numeric(as.character(food[,7]))
food[,8] <- as.numeric(as.character(food[,8]))
food[,9] <- as.numeric(as.character(food[,9]))
food[,10] <- as.numeric(as.character(food[,10]))
food[,11] <- as.numeric(as.character(food[,11]))
food[,12] <- as.numeric(as.character(food[,12]))
food[,13] <- as.numeric(as.character(food[,13]))
food[,14] <- as.numeric(as.character(food[,14]))
food[,15] <- as.numeric(as.character(food[,15]))
food[,16] <- as.numeric(as.character(food[,16]))
food[,17] <- as.numeric(as.character(food[,17]))
food[,18] <- as.numeric(as.character(food[,18]))
food[,19] <- as.numeric(as.character(food[,19]))
food[,20] <- as.numeric(as.character(food[,20]))
food[,21] <- as.numeric(as.character(food[,21]))
food[,22] <- as.numeric(as.character(food[,22]))
food[,23] <- as.numeric(as.character(food[,23]))
food[,24] <- as.numeric(as.character(food[,24]))
food[,25] <- as.numeric(as.character(food[,25]))
food[,26] <- as.numeric(as.character(food[,26]))
food[,27] <- as.numeric(as.character(food[,27]))
food[,28] <- as.numeric(as.character(food[,28]))
food[,29] <- as.numeric(as.character(food[,29]))
food[,30] <- as.numeric(as.character(food[,30]))
food[,31] <- as.numeric(as.character(food[,31]))


food$town <- gsub("CAPECOAST", "CAPE COAST", food$town) 
food <- arrange(food, town)
food$n <- 1:nrow(food)
write.csv(food, "Ghana_Food_Prices.csv")