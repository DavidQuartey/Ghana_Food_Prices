# Load Library
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(ggthemes)

# Import data into R
food <- read.csv() # Insert directory to cleaned data

# Separate data to change class then combine back
food_date <- select(food, n, date, town) ; food_date$date <- as.Date(food$date, format = "%Y-%m-%d")
View(food_date)
food_commodities <- select(food, -town, -date) %>% mutate_if(is.character, as.numeric)
View(food_commodities)


# Join both dataframes by n column
food <- left_join(food_commodities, food_date, by = "n")
food <- select(food, -X)
View(food)


### Easy run script for Food Commodity Volatility by Town ####
accra <- filter(food, town == "ACCRA")
accra$year <- format(as.Date(accra$date), format = "%Y-%m")
accra <- group_by(accra, year)
select(accra, -date, -town)
a <- colnames(accra)
accra <- summarise(accra, maize = mean(maize))
accra$year <- paste0(accra$year, "-1")
accra$year <- as.Date(as.character(accra$year), format = "%Y-%m-%d")


b <- filter(food, town == "ACCRA")
b$year <- format(as.Date(b$date), format = "%Y-%m")
b <- group_by(b, year)
select(b, -date, -town)
a <- colnames(accra)
b <- summarise(b, millet = mean(millet))
b$year <- paste0(b$year, "-1")
b$year <- as.Date(as.character(b$year), format = "%Y-%m-%d")

z <- merge(accra, b, all.x = TRUE, by = "year")

z$town <- "ACCRA"


c <- filter(food, town == "ACCRA")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, eggscommercial = mean(eggscommercial))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

c <- filter(food, town == "ACCRA")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, tomatoes = mean(tomatoes))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

c <- filter(food, town == "ACCRA")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, ricelocallocalperfumed = mean(ricelocallocalperfumed))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

c <- filter(food, town == "ACCRA")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, riceimportedperfumed = mean(riceimportedperfumed))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

y <- z

############################################################

accra <- filter(food, town == "WA")
accra$year <- format(as.Date(accra$date), format = "%Y-%m")
accra <- group_by(accra, year)
select(accra, -date, -town)
a <- colnames(accra)
accra <- summarise(accra, maize = mean(maize))
accra$year <- paste0(accra$year, "-1")
accra$year <- as.Date(as.character(accra$year), format = "%Y-%m-%d")


b <- filter(food, town == "WA")
b$year <- format(as.Date(b$date), format = "%Y-%m")
b <- group_by(b, year)
select(b, -date, -town)
a <- colnames(accra)
b <- summarise(b, millet = mean(millet))
b$year <- paste0(b$year, "-1")
b$year <- as.Date(as.character(b$year), format = "%Y-%m-%d")

z <- merge(accra, b, all.x = TRUE, by = "year")

z$town <- "WA"


c <- filter(food, town == "WA")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, eggscommercial = mean(eggscommercial))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

c <- filter(food, town == "WA")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, tomatoes = mean(tomatoes))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

c <- filter(food, town == "WA")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, ricelocallocalperfumed = mean(ricelocallocalperfumed))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

c <- filter(food, town == "WA")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, riceimportedperfumed = mean(riceimportedperfumed))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

x <- z

###########################################################################

accra <- filter(food, town == "BOLGATANGA")
accra$year <- format(as.Date(accra$date), format = "%Y-%m")
accra <- group_by(accra, year)
select(accra, -date, -town)
a <- colnames(accra)
accra <- summarise(accra, maize = mean(maize))
accra$year <- paste0(accra$year, "-1")
accra$year <- as.Date(as.character(accra$year), format = "%Y-%m-%d")


b <- filter(food, town == "BOLGATANGA")
b$year <- format(as.Date(b$date), format = "%Y-%m")
b <- group_by(b, year)
select(b, -date, -town)
a <- colnames(accra)
b <- summarise(b, millet = mean(millet))
b$year <- paste0(b$year, "-1")
b$year <- as.Date(as.character(b$year), format = "%Y-%m-%d")

z <- merge(accra, b, all.x = TRUE, by = "year")

z$town <- "BOLGATANGA"


c <- filter(food, town == "BOLGATANGA")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, eggscommercial = mean(eggscommercial))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

c <- filter(food, town == "BOLGATANGA")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, tomatoes = mean(tomatoes))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

c <- filter(food, town == "BOLGATANGA")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, ricelocallocalperfumed = mean(ricelocallocalperfumed))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

c <- filter(food, town == "BOLGATANGA")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, riceimportedperfumed = mean(riceimportedperfumed))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

w <- z

###########################################################################

accra <- filter(food, town == "CAPE COAST")
accra$year <- format(as.Date(accra$date), format = "%Y-%m")
accra <- group_by(accra, year)
select(accra, -date, -town)
a <- colnames(accra)
accra <- summarise(accra, maize = mean(maize))
accra$year <- paste0(accra$year, "-1")
accra$year <- as.Date(as.character(accra$year), format = "%Y-%m-%d")


b <- filter(food, town == "CAPE COAST")
b$year <- format(as.Date(b$date), format = "%Y-%m")
b <- group_by(b, year)
select(b, -date, -town)
a <- colnames(accra)
b <- summarise(b, millet = mean(millet))
b$year <- paste0(b$year, "-1")
b$year <- as.Date(as.character(b$year), format = "%Y-%m-%d")

z <- merge(accra, b, all.x = TRUE, by = "year")

z$town <- "CAPE COAST"


c <- filter(food, town == "CAPE COAST")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, eggscommercial = mean(eggscommercial))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

c <- filter(food, town == "CAPE COAST")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, tomatoes = mean(tomatoes))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

c <- filter(food, town == "CAPE COAST")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, ricelocallocalperfumed = mean(ricelocallocalperfumed))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

c <- filter(food, town == "CAPE COAST")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, riceimportedperfumed = mean(riceimportedperfumed))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

v <- z

###########################################################################

accra <- filter(food, town == "HO")
accra$year <- format(as.Date(accra$date), format = "%Y-%m")
accra <- group_by(accra, year)
select(accra, -date, -town)
a <- colnames(accra)
accra <- summarise(accra, maize = mean(maize))
accra$year <- paste0(accra$year, "-1")
accra$year <- as.Date(as.character(accra$year), format = "%Y-%m-%d")


b <- filter(food, town == "HO")
b$year <- format(as.Date(b$date), format = "%Y-%m")
b <- group_by(b, year)
select(b, -date, -town)
a <- colnames(accra)
b <- summarise(b, millet = mean(millet))
b$year <- paste0(b$year, "-1")
b$year <- as.Date(as.character(b$year), format = "%Y-%m-%d")

z <- merge(accra, b, all.x = TRUE, by = "year")

z$town <- "HO"


c <- filter(food, town == "HO")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, eggscommercial = mean(eggscommercial))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

c <- filter(food, town == "HO")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, tomatoes = mean(tomatoes))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

c <- filter(food, town == "HO")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, ricelocallocalperfumed = mean(ricelocallocalperfumed))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

c <- filter(food, town == "HO")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, riceimportedperfumed = mean(riceimportedperfumed))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

u <- z

###########################################################################

accra <- filter(food, town == "EJURA")
accra$year <- format(as.Date(accra$date), format = "%Y-%m")
accra <- group_by(accra, year)
select(accra, -date, -town)
a <- colnames(accra)
accra <- summarise(accra, maize = mean(maize))
accra$year <- paste0(accra$year, "-1")
accra$year <- as.Date(as.character(accra$year), format = "%Y-%m-%d")


b <- filter(food, town == "EJURA")
b$year <- format(as.Date(b$date), format = "%Y-%m")
b <- group_by(b, year)
select(b, -date, -town)
a <- colnames(accra)
b <- summarise(b, millet = mean(millet))
b$year <- paste0(b$year, "-1")
b$year <- as.Date(as.character(b$year), format = "%Y-%m-%d")

z <- merge(accra, b, all.x = TRUE, by = "year")

z$town <- "EJURA"


c <- filter(food, town == "EJURA")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, eggscommercial = mean(eggscommercial))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

c <- filter(food, town == "EJURA")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, tomatoes = mean(tomatoes))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

c <- filter(food, town == "EJURA")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, ricelocallocalperfumed = mean(ricelocallocalperfumed))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

c <- filter(food, town == "EJURA")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, riceimportedperfumed = mean(riceimportedperfumed))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

t <- z

##########################################################################

accra <- filter(food, town == "KOFORIDUA")
accra$year <- format(as.Date(accra$date), format = "%Y-%m")
accra <- group_by(accra, year)
select(accra, -date, -town)
a <- colnames(accra)
accra <- summarise(accra, maize = mean(maize))
accra$year <- paste0(accra$year, "-1")
accra$year <- as.Date(as.character(accra$year), format = "%Y-%m-%d")


b <- filter(food, town == "KOFORIDUA")
b$year <- format(as.Date(b$date), format = "%Y-%m")
b <- group_by(b, year)
select(b, -date, -town)
a <- colnames(accra)
b <- summarise(b, millet = mean(millet))
b$year <- paste0(b$year, "-1")
b$year <- as.Date(as.character(b$year), format = "%Y-%m-%d")

z <- merge(accra, b, all.x = TRUE, by = "year")

z$town <- "KOFORIDUA"


c <- filter(food, town == "KOFORIDUA")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, eggscommercial = mean(eggscommercial))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

c <- filter(food, town == "KOFORIDUA")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, tomatoes = mean(tomatoes))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

c <- filter(food, town == "KOFORIDUA")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, ricelocallocalperfumed = mean(ricelocallocalperfumed))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

c <- filter(food, town == "KOFORIDUA")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, riceimportedperfumed = mean(riceimportedperfumed))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

s <- z

##########################################################

accra <- filter(food, town == "TEMA")
accra$year <- format(as.Date(accra$date), format = "%Y-%m")
accra <- group_by(accra, year)
select(accra, -date, -town)
a <- colnames(accra)
accra <- summarise(accra, maize = mean(maize))
accra$year <- paste0(accra$year, "-1")
accra$year <- as.Date(as.character(accra$year), format = "%Y-%m-%d")


b <- filter(food, town == "TEMA")
b$year <- format(as.Date(b$date), format = "%Y-%m")
b <- group_by(b, year)
select(b, -date, -town)
a <- colnames(accra)
b <- summarise(b, millet = mean(millet))
b$year <- paste0(b$year, "-1")
b$year <- as.Date(as.character(b$year), format = "%Y-%m-%d")

z <- merge(accra, b, all.x = TRUE, by = "year")

z$town <- "TEMA"


c <- filter(food, town == "TEMA")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, eggscommercial = mean(eggscommercial))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

c <- filter(food, town == "TEMA")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, tomatoes = mean(tomatoes))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

c <- filter(food, town == "TEMA")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, ricelocallocalperfumed = mean(ricelocallocalperfumed))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

c <- filter(food, town == "TEMA")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, riceimportedperfumed = mean(riceimportedperfumed))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

r <- z

########################################################################

accra <- filter(food, town == "TAMALE")
accra$year <- format(as.Date(accra$date), format = "%Y-%m")
accra <- group_by(accra, year)
select(accra, -date, -town)
a <- colnames(accra)
accra <- summarise(accra, maize = mean(maize))
accra$year <- paste0(accra$year, "-1")
accra$year <- as.Date(as.character(accra$year), format = "%Y-%m-%d")


b <- filter(food, town == "TAMALE")
b$year <- format(as.Date(b$date), format = "%Y-%m")
b <- group_by(b, year)
select(b, -date, -town)
a <- colnames(accra)
b <- summarise(b, millet = mean(millet))
b$year <- paste0(b$year, "-1")
b$year <- as.Date(as.character(b$year), format = "%Y-%m-%d")

z <- merge(accra, b, all.x = TRUE, by = "year")

z$town <- "TAMALE"


c <- filter(food, town == "TAMALE")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, eggscommercial = mean(eggscommercial))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

c <- filter(food, town == "TAMALE")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, tomatoes = mean(tomatoes))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

c <- filter(food, town == "TAMALE")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, ricelocallocalperfumed = mean(ricelocallocalperfumed))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

c <- filter(food, town == "TAMALE")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, riceimportedperfumed = mean(riceimportedperfumed))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

q <- z

###############################################################

accra <- filter(food, town == "SUNYANI")
accra$year <- format(as.Date(accra$date), format = "%Y-%m")
accra <- group_by(accra, year)
select(accra, -date, -town)
a <- colnames(accra)
accra <- summarise(accra, maize = mean(maize))
accra$year <- paste0(accra$year, "-1")
accra$year <- as.Date(as.character(accra$year), format = "%Y-%m-%d")


b <- filter(food, town == "SUNYANI")
b$year <- format(as.Date(b$date), format = "%Y-%m")
b <- group_by(b, year)
select(b, -date, -town)
a <- colnames(accra)
b <- summarise(b, millet = mean(millet))
b$year <- paste0(b$year, "-1")
b$year <- as.Date(as.character(b$year), format = "%Y-%m-%d")

z <- merge(accra, b, all.x = TRUE, by = "year")

z$town <- "SUNYANI"


c <- filter(food, town == "SUNYANI")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, eggscommercial = mean(eggscommercial))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

c <- filter(food, town == "SUNYANI")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, tomatoes = mean(tomatoes))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

c <- filter(food, town == "SUNYANI")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, ricelocallocalperfumed = mean(ricelocallocalperfumed))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

c <- filter(food, town == "SUNYANI")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, riceimportedperfumed = mean(riceimportedperfumed))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

p <- z

##########################################################################

accra <- filter(food, town == "TECHIMAN")
accra$year <- format(as.Date(accra$date), format = "%Y-%m")
accra <- group_by(accra, year)
select(accra, -date, -town)
a <- colnames(accra)
accra <- summarise(accra, maize = mean(maize))
accra$year <- paste0(accra$year, "-1")
accra$year <- as.Date(as.character(accra$year), format = "%Y-%m-%d")


b <- filter(food, town == "TECHIMAN")
b$year <- format(as.Date(b$date), format = "%Y-%m")
b <- group_by(b, year)
select(b, -date, -town)
a <- colnames(accra)
b <- summarise(b, millet = mean(millet))
b$year <- paste0(b$year, "-1")
b$year <- as.Date(as.character(b$year), format = "%Y-%m-%d")

z <- merge(accra, b, all.x = TRUE, by = "year")

z$town <- "TECHIMAN"


c <- filter(food, town == "TECHIMAN")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, eggscommercial = mean(eggscommercial))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

c <- filter(food, town == "TECHIMAN")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, tomatoes = mean(tomatoes))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

c <- filter(food, town == "TECHIMAN")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, ricelocallocalperfumed = mean(ricelocallocalperfumed))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

c <- filter(food, town == "TECHIMAN")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, riceimportedperfumed = mean(riceimportedperfumed))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

o <- z

##########################################################################

accra <- filter(food, town == "TAKORADI")
accra$year <- format(as.Date(accra$date), format = "%Y-%m")
accra <- group_by(accra, year)
select(accra, -date, -town)
a <- colnames(accra)
accra <- summarise(accra, maize = mean(maize))
accra$year <- paste0(accra$year, "-1")
accra$year <- as.Date(as.character(accra$year), format = "%Y-%m-%d")


b <- filter(food, town == "TAKORADI")
b$year <- format(as.Date(b$date), format = "%Y-%m")
b <- group_by(b, year)
select(b, -date, -town)
a <- colnames(accra)
b <- summarise(b, millet = mean(millet))
b$year <- paste0(b$year, "-1")
b$year <- as.Date(as.character(b$year), format = "%Y-%m-%d")

z <- merge(accra, b, all.x = TRUE, by = "year")

z$town <- "TAKORADI"


c <- filter(food, town == "TAKORADI")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, eggscommercial = mean(eggscommercial))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

c <- filter(food, town == "TAKORADI")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, tomatoes = mean(tomatoes))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

c <- filter(food, town == "TAKORADI")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, ricelocallocalperfumed = mean(ricelocallocalperfumed))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

c <- filter(food, town == "TAKORADI")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, riceimportedperfumed = mean(riceimportedperfumed))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

n <- z

##########################################################################

accra <- filter(food, town == "OBUASI")
accra$year <- format(as.Date(accra$date), format = "%Y-%m")
accra <- group_by(accra, year)
select(accra, -date, -town)
a <- colnames(accra)
accra <- summarise(accra, maize = mean(maize))
accra$year <- paste0(accra$year, "-1")
accra$year <- as.Date(as.character(accra$year), format = "%Y-%m-%d")


b <- filter(food, town == "OBUASI")
b$year <- format(as.Date(b$date), format = "%Y-%m")
b <- group_by(b, year)
select(b, -date, -town)
a <- colnames(accra)
b <- summarise(b, millet = mean(millet))
b$year <- paste0(b$year, "-1")
b$year <- as.Date(as.character(b$year), format = "%Y-%m-%d")

z <- merge(accra, b, all.x = TRUE, by = "year")

z$town <- "OBUASI"


c <- filter(food, town == "OBUASI")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, eggscommercial = mean(eggscommercial))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

c <- filter(food, town == "OBUASI")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, tomatoes = mean(tomatoes))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

c <- filter(food, town == "OBUASI")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, ricelocallocalperfumed = mean(ricelocallocalperfumed))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

c <- filter(food, town == "OBUASI")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, riceimportedperfumed = mean(riceimportedperfumed))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

m <- z

##########################################################################

accra <- filter(food, town == "MANKESSIM")
accra$year <- format(as.Date(accra$date), format = "%Y-%m")
accra <- group_by(accra, year)
select(accra, -date, -town)
a <- colnames(accra)
accra <- summarise(accra, maize = mean(maize))
accra$year <- paste0(accra$year, "-1")
accra$year <- as.Date(as.character(accra$year), format = "%Y-%m-%d")


b <- filter(food, town == "MANKESSIM")
b$year <- format(as.Date(b$date), format = "%Y-%m")
b <- group_by(b, year)
select(b, -date, -town)
a <- colnames(accra)
b <- summarise(b, millet = mean(millet))
b$year <- paste0(b$year, "-1")
b$year <- as.Date(as.character(b$year), format = "%Y-%m-%d")

z <- merge(accra, b, all.x = TRUE, by = "year")

z$town <- "MANKESSIM"


c <- filter(food, town == "MANKESSIM")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, eggscommercial = mean(eggscommercial))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

c <- filter(food, town == "MANKESSIM")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, tomatoes = mean(tomatoes))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

c <- filter(food, town == "MANKESSIM")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, ricelocallocalperfumed = mean(ricelocallocalperfumed))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

c <- filter(food, town == "MANKESSIM")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, riceimportedperfumed = mean(riceimportedperfumed))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

l <- z

##########################################################################

accra <- filter(food, town == "KUMASI")
accra$year <- format(as.Date(accra$date), format = "%Y-%m")
accra <- group_by(accra, year)
select(accra, -date, -town)
a <- colnames(accra)
accra <- summarise(accra, maize = mean(maize))
accra$year <- paste0(accra$year, "-1")
accra$year <- as.Date(as.character(accra$year), format = "%Y-%m-%d")


b <- filter(food, town == "KUMASI")
b$year <- format(as.Date(b$date), format = "%Y-%m")
b <- group_by(b, year)
select(b, -date, -town)
a <- colnames(accra)
b <- summarise(b, millet = mean(millet))
b$year <- paste0(b$year, "-1")
b$year <- as.Date(as.character(b$year), format = "%Y-%m-%d")

z <- merge(accra, b, all.x = TRUE, by = "year")

z$town <- "KUMASI"


c <- filter(food, town == "KUMASI")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, eggscommercial = mean(eggscommercial))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

c <- filter(food, town == "KUMASI")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, tomatoes = mean(tomatoes))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

c <- filter(food, town == "KUMASI")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, ricelocallocalperfumed = mean(ricelocallocalperfumed))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

c <- filter(food, town == "KUMASI")
c$year <- format(as.Date(c$date), format = "%Y-%m")
c <- group_by(c, year)
select(c, -date, -town)
c <- summarise(c, riceimportedperfumed = mean(riceimportedperfumed))
c$year <- paste0(c$year, "-1")
c$year <- as.Date(as.character(c$year), format = "%Y-%m-%d")

z <- merge(z, c, all.x = TRUE, by = "year")

k <- z

#### Combine all subsets created ####
food <- rbind(k, l, m, n, o, p, q, r, s, t, u, v, w, x, y)
food$n <- 1:nrow(food)


#### Find volatility of each commodity in each market ####
j <- arrange(food, town) %>% select(tomatoes, n, -town) %>% na.omit() %>% 
  filter(tomatoes != 0) %>% mutate(diff = log(lag(tomatoes)) - log(tomatoes)) %>% 
  select(diff, n) %>% rename(tomatoes = diff)
j[is.na(j)] <- 0
sd(j$tomatoes)

g <- select(food, n, town)
p <- merge(g , j, by = "n", all.x = TRUE)

a <- select(food, maize, n, -town) %>% na.omit() %>% 
  mutate(diff = log(lag(maize)) - log(maize)) %>% 
  select(diff, n) %>% rename(maize = diff)
a[is.na(a)] <- 0
sd(a$maize)

p <- merge(p , a, by = "n", all.x = TRUE)

a <- select(food, eggscommercial, n, -town) %>% na.omit() %>% 
  mutate(diff = log(lag(eggscommercial)) - log(eggscommercial)) %>% 
  select(diff, n) %>% rename(eggscommercial = diff)
a[is.na(a)] <- 0
sd(a$eggscommercial)

p <- merge(p , a, by = "n", all.x = TRUE)

a <- select(food, millet, n, -town) %>% na.omit() %>% 
  mutate(diff = log(lag(millet)) - log(millet)) %>% 
  select(diff, n) %>% rename(millet = diff)
a[is.na(a)] <- 0
sd(a$millet)

p <- merge(p , a, by = "n", all.x = TRUE)

#a <- select(food, tomatoes, n, -town) %>% na.omit() %>% 
#  mutate(diff = log(lag(tomatoes)) - log(tomatoes)) %>%           #Unlocked if using monthly series
#  select(diff, n) %>% rename(tomatoes = diff)
#a[is.na(a)] <- 0
#sd(a$tomatoes)

#p <- merge(p , a, by = "n", all.x = TRUE)

a <- select(food, ricelocallocalperfumed, n, -town) %>% na.omit() %>% 
  mutate(diff = log(lag(ricelocallocalperfumed)) - log(ricelocallocalperfumed)) %>% 
  select(diff, n) %>% rename(ricelocallocalperfumed = diff)
a[is.na(a)] <- 0
sd(a$ricelocallocalperfumed)

p <- merge(p , a, by = "n", all.x = TRUE)

a <- select(food, riceimportedperfumed, n, -town) %>% na.omit() %>% 
  mutate(diff = log(lag(riceimportedperfumed)) - log(riceimportedperfumed)) %>% 
  select(diff, n) %>% rename(riceimportedperfumed = diff)
a[is.na(a)] <- 0
sd(a$riceimportedperfumed)


p <- merge(p , a, by = "n", all.x = TRUE)

p <- arrange(p, town) %>% group_by(town) %>% summarise_each(funs(sd(.,na.rm = TRUE)))
#arrange(p, town) %>% group_by(town) %>% summarise_each(p, funs(sd(na.rm = TRUE)), maize) Gave me problems Looks like package not loading or something

#p <- p %>% mutate_if(is.numeric, funs(.*(sqrt(53)*100))) #Used when using monthly series
p <- p %>% mutate_if(is.numeric, funs(.*(sqrt(12))))#*100)))
#arrange(p, town) %>% group_by(town) %>% summarise_each(funs(sd(.,na.rm = TRUE)))
#p *sqrt(53) * 100



p <- select(p, -n) %>% gather("commodity", value = "volatility", -town)

p$commodity <- recode(p$commodity, eggscommercial = "Eggs", maize = "Maize", 
                      millet = "Millet", tomatoes = "Tomatoes", 
                      ricelocallocalperfumed = "Local Rice", 
                      riceimportedperfumed = "Imported \n Rice")

#### Create median of volatility by Town or Market ####
p1 <- group_by(p, town) %>% summarise(town_median = median(volatility, na.rm = TRUE))

p <- merge(p, p1, all.x = TRUE)

p$paste <- paste0("Median = ", round(p$town_median*100), "%")

p1 <- mutate(p, volatilty_percent = volatility * 100)

p <- merge(p, p1, all.x = TRUE)


#### Plot undergoing make over #### Reduced width with "width", gave each chart boundary with theme_few
ggplot(p, aes(x = commodity)) + geom_col(position = "dodge", aes(y = volatility, fill = volatility), width = 0.5) + 
  facet_wrap(~town + paste, nrow = 5) + ylab("Volatility (%)") + xlab("Food Commodity") + 
  ggtitle("Regional Food Commodity Price Volatility in Ghana (2015 - 2016)") + 
  theme_minimal() + scale_fill_continuous(high = "firebrick1", low = "darkblue") + 
  geom_hline(aes(yintercept = p$town_median), size = 1) +  scale_y_continuous(labels = percent) + 
  theme(legend.position = "none", panel.grid = element_line(colour = "grey75")) + labs(caption = "By: David Quartey - Source: MOFA Ghana") +
  theme_few(base_size = 10)

#### Plotting time!! #### Used for project work
ggplot(p, aes(x = commodity)) + geom_col(position = "dodge", aes(y = volatility, fill = volatility)) + 
  facet_wrap(~town + paste, nrow = 5) + ylab("Volatility (%)") + xlab("Food Commodity") + 
  ggtitle("Regional Food Commodity Price Volatility in Ghana (2015 - 2016)") + 
  theme_minimal() + scale_fill_continuous(high = "firebrick1", low = "navyblue") + 
  geom_hline(aes(yintercept = p$town_median), size = 1) +  scale_y_continuous(labels = percent) + 
  theme(legend.position = "none") + labs(caption = "By: David Quartey - Source: MOFA Ghana")