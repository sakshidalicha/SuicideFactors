rm(list=ls()) # wipe environment

require(dplyr)
require(tidyr)
# read into dataframe and check NA
df <- read.csv('master.csv')
#attach(df)
summary.data.frame(df)
str(df)

View(df)
# view the column name first
colnames(df)
# modify the column name
colnames(df)[1] <- 'country'
colnames(df)[10] <- 'gdp_for_year'
colnames(df)[11] <- 'gdp_per_capita'
attach(df)

# standarlize the structure of each column (except HDI)
str(df)

# create function of converting factor into number
facToNum <- function(column) {
  column <- as.character(column)
  column <- gsub(',', '', column) %>%
    as.numeric()
  return(column)
}
# convert factor of gdp_for_year into numeric
df$gdp_for_year <- facToNum(df$gdp_for_year)

# omit HDI column because there are branch of NA value
newdf <- df[-c(9)]
str(newdf)
# export new dataset into csv
write.csv(newdf, 'suicide_data.csv')
##################################################################




#install.packages("ggplot2")
require(ggplot2)
##### Exploratory Data Analysis (EDA)
# total suicide number per year per country
suicide_yearCountry <- newdf %>%
  group_by(year, country) %>%
  summarise(sum(suicides_no)) %>%
  ungroup()

colnames(suicide_yearCountry)[3] <- 'suicide_sum'

# average suicide per year  (sum(suicide_no)/country)
suicide_year <- suicide_yearCountry %>%
  group_by(year) %>%
  summarise(mean(suicide_sum))
colnames(suicide_year)[2] <- "suicide_mean"

# plot the average suicide number per year, devided by country
ggplot(data = suicide_year, mapping = aes(x=year, y=suicide_mean)) + 
  geom_line(color = "blue", linetype = "dashed", size = 1.2) + 
  geom_point(size = 3) + 
  ggtitle("Suicide Per Year")

# count country number in each year
n <- suicide_yearCountry %>%
  group_by(year) %>%
  summarise(n()) %>%
  ungroup()
colnames(n)[2] <- 'number'

# boxplot to find outlier
boxplot(n$number)
# there is one outlier and find it
filter(n, n$number == min(n$number)) # result is 2016
# better to omit the data from 2016
newdf_1 <- newdf[!(newdf$year == 2016), ]
# write into new csv file
write.csv(newdf_1, "suicide_data1.csv")


#################################################################
####### EDA
# add gender column to change label male and female into 1 and 2
newdf_1$gender <- sapply(newdf_1$sex, function(x) {ifelse(x=="male", 1, 2)})
cor(newdf_1[c(2, 12, 7, 10)])

# relation between gdp and suicide
plot(newdf_1$gdp_per_capita, newdf_1$suicides.100k.pop , main="Suicides per 100k vs. Year", xlab="GDP per capita", ylab="Suicides per 100K")
ggplot(data = newdf_1, aes(x=gdp_per_capita, y=suicides.100k.pop, shape=sex, color=sex)) +
  geom_point()

# scatter plot the suicide rate based on the year and the age group
ggplot(data = newdf_1, mapping = aes(x=year, y=suicides.100k.pop, shape=age, color=age)) +
  geom_point()


# GDP per capita in a year of each country
gdp_capita <- newdf_1 %>%
  group_by(country, year) %>%
  summarise(mean(gdp_per_capita)) %>%
  ungroup()
ggplot(data = gdp_capita, aes(year, `mean(gdp_per_capita)`)) +
  geom_point()







