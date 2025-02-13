#Ramish Fatima
#SEC 22 Fall 2023 "
#CPS [BOS-A-HY]
#09/28/2023

#Boilerplate code
cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) #clears packages
options(scipen = 100) # disables scientific notation for entire Rsession

#Libaries
library(pacman)
p_load(tidyverse)
p_load(janitor)
library(ggplot2)

#Q1: Read 2015.csv and storing it in the vector variable data_2015
data_2015 <- read.csv("2015.csv")
#Checking the first few rows of the data to verify it with assignment.
head(data_2015)

#Q2: function names to produce the column names for data set.
names(data_2015)

#Q3: Using the View() function to open the data frame in a separate tab
View(data_2015)

#Q4: Using the glimpse function to view the data set in another configuration.
glimpse(data_2015)

#Q5: 
#Used p_load to install the janitor package in console
#Storing the resulting converted data frame in data_2015
data_2015 <- clean_names(data_2015)
#View and verify the data  set
data_2015


#Q6: Loaded the tidyverse library

# Select specific columns and store the result in happy_df
happy_df <- data_2015 %>% select(country, region, happiness_score, freedom)


#Q7: 
# Slicing the first 10 rows from happy_df without using the pipe operator
top_ten_df <- slice(happy_df, 1:10)


#Slicing the first 10 rows from happy_df Using the pipe operator
top_ten_df <- happy_df %>% slice(1:10)


#Q8:Filtering the happy_df table for freedom values < 0.20 and storing it as no_freedom_df
no_freedom_df <- filter(happy_df, freedom < 0.20)


#Q9: Arranging the values in happy_df by freedom values in descending order and storing it as best_freedom_df
best_freedom_df <- arrange(happy_df, desc(freedom))

#Q10:Create a new column gff_stat in data_2015 as the sum of family, freedom, and generosity values
data_2015 <- mutate(data_2015, gff_stat = family + freedom + generosity)


#Q11:Summarizing the happy_df data set
#the mean happiness_score in a column called mean_happiness
#the max happiness_score in a column called max_happiness
#the mean freedom in a column called mean_freedom 
#the max freedom in a column called max_freedom. 
#Storing the resulting table as happy_summary
happy_summary <- happy_df %>%
  summarize(
    mean_happiness = mean(happiness_score),
    max_happiness = max(happiness_score),
    mean_freedom = mean(freedom),
    max_freedom = max(freedom)
  )

#Q12:Group the happy_df data by region and calculate statistics for each region
#group_by(region) groups the data by the region column
#then summarize() calculates the number of countries (country_count), mean happiness (mean_happiness), and mean freedom (mean_freedom) for each region. 
#The results are stored in regional_stats_df

regional_stats_df <- happy_df %>%
  group_by(region) %>%
  summarize(
    country_count = n(),
    mean_happiness = mean(happiness_score),
    mean_freedom = mean(freedom)
  )


#Q13: Filter the data for Western European and Sub-Saharan African countries
#And arrange by happiness score

# Filtering the ten least happy Western European countries
western_europe_least_happy <- data_2015 %>%
  filter(region == "Western Europe") %>%
  arrange(happiness_score) %>%
  head(10)

# Filtering the ten happiest Sub-Saharan African countries
sub_saharan_africa_happiest <- data_2015 %>%
  filter(region == "Sub-Saharan Africa") %>%
  arrange(desc(happiness_score)) %>%
  head(10)

# Calculate the average GDP per capita for each group and rounding off
europe_gdp <- round(mean(western_europe_least_happy$economy_gdp_per_capita),2)
africa_gdp <- round(mean(sub_saharan_africa_happiest$economy_gdp_per_capita),2)

# Creating a data frame to store the results
gdp_df <- data.frame(europe_gdp, africa_gdp)

#Printing gdp_df data frame
gdp_df


#Q14:
library(ggplot2)
# Define specific axis ranges and breaks
x_limits <- c(4, 7.5)
y_limits <- c(0.3, 0.7)
x_breaks <- seq(4, 7.5,1)
y_breaks <- seq(0.3, 0.7, 0.1)

# Calculate min and max values
min_happiness <- min(regional_stats_df$mean_happiness)
max_happiness <- max(regional_stats_df$mean_happiness)
min_freedom <- min(regional_stats_df$mean_freedom)
max_freedom <- max(regional_stats_df$mean_freedom)

# Scatter plot with specific axis ranges and breaks
scatterplot <- ggplot(regional_stats_df, aes(x = mean_happiness, y = mean_freedom, color = region)) +
  geom_point() +
  labs(x = "mean_happiness", y = "mean_freedom") +
  #theme_minimal() +
  theme(panel.background = element_rect(fill = "#EAEAEA"))+
  scale_x_continuous(limits = x_limits, breaks = x_breaks) +
  scale_y_continuous(limits = y_limits, breaks = y_breaks)

# Calculate min and max values for y-axis
min_freedom <- 0.31
max_freedom <- 0.68

# Draw line segment
scatterplot_with_line <- scatterplot +
  geom_segment(aes(x = min_happiness, xend = max_happiness, y = min_freedom, yend = max_freedom), linetype = "solid", color = "black")

# Display the plot with the line segment
print(scatterplot_with_line)

#-------------------PART 2-----------------------------

# Reading baseball.csv into a variable called 'baseball'
baseball <- read.csv("baseball.csv")

#Q3:
class(baseball)
#it represents [1] "data.frame" after executing script

#Q4: 
#summarise() function calculates the average number of home runs, hits, and runs scored for each age group.
#The results are stored in age_stats_df
age_stats_df <- baseball %>%
  group_by(Age) %>%
  summarize(
    Number_of_People = n(),
    average_home_runs = round(mean(HR), 1),
    average_hits = round(mean(H), 1),
    average_runs_scored = round(mean(R), 1)
  )

#Q5:Remove players with 0 at bats
#filtered out the rows where the number of bats (AB) is 0 so fetched where AB>0
baseball <- baseball %>%
  filter(AB > 0)

#The result fetched 726X16 records. 

#Q6:
# Calculating the batting average and adding it as a new column (BA)
baseball$BA <- baseball$H / baseball$AB

#Q7:
# Rounding the BA column to three decimal places
baseball$BA <- round(baseball$BA, 3)

#Q8: Creating the new OBP variable: On-base Percentage (OBP) and adding it as a new column called OBP in the baseball data frame.
baseball$OBP <- (baseball$H + baseball$BB) / (baseball$AB + baseball$BB)

#Q9:# Rounding the OBP column to three decimal places
baseball$OBP <- round(baseball$OBP, 3)

#Q10:
# Find the top 10 players with the most strikeouts
strikeout_artist <- baseball %>% 
  arrange(desc(SO)) %>% 
  head(10)


#Q11:
# Create a scatter plot of HRs versus RBIs using geom_plot()
ggplot(baseball, aes(x = HR, y = RBI)) +
  geom_point() +
  xlab("HR") +
  ylab("RBI")

#Q12:
# Filtering eligible players with at least 300 at-bats or at least 100 games played
eligible_df <- baseball %>%
  filter(AB >= 300 | G >= 100)

#Q13:
# Creating a histogram of batting average for eligible players with specified color
ggplot(eligible_df, aes(x = BA)) +
  geom_histogram(binwidth = 0.025, color = "blue", fill = "green", alpha = 0.7) +
  xlab("BA") +
  ylab("count")

#Q14: Use the following code to create a ranking column of eligible players with regard to home runs (HRs). Store the result in eligible_df.
eligible_df <- eligible_df |>
  mutate(RankHR =rank(-1 * HR, ties.method = "min"))


#Q15: 
# Create a ranking column for RBI
#calculating the ranking for runs batted in (RBI) and storing the result in the new column RankRBI
#the ties.method argument is used to tie values based on the descending order
eligible_df <- eligible_df %>%
  mutate(RankRBI = rank(-1 * RBI, ties.method = "min"))

# Create a ranking column for OBP
#calculating the ranking for OBP and storing the result in the new column RankOBP
eligible_df <- eligible_df %>%
  mutate(RankOBP = rank(-1 * OBP, ties.method = "min"))

#Q16:
#The values in TotalRank are calculated by adding the values from the RankHR, RankRBI, and RankOBP columns for each row. 
eligible_df <- eligible_df %>%
  mutate(TotalRank = RankHR + RankRBI + RankOBP)


#Q17:
#slice_min selects the rows with the lowest TotalRank scores (n = 20) based on the order_by argument in TotalRank
mvp_candidates <- eligible_df %>%
  slice_min(order_by = TotalRank, n = 20)
view (mvp_candidates)


#Q18:
#TotalRank was not in the question but I added it since it was part of solution.
mvp_candidates_abbreviated <- mvp_candidates %>%
  select(First, Last, RankHR, RankRBI, RankOBP,TotalRank)
mvp_candidates_abbreviated

#Q19:
#Why I excluded Pitchers in my analysis
#Ans: MVP award usually goes to players who contribute in many ways, like hitting, fielding, and running. 
#MVP is for players who do a bit of everything in every game and Pitchers have their own special category so
#Iam excluding them from the analysis


# In order to analyze the data I created the heatmap of each player as per each category

#Clearly we have 2 players with same total ranking=20 named Don Mattingly (7,5,8) and Mike Schmidt (2,2,)
#Now inorder to consider the Most Valueable player we need to see who is the most consistent in all three categories. 
# Create a heatmap to visualize tied rankings

mvp_candidates <- tidyr::gather(mvp_candidates_abbreviated, key = "Attribute", value = "Rank", -c(First, Last))

ggplot(mvp_candidates, aes(x = Attribute, y = paste(First), fill = Rank)) +
  geom_tile() +
  labs(x = "Categories", y = "Players",
       title = "MVP Candidates Analysis") +
  scale_fill_viridis_c(trans = "reverse") + 
  theme(axis.text.x = element_text(angle = 50, hjust = 1))

#It is evident from the heatmap the DON Mattingly should be the MVP. 
#WHY: He is the one with the most consistent ranks (7,5,8) in all three categories i.e HR,OBP and RBI. 






