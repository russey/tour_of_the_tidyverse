# @title A Whistle-Stop Tour of the Tidyverse
# @author agott <agott@mango-solutions.com>

library(tidyverse)

# tidy manifesto
## Reuse existing data structures
## Compose simple functions with the pipe
women %>% plot
## Embrace functional programming
## Design for humans



# data analysis workflow
## import
## tidy
## transform
## visualise
## model
## communicate



# Import the data

summer <- read_csv("SummerOlympicsFunding.csv", 
                   na = c("n/a", "n/a**"),
                   col_types = cols(.default = col_number(),
                                    Sports = col_character()))

summer <- read_csv("https://gist.githubusercontent.com/CSJCampbell/d4e6edef3596206c48b208696617a93a/raw/a41ea995c152126b07277e30a691390e235d1180/SummerOlympicsFunding.csv", 
    na = c("n/a", "n/a**"),
    col_types = cols(
        .default = col_number(),
        Sport = col_character())
) %>%
  rename(Sports = Sport)


View(summer)


# Use tibble/tribble to create tibbles from scratch
years <- tribble(
  ~Location, ~Year, ~Month, ~Day,
  "Sydney", 2000, 9, 15,
  "Athens", 2004, 8, 13,
  "Beijing", 2008, 8, 8,
  "London", 2012, 7, 27,
  "Rio de Janeiro", 2016, 8, 5
)

years



# Tidy the summer data
library(tidyr)

summer <- pivot_longer(summer,
                       cols = -Sports,
                       names_to = "Location",
                       values_to = "Funding"
                       ) %>%
  replace_na(replace = list(Funding = 0))

View(summer)



# dplyr verbs

summer

# Filter to obtain certain rows by condition
filter(summer, Location %in% c("Athens", "Sydney"))

# Select certain columns
select(summer, 2:3)

# Sort (use desc() or - to sort by descending values)
arrange(summer, -Funding)

# Create new columns - can create them in succession
mutate(summer, 
       funding_rounded = signif(Funding, 4),
       fund_mult = funding_rounded * 3.2)

# Aggregate our data
summarise(summer, mean(Funding))

# Split our data in to categories using group_by - 
# then summarise by each category
group_by(summer, Location) %>%
  summarise(mean_funding = mean(Funding))




# Join the year data
summer <- full_join(summer, years) %>%
  filter(Sports != "Total")

noFunding <- filter(summer, Funding == 0)
View(noFunding)


# Factors

library(forcats)

numberNoFund <- noFunding %>% 
  count(Location) %>% 
  left_join(years) %>% 
  mutate(Location = fct_reorder(Location, Year))


# Convert location to all caps

library(stringr)

summer_caps <- mutate(summer, 
                      Location = str_to_upper(Location))


library(lubridate)

summer <- mutate(summer, 
                 Date = str_c(Year, Month, Day, sep = "-"), 
                 Date = ymd(Date)) 



# Plotting
ggplot(data = summer, mapping = aes(x = fct_reorder(Location, Year), 
                                    y = Funding, 
                                    group = Sports, 
                                    colour = Sports)) + 
  geom_line() +
  ggtitle("Funding by Sport Over Summer Olympics") +
  xlab("Location of Olympics") +
  scale_y_continuous(labels = scales::comma)

ggplot(data = noFunding, 
       mapping = aes(x = Location)) +
  geom_bar(fill = "lightblue") +
  ggtitle("Counts of Sports with No Funding by Olympics") +
  ylab("Number of sports without funding")



# purrr

sportData <- summer %>% 
  group_by(Sports) %>%
  nest()

sportsModels <- map(sportData$data, ~lm(Funding ~ Year, data = .))

sportResid <- map2_df(sportData$data, sportsModels, add_residuals, .id = "Sport")

