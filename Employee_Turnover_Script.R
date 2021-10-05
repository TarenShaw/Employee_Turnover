# Load Packages
pacman::p_load(tidyverse, readr, lubridate, skimr, styler)

# Load Data
df <- readr::read_csv("HRDataset_v14.csv")

# Summary or Data
skim(df)

# Transform Data
df <- df %>% mutate(
  DateofTermination = mdy(DateofTermination),
  DateofHire = mdy(DateofHire)
)

# How many leavers per year
df %>%
  mutate(Year_termination = year(df$`DateofTermination`)) %>%
  count(Year_termination) %>%
  drop_na(Year_termination) %>%
  mutate(Year_termination = factor(Year_termination)) %>%
  mutate(Year_termination = fct_reorder(Year_termination, n)) %>%
  ggplot(aes(x = Year_termination, y = n)) +
  geom_col(fill = "#0072b1") +
  geom_text(aes(label = n), hjust = -0.2) +
  coord_flip() +
  theme_bw()

# What is the mean number of leavers - some extreme outliers
median_terminations <- df %>%
  mutate(Year_termination = year(df$`DateofTermination`)) %>%
  count(Year_termination) %>%
  drop_na(Year_termination) %>%
  summarise(mean = median(n)) %>%
  as.numeric() %>%
  print()

# compare average leavers to leavers per year
df %>%
  mutate(Year_termination = year(df$`DateofTermination`)) %>%
  count(Year_termination) %>%
  drop_na(Year_termination) %>%
  ggplot(aes(x = Year_termination, y = n)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = n), vjust = -0.5) +
  theme_bw() +
  geom_hline(yintercept = median_terminations, linetype = "dashed", color = "red")

# number of leavers in a year
n_leavers <- function(year = "2010") {
  df %>%
    filter(year(DateofTermination) == year) %>%
    nrow()
}

n_leavers()

# number of emplyees at beginnning of year
n_emp_year_start <- function(year = "2010") {
  fun_year <- paste(year, "-01-01", sep = "")

  df %>%
    select(DateofHire, DateofTermination) %>%
    filter(
      DateofHire <= fun_year,
      DateofTermination > fun_year | is.na(DateofTermination)
    ) %>%
    nrow()
}

n_emp_year_start()

# number of employees at end of year
n_emp_year_end <- function(year = "2010") {
  year_num_plus <- as.character(as.numeric(year) + 1)
  fun_year <- paste(year_num_plus, "-01-01", sep = "")

  df %>%
    select(DateofHire, DateofTermination) %>%
    filter(
      DateofHire <= fun_year,
      DateofTermination > fun_year | is.na(DateofTermination)
    ) %>%
    nrow()
}

n_emp_year_end()

# Employee turnover function
emp_turnover <- function(year = "2010") {

  # Number of leavers

  leavers <- df %>%
    filter(year(DateofTermination) == year) %>%
    nrow()

  # number of employees at beginning of year

  fun_year1 <- paste(year, "-01-01", sep = "")

  start <- df %>%
    select(DateofHire, DateofTermination) %>%
    filter(
      DateofHire <= fun_year1,
      DateofTermination > fun_year1 | is.na(DateofTermination)
    ) %>%
    nrow()

  # Number of employees end of year

  year_num_plus <- as.character(as.numeric(year) + 1)
  fun_year2 <- paste(year_num_plus, "-01-01", sep = "")

  end <- df %>%
    select(DateofHire, DateofTermination) %>%
    filter(
      DateofHire <= fun_year2,
      DateofTermination > fun_year2 | is.na(DateofTermination)
    ) %>%
    nrow()

  # Average number of employees
  avg_emp_period <- (start + end) / 2

  # Enmployee turnover

  (leavers / avg_emp_period) * 100
}

emp_turnover()

# Create a list of year
tot_year <- c("2010", "2011", "2012", "2013", 
              "2014", "2015", "2016", "2017", 
              "2018")

# Map years to emp_turnover function
<<<<<<< HEAD
Turnover_Rate <- map_dbl(tot_year, emp_turnover) %>%
=======
TurnoverRate <- map_dbl(tot_year, emp_turnover) %>%
>>>>>>> 668bf4ccd5bc22d1e8ccc59cc652e9d6e9cec47b
  setNames(tot_year) %>%
  as.data.frame() %>%
  setNames("TurnoverRate") %>%
  rownames_to_column("Year")

<<<<<<< HEAD
# Median turnover
Median_turnover <- Turnover_Rate %>% 
  summarise(median = median(TurnoverRate)) %>% 
  as.numeric() %>% print()

# Graph turnover rate
ggplot(Turnover_Rate, aes(x = Year, y = TurnoverRate, group = 1)) + 
  geom_line() + 
  geom_point() +
  geom_text(aes(label = round(TurnoverRate, digits = 2)), vjust = -0.5) +
  geom_hline(yintercept = Median_turnover, linetype = "dashed", color = "red") + 
  theme_bw()
=======
# Graph turnover rate
ggplot(TurnoverRate, aes(x = Year, y = TurnoverRate, group = 1)) + 
  geom_line() + 
  geom_point() +
  geom_text(aes(label = round(TurnoverRate, digits = 2)), vjust = -0.5)
>>>>>>> 668bf4ccd5bc22d1e8ccc59cc652e9d6e9cec47b
  
