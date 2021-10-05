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
  mutate(Year_termination = year(DateofTermination)) %>%
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
  mutate(Year_termination = year(DateofTermination)) %>%
  count(Year_termination) %>%
  drop_na(Year_termination) %>%
  summarise(mean = median(n)) %>%
  as.numeric() %>%
  print()

# compare average leavers to leavers per year
df %>%
  mutate(Year_termination = year(DateofTermination)) %>%
  count(Year_termination) %>%
  drop_na(Year_termination) %>%
  ggplot(aes(x = Year_termination, y = n)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = n), vjust = -0.5) +
  theme_bw() +
  geom_hline(yintercept = median_terminations, linetype = "dashed", color = "red")

# Find min/max termination date
min_max <- df %>%
  drop_na(DateofTermination) %>%
  summarise(min = min(DateofTermination), max = max(DateofTermination)) %>% 
  print()

# Define years range
years <- seq(from = year(min_max$min), to = year(min_max$max), by = 1) %>%
  as.character() 

# number of leavers in a year
n_leavers <- function(year = "2010") {
  df %>%
    filter(year(DateofTermination) == year) %>%
    nrow()
}

# Create leavers dataframe
map_dbl(years, n_leavers) %>% 
  setNames(years) %>%
  as.data.frame() %>%
  setNames("leavers") %>%
  rownames_to_column("Year")

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

# Create n_emp_year_start dataframe
map_dbl(years,n_emp_year_start) %>% 
  setNames(years) %>%
  as.data.frame() %>%
  setNames("n_start") %>%
  rownames_to_column("Year")
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

# Create n_emp_year_end dataframe
map_dbl(years,n_emp_year_end) %>% 
  setNames(years) %>%
  as.data.frame() %>%
  setNames("n_end") %>%
  rownames_to_column("Year")
n_emp_year_start()

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
tot_year <- c(
  "2010", "2011", "2012", "2013",
  "2014", "2015", "2016", "2017",
  "2018"
)

# Map years to emp_turnover function
turnover_rate <- map_dbl(tot_year, emp_turnover) %>%
  setNames(tot_year) %>%
  as.data.frame() %>%
  setNames("TurnoverRate") %>%
  rownames_to_column("Year")

# Median turnover
median_turnover <- turnover_rate %>%
  summarise(median = median(TurnoverRate)) %>%
  as.numeric() %>%
  print()

# Graph turnover rate
ggplot(turnover_rate, aes(x = Year, y = TurnoverRate, group = 1)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = round(TurnoverRate, digits = 2)), vjust = -0.5) +
  geom_hline(yintercept = median_turnover, linetype = "dashed", color = "red") +
  theme_bw()
