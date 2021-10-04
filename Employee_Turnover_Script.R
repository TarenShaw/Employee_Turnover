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

