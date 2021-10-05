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

# Find min/max termination date
min_max <- df %>%
  drop_na(DateofTermination) %>%
  summarise(min = min(DateofTermination), max = max(DateofTermination)) %>%
  print()

# Define years range
years <- seq(from = year(min_max$min), to = year(min_max$max), by = 1) %>%
  as.character()

years

# number of leavers in a year
n_leavers <- function(year = "2010") {
  df %>%
    filter(year(DateofTermination) == year) %>%
    nrow()
}

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

# Dataframes of Variabel functions
df_count <- function(x) {
  map_dbl(years, x) %>%
    setNames(years) %>%
    as.data.frame() %>%
    setNames("n") %>%
    rownames_to_column("Year")
}

functions <- c(n_leavers, n_emp_year_start, n_emp_year_end)
func_names <- c("n_leavers", "n_emp_year_start", "n_emp_year_end")

test <- map(functions, df_count) %>% setNames(func_names)
test

# Plot variables of tunrover function.
var_turn_plt <- function(x, title = "Title") {

  # find the median of x
  print("Median")
  median <- map_dbl(years, x) %>%
    setNames(years) %>%
    as.data.frame() %>%
    setNames("n") %>%
    rownames_to_column("Year") %>%
    summarise(median = median(n)) %>%
    as.numeric() %>%
    print()

  # Plot df with median
  print("Data.frame")
  plt <- map_dbl(years, x) %>%
    setNames(years) %>%
    as.data.frame() %>%
    setNames("n") %>%
    rownames_to_column("Year") %>%
    print() %>%
    ggplot(aes(x = .[, 1], y = .[, 2], group = 1)) +
    geom_line(color = "#0072b1") +
    geom_point(color = "#0072b1") +
    geom_hline(yintercept = median, linetype = "dashed", color = "red") +
    theme_bw() +
    geom_text(aes(label = n), vjust = -0.5) +
    labs(title = title) +
    xlab("Year") +
    ylab("n")

  return(plt)
}

var_turn_plt(n_leavers, "Terminations")
var_turn_plt(n_emp_year_start, "Employees Year Start")
var_turn_plt(n_emp_year_end, "Employees Year End")

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

# Map years to emp_turnover function
turnover_rate <- map_dbl(years, emp_turnover) %>%
  setNames(years) %>%
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
  geom_line(color = "#0072b1") +
  geom_point(color = "#0072b1") +
  geom_text(aes(label = round(TurnoverRate, digits = 2)), vjust = -0.5) +
  geom_hline(yintercept = median_turnover, linetype = "dashed", color = "red") +
  theme_bw()
