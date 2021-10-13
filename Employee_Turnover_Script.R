# Load Packages
pacman::p_load(
  tidyverse, readr, lubridate,
  skimr, RColorBrewer, janitor,
  rlang
)

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

# number of hires in a year
n_hires <- function(year = "2010") {
  df %>%
    filter(year(DateofHire) == year) %>%
    nrow()
}

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

# Plot of Beginning and End Headcount, Hires and Terminations
df_count(n_emp_year_start) %>%
  right_join(y = df_count(n_emp_year_end), by = "Year") %>%
  right_join(y = df_count(n_hires), by = "Year") %>%
  right_join(y = df_count(n_leavers), by = "Year") %>%
  setNames(c("Year", "Year Start", "Year End", "Hires", "Terminations")) %>%
  gather("WFA", "n", 2:5) %>%
  ggplot(aes(x = Year, y = n, color = WFA, group = WFA)) +
  geom_line() +
  theme_bw() +
  geom_text(aes(label = n), vjust = -0.5) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Workfroce Adminstration Stats by Year")

# Iterate over functions
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

emp_turnover()

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

# Graph total turnover rate by year
ggplot(turnover_rate, aes(x = Year, y = TurnoverRate, group = 1)) +
  geom_line(color = "#0072b1") +
  geom_point(color = "#0072b1") +
  geom_text(aes(label = round(TurnoverRate, digits = 2)), vjust = -0.5) +
  geom_hline(yintercept = median_turnover, linetype = "dashed", color = "red") +
  labs(title = "Turnover Rate by Year") +
  theme_bw()

# Current Year Leavers studied by Term Reason, Employment Status
n_leavers2 <- function(year = "2010") {
  df %>%
    filter(year(DateofTermination) == year)
}

n_leavers2("2018") %>%
  select(TermReason, EmploymentStatus) %>%
  count(TermReason, EmploymentStatus) %>%
  mutate(TermReason = fct_reorder(TermReason, n)) %>%
  ggplot(aes(x = TermReason, y = n, fill = EmploymentStatus)) +
  geom_col() +
  theme_bw() +
  coord_flip()

# Job role Active vs Inactive
df %>%
  mutate(EmploymentStatus = case_when(
    EmploymentStatus == "Active" ~ "Active",
    TRUE ~ "Inactive"
  )) %>%
  count(Position, EmploymentStatus) %>%
  mutate(Position = fct_reorder(Position, n)) %>%
  arrange(desc(Position)) %>%
  ggplot(aes(y = Position, x = n)) +
  geom_col(aes(fill = EmploymentStatus), position = position_stack(reverse = TRUE)) +
  theme_bw()


# Turnover Rate by job profile and year
emp_turnover_JP <- function(year = "2010") {
  term_test <- df %>%
    filter(year(DateofTermination) == year) %>%
    count(Position)

  # Start employees by job profile
  fun_year_job <- paste(year, "-01-01", sep = "")

  start_test <- df %>%
    select(DateofHire, DateofTermination, Position) %>%
    filter(
      DateofHire <= fun_year_job,
      DateofTermination > fun_year_job | is.na(DateofTermination)
    ) %>%
    count(Position)

  # Employees End year per position
  year_pos <- year %>% as.character()
  year_num_plus_pos <- as.character(as.numeric(year_pos) + 1)
  fun_year2_pos <- paste(year_num_plus_pos, "-01-01", sep = "")

  end_test <- df %>%
    select(DateofHire, DateofTermination, Position) %>%
    filter(
      DateofHire <= fun_year2_pos,
      DateofTermination > fun_year2_pos | is.na(DateofTermination)
    ) %>%
    count(Position)

  join_turnover_year <- full_join(start_test, end_test, by = "Position") %>%
    full_join(y = term_test, by = "Position") %>%
    setNames(c("Position", "Start_Headcount", "End_Headcount", "Terminations")) %>%
    group_by(Position) %>%
    summarise(Turnover = ((Terminations) / (Start_Headcount + End_Headcount)) * 100)

  return(join_turnover_year)
}

# Create a df of turnover by job profile
map(years, emp_turnover_JP) %>%
  reduce(full_join, by = "Position") %>%
  setNames(c("Position", years)) %>%
  clean_names() %>%
  gather("Year", "Turnover", 2:10) %>%
  drop_na(Turnover) %>%
  spread(key = "Year", value = "Turnover")

# Arrange Turnover by job profile and year
map(years, emp_turnover_JP) %>%
  reduce(full_join, by = "Position") %>%
  setNames(c("Position", years)) %>%
  clean_names() %>%
  gather("Year", "Turnover", 2:10) %>%
  mutate(position = fct_reorder(position, Turnover)) %>%
  arrange(desc(Turnover))

# Graph Turnover by job profile and year
map(years, emp_turnover_JP) %>%
  reduce(full_join, by = "Position") %>%
  setNames(c("Position", years)) %>%
  clean_names() %>%
  gather("Year", "Turnover", 2:10) %>%
  mutate(position = fct_reorder(position, Turnover)) %>%
  drop_na(Turnover) %>%
  ggplot(aes(x = position, y = Turnover)) +
  geom_col(fill = "#0e76a8") +
  facet_wrap(~Year) +
  coord_flip() +
  theme_bw()

# Employee turnover - function with data, var and year argument
Overall_turnover_rate <- function(data, colName, year) {
  
  # Convert colName to symbol or check if symbol
  colName <- ensym(colName)
  
  # Convert ColName to string
  colName_str <- as_string(colName)
  
  # Terminations by year and variable in df
  Terminations <- data %>%
    filter(year(DateofTermination) == year) %>%
    count(!!(colName))
  
  # Start employees by var and year
  Fun_year <- paste(year, "-01-01", sep = "")
  Headcount_year_start <- data %>%
    select(DateofHire, DateofTermination, !!(colName)) %>%
    filter(
      DateofHire <= Fun_year,
      DateofTermination > Fun_year | is.na(DateofTermination)
    ) %>%
    count(!!(colName))
  
  # End employees by year and var
  Year_chr <- year %>% as.character()
  Year_plus_one <- as.character(as.numeric(Year_chr) + 1)
  Fun_year2 <- paste(Year_plus_one, "-01-01", sep = "")
  
  Headcount_year_end <- data %>%
    select(DateofHire, DateofTermination, !!(colName)) %>%
    filter(
      DateofHire <= Fun_year2,
      DateofTermination > Fun_year2 | is.na(DateofTermination)
    ) %>%
    count(!!(colName))
  
  # Join the objects together.
  Overall_turnover_rate <- full_join(Headcount_year_start, Headcount_year_end,
                                     by = colName_str
  ) %>%
    full_join(y = Terminations, by = colName_str) %>%
    setNames(c(
      colName_str, "Start_Headcount", "End_Headcount",
      "Terminations"
    )) %>%
    group_by({{ colName }}) %>%
    summarise(Turnover = ((Terminations) / (Start_Headcount + End_Headcount)) * 100)
  
  x <- list(Headcount_year_start, Headcount_year_end, Terminations, Overall_turnover_rate) %>% setNames(c("Headcount_year_start", "Headcount_year_end", "Terminations", "Overall_turnover_rate"))
  return(x)
  
}

x <- map(years, ~ Overall_turnover_rate(df, Department, year = .x)) %>% setNames(years)

