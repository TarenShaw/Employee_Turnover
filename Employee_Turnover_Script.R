pacman::p_load(tidyverse,readr, lubridate)

df <- readr::read_csv("HRDataset_v14.csv")

skimr::skim(df)

df <- df %>% mutate(`DateofTermination` = mdy(df$`DateofTermination`), 
                    `DateofHire` = mdy(df$`DateofHire`), 
                    Md_termination = format(`DateofTermination`,'%Y-%m'))


#How many leavers per year
df %>% mutate(Year_termination = year(df$`DateofTermination`)) %>% 
  count(Year_termination) %>%
  drop_na(Year_termination) %>% 
  mutate(Year_termination =factor(Year_termination)) %>% 
  mutate(Year_termination = fct_reorder(Year_termination, n)) %>% 
  ggplot(aes(x =Year_termination, y =n)) + geom_col(fill = "#0072b1") +
  geom_text(aes(label = n), hjust = -0.2) + 
  coord_flip() + theme_bw() 

#What is the mean number of leavers 
avg_terminations <- df %>% mutate(Year_termination = year(df$`DateofTermination`)) %>% 
  count(Year_termination) %>% #We omit 2010 since it only has two observations
  drop_na(Year_termination) %>% 
  summarise(mean = mean(n)) %>%  
  as.numeric() %>% print()

#compare average leavers to leavers per year
df %>% mutate(Year_termination = year(df$`DateofTermination`)) %>% 
  count(Year_termination) %>%
  drop_na(Year_termination) %>% 
  ggplot(aes(x =Year_termination, y =n)) + geom_col(fill = "#0072b1") +
  geom_text(aes(label = n), vjust = -0.2) + 
  theme_bw() + geom_hline(yintercept= avg_terminations, linetype="dashed", color = "red")








