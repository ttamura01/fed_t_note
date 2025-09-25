### To Study Historical Fed's Rate-cuts and impacts to Treasury Yield and Mortgage Rates 
## 2025-09-25

setwd("/Users/takayukitamura/Documents/R_Computing/fed_rate_cut")
# Install/load required packages
library(tidyverse)
library(lubridate)
library(ggtext)
library(scales)
library(plotly)
library(quantmod)   # for Fed Funds
library(fredr) 

#Set my FRED API key
fredr_set_key("0c5fd2514c7d98427fe3c931e2fcb244")

t_note <- fredr(series_id = "DGS10") %>% 
  select(date, yld_10y = value)

fed_rate <- fredr(series_id = "FEDFUNDS") %>% 
  select(date, f_rate = value)

mortgage_30y <- fredr(series_id = "MORTGAGE30US") %>% 
  select(date, mortgage_30y = value)

#df <- 
df <- fed_rate %>% 
  left_join(., t_note, by = "date") %>% 
  left_join(., mortgage_30y, by = "date") %>% 
  arrange(date) %>% 
  mutate(f_rate = zoo::na.locf(f_rate, na.rm = FALSE),
         yld_10y = zoo::na.locf(yld_10y, na.rm = FALSE),
         mortgage_30y = zoo::na.locf(mortgage_30y, na.rm = FALSE)) %>% 
  filter(date >= "1988-01-01") %>% 
  na.omit()

updates <- tribble(~date, ~f_rate, ~yld_10y, ~mortgage_30y,
                   "2025-09-01", 4.33, 4.28, 6.5,
                   "2025-09-22", 4.14, 4.08, 6.20)

updates$date <- as.Date(updates$date)

df <- rbind(df, updates)

latest_data <- df %>% 
  slice_max(date)

longer_df <- df %>%   
  pivot_longer(-date)



order_levels <- df %>% 
  filter(date == max(df$date)) %>% 
  select(-date) %>% 
  pivot_longer(everything(), names_to = "name", values_to = "value") %>% 
  # arrange(desc(case == "Average"), desc(percentage)) %>% 
  arrange(desc(value)) %>% 
  pull(name)

longer_df %>% 
  mutate(name = factor(name, levels = order_levels)) %>% 
  ggplot(aes(x = date, y = value, colour = name)) +
  geom_line() +
  scale_colour_manual(
    name = NULL,
    values = c("mortgage_30y" = "#1E88E5",
               "f_rate" = "#D81B60",
               "yld_10y" = "#004D40"),
    labels = c("30-Year Mortgage Rate",
               "Effective Federal Funds Rate",
               "10-Year Treasury Yield")
  ) +
  labs(x = NULL, y = NULL,
       title = "Effective Federal Fund Rate, 10-Year Treasury Yield and 30-Year Mortgage Rate since 1989") +
  theme(legend.key = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.position = "inside",
        legend.position.inside = c(0.65, 0.75),
        panel.background = element_blank(),
        axis.text = element_text(size = 14),
        plot.title.position = "plot",
        plot.title = element_textbox_simple(size = 16, face = "bold", margin = margin(t = 10, b = 10))
        
  )