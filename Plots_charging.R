###### Creates the charging plot and the variable plot on August 24


## Charging plot
library(ggplot2)
library(readr)
library(tidyr)
library(dplyr)
library(stringr)

prices <- read_csv("C:/Users/daniq/iCloudDrive/Erasmus University Rotterdam/Master/Thesis/Output/Full_true/True_values_24.csv", col_names = TRUE)
charging <- read_csv("C:/Users/daniq/iCloudDrive/Erasmus University Rotterdam/Master/Thesis/Output/Schedules/Charging/Regular_LQRA_0.5_charge_schedule_72.csv", col_names = TRUE)
availability <- read_csv("C:/Users/daniq/iCloudDrive/Erasmus University Rotterdam/Master/Thesis/Output/Schedules/Y_available_24.csv", col_names = TRUE)

charging <- charging[, -1]  
availability <- availability[,-1 ]  

prices <- as.matrix(prices) %>% apply(2, as.numeric)
charging <- as.matrix(charging) %>% apply(2, as.numeric)
availability <- as.matrix(availability) %>% apply(2, as.logical)

colnames(prices) <- paste0("H", 1:ncol(prices))
colnames(charging) <- paste0("H", 1:ncol(charging))
colnames(availability) <- paste0("H", 1:ncol(availability))

days_to_plot <- 43:49

charging_long <- charging[days_to_plot, 1:24] %>%
  as.data.frame() %>%
  mutate(Day = days_to_plot) %>%
  pivot_longer(-Day, names_to = "Hour", values_to = "Charging") %>%
  mutate(Hour = as.integer(str_remove(Hour, "H")),
         Time = (Day - min(Day)) * 24 + Hour)

prices_long <- prices[days_to_plot, ] %>%
  as.data.frame() %>%
  mutate(Day = days_to_plot) %>%
  pivot_longer(-Day, names_to = "Hour", values_to = "Price") %>%
  mutate(Hour = as.integer(str_remove(Hour, "H")),
         Time = (Day - min(Day)) * 24 + Hour)

availability_long <- availability[days_to_plot, 1:24] %>%
  as.data.frame() %>%
  mutate(Day = days_to_plot) %>%
  pivot_longer(-Day, names_to = "Hour", values_to = "Available") %>%
  mutate(Hour = as.integer(str_remove(Hour, "H")),
         Time = (Day - min(Day)) * 24 + Hour)

df <- charging_long %>%
  left_join(prices_long, by = c("Day", "Hour", "Time")) %>%
  left_join(availability_long, by = c("Day", "Hour", "Time"))

charging_max <- max(df$Charging, na.rm = TRUE)
price_max <- max(df$Price, na.rm = TRUE)
scale_factor <- charging_max / price_max

availability_rects <- df %>%
  arrange(Time) %>%
  mutate(Available_Num = as.integer(Available),
         Group = cumsum(Available_Num != lag(Available_Num, default = 0))) %>%
  filter(Available) %>%
  group_by(Group) %>%
  summarise(xmin = min(Time) - 1,
            xmax = max(Time) + 1,
            ymin = -Inf,
            ymax = Inf)

ggplot(df, aes(x = Time)) +
  geom_rect(data = availability_rects,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "darkgreen", alpha = 0.1, inherit.aes = FALSE) +
  geom_line(aes(y = Charging), color = "blue") +
  geom_line(aes(y = Price * scale_factor), color = "red3", linetype = "dashed") +
  scale_y_continuous(
    name = "Charging Power (kW)",
    sec.axis = sec_axis(~ . / scale_factor, name = "Price (€/kWh)")
  ) +
  scale_x_continuous(
    name = "Date",
    breaks = seq(12, 156, by = 24),
    labels = c("18-8", "19-8", "20-8", "21-8", "22-8", "23-8", "24-8")
  ) +
  theme_minimal(base_size = 14)


prices <- read_csv("C:/Users/daniq/iCloudDrive/Erasmus University Rotterdam/Master/Thesis/Output/Full_true/True_values_24.csv", col_names = TRUE)
charging <- read_csv("C:/Users/daniq/iCloudDrive/Erasmus University Rotterdam/Master/Thesis/Output/Schedules/Charging/Regular_LQRA_0.5_charge_schedule_168.csv", col_names = TRUE)
availability <- read_csv("C:/Users/daniq/iCloudDrive/Erasmus University Rotterdam/Master/Thesis/Output/Schedules/Y_available_24.csv", col_names = TRUE)

charging <- charging[, -1]  
availability <- availability[,-1 ] 

prices <- as.matrix(prices) %>% apply(2, as.numeric)
charging <- as.matrix(charging) %>% apply(2, as.numeric)
availability <- as.matrix(availability) %>% apply(2, as.logical)

colnames(prices) <- paste0("H", 1:ncol(prices))
colnames(charging) <- paste0("H", 1:ncol(charging))
colnames(availability) <- paste0("H", 1:ncol(availability))

charging_long <- charging[days_to_plot, 1:24] %>%
  as.data.frame() %>%
  mutate(Day = days_to_plot) %>%
  pivot_longer(-Day, names_to = "Hour", values_to = "Charging") %>%
  mutate(Hour = as.integer(str_remove(Hour, "H")),
         Time = (Day - min(Day)) * 24 + Hour)

prices_long <- prices[days_to_plot, ] %>%
  as.data.frame() %>%
  mutate(Day = days_to_plot) %>%
  pivot_longer(-Day, names_to = "Hour", values_to = "Price") %>%
  mutate(Hour = as.integer(str_remove(Hour, "H")),
         Time = (Day - min(Day)) * 24 + Hour)

availability_long <- availability[days_to_plot, 1:24] %>%
  as.data.frame() %>%
  mutate(Day = days_to_plot) %>%
  pivot_longer(-Day, names_to = "Hour", values_to = "Available") %>%
  mutate(Hour = as.integer(str_remove(Hour, "H")),
         Time = (Day - min(Day)) * 24 + Hour)

df <- charging_long %>%
  left_join(prices_long, by = c("Day", "Hour", "Time")) %>%
  left_join(availability_long, by = c("Day", "Hour", "Time"))

charging_max <- max(df$Charging, na.rm = TRUE)
price_max <- max(df$Price, na.rm = TRUE)
scale_factor <- charging_max / price_max

availability_rects <- df %>%
  arrange(Time) %>%
  mutate(Available_Num = as.integer(Available),
         Group = cumsum(Available_Num != lag(Available_Num, default = 0))) %>%
  filter(Available) %>%
  group_by(Group) %>%
  summarise(xmin = min(Time) - 1,
            xmax = max(Time) + 1,
            ymin = -Inf,
            ymax = Inf)
ggplot(df, aes(x = Time)) +
  # Transparent green background for availability
  geom_rect(data = availability_rects,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "darkgreen", alpha = 0.1, inherit.aes = FALSE) +
  geom_line(aes(y = Charging), color = "blue") +
  geom_line(aes(y = Price * scale_factor), color = "red3", linetype = "dashed") +
  scale_y_continuous(
    name = "Charging Power (kW)",
    sec.axis = sec_axis(~ . / scale_factor, name = "Price (€/kWh)")
  ) +
  scale_x_continuous(
    name = "Date",
    breaks = seq(12, 156, by = 24),
    labels = c("18-8", "19-8", "20-8", "21-8", "22-8", "23-8", "24-8")
  ) +
  theme_minimal(base_size = 14)


## Create variable plot ont August 24th, 2024
library(tidyverse)
library(lubridate)
library(readr)


df <- read_csv("C:/Users/daniq/iCloudDrive/Erasmus University Rotterdam/Master/Thesis/Data/All_Combined_new.csv",
               col_types = cols(start_datetime = col_datetime(), end_datetime = col_datetime()))

df <- df %>% 
  drop_na() %>%
  select(start_datetime, Load, Solar, WindShore, WindOffShore, Net_Import, Coal_Price, Price) %>%
  filter(date(start_datetime) == as.Date("2024-08-24"))

scale_factor <- max(df$Price, na.rm = TRUE) / max(df$Load, na.rm = TRUE)
df <- df %>% mutate(across(c(Load, Solar, WindShore, WindOffShore, Net_Import),
                           ~ . * scale_factor, .names = "scaled_{.col}"))

df_long_kwh <- df %>%
  pivot_longer(cols = starts_with("scaled_"),
               names_to = "Variable", values_to = "Value") %>%
  mutate(Variable = str_remove(Variable, "scaled_"))

availability_vec <- read_csv("C:/Users/daniq/iCloudDrive/Erasmus University Rotterdam/Master/Thesis/Output/Schedules/Y_available_24.csv",
                             col_names = FALSE)
availability_vec <- availability_vec[-1,-1]
availability_vec <- availability_vec[49, 1:24] %>% unlist() %>% as.logical() %>% as.numeric()

availability_df <- tibble(
  start_datetime = seq(ymd_h("2024-08-24 00"), by = "1 hour", length.out = 24),
  available = availability_vec
)

lqra_df <- read_csv("C:/Users/daniq/iCloudDrive/Erasmus University Rotterdam/Master/Thesis/Output/Forecasts/Probability/Regular_LQRA_median_forecast.csv", col_names = TRUE)

median_values <- lqra_df[49, 1:24] %>% unlist() %>% as.numeric()
date_seq <- seq(ymd_h("2024-08-24 00"), by = "1 hour", length.out = 24)

lqra_median_df <- tibble(
  datetime = date_seq,
  median_forecast = median_values
)

ggplot() +
  geom_rect(data = availability_df %>% filter(available == 1),
            aes(xmin = start_datetime, xmax = start_datetime + hours(1),
                ymin = -Inf, ymax = Inf),
            fill = "darkgreen", alpha = 0.1) +
  geom_line(data = df, aes(x = start_datetime, y = Price, colour = "Price"), size = 0.9) +
  geom_line(data = df, aes(x = start_datetime, y = Coal_Price, colour = "Coal Price"), size = 1, linetype = "dashed") +
  geom_line(data = df_long_kwh, aes(x = start_datetime, y = Value, colour = Variable), size = 1) +
  scale_y_continuous(
    name = "Price / Coal Price (EUR)",
    sec.axis = sec_axis(~ . / scale_factor, name = "Energy Variables (kWh)")
  ) +
  scale_colour_manual(
    name = "Variable",
    values = c(
      "Price" = "black",               
      "Coal Price" = "#E69F00",         
      "Load" = "#009E73",              
      "Solar" = "#CBA135",             
      "WindShore" = "#56B4E9",         
      "WindOffShore" = "#D55E00",       
      "Net_Import" = "#CC79A7"          
    )
  ) +
  labs(x = "Variables on 24-08-2024") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = c(0.05, 0.00),       
    legend.justification = c("left", "bottom")  
  )


