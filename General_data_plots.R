## Makes Outliers, Seasonal,  Johnon-SU vs Mixture and general variable plot

#Load data
library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)
library(mixtools)

Prices_2025 <- read_excel(
  path = "C:/Users/daniq/iCloudDrive/Erasmus University Rotterdam/Master/Thesis/Data/Energy_Prices_2025.xlsx",
  skip = 7,  # Skip the first row
  col_names = c("Timestamp", "Price"),  
  col_types = c("text", "numeric")
)
Prices_2025 <- Prices_2025 %>%
  separate(col = 1, into = c("start", "end"), sep = " - ") %>%
  mutate(
    start_datetime = dmy_hms(start),
    end_datetime = dmy_hms(end),
    start_day = as.Date(start_datetime),
    start_time = format(start_datetime, "%H:%M:%S"),
    end_day = as.Date(end_datetime),
    end_time = format(end_datetime, "%H:%M:%S")
  ) %>%
  select(start_day, start_time, end_time, end_day, start_datetime, end_datetime, everything()) %>%
  select(-start, -end)

Prices_2024 <- read_excel(
  path = "C:/Users/daniq/iCloudDrive/Erasmus University Rotterdam/Master/Thesis/Data/Energy_Prices_2024.xlsx",
  skip = 7,  # Skip the first row
  col_names = c("Timestamp", "Price"),
  col_types = c("text", "numeric")
)

Prices_2024 <- Prices_2024 %>%
  separate(col = 1, into = c("start", "end"), sep = " - ") %>%
  mutate(
    start_datetime = dmy_hms(start),
    end_datetime = dmy_hms(end),
    start_day = as.Date(start_datetime),
    start_time = format(start_datetime, "%H:%M:%S"),
    end_day = as.Date(end_datetime),
    end_time = format(end_datetime, "%H:%M:%S")
  ) %>%
  select(start_day, start_time, end_time, end_day, start_datetime, end_datetime, everything()) %>%
  select(-start, -end)


Prices_2023 <- read_excel(
  path = "C:/Users/daniq/iCloudDrive/Erasmus University Rotterdam/Master/Thesis/Data/Energy_Prices_2023.xlsx",
  skip = 7,  # Skip the first row
  col_names = c("Timestamp", "Price"),  
  col_types = c("text", "numeric")
)

Prices_2023 <- Prices_2023 %>%
  separate(col = 1, into = c("start", "end"), sep = " - ") %>%
  mutate(
    start_datetime = dmy_hms(start),
    end_datetime = dmy_hms(end),
    start_day = as.Date(start_datetime),
    start_time = format(start_datetime, "%H:%M:%S"),
    end_day = as.Date(end_datetime),
    end_time = format(end_datetime, "%H:%M:%S")
  ) %>%
  select(start_day, start_time, end_time, end_day, start_datetime, end_datetime, everything()) %>%
  select(-start, -end)

comb_data <- rbind(Prices_2023, Prices_2024, Prices_2025)

ggplot(comb_data, aes(x = start_datetime, y = Price)) +
  geom_line(color = "aquamarine4") +
  labs(
       x = "Time",
       y = "Price (€/MWh)") +
  theme_minimal()


comb_data_new <- read_csv(
  file = "C:/Users/daniq/iCloudDrive/Erasmus University Rotterdam/Master/Thesis/Data/All_Combined_new.csv"
)




# Outlier plot and matrix
ts_data <- ts(comb_data_new$Price, frequency = 24)  # adjust frequency as needed
decomp <- stl(ts_data, s.window = "periodic")
residuals <- decomp$time.series[, "remainder"]

threshold <- 3 * sd(residuals, na.rm = TRUE)
outliers <- abs(residuals) > threshold

plot(ts_data, main = "STL-based Outlier Detection")
points(time(ts_data)[outliers], ts_data[outliers], col = "red", pch = 19)
sum(outliers == TRUE)


comb_data_new$residuals <- residuals
comb_data_new$outlier <- abs(residuals) > threshold

ggplot(comb_data_new, aes(x = start_datetime, y = Price)) +
  geom_line(color = "darkgray") +
  geom_point(data = subset(comb_data_new, outlier), aes(x = start_datetime, y = Price), color = "red") +
  labs(x = "Time", y = "Price (€/MWh)") +
  theme_minimal()

library(readr)
library(dplyr)
library(lubridate)

test_days <- read_csv(
  file = "C:/Users/daniq/iCloudDrive/Erasmus University Rotterdam/Master/Thesis/Data/test_dates.csv",  
  skip = 1,
  col_names = "start_day",
  col_types = "c"
)  %>%
  mutate(start_day = as.POSIXct(start_day, tz = "UTC")) 

outlier_matrix <- matrix(FALSE, nrow = nrow(test_days), ncol = 144)

for (i in seq_len(nrow(test_days))) {
  start <- test_days$start_day[i]
  time_window <- comb_data_new %>%
    filter(start_datetime >= start, start_datetime < start + hours(144)) %>%
    arrange(start_datetime)
  
  if (nrow(time_window) >= 144) {
    outlier_matrix[i, ] <- time_window$outlier[1:144]
  } else {
    warning(paste("Less than 72 hours available for test day:", start))
    outlier_matrix[i, 1:nrow(time_window)] <- time_window$outlier
  }
}

write.csv(outlier_matrix,
          file = "C:/Users/daniq/iCloudDrive/Erasmus University Rotterdam/Master/Thesis/Data/outlier_matrix_144.csv",
          row.names = FALSE)



#Plots for the seasons
comb_data$start_datetime.month <- as.numeric(format(comb_data$start_datetime, "%m"))
comb_data$Winter <- ifelse(comb_data$start_datetime.month %in% c(1, 2, 12), 1, 0)
comb_data$Summer <- ifelse(comb_data$start_datetime.month  %in% c(6, 7, 8), 1, 0)
comb_data$Fall <- ifelse(comb_data$start_datetime.month  %in% c(9, 10, 11), 1, 0)
comb_data$Spring <- ifelse(comb_data$start_datetime.month  %in% c(3, 4, 5), 1, 0)

comb_data$start_datetime.year <- as.numeric(format(comb_data$start_datetime, "%y"))

train_df <- comb_data[
  (comb_data$start_datetime.year == 23),
]

val_df <- comb_data[
    (comb_data$start_datetime.year == 24 & comb_data$start_datetime.month <= 9),
]

test_df <- comb_data[
  (comb_data$start_datetime.year == 24 & comb_data$start_datetime.month >= 7) |
    (comb_data$start_datetime.year == 25 & comb_data$start_datetime.month <= 4),
]


summary(train_df$Price)
summary(val_df$Price)
summary(test_df$Price)

common_ylim <- c(-200, 350) 

ggplot(train_df, aes(x = start_datetime, y = Price)) +
  geom_line(color = "darkseagreen3") +
  labs(
    x = "Time",
    y = "Price (€/MWh)") +
  scale_x_datetime(date_labels = "%b %Y") +
  coord_cartesian(ylim = common_ylim) +
  theme_minimal()

ggplot(val_df, aes(x = start_datetime, y = Price)) +
  geom_line(color = "pink2") +
  labs(
    x = "Time",
    y = "Price (€/MWh)") +
  scale_x_datetime(date_labels = "%b %Y") +
  coord_cartesian(ylim = common_ylim) +
  theme_minimal()

ggplot(test_df, aes(x = start_datetime, y = Price)) +
  geom_line(color = "bisque2") +
  labs(
    x = "Time",
    y = "Price (€/MWh)") +
  scale_x_datetime(date_labels = "%b %Y") +
  coord_cartesian(ylim = common_ylim) +
  theme_minimal()


hourly_data <- comb_data %>%
  filter(start_time >= "12:00:00" & start_time < "13:00:00")


hourly_data <- hourly_data %>%
  mutate(Season = case_when(
    Winter == 1 ~ "Winter",
    Spring == 1 ~ "Spring",
    Summer == 1 ~ "Summer",
    Fall   == 1 ~ "Fall"
  ))


all_data <- hourly_data %>%
  mutate(Season = "All")

plot_data <- bind_rows(hourly_data, all_data)

group1 <- plot_data %>% filter(Season %in% c("Fall", "Spring", "All"))
group2 <- plot_data %>% filter(Season %in% c("Winter", "Summer", "All"))

season_colors1 <- c("Fall" = "darkorange2", "Spring" = "palegreen4", "All" = "magenta4")
season_colors2 <- c("Winter" = "cornflowerblue", "Summer" = "red3", "All" = "magenta4")

p1 <- ggplot(group1, aes(x = Price, color = Season)) +
  geom_density(size = 1, show.legend = TRUE) +
  scale_color_manual(values = season_colors1) +
  coord_cartesian(xlim = c(-200, 250)) +
  labs(x = "Price (€/MWh)", y = "Density", color = "Season") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    legend.key = element_blank(),        # removes color boxes
    legend.key.height = unit(1.2, "lines"),
    legend.title = element_text(face = "bold")
  )

p2 <- ggplot(group2, aes(x = Price, color = Season)) +
  geom_density(size = 1, show.legend = TRUE) +
  scale_color_manual(values = season_colors2) +
  coord_cartesian(xlim = c(-200, 250)) +
  labs(x = "Price (€/MWh)", y = "Density", color = "Season") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    legend.key = element_blank(),
    legend.key.height = unit(1.2, "lines"),
    legend.title = element_text(face = "bold")
  )

print(p1)
print(p2)

ggplot(hourly_data, aes(x = Price)) +
  geom_density(color = "pink3") +
  labs(x = "Price (€/MWh)",
       y = "Density") +
  theme_minimal()

ggplot(hourly_data, aes(x = Price)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "white") +
  labs(x = "Price (€/MWh)",
       y = "Frequency") +
  xlim(-100, 150) +
  theme_minimal()

hourly_data <- comb_data %>%
  filter(start_time >= "17:00:00" & start_time < "18:00:00")

# Histogram
ggplot(hourly_data, aes(x = Price)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "white") +
  labs(x = "Price (€/MWh)",
       y = "Frequency") +
  xlim(-50,250) +
  theme_minimal()



## Mixture and johnson van 5 to 6
library(dplyr)
library(ggplot2)
library(SuppDists)

hourly_data <- comb_data %>%
  filter(start_time >= "17:00:00" & start_time < "18:00:00")

fit_jsu <- JohnsonFit(hourly_data$Price, moment = "four")

x_vals <- seq(min(hourly_data$Price), max(hourly_data$Price), length.out = 500)
jsu_density <- dJohnson(x_vals, parms = fit_jsu)

fitted_df <- data.frame(x = x_vals, y = jsu_density)
fitted_df$Type <- "Johnson SU"

hourly_data$Type <- "Actual Values"
comp1_data <- hourly_data$Price[abs(hourly_data$Price) <= 1.5]
comp2_data <- hourly_data$Price[abs(hourly_data$Price) > 1.5]

fit_jsu1 <- JohnsonFit(comp1_data, moment = "quant")
fit_jsu2 <- JohnsonFit(comp2_data, moment = "quant")

mixture_johnsonsu <- function(n, pi1, parms1, pi2, parms2) {
  choice <- rbinom(n, 1, pi1)
  n1 <- sum(choice)
  n2 <- n - n1
  c(rJohnson(n1, parms1), rJohnson(n2, parms2))
}

set.seed(123) 
weight1 = length(comp1_data)*3/length(hourly_data$Price)
weight2 = 1 - weight1
mixture_data <- mixture_johnsonsu(n = 5000, pi1 = weight1, parms1 = fit_jsu1, pi2 = weight2, parms2 = fit_jsu2)

mixture_density <- density(mixture_data, from = min(hourly_data$Price), to = max(hourly_data$Price))
mixture_df <- data.frame(x = mixture_density$x, y = mixture_density$y)
mixture_df$Type <- "Mixture of Johnson SU"

ggplot() +
  geom_histogram(data = hourly_data,
                 aes(x = Price, y = ..density.., color = Type, fill = Type),
                 binwidth = 5, alpha = 0.6, position = "identity") +
  geom_line(data = fitted_df,
            aes(x = x, y = y, color = Type), size = 1.2, linetype = 'longdash') +
  geom_line(data = mixture_df,
            aes(x = x, y = y, color = Type), size = 1.2) +
  scale_color_manual(name = "Legend",
                     values = c("Actual Values" = "skyblue2", 
                                "Johnson SU" = "seagreen3", 
                                "Mixture of Johnson SU" = "red1")) +
  scale_fill_manual(name = "Legend",
                    values = c("Actual Values" = "skyblue2", 
                               "Johnson SU" = NA, 
                               "Mixture of Johnson SU" = NA)) +
  guides(fill = "none",
         color = guide_legend(override.aes = list(
           fill = c("skyblue2", NA, NA),
           linetype = c("blank", "solid", "dashed"),
           shape = c(NA, NA, NA)
         ))) +
  labs(x = "Price (€/MWh)", y = "Density") +
  xlim(-50, 250) +
  theme_minimal() +
  theme(legend.position = c(0.90, 0.90),
        legend.justification = c("right", "top"),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 10))


## Mixture and johnson from 12 to 13
library(dplyr)
library(ggplot2)
library(SuppDists)

hourly_data <- comb_data %>%
  filter(start_time >= "12:00:00" & start_time < "13:00:00")

fit_jsu <- JohnsonFit(hourly_data$Price, moment = "four")

x_vals <- seq(min(hourly_data$Price), max(hourly_data$Price), length.out = 500)
jsu_density <- dJohnson(x_vals, parms = fit_jsu)

fitted_df <- data.frame(x = x_vals, y = jsu_density)
fitted_df$Type <- "Johnson SU"

hourly_data$Type <- "Actual Values"

comp1_data <- hourly_data$Price[abs(hourly_data$Price) <= 1.5]
comp2_data <- hourly_data$Price[abs(hourly_data$Price) > 1.5]

fit_jsu1 <- JohnsonFit(comp1_data, moment = "quant")
fit_jsu2 <- JohnsonFit(comp2_data, moment = "quant")

mixture_johnsonsu <- function(n, pi1, parms1, pi2, parms2) {
  choice <- rbinom(n, 1, pi1)
  n1 <- sum(choice)
  n2 <- n - n1
  c(rJohnson(n1, parms1), rJohnson(n2, parms2))
}

set.seed(123) 
weight1 = length(comp1_data)*2/length(hourly_data$Price)
weight2 = 1 - weight1
mixture_data <- mixture_johnsonsu(n = 5000, pi1 = weight1, parms1 = fit_jsu1, pi2 = weight2, parms2 = fit_jsu2)

mixture_density <- density(mixture_data, from = min(hourly_data$Price), to = max(hourly_data$Price))
mixture_df <- data.frame(x = mixture_density$x, y = mixture_density$y)
mixture_df$Type <- "Mixture of Johnson SU"


ggplot() +
  geom_histogram(data = hourly_data,
                 aes(x = Price, y = ..density.., color = Type, fill = Type),
                 binwidth = 5, alpha = 0.6, position = "identity") +
  geom_line(data = fitted_df,
            aes(x = x, y = y, color = Type), size = 1.2, linetype = 'longdash') +
  geom_line(data = mixture_df,
            aes(x = x, y = y, color = Type), size = 1.2) +
  scale_color_manual(name = "Legend",
                     values = c("Actual Values" = "skyblue2", 
                                "Johnson SU" = "seagreen3", 
                                "Mixture of Johnson SU" = "red1")) +
  scale_fill_manual(name = "Legend",
                    values = c("Actual Values" = "skyblue2", 
                               "Johnson SU" = NA, 
                               "Mixture of Johnson SU" = NA)) +
  guides(fill = "none",
         color = guide_legend(override.aes = list(
           fill = c("skyblue2", NA, NA),
           linetype = c("blank", "solid", "dashed"),
           shape = c(NA, NA, NA)
         ))) +
  labs(x = "Price (€/MWh)", y = "Density") +
  xlim(-50, 150) +
  theme_minimal() +
  theme(legend.position = c(0.90, 0.90),
        legend.justification = c("right", "top"),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 10))









# General variable plot

library(ggplot2)
library(patchwork)
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)

df <- read_csv("C:/Users/daniq/iCloudDrive/Erasmus University Rotterdam/Master/Thesis/Data/All_Combined_new.csv",
               col_types = cols(start_datetime = col_datetime(), end_datetime = col_datetime()))

df <- df %>% 
  drop_na() %>%
  select(start_datetime, Load, Solar, WindShore, WindOffShore, Net_Import, Coal_Price)
desired_order <- c("Solar", "WindShore", "WindOffShore", "Load", "Net_Import", "Coal_Price")

df_long <- df %>% 
  pivot_longer(cols = -start_datetime, 
               names_to = "Variable", 
               values_to = "Value")

df_long$Variable <- factor(df_long$Variable, levels = desired_order)

ggplot(df_long, aes(x = start_datetime, y = Value)) +
  geom_line(color = "darkcyan") +
  facet_wrap(~ Variable, scales = "free_y", ncol = 1) +
  labs(x = "Date",
       y = "Value") +
  theme_minimal()






