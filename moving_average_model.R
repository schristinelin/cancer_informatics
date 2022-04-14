library(dplyr)
#install.packages('smooth')
require(smooth)
root <- dirname(rstudioapi::getSourceEditorContext()$path)

###load dataset
all_cancer_long <- read.csv(paste0(root, "/cancer_forecast_input.csv"))

country_list <- unique(all_cancer_long$country)
measure_list <- unique(all_cancer_long$newordeath)
gender_list <- unique(all_cancer_long$gender)
cancer_types <- unique(all_cancer_long$type)

MA5_forecast <- function(df) {

    df$prob <- as.numeric(df$prob)
    df$year <- as.numeric(df$year)
    forcast_value <- sum(as.numeric(slice_tail(df, n = 10)$prob)) / 10
    last_row <- slice_tail(df, n = 1)
    new_row = c(last_row$country, last_row$year +1, forcast_value, last_row$gender, last_row$newordeath, last_row$type, last_row$iso3, last_row$continent)
    df[nrow(df) + 1,] = new_row
    return(df)
}

#country_list = c('Albania', 'Denmark')
df_forecast <- data.frame()
print('Forecasting values using 10-year Moving Average Algorithm')
for (i in country_list) {
  print(i)
  country_df <- all_cancer_long %>% filter(country == i)
  for (j in gender_list) {
    gender_df <- country_df %>% filter(gender == j)
    for (measure in measure_list) {
      measure_df <- gender_df %>% filter(newordeath == measure)
      for(cancer in cancer_types) {
        df <- measure_df %>% filter(type == cancer)
        if(nrow(df) > 0) {
        while (max(df$year) < 2025) {
          df <- MA5_forecast(df)
          df$prob <- round(as.numeric(df$prob), digits = 2)}}
        if(nrow(df_forecast) == 0) {
          df_forecast <- data.frame(row.names = rownames(df))
          df_forecast <- cbind(df_forecast, df)
        } else {
          df_forecast <- rbind(df_forecast, df)
        }
        }
    }
  }
}


write.csv(df_forecast, paste0(root, '/all_cancer_long.csv'), row.names = FALSE)
