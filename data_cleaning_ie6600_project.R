
# import libraries
library(tidyr)
library(magrittr)
library(rvest)
library(dplyr)
library(countrycode)
library(data.table)

# get system paths
root <- dirname(rstudioapi::getSourceEditorContext()$path)

# data cleaning function
file_clean <- function(filename, year_val) {
  #filename <- files[1]
  suffix <- strsplit(filename, '.csv')[[1]]
  df <- read.csv(paste0(root, dir_name, filename))
  colnames(df)[1] <- "country"
  drop_countries <- c('French Polynesia', 'Hong Kong, China', 'Serbia and Montenegro', 'Czechoslovakia')
  
  #get X out of the column names
  for (col in 1:ncol(df)){
    colnames(df)[col] <-  sub("X", "", colnames(df)[col])
  }
  
  #change wide data to long data
  df <- df %>% gather(year, prob, -c(country))
  colnames(df)[3] <- suffix
  
  #change 'year' column from character to numeric
  df$year <- as.numeric(df$year)
  
  #drop data for 'year' before 1995 and specific countries
  df <- df[!(df$year < year_val | 
               df$country %in% drop_countries),]
  
  df$iso3 <- countrycode(df$country, "country.name", "iso3c")
  
return(df)  
  
}

# toggles
year_val <- 1995

for (gender in c('males', 'females')) {
# gender <- 'males'
  print(gender)
  dir_name = paste0('/datasets/', gender, '/')
  files = list.files(paste0(root, dir_name))
  
  # loop through files with cleaning algorithm
  df_all <- data.frame()
  
  for (file in files) {
    #file <- files[1]
    print(file)
    df <- file_clean(file, year_val)
    if(nrow(df_all) == 0) {
      df_all <- data.frame(row.names = rownames(df))
      df_all <- cbind(df_all, df)
    } else {
      df_all <- merge(df, df_all, on = c('year', 'country'))
    }
  }
  
  df_all$mean_prob_cancer <- rowMeans(sapply(df_all[,-c(1:3)], as.numeric))
  
  if(gender == 'males') {
    df_males <- copy(df_all)
  }
  
  if(gender == 'females') {
    df_females <- copy(df_all)
  }

  write.csv(df_all, paste0(root, '/', gender, '_cancer.csv'), row.names = FALSE)
  
  # make sure it resets dataframe for next gender
  df_all <- data.frame()
  print(paste0('record for ', gender, ' has been completed'))
}


print(paste0('constructing dataframe for both genders'))
# cancers that has both genders
df_nonfs <- df_females[, !grepl('breast|cervical', names(df_females))]
df_nonms <- df_males[, !grepl('prostate', names(df_males))]

df_both <- df_nonfs[,1:3]

for (i in seq(4,ncol(df_nonfs))) {
  df_both[i] <- (df_nonfs[i] + df_nonms[i])/2
}

colnames(df_both) <- gsub("women", "both_genders", colnames(df_both))  

write.csv(df_both, paste0(root, '/both_gender_cancer.csv'), row.names = FALSE)




