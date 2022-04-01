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

# print execution message
print('Reading input datasets')

for (gender in c('males', 'females')) {
# gender <- 'males'
  print(gender)
  dir_name = paste0('/datasets/', gender, '/')
  files = list.files(paste0(root, dir_name))
  
  # loop through files with cleaning algorithm
  df_all <- data.frame()
  
  for (file in files) {
    print(file)
    df <- file_clean(file, year_val)
    if(nrow(df_all) == 0) {
      df_all <- data.frame(row.names = rownames(df))
      df_all <- cbind(df_all, df)
    } else {
      df_all <- merge(df, df_all, on = c('year', 'country'))
    }
  }
  
  # make copies of data for later use
  if(gender == 'males') {
    df_males <- copy(df_all)
  }
  
  if(gender == 'females') {
    df_females <- copy(df_all)
  }

  
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


# clean up for final input
df_full <- merge(df_males, merge(df_females, df_both, by = c('year', 'country', 'iso3')), by = c('year', 'country', 'iso3'))

df_full <- melt(df_full, id.vars = c('year', 'country', 'iso3'))
df_full$gender <- ifelse(grepl("women", df_full$variable, ignore.case = T), "Female", 
                  ifelse(grepl("men", df_full$variable, ignore.case = T), "Male",
                  ifelse(grepl('both_genders', df_full$variable, ignore.case = T), 'Both', NA)))

df_full$newordeath <- ifelse(grepl("new_cases", df_full$variable, ignore.case = T), "New Cases", 
                         ifelse(grepl("deaths", df_full$variable, ignore.case = T), "Deaths", NA))

df_full$type <- ifelse(grepl("breast", df_full$variable, ignore.case = T), "Breast", 
                ifelse(grepl("cervical", df_full$variable, ignore.case = T), "Cervical",
                ifelse(grepl('colon', df_full$variable, ignore.case = T), 'Colon and Rectum',
                ifelse(grepl('liver', df_full$variable, ignore.case = T), 'Liver',
                ifelse(grepl('lung', df_full$variable, ignore.case = T), 'Lung',
                ifelse(grepl('stomach', df_full$variable, ignore.case = T), 'Stomach',
                ifelse(grepl('prostate', df_full$variable, ignore.case = T), 'Prostate', NA)))))))

df_full <- subset(df_full, select=-c(variable))

df_full$continent <- countrycode(sourcevar = df_full[, "country"],
                            origin = "country.name",
                            destination = "continent")

# rename and reorder
colnames(df_full)[which(names(df_full) == "value")] <- "prob"
df_full <- df_full[, c("country", "year", "prob", "gender", "newordeath", "type", "iso3", "continent")]

write.csv(df_full, paste0(root, '/all_cancer.csv'), row.names = FALSE)

