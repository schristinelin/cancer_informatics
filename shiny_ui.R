library(ggplot2)
library(tidyr)
library(magrittr)
library(rvest)
library(dplyr)

# get system paths
root <- dirname(rstudioapi::getSourceEditorContext()$path)
list.files(root)

# read datasets
m_cancer <- read.csv(paste0(root, '/m_cancer.csv'))
f_cancer <- read.csv(paste0(root, '/f_cancer.csv'))

df_sub <- m_cancer[m_cancer$iso3 %in% c('AFG', 'AGO', 'ALB'),]
ggplot(df_sub, aes_string(x = 'year', y = 'mean_prob_cancer')) + geom_line() + facet_grid(. ~ iso3, scales = 'free') 


plotdf <- function(df, colN, Nbins = NULL) {
  if (is.numeric(df[[colN]]) == T) {
    ggplot(df, aes_string(x = colN)) + geom_histogram(bins = Nbins)
  } else {
    ggplot(df, aes_string(x = colN)) + geom_bar() + theme(axis.text.x=element_text(angle = 60, hjust = 1))
  }
  
}