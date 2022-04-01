library(DT)
library(shiny)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(maps)
library(reshape2)

# get system paths
root <- dirname(rstudioapi::getSourceEditorContext()$path)
# read datasets
m_cancer <- read.csv(paste0(root, '/males_cancer.csv'))
f_cancer <- read.csv(paste0(root, '/females_cancer.csv'))
all_cancer <- read.csv(paste0(root, '/both_gender_cancer.csv'))

cancer_types = c('Breast Cancer', 'Cervical Cancer', 'Colon and Rectum Cancer', 'Liver Cancer', 'Lung Cancer', 'Stomach Cancer', 'Prostate Cancer')
genders = c('Female', 'Male', 'Both genders')
measures = c('Incidence', 'Death rate')


test_str <- 'Female'
test_str <- 'Stomach Cancer'
test <- ifelse(test_str == 'Female', 'women', ifelse(test_str == 'Male', 'men', ifelse(test_str == 'Both genders', 'both_genders', '')))




ui <- fluidPage(titlePanel("Global Cancer Data Visualization"),
                sidebarLayout(
                  sidebarPanel(
                    width = 2,
                    selectInput("cancer", "Cancer Type", choices = cancer_types),
                    selectInput("gender", "Gender", choices = genders),
                    selectInput("countries", "Countries", choices = unique(all_cancer$country), multiple = TRUE),
                    selectInput('measure', 'Measure', choices = measures),
                    uiOutput("obs1")
                  ),
                mainPanel(
                  plotOutput('plot')
                  )))

server <- function(input, output) {
  output$plot <- renderPlot({
    cancer_condition <- strsplit(tolower(input$cancer), ' ')[[1]][1]
    gender_condition <- ifelse(input$gender == 'Female', 'women', ifelse(input$gender == 'Male', 'men', ifelse(input$gender == 'Both genders', 'both_genders', '')))
    measure_condition <- ifelse(input$measure == 'Incidence', 'new_cases', 'deaths')
    
    plot.df <- cbind(all_cancer[1:3], all_cancer[, grepl(cancer_condition, names(all_cancer))])
    plot.df <- cbind(all_cancer[1:3], plot.df[, grepl(gender_condition, names(plot.df))])
    plot.df <- cbind(all_cancer[1:3], value = plot.df[, grepl(measure_condition, names(plot.df))])
    
    plot.df <- plot.df[plot.df$country %in% input$countries,]
    plot.df <- plot.df %>% filter(between(year, input$year_range[1], input$year_range[2]))
                                                
    
    #print(plot.data)
    ggplot(plot.df) + geom_line(aes(x = year, y = value, colour = country))})
  
      output$obs1 <- renderUI({
        sliderInput(
          "year_range", 
          label = "Year:",
          min = 1995, 
          max = 2015, 
          value = c(2000, 2010)
        )
      })
    
}

shinyApp(ui, server)
