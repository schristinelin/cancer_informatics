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
all_cancer <- read.csv(paste0(root, '/all_cancer_long.csv'))

cancer_types = c('Breast Cancer', 'Cervical Cancer', 'Colon and Rectum Cancer', 'Liver Cancer', 'Lung Cancer', 'Stomach Cancer', 'Prostate Cancer')
genders = c('Female', 'Male', 'Both genders')
measures = c('New Cases', 'Death Rate')


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
    cancer_condition <- strsplit(input$cancer, ' ')[[1]][1]
    gender_condition <- strsplit(input$gender, ' ')[[1]][1]
    measure_condition <- strsplit(input$measure, ' ')[[1]][1]
    
    plot.df <- all_cancer[(all_cancer$type %like% cancer_condition) & (all_cancer$gender %like% gender_condition) & (all_cancer$newordeath %like% measure_condition),]
    plot.df <- plot.df[plot.df$country %in% input$countries,]
    plot.df <- plot.df %>% filter(between(year, input$year_range[1], input$year_range[2]))
                                                
    
    #print(plot.data)
    ggplot(plot.df) + geom_line(aes(x = year, y = prob, colour = country))})
  
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
