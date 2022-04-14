library(shiny)
library(plotly) 
library(dplyr)
library(data.table)
library(ggplot2)

## get system path
root <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(root)

# launch data script if file does not exist
if (file.exists(paste0(root, "/cancer_forecast_input.csv")) == FALSE) {
  source('input_data_scripting.R')
}

if (file.exists(paste0(root, "/all_cancer_long.csv")) == FALSE) {
  source('moving_average_model.R')
}


###load dataset
all_cancer_long <- read.csv(paste0(root, "/all_cancer_long.csv"))

#all_cancer_long$year <- as.character(all_cancer_long$year)

###world map aesthetics
l <- list(color = toRGB("grey"), width = 0.5)

g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  project = list(type = 'Mercator')
)

###UI
ui <- fluidPage(
  titlePanel("Cancer Cases"),
  selectInput(inputId = "select", label = "Select visualization Type",
              choices = c("World Map","Line Plot", 'Paired Bar Chart'), plotOutput(outputId = "plot")),
  sidebarLayout(
    sidebarPanel(width = 3,
                 selectInput("gender", "Gender", choices = c("Both", "Female", "Male")),
                 selectInput("type", "Type of Cancer", choices = NULL),
                 uiOutput("newordeath"),
                 uiOutput("year_select_line"),
                 uiOutput("year_select_world"),
                 uiOutput('country_multiple'),
                 verbatimTextOutput("text")
    ),
    mainPanel(
      conditionalPanel("input.select == 'World Map'",
                       plotlyOutput("world_map")),
      conditionalPanel("input.select == 'Line Plot'",
                       plotOutput('line_plot')),
      conditionalPanel("input.select == 'Paired Bar Chart'",
                       plotOutput('paired_bar_chart'))
      
    )
  ))

###Server
server <- function(input, output, session) {
  observe({
    y <- input$gender
    cancer_types <- all_cancer_long %>% filter(gender == y) %>%
      group_by(type) %>% summarize()
    updateSelectInput(session, "type", "Type of Cancer", choices = cancer_types)
  })
  
  observe({
    if(input$select=="World Map"){
      
      output$world_map <- renderPlotly({
        data <- subset(all_cancer_long, 
                       year == input$year & gender == input$gender & newordeath %like% input$newordeath & type == input$type, 
                       select=c("country", "iso3", "prob"))
        plot_ly(data, type = 'choropleth', locations = data$iso3, z = data$prob, text=data$country, colors = "YlOrBr", marker = list(line=l)) %>%
          colorbar(title = 'per 100,000 people') %>%
          layout(geo=g)
      })
      
      output$newordeath <- renderUI({
        selectInput("newordeath", "Death/New Cases", choices = c("New Cases", "Death"))
      })
      
      output$year_select_world <- renderUI({
        sliderInput("year", "Year", min = min(all_cancer_long$year), max = max(all_cancer_long$year), value = 2005, step = 1)
      })
      
      output$year_select_line <- NULL
      output$line_plot <- NULL
      output$country_multiple <- NULL
      
    } else if (input$select=="Line Plot") {
      output$world_map <- NULL
      output$line_plot <- renderPlot({
        cancer_condition <- strsplit(input$type, ' ')[[1]][1]
        gender_condition <- strsplit(input$gender, ' ')[[1]][1]
        measure_condition <- strsplit(input$newordeath, ' ')[[1]][1]
        
        plot.df <- all_cancer_long[(all_cancer_long$type %like% cancer_condition) & 
                                     (all_cancer_long$gender %like% gender_condition) & 
                                     (all_cancer_long$newordeath %like% measure_condition),]
        plot.df <- plot.df[plot.df$country %in% input$countries,]
        plot.df <- plot.df %>% filter(between(year, input$year_range[1], input$year_range[2]))
        
        ggplot(plot.df) + 
          geom_line(aes(x = year, y = prob, colour = country)) + 
          xlab("Year") +
          ylab("Cases per 100,000 People") +
          labs(colour = "Country") + 
          guides(color = guide_legend(override.aes = list(size = 5)))+
          theme_bw()
        })
      
      output$newordeath <- renderUI({
        selectInput("newordeath", "Death/New Cases", choices = c("New Cases", "Death"))
      })
      
      output$year_select_line <- renderUI({
        sliderInput(
          "year_range", 
          label = "Year:",
          min = min(all_cancer_long$year), 
          max = max(all_cancer_long$year), 
          value = c(2000, 2010),
          step = 1
        )
      })
      output$country_multiple <- renderUI({
        selectInput("countries", "Countries", choices = unique(all_cancer_long$country), multiple = TRUE)
      })
      output$year_select_world <- NULL
      
    }
    else if (input$select =='Paired Bar Chart') {
      output$world_map <- NULL
      output$line_plot <- NULL
      output$paired_bar_chart <- renderPlot({
        data_subset_bar <- subset(all_cancer_long, year == input$year & gender == input$gender & type == input$type)
        data_bar <- aggregate(data_subset_bar$prob, by = list(data_subset_bar$continent, data_subset_bar$newordeath), FUN = mean)
        data_bar <- data_bar %>% rename(continent = Group.1, newordeath = Group.2, prob = x)
        
        ggplot(data_bar, aes(fill = newordeath, y = prob, x = continent)) + 
          geom_bar( position = "dodge", stat = "identity") +
          geom_text(aes(label=round(prob, digits = 1)), vjust=1.6, color = "white",
                    position = position_dodge(0.9), size = 3.5) +
          xlab("Continent") +
          ylab("Cases per 100,000 People") +
          labs(fill = "Death/New Cases")+ 
          scale_fill_manual(values = c("#054C70","#05C3DE"))
        
      })

      
      output$year_select_line <- NULL
      output$line_plot <- NULL
      output$newordeath <- NULL
      output$country_multiple <- NULL
    }
  })
  
  
}



shinyApp(ui, server)

