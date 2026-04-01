# Hope Mullins QMB6304 Project Stage 3

rm(list = ls())

library(shiny)
library(readxl)
library(ggplot2)
library(dplyr)
library(scales)


turnout <- read_excel("C:/Users/hem10/OneDrive/Desktop/CPS Turnout Rates.xlsx")


ui <- fluidPage(
  titlePanel("QMB6304: Project Stage 3"),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(12,
               sliderInput("year_range", "Select Year Range",
                           min = min(turnout$year),
                           max = max(turnout$year),
                           value = c(min(turnout$year), max(turnout$year)),
                           step = 1,
                           sep = "")
        )
      ),
      
      fluidRow(
        column(12,
               selectInput("age_range", "Age Range",
                           choices = c("18-29", "30-44", "45-59", "60+"),
                           selected = "18-29")
        )
      ),
      
      fluidRow(
        column(6,
               selectInput("compare1", "Compare", 
                           choices = c("Hispanic Turnout", "Black Turnout", "White Turnout",
                                       "Other Turnout", "Hispanic Electorate Share", "Black Electorate Share",
                                       "White Electorate Share", "Other Electorate Share"),
                           selected = "Hispanic Turnout")
        ),
        
        column(6,
               selectInput("compare2", "With",
                           choices = c("Hispanic Turnout", "Black Turnout", "White Turnout",
                                       "Other Turnout", "Hispanic Electorate Share", "Black Electorate Share",
                                       "White Electorate Share", "Other Electorate Share"),
                           selected = "Hispanic Electorate Share")
        )
      )
    ),
    
    mainPanel(
      plotOutput("linePlot", height = "400px"),
      plotOutput("scatterplot", height = "400px")
    )
  )
)


server <- function(input, output) {
  output$linePlot <- renderPlot({
    voter <- paste0("voterage: ", input$age_range)
    electorate <- paste0("electorateage: ", input$age_range)
    
    filtered <- turnout %>%
      filter(year >= input$year_range[1], year <= input$year_range[2]) %>%
      select(year, !!sym(voter), !!sym(electorate))
    colnames(filtered) <- c("Year", "Voter", "Electorate")
    
    ggplot(filtered, aes(x = Year)) +
      geom_line(aes(y = Voter, color = "Voter"), size = 1) +
      geom_line(aes(y = Electorate, color = "Electorate"), size = 1) +
      labs(title = paste("Voters and Electorates Ages", input$age_range),
           x = "Year", y = "", color = "") +
      scale_color_manual(values = c("Voter" = "red", "Electorate" = "blue")) +
      scale_y_continuous(labels = percent_format(scale = 100)) +
      theme(panel.background = element_rect(fill = "white"),
            panel.grid.major = element_line(color = "gray95"),
            panel.grid.minor = element_line(color = "gray95"),
            axis.line = element_line(color = "black"),
            legend.text = element_text(size = 12),
            plot.title = element_text(size = 15, face = "bold"),
            axis.title = element_text(size = 12))
  })
  
  output$scatterplot <- renderPlot({
    compare1 <- input$compare1
    compare2 <- input$compare2
    
    filtered2 <- turnout %>%
      filter(year >= input$year_range[1], year <= input$year_range[2]) %>%
      select(year, !!sym(compare1), !!sym(compare2))
    colnames(filtered2) <- c("Year", "Compare1", "Compare2")
    
    ggplot(filtered2, aes(x = Year)) +
      geom_point(aes(y = Compare1, color = compare1), size = 3) +
      geom_point(aes(y = Compare2, color = compare2), size = 3) +
      labs(
        title = paste("Comparison of", input$compare1,
                      "and", input$compare2),
        x = "Year", y = "Race and Ethnicity",
        color = ""
      ) +
      scale_color_manual(values = c("Hispanic Turnout" = "purple",
                                    "Black Turnout" = "lightblue",
                                    "White Turnout" = "darkred",
                                    "Other Turnout" = "forestgreen",
                                    "Hispanic Electorate Share" = "pink",
                                    "Black Electorate Share" = "darkblue",
                                    "White Electorate Share" = "yellow",
                                    "Other Electorate Share" = "turquoise"),
                         breaks = c(compare1, compare2),
                         labels = c(compare1, compare2)
      ) +
      scale_y_continuous(labels = percent_format(scale = 100)) +
      theme(panel.background = element_rect(fill = "white"),
            panel.grid.major.y = element_line(color = "gray95"),
            panel.grid.minor.y = element_line(color = "gray95"),
            axis.line = element_line(color = "black"),
            legend.text = element_text(size = 12),
            plot.title = element_text(size = 15, face = "bold"),
            axis.title = element_text(size = 12))
  })
}

shinyApp(ui, server)
