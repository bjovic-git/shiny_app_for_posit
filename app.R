# This is a Shiny web application to test Posit Workbench Publishing capabilities
library(shiny)
library(lubridate)
library(dplyr)
library(ggplot2)

options(scipen = 999)

df <- read.csv("data/traveller_data.csv")

ui <- fluidPage(
  
  titlePanel("Traveller Counts Dashboard"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectizeInput(
        "airport",
        "Airport",
        choices = c("All", sort(unique(df$airport))), 
        selected = "All", 
        multiple = TRUE, 
      ),
      
      selectizeInput(
        "year",
        "Year",
        choices = c("All", sort(unique(df$year))), 
        selected = "All", 
        multiple = TRUE
      ), 
      
      selectInput(
        "country",
        "Residence Country",
        choices = c("All", sort(unique(df$residence_country)))
      ),
      
      selectInput(
        "traveller",
        "Traveller Type",
        choices = c("All", sort(unique(df$traveller_type)))
      )
      
    ),
    
    mainPanel(
      
      h3(textOutput("total_count")),
      hr(), 
      plotOutput("airport_plot"),
      hr(), 
      plotOutput("trend_plot"),
      
    )
  )
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    
    data <- df
    
    if (!("All" %in% input$airport)) {
      data <- data %>% 
        filter(airport %in% input$airport)
    }
    
    if (input$country != "All")
      data <- filter(data, residence_country == input$country)
    
    if (input$traveller != "All")
      data <- filter(data, traveller_type == input$traveller)
    
    if (!("All" %in% input$year)) {
      data <- filter(data, year %in% input$year)
    }
    
    data
  })
  
  output$total_count <- renderText({
    paste(
      "Total Travellers:",
      format(sum(filtered_data()$counts), big.mark = ",")
    )
  })
  
  output$airport_plot <- renderPlot({
    
    filtered_data() %>%
      group_by(airport) %>%
      summarise(
        counts = sum(counts),
        .groups = "drop"
      ) %>%
      arrange(desc(counts)) %>%
      ggplot(aes(
        x = reorder(airport, counts),
        y = counts
      )) +
      geom_col(fill = "steelblue") +
      geom_text(
        aes(label = scales::comma(counts)), 
        hjust = -0.2, 
        size = 3
      ) + 
      coord_flip() +
      labs(
        title = "Traveller Counts by Airport",
        x = "Airport",
        y = "Traveller Count"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()
      )
    
  })
  
  output$trend_plot <- renderPlot({
    plot_data <- filtered_data() %>%
      group_by(year, month) %>%
      summarise(counts = sum(counts), .groups = "drop") %>%
      mutate(
        month = factor(
          month, 
          levels = month.abb
        )
      )
    
    ggplot(
      plot_data,
      aes(
        x = month,
        y = counts,
        color = factor(year),
        group = year
      )
    ) +
      geom_line(linewidth = 0.8) +
      geom_point(size = 2) +
      scale_y_continuous(
        labels = scales::comma
      ) + 
      labs(
        title = "Monthly Traveller Counts by Year",
        x = "Month",
        y = "Traveller Count",
        color = "Year"
      ) +
      theme_minimal()
  })
}

# Run the application 
shinyApp(ui, server)
