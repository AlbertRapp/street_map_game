

library(shiny)
library(readr)
library(dplyr)
library(sf)
library(ggplot2)
library(markdown)
library(ggmap)

street_lines_ulm <- read_rds("street_lines_ulm.rds") %>%
  select(name, geometry)
background_map <- read_rds("background_map.rds")



ui <- fluidPage(
  theme = bslib::bs_theme(
    # Colors (background, foreground, primary)
    bg = 'white', 
    fg = '#06436e', 
    primary = colorspace::lighten('#06436e', 0.3),
    
    # Fonts (Use multiple in case a font cannot be displayed)
    base_font = c('Source Sans Pro',  'Lato', 'Merriweather', 'Roboto Regular', 'Cabin Regular'),
    heading_font = c('Oleo Script', 'Prata', 'Roboto', 'Playfair Display', 'Montserrat'),
    font_scale = 1.25
  ),
  tabsetPanel(
    header = tags$head(tags$script('$(document).on("shiny:connected", function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            $(window).resize(function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            ')),
    tabPanel(
      "Game",
      fluidRow(
        column(2),
        column(
          8,
          h1("Where is the street?"),
          actionButton("btn", "Draw a street"),
          verbatimTextOutput("street_text"),
          plotOutput("map", click = 'map_click')
        ),
        column(2)
      )
    ),
    tabPanel(
      "About",
      fluidRow(
        column(2),
        column(
          8,
          includeMarkdown("about.md")
        ),
        column(2)
      )
    )
  )
)


server <- function(input, output) {
  
  street_name <- reactive({
    street_lines_ulm %>% 
      filter(!is.na(name)) %>% 
      slice_sample(n = 1) %>% 
      pull(name)
  }) %>% bindEvent(input$btn)
  
  street <- reactive({
    street_lines_ulm %>% 
      filter(name == street_name()) %>% 
      pull(geometry) %>% 
      st_union() %>% 
      st_sfc(crs = 4326)
  })
  
  plot <- reactiveVal()
  
  observe({
    plot(
      ggmap(background_map) +
        geom_sf(
          data = street_lines_ulm,
          inherit.aes = F,
          size = 1
        )
    )
  }) %>% bindEvent(input$btn)
  
  
  observe({
    point <- st_point(c(input$map_click$x, input$map_click$y)) %>% 
      st_sfc(crs = 4326)
    
    distance <- st_distance(point, street()) %>% round()
    
    circle <- point %>% st_buffer(distance)
    
    plot(
      ggmap(background_map) +
        geom_sf(
          data = street_lines_ulm,
          inherit.aes = F,
          size = 1
        ) +
        geom_sf(
          data = point,
          inherit.aes = F,
          size = 2.5,
          color = 'red'
        ) +
        geom_sf(
          data = street(),
          inherit.aes = F,
          size = 2.5,
          color = 'red'
        ) +
        geom_sf(
          data = circle,
          inherit.aes = F,
          size = 2.5,
          color = 'blue',
          fill = NA
        ) +
        annotate(
          'label',
          x = input$map_click$x,
          y = input$map_click$y + 0.0025,
          label = paste0(distance, 'm')
        )
    )
  }) %>% bindEvent(input$map_click)
  
  output$street_text <- renderText({
    street_name()
  })
  
  plot_height <- reactive({
    if_else(
      !is.null(input$innerWidth),
      input$innerWidth * 6 /12,
      0
    )
  })
  
  output$map <- renderPlot({
    req(input$btn)
    plot()
  }, height = plot_height)
}




# Run the application
shinyApp(ui = ui, server = server)
