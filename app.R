library(shiny)
library(tidyverse)
library(plotly)
library(bslib)

my_artists = arrow::read_feather('my_artists.arrow')
my_artist_count_data = arrow::read_feather('my_artist_count_data.arrow') 
artists = arrow::read_feather('artist_id.arrow')

ui <- fluidPage(
  shinyWidgets::setSliderColor(paste0('#',probe::palette_feather['Ocean']), 1),
  theme = bs_theme(
    bg = paste0('#',probe::palette_feather['Powder']),
    fg = '#000000',
    base_font = c("Helvetica", "Arial", "sans-serif") # matches corcept.com
  ),
  titlePanel(
    "Which artists share the most playlists with a given artist?"
  ),
  fluidRow(
    column(4,selectInput(
      "artist", "Select an artist", setNames(my_artists$artist_id,my_artists$artist_name))),
    column(4,numericInput("n", "Number of artists to display", value=5, min=1, max=50)),
    column(4,sliderInput("popularity_index", "Popularity index", value = 1, min = 0, max = 1))
  ),
  fluidRow(
    plotlyOutput("plot")
  )
)

server <- function(input, output, session) {
  selected = reactive(
    my_artist_count_data %>% 
      filter(artist_id_1 == input$artist)
  )
  
  output_data = reactive({
      selected() %>% 
        mutate(
         scaled_output = 
           (playlist_count_2_given_1/max_playist_count_2_given_1)*(input$popularity_index) -
           (playlist_count_2/max_playist_count_2)*(1-input$popularity_index),
         hover_text = str_c("Shared playlists: ",scales::comma(playlist_count_2_given_1))
        ) %>% 
        arrange(desc(scaled_output)) %>% 
        head(input$n) %>% 
        mutate(
          output = pmax(
            (scaled_output-min(scaled_output))/(max(scaled_output)-min(scaled_output)),
            0.01
          )*100
        ) %>% 
        left_join(artists, by = c("artist_id_2"="artist_id")) %>% 
        mutate(
          # github_install('colej1390/probe')
          artist_name = probe::add_sorting(artist_name,method='order')
        )
  })
  
  output$plot = renderPlotly({output_data() %>% 
    plotly::plot_ly(
        x = ~artist_name,
        y = ~output,
        hovertext = ~hover_text,
        hoverinfo = 'text',
        marker = list(color = paste0('#',probe::palette_feather['Ocean'])),
        type='bar'
      ) %>%
      plotly::layout(
        plot_bgcolor=paste0('#',probe::palette_feather['Powder']),
        xaxis = list(title=''),
        yaxis = list(title='Scaled match index', ticksuffix = '%')
      ) %>% layout(
        font = list(
          family = "Helvetica, Arial, sans-serif"
        )
      )
  })
}

shinyApp(ui, server)
