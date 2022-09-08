library(shiny)
library(tidyverse)
library(plotly)

my_artists = arrow::read_feather('my_artists.arrow')
my_artist_counts = arrow::read_feather('my_artist_counts.arrow') %>% 
  arrange(desc(playlist_count))
artists = arrow::read_feather('artist_id.arrow')

ui <- fluidPage(
  titlePanel("Which artists share the most playlists with a given artist?"),
  fluidRow(
    column(6,selectInput("artist", "Select an artist", setNames(my_artists$artist_id,my_artists$artist_name))),
    column(6,numericInput("n", "Number of artists to display", value=5, min=1, max=50))
  ),
  fluidRow(
    plotlyOutput("plot")
  )
)

server <- function(input, output, session) {
  selected = reactive(
    my_artist_counts %>% 
      filter(artist_id_1 == input$artist) %>% 
      arrange(desc(playlist_count)) %>% 
      head(input$n) %>% 
      left_join(artists, by = c("artist_id_2"="artist_id")) %>% 
      mutate(
        # github_install('colej1390/probe')
        artist_name = probe::add_sorting(artist_name,method='order')
      )
  )
  
  output$plot = renderPlotly({selected() %>% 
    plotly::plot_ly(
        x = ~artist_name,
        y = ~playlist_count
      ) %>%
      plotly::layout(
        xaxis = list(title=''),
        yaxis = list(title='')
      )
  })
}

shinyApp(ui, server)