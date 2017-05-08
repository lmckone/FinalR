#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

require(shiny)
require(magrittr)
require(dplyr)
require(dplyr)
require(data.table)
require(ggplot2)
require(dplyr)
require(sqldf)
require(ggplot2)
require(maptools)
require(rgeos)
require(ggmap)
require(scales)
require(RColorBrewer)
require(plotly)


undetermined <- read.csv("undetermined.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Number of Undetermined Fire Causes, 2006-2015"),
   p("The top plot allows you to compare the number of fires with undetermined causes in up to three states.
     The bottom map renders a density map of the number of fires with undetermined causes by state for the year that you select."),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("state1",
                     "State 1",
                     choices = undetermined$id, selected = "AK"),
         selectInput("state2",
                     "State 2",
                     choices = undetermined$id,
                     selected = "TX"),
         selectInput("state3",
                     "State 3",
                     choices = undetermined$id,
                     selected = "CA")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotlyOutput("statePlot")
      )
   ),
   sidebarLayout(
     sidebarPanel(
       selectInput("year",
                   "Year",
                   choices = undetermined$YEAR, selected = "2009")
     ),
     
     #p("This map shows the count of fires with undetermined causes in a given year. This count has not been normalized by population, but that
       #would be an interesting next step for the analysis. If you take a look at the legend to the right of the graph, you'll notice that the
       #range varies significantly from year to year. This map can be used to visualize anomalies or areas of interest in the data."),
     # Show a plot of the generated distribution
     mainPanel(
       plotlyOutput("mapPlot")
     )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$statePlot <- renderPlotly({
      states <- c(input$state1, input$state2, input$state3)
      
      p <- ggplot(filter(undetermined, id %in% states), aes(YEAR, NUM_U, group=id, color=id)) + geom_line(size=1) + scale_color_manual(values=c("cadetblue2", "coral", "black")) + labs(x="year", y="number undetermined")
      
      ggplotly(p)
   })
    output$mapPlot <- renderPlotly({
      #read a shapefile for the US states
      states.shp <- readShapeSpatial("cb_2016_us_state_5m.shp")
      
      #change levels so they match those of the fire data
      levels(states.shp$NAME) <- c("AL", "AK", "AS", "AZ", "AR", "CA", "CO", "MP", "CT", "DE", "DC", "FL", "GA", "GU", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD","MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN", "TX", "VI", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
      #states.shp.f %<>%filter(id != "MP" || NAME != "GU" || NAME != "PR" || NAME!="VI" || NAME!="AS")
      
      #fortify the shapefile into a dataframe
      states.shp.f <- fortify(states.shp, region = "NAME")
      
      #change names of our master table so that we can merge it with the coordinates
      
      #merge our master table with the coordinates... if we filter by different years, we'll get different maps!
      merge.shp.coef<-merge(states.shp.f, filter(undetermined, YEAR==input$year), by='id', all.x=TRUE)
      final.plot<-merge.shp.coef[order(merge.shp.coef$order), ] 
      
      #Make our map! with the fill corresponding to number of unidentified non-residential fire causes
      #in that year
      m <- ggplot() +
        geom_polygon(data = final.plot, 
                     aes(x = long, y = lat, group = group, fill = NUM_U), 
                     color = "black", size = 0.10) + 
        coord_map(xlim=c(-124.848974, -65), ylim=c(20, 49.384358)) + scale_fill_gradient(low = "white", high = "coral", space = "Lab", na.value = "grey50", guide = "colourbar")
      ggplotly(m)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

