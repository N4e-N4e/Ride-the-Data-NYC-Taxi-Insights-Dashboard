#Final Project 
library(shinydashboard) 
library(shiny)
library(sf)
library(leaflet)
library(dplyr)
library(ggplot2)
library(plotly)

boroughs = st_read("boroughs/boroughs.shp")
boroughs <- st_transform(boroughs, crs = 4326)
df <- read.csv('Taxi_data_8310.csv')

#------------------------------------------------------------------------------------------------------------------------------

days <- c("Monday", "Tuesday", "Wednesday", "Thursday","Friday", "Saturday", "Sunday")
df$day_of_week <- factor(df$day_of_week, levels = days)

tods <- c("Morning", "Afternoon", "Evening", "Night")
df$time_of_day <- factor(df$time_of_day, levels = tods)

#-----------------------------------------------------------------------------------------------------------------------------

boro_df <- df %>%
            group_by(DO_Borough) %>%
            summarise(total_trips = n(), popular_day = names(sort(table(day_of_week), decreasing = TRUE))[1])
        
boroughs_j  <-boroughs %>% left_join(boro_df, by = c("boro_name" = "DO_Borough"))

trip_bins <- c(0,1000,10000,100000,1000000,Inf)

pal <- colorBin(palette = "YlOrRd",domain = boroughs_j$total_trips, bins = trip_bins, na.color = "#f0f0f0")

#----------------------------------------------------------------------------------------------------------------------------

#UI
ui <- dashboardPage(
    
    title = "Final Project",
    
    skin = "yellow",
    
    dashboardHeader(title = span("Ride the Data: NYC Taxi Insights", style = "color:#000000;text-align:center;font-weight:bold;"), titleWidth = "100%"),
    
    dashboardSidebar(disable = TRUE),
    
    dashboardBody(
        tags$head(includeCSS("www/custom.css")),
        
        fluidRow(class = "tight-row",
                 column(7, div(class = "Vis-Container", box(title = "NYC Taxi Hotspots",  solidHeader = TRUE,width = 12, leafletOutput('map')))),
                 column(5, div(class = "Vis-Container", box(title = "Details",  solidHeader = TRUE, width = 12, HTML(
                     "<div>",
                     "<h4 style='margin-bottom:8px; text-decoration: underline;'>What is its Purpose:</h4>",
                     "</div>",
                     
                     " Lets users discover NYC taxi trends. The dashboard shows the distribution of trips across boroughs and times of day, and compares average fares throughout the week.<br><br>",
                     
                     "<div>",
                     "<h4 style='margin-bottom:8px; text-decoration: underline;'>Hints: <br></h4>",
                     "</div>",
                     
                     "• Click a borough on the map to load trip details<br>
                     • Hover over a borough to see total trips and most popular day<br>
                     • Hover over bars to see trip percentages<br>
                     • Click a time of day to compare average fares by day<br>
                     • Hover over a line to see the average fare for that day<br><hr>"), htmlOutput("info"))))
                 ),
        fluidRow(class = "tight-row",
                 column(4, div(class = "Vis-Container", box(title = textOutput("tle1"),  solidHeader = TRUE, width = 12, plotlyOutput('clicked_plot1')))),
                 column(8, div(class = "Vis-Container", box(title = textOutput("tle2"),  solidHeader = TRUE, width = 12, plotlyOutput('clicked_plot2'))))
                 )
        
    )

)

#------------------------------------------------------------------------------------------------------------------------------

#Server
server <- function(input, output) {
    
    
    clicked_borough <- reactiveVal(NULL)
    
    clicked_tod <- reactiveVal(NULL)
#------------------------------------------------------------------------------------------------------------------------------
    
    borough_data <- reactive({ 
        req(clicked_borough()) 
        df %>% filter(DO_Borough == clicked_borough()) 
    })
    
    
#------------------------------------------------------------------------------------------------------------------------------
    
       tod_summary <- reactive({
        borough_data() %>%
        count(time_of_day) %>%
        mutate(percentage = n / sum(n) * 100)
    })
    
    
#-------------------------------------------------------------------------------------------------------------------------------
    
    weekly_fare_tod <- reactive({
        req(clicked_tod())
        borough_data() %>%
        filter(time_of_day == clicked_tod()) %>%
        group_by(day_of_week) %>%
        summarise(avg_fare = mean(fare_amount, na.rm = TRUE))
 })

#------------------------------------------------------------------------------------------------------------------------------    
    
    
    weekly_fare_boro <- reactive({
        borough_data() %>%
        group_by(day_of_week) %>%
        summarise(avg_fare = mean(fare_amount))
    })

#-----------------------------------------------------------------------------------------------------------------------------
    
    borough_summary <- reactive({
        d <- borough_data()
        list(
            avg_fare = mean(d$fare_amount),
            avg_distance = mean(d$trip_distance),
            avg_duration = mean(d$trip_duration_min),
            toll_likelihood = (mean(d$has_toll == 1)*100),
            congestion_likelihood = (mean(d$is_cbd_congestion == 1)*100)
        )
    })


    
#-------------------------------------------------------------------------------------------------------------------------------
    
    output$map <- renderLeaflet({
        
        
        
        
        leaflet() %>%
        setView(lng = -73.94, lat = 40.70, zoom = 10) %>%
        addPolygons(data=boroughs_j, weight=1, smoothFactor=0.5, color = "black",  fillColor=~pal(total_trips), fillOpacity = 0.5, 
                    label = ~paste0(boro_name, "<br>Total trips: ", total_trips,"<br>Popular day: ", popular_day)%>% lapply(htmltools::HTML),
                     
                    labelOptions = labelOptions(style = list("font-weight" = "bold", padding = "3px 8px")), layerId = ~boro_name) %>%
        
        addLegend(pal = pal, values = boroughs_j$total_trips, opacity = 0.9, title = "Total Trips", position = "bottomleft")
      
  })
    
    
    
    
#-----------------------------------------------------------------------------------------------------------------------------
    
    output$clicked_plot1 <- renderPlotly({
        data <- tod_summary()
        data <- data %>%mutate(Category = ifelse(percentage == max(percentage), "Popular", "Remaining"))
        p <- ggplot(data, aes(x = percentage, y = time_of_day, fill = Category, text = paste0("Time of Day: ",time_of_day, "\nPercentage: ", round(percentage,2), "%"))) +
        geom_col() +
        scale_fill_manual(values = c("Popular" = "#F39C12", "Remaining" = "grey")) +
        ylab("Time of Day")+
        xlab("Percentage of Trips (%)") +
        scale_x_continuous(limits=c(0,50))+
        theme_minimal()
        
        ggplotly(p, tooltip = "text", source = "clicked_plot1")
})

#----------------------------------------------------------------------------------------------------------------------------
    
    
    output$clicked_plot2 <- renderPlotly({
        req(clicked_tod())
        
        p2 <- ggplot() +
        geom_line(data = weekly_fare_tod(),aes(day_of_week,avg_fare, text = paste0("Avg Fare of Selected Time\n", "Day of the Week: ", day_of_week, "\n", "Average Fare: $", round(avg_fare,2))),linetype = "solid", color = 'black', group=1)+
        geom_line(data = weekly_fare_boro(),aes(day_of_week,avg_fare, text = paste0("Avg Fare of Borough\n", "Day of the Week: ", day_of_week, "\n", "Average Fare: $", round(avg_fare,2))),linetype = "dashed", color = 'black', group = 1) +
        xlab("Day of the week")+
        ylab("Average Fare ($)")+
        theme_minimal()
        
        ggplotly(p2, tooltip = "text", source = "clicked_plot2")
    
    })
    
   
#------------------------------------------------------------------------------------------------------------------------------    
    output$info <- renderUI({
        
        if(is.null(clicked_borough())) {return (HTML("<i>Click on a borough!!!!</i>"))}
        
        data <- borough_summary()
        
        HTML(paste0(
            "<div>",
            "<h4 style='margin-bottom:8px; text-decoration: underline;'>Key Metrics:</h4>",
            "</div>",
            
            "<b>Borough:</b> ", clicked_borough(), "<br>",
            "<b>Avg fare:</b> $", round(data$avg_fare, 2), "<br>",
            "<b>Avg distance:</b> ", round(data$avg_distance, 2), " miles", "<br>",
            "<b>Avg duration:</b> ", round(data$avg_duration, 2), " minutes", "<br>",
            "<b>Likelihood of going through toll:</b> ", round(data$toll_likelihood,2), "%", "<br>",
            "<b>Likelihood of being stuck in traffic:</b> ", round(data$congestion_likelihood,2), "%", "<br>"
            
        
  ))
        
    })
    
    
    
#------------------------------------------------------------------------------------------------------------------------------
    
    observeEvent(input$map_shape_click, {
        
        clicked_borough(input$map_shape_click$id)
            
            })
    
#-----------------------------------------------------------------------------------------------------------------------------
    
     observeEvent(event_data("plotly_click", source = "clicked_plot1"), {
         
         temp <- event_data("plotly_click", source = "clicked_plot1")
         click <- round(temp$y)
         levels <- tod_summary()$time_of_day
         clicked_tod(levels[click])
     })  
    
#------------------------------------------------------------------------------------------------------------------------------
    output$tle1 <- renderText({ 
        if (is.null(clicked_borough())) {"Click on a borough to see trip distribution!!!"} 
        else {paste("Most Popular Time of Visit in", clicked_borough())}
    })


    output$tle2 <- renderText({ 
        if (is.null(clicked_tod())) {"Click on a time of day to compare fares!!!"} 
        else {paste("Average Fare Comparison by Day in ", clicked_borough(), " (", clicked_tod(), ")" )}
    })
    
    

#------------------------------------------------------------------------------------------------------------------------------
    
    

}

shinyApp(ui, server)