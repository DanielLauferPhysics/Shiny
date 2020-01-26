#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

library(shiny)
library(shinydashboard)

# Define UI

ui <- fluidPage(shinyUI(
    dashboardPage(
    dashboardHeader(title = 'Airbnb'),
    dashboardSidebar(
        sidebarUserPanel('Daniel Laufer', 
          image = 'https://external-content.duckduckgo.com/iu/?u=http%3A%2F%2Frtuttinsights.com%2Fwp-content%2Fuploads%2F2017%2F11%2FSuperhero-5-e1519715515935.jpg&f=1&nofb=1'),
        sidebarMenu(
          menuItem("Plot", tabName = "plot", icon = icon("database")),
          menuItem("Data", tabName = "data", icon = icon("database")),
          menuItem("Map", tabName = "map", icon = icon("database"))
        )),
    
        dashboardBody(
            tabItems(
                tabItem(tabName = "plot",
                    fluidRow(
                        tabBox(
                            title = tagList(shiny::icon("gear"), 'Box and Whisker Plots'),
                            id = 'tabset1',
                            tabPanel('Tab 1', plotOutput("listing.boxplot")),
                            tabPanel('Tab 2', plotOutput('avail.boxplot')),
                            tabPanel('Tab 3', plotOutput('price.by.room.boxplot')),
                            tabPanel('Tab 4', plotOutput('night.boxplot')),
                            tabPanel('Tab 5', plotOutput('neighbourhood.count'))
                              ),
                        tabBox(
                            title = 'Density Plots',
                            id = 'tabset2',
                            tabPanel('Tab 1', plotOutput('listing.char.density')),
                            tabPanel('Tab 2', plotOutput('avail.density')),
                            tabPanel('Tab 3', plotOutput('night.density')),
                            tabPanel('Tab 4', plotOutput('price.density'))
                            )
                        )
                    ),
                
                tabItem(tabName = "data",
                    fluidRow(box(DT::dataTableOutput("table"), width = 12))
                    ),
                
                tabItem(tabName = 'map',
                    fluidRow(
                        tabBox(
                            title = 'Density Plots',
                            id = 'tabset2',
                            tabPanel('Tab 1', leafletOutput("nycclust")), 
                            tabPanel('Tab 2', leafletOutput('nyccirc')),
                            tabPanel('Tab 3', leafletOutput('nyccirclabel')),
                            width=12
                                )
                              )
                            )
                        )
                    )
                )
            )
        )

# Define server logic 

server <- shinyServer(function(input, output){
    
    #log number of listings for owners plot   
    output$listing.boxplot <- renderPlot(
        airbnb %>% 
            group_by(.,host_id, neighbourhood_group) %>% 
            summarise(., tot=log(n(), 2)) %>% 
            filter(., tot>0) %>% 
            ggplot(data=., aes(x=reorder(neighbourhood_group, tot), y=tot))+ 
            geom_boxplot(size=0.05)+
            labs(title='Log Number of Listings For Single Host', y='Log of Listing Number')+
            theme(axis.title.x = element_blank())
    )
    
    #listing char length density plot by borough
    output$listing.char.density <- renderPlot(
        airbnb %>%
            group_by(., name, neighbourhood_group) %>% 
            ggplot(data=., aes(x=name.length))+ 
            geom_density(aes(color=neighbourhood_group))+ 
            labs(title='Listing Density by Listing Length', x='Character Length')+
            coord_cartesian(xlim=c(0,110))+
            theme(axis.text.y=element_blank(),
                  axis.ticks.y = element_blank(),
                  axis.title.y = element_blank(), 
                  legend.position = c(0.75, 0.7))
    )
    
    #price density plot by borough
    output$price.density <- renderPlot(
        airbnb %>%
            group_by(., price, neighbourhood_group) %>% 
            ggplot(data=., aes(x=price))+ 
            geom_density(aes(color=neighbourhood_group))+ 
            labs(title='Listing Density by Price', x='Price')+
            coord_cartesian(xlim=c(0,1200))+
            theme(axis.text.y=element_blank(),
                  axis.ticks.y = element_blank(),
                  axis.title.y = element_blank(), 
                  legend.position = c(0.5, 0.7))
    )
    
    #price distribution by room type and borough
    output$price.by.room.boxplot <- renderPlot(
        airbnb %>% 
            group_by(., neighbourhood_group, room_type, price) %>% 
            ggplot(data = ., aes(x=room_type, y=log(price,2)))+ 
            geom_boxplot(size=0.05)+ 
            facet_wrap(~neighbourhood_group, ncol = 2)+
            theme_minimal()+ 
            theme(axis.text.x = element_text(angle = 55, vjust = 1, 
                                             size = 7, hjust = 1))
    )
    
    #listing density by availability and borough
    output$avail.density <- renderPlot(
        airbnb %>% 
            group_by(., neighbourhood_group, availability_365) %>% 
            ggplot(data = ., aes(x=availability_365))+ 
            geom_density(aes(color=neighbourhood_group))+
            labs(title='Listing Density by Availability', x='Available Days per Year')+
            theme(axis.text.y=element_blank(),
                  axis.ticks.y = element_blank(),
                  axis.title.y = element_blank(), 
                  legend.position = c(0.52, 0.82))
    )
    
    #availability boxplot 
    output$avail.boxplot<- renderPlot(
        airbnb %>% 
            group_by(., neighbourhood_group, availability_365) %>% 
            ggplot(data = ., aes(x=reorder(neighbourhood_group, availability_365), y=availability_365))+
            geom_boxplot(size=0.05)+
            labs(title='Availability of Listings by Borough', y='Number of Days Available')+
            theme(axis.title.x = element_blank())
    )
    
    #listing density by availability and borough
    output$night.density <- renderPlot(
        airbnb %>% 
            group_by(., neighbourhood_group, minimum_nights) %>% 
            ggplot(data = ., aes(x=log(minimum_nights, 2)))+ 
            geom_density(aes(color=neighbourhood_group))+
            coord_cartesian(xlim=c(0,8))+
            labs(title='Listing Density by Minimum Nights', x='Log of the Number of Days Available')+
            theme(axis.text.y=element_blank(),
                  axis.ticks.y = element_blank(),
                  axis.title.y = element_blank(),
                  legend.position = c(0.6, 0.7))
    )
    
    #minimum nights boxplot 
    output$night.boxplot<- renderPlot(
        airbnb %>% 
            group_by(., neighbourhood_group, minimum_nights) %>% 
            ggplot(data = ., aes(x=reorder(neighbourhood_group, minimum_nights), y=log(minimum_nights,2)))+
            geom_boxplot(size=0.05)+
            labs(title='Minimum Nights Offered by Borough', y='Minimum Nights Offered by Host')+
            theme(axis.title.x = element_blank())
    )
    
    #neighbourhood count boxplot 
    output$neighbourhood.count<- renderPlot(
        airbnb %>% 
            group_by(., neighbourhood_group, neighbourhood) %>% summarise(., tot=n()) %>% 
            ggplot(data = ., aes(x=reorder(neighbourhood_group, tot), y=tot))+
            geom_boxplot(size=0.05)+
            labs(title='Number of Listings in Neighbourhood', y='Neighbourhood Listing Count')+
            theme(axis.title.x = element_blank())
    )
    
    
    #cluster map of nyc
    output$nycclust <- renderLeaflet(
        leaflet(data=airbnb[7:8]) %>%
            addProviderTiles(providers$CartoDB.Positron) %>% 
            addMarkers(
                clusterOptions = markerClusterOptions()
        )
    )
    
    #circle marker map of nyc
    output$nyccirc <- renderLeaflet(
        leaflet(data=airbnb[7:8]) %>%
            addProviderTiles(providers$CartoDB.Positron) %>% 
            addCircleMarkers(radius= 0.01,
                             fillOpacity = 0.01,
                             fill = 0.01,
                             color = ~ifelse(airbnb$neighbourhood_group=='Manhattan', 'navy',
                                             ifelse(airbnb$neighbourhood_group=='Brooklyn', 'red',
                                                    ifelse(airbnb$neighbourhood_group=='Staten Island', 'green',
                                                           ifelse(airbnb$neighbourhood_group=='Bronx', 'yellow', 'purple'))))
            )
    )
    
    #circle marker neighbourhood map of nyc
    output$nyccirclabel <- renderLeaflet(
        airbnb %>% 
            group_by(., neighbourhood) %>% 
            summarise(., avg_lat=mean(latitude), avg_long=mean(longitude)) %>%
            select(., avg_lat, avg_long) %>% 
        leaflet(data=.) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addMarkers(~avg_lat, ~avg_long, label= ~htmlEscape(neighbourhood))

            )
 #   )

    #correlation heat map 
    #(fix later)
    output$cor.heatmap <- renderPlot(
        select(airbnb, colnames(airbnb)[sapply(airbnb, is.numeric)]) %>%
            cor() %>% 
            round(.,2) %>% 
            reorder_cordata(.) %>% 
            get_tri(.) %>% 
            melt(.) %>% 
            ggplot(data = ., aes(Var2, Var1, fill = value))+
            geom_tile(color = "white")+
            scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                                 midpoint = 0, limit = c(-1,1), space = "Lab", 
                                 name="Pearson\nCorrelation") +
            theme_minimal()+ 
            theme(axis.text.x = element_text(angle = 55, vjust = 1, 
                                             size = 12, hjust = 1))+
            coord_fixed()+
            geom_text(aes(Var2, Var1, label = value), color = "black", size = 2) +
            theme(
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                panel.grid.major = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank(),
                axis.ticks = element_blank(),
                legend.justification = c(1, 0),
                legend.position = c(0.6, 0.95),
                legend.direction = "horizontal")+
            guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                         title.position = "left", title.hjust = 0.5))
    )
    
    # show data using DataTable (add possible tweaks if there is time)
    output$table <- DT::renderDataTable(datatable(airbnb, 
                                        options = list(scrollX=TRUE) %>%
                                        formatStyle(input$selected, 
                                                    background="skyblue", 
                                                    fontWeight='bold')))
})

# Run the application 

shinyApp(ui = ui, server = server)
