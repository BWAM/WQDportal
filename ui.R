library(shiny)
library(shinythemes)
library(leaflet)
library(DT)

#####This script will create the Sidebar page and tabset main panel layout with controls to select a dataset and specify
##### the number of observations to view

#Call the fluid page format for the page
shinyUI(fluidPage(
  theme = shinytheme("cerulean"),
#Application Title
  titlePanel("Mohawk River Watershed Water Quality Data Portal"),

#Call the sidebar layout for the page
  sidebarLayout(position = "left", 
      sidebarPanel(selectInput("dataset", "Choose a dataset:",
        choices = c("Water Chemistry", "Biological Assessment", "Toxicity", "Bacteria", "Sites"))),
      mainPanel(position = "right",
        tabsetPanel(
          tabPanel("Data View", DT::dataTableOutput('data_table')),
          tabPanel("Map View", leafletOutput("mymap", height = 800, width = 1500))
                ))
  )
)
)



