library(shiny)
library(DT)
library(data.table)

function(input, output) {
  # Create the DataTable.
  dt.react <- reactive({
    
    final.dt <- DT::datatable(internal.df,
                              options = list(
                                #scrollY = 700,
                                #pageLength = 25,
                                color = "black",
                                columnDefs = list(list(className = 'dt-center'))))
    
    return(final.dt)
    
  # Create the Map of Data  
    source("map.R", local = TRUE)
  })
  #---------------------------------------------------------------------------- 
  # Render a table representing the selected Site and Parameter.
  output$data_table <- DT::renderDataTable(dt.react()) # End output$param_table
  #---------------------------------------------------------------------------- 
  
}