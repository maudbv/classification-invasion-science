modFunction <- function(input, output, session, data,reset) {
  
  v <- reactiveValues(data = data)
  
  proxy = dataTableProxy("mod_table")
  
  observeEvent(input$mod_table_cell_edit, {
    print(names(v$data))
    info = input$mod_table_cell_edit
    str(info)
    i = info$row
    j = info$col
    k = info$value
    str(info)
    
    isolate(
      if (j %in% match(c("ratio","cost","updated_price"), names(v$data))) {
        print(match(c("ratio","cost", "updated_price"), names(v$data)))
        v$data[i, j] <<- DT::coerceValue(k, v$data[i, j])
        print(v$data)
        
        if (j %in% match("cost", names(v$data))) {
          v$data$updated_price <<- v$data$cost * v$data$ratio
        }
        if (j %in% match("ratio", names(v$data))) {
          v$data$updated_price <<- v$data$cost * v$data$ratio
        }
      } else {
        stop("You cannot change this column.") # check to stop the user from editing only few columns
      }
    )
    replaceData(proxy, v$data, resetPaging = FALSE)  # replaces data displayed by the updated table
  })
  ### Reset Table
  observeEvent(reset(), {
    v$data <- data # your default data
  })
  
  print(isolate(colnames(v$data)))
  output$mod_table <- DT::renderDataTable({
    DT::datatable(v$data, editable = TRUE)
    
  })
}