templateUI <- function(id) {
  ns <- NS(id)
  tagList(
    
    h4("Template Files"),
    downloadButton(ns('file1'), 'GUI Instructions'),
    downloadButton(ns('file2'), 'Simple Template Example'),
    downloadButton(ns('file3'), 'Full Template Example'),
    downloadButton(ns('file4'), 'Simple Template'),
    downloadButton(ns('file5'), 'Full Template')
    
  )
}

templateServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      f1 = 
      
      output$file1 <- downloadHandler(
        filename = function() {
          paste('Shiny App GUI Instructions and Data Template_v_061621.xlsx')
        },
        content = function(file) {
          file.copy('other/Shiny App GUI Instructions and Data Template_v_061621.xlsx', file)
        }
      )
      
    }
  )
}
