templateUI <- function(id) {
  ns <- NS(id)
  tagList(
    
    selectInput(ns("which_template_file"),label="Download Instructions or Template Files (optional)",
                choices=list("Readme File"='rm',
                             "Full Template (empty)"='ft',
                             "Shiny App GUI Instructions and Template"='inst',
                             "Full Template Example 1"='fte1',
                             "Full Template Example 2"='fte2',
                             "Full Template Example 3"='fte3',
                             "Simple Template (empty)"='st',
                             "Simple Template Example"='ste') ),
    downloadButton(ns('the_file'), 'Download Selected File'),
    br(),
    br(),
    br()
    
  )
}

templateServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$the_file <- downloadHandler(
        filename = function() {
          switch(input$which_template_file,
                 'rm'="COMET_readme.xlsx",
                 'ft'="Full_Data_Template_COMET.csv",
                 'inst'="Shiny App GUI Instructions and Data Template.xlsx",
                 'fte1'="Full_Template_Example_1_COMET.csv",
                 'fte2'="Full_Template_Example_2_COMET.csv",
                 'fte3'="Full_Template_Example_3_COMET.csv",
                 'st'="Simple_Data_Template_COMET.xlsx",
                 'ste'="Simple Template Example 1 COMET.xlsx")
        },
        content = function(file) {
          
          fname = switch(input$which_template_file,
                         'rm'="other/COMET READ ME FILE v 20210622_LP.xlsx",
                         'ft'="other/Full_Data_Template_COMET.csv",
                         'inst'="other/Shiny App GUI Instructions and Data Template_v_061621.xlsx",
                         'fte1'="other/Full_Template_Example_1_COMET.csv",
                         'fte2'="other/Full_Template_Example_2_COMET.csv",
                         'fte3'="other/Full_Template_Example_3_COMET.csv",
                         'st'="other/Simple_Data_Template_COMET.xlsx",
                         'ste'="other/Simple Template Example 1 COMET.xlsx")
          
          file.copy(fname, file)
        }
      )
      
    }
  )
}
