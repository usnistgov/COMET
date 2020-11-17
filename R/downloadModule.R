downloadUI <- function(id) {
  ns <- NS(id)
  downloadButton(ns('downloadData'), 'Download Results')

}

downloadServer <- function(id, input_file, Metrics) {
  moduleServer(
    id,
    
    function(input,output,session) {
      output$downloadData <- downloadHandler(
        filename = function() {
          file.name<-gsub(".csv","",input_file()$name)%>%
            gsub(".txt","",.)
          paste0(file.name, "_Analysis_Results.csv")
        },
        
        content = function(con) {
          cat(paste("User specified parameters: \n","File name:",input_file()$name,"\n",
                    "var_func<-function(mn)",body(Metrics()$var_func),"\n",
                    "Flexible model was polynomial of order",Metrics()$smooth_df,"\n",
                    "Number of bootstrap iterations conducted:",Metrics()$n_boot,"\n",
                    "Confidence Level:",Metrics()$conf_lev,"\n",
                    "\n","Factor Levels \n"),
              file=con)
          
          dat<-Metrics()$dat
          cat(paste0(colnames(dat)[1:12],": ",
                     apply(dat[,1:12],2,function(x)paste(unique(x),collapse="; ")),"\n"),
              file=con,append=TRUE)
          cat("\n Metrics \n",file=con,append=TRUE)
          write.table(select(Metrics()$metrics,-comp_factor), file=con, append=TRUE,row.names=FALSE,sep=",")
          if(Metrics()$n_boot>20){
            cat("\n Comparing Metrics\n",file=con,append=TRUE)
            write.table(Metrics()$compare, file=con, append=TRUE,row.names=FALSE,sep=",")
          } 
        },
        contentType = "text/csv"
      )
    }
  )
}

