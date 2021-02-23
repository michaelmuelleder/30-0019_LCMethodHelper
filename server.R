library(shiny);library(tidyverse)

server <- function(input, output, session) {
  
  observe({
    updateSelectInput(session, "AnaMethod",choices = Methods[[input$selectMethod]]$WL.fun[["analysis.method"]])
  })
  
  values <- reactiveValues(
    upload_state = NULL
  )
  
  observeEvent(input$file1, {
    values$upload_state <- 'uploaded'
  })
  
  observeEvent(input$reset, {
    values$upload_state <- 'reset'
  })
  
  output$WL <- renderDataTable({
    if (is.null(values$upload_state)) {
      return(NULL)
    } else if (values$upload_state == 'uploaded') {
      temp = read_csv(input$file1$datapath) 
      if(all(c("Sample Name", "Sample Position")%in%colnames(temp))){
        return(temp %>%
                 dplyr::select(`Sample Name`, `Sample Position`))}else{
                   return(data_frame("Warning: Check column headers " = ""))
                 }
    } else if (values$upload_state == 'reset') {
      return(NULL)
    }
  },options = list(pageLength = 50))
  
  tempTable = reactive({
    if (is.null(values$upload_state)) {
      return(NULL)
    } else if (values$upload_state == 'uploaded') {
      temp = read_csv(input$file1$datapath)
      if(all(c("Sample Name", "Sample Position")%in%colnames(temp))){
        temp = make.WL(filename = input$file1$datapath,
                       sample.path = ifelse(input$development == F,
                                            file.path("D:","MassHunter","Data",input$FolderName,fsep = "\\"),
                                            file.path("D:","MassHunter","Data-Development",input$User,input$FolderName,fsep = "\\")),
                       sample.prefix = ifelse(input$development == F,
                                              paste(format(Sys.time(), "%Y%m%d"),"TQ1",input$User,"runningID",input$FolderName,sep = "_"),
                                              paste(format(Sys.time(), "%Y%m%d"),"TQ1",input$User,sep = "_")),
                       method.path = Methods[[input$selectMethod]]$WL.fun[["method.path"]],
                       eq.method = Methods[[input$selectMethod]]$WL.fun[["eq.method"]],
                       eq.MRM = Methods[[input$selectMethod]]$WL.fun[["eq.MRM"]],
                       analysis.method = input$AnaMethod,#Methods[[input$selectMethod]]$WL.fun[["analysis.method"]][2],
                       wash.method = Methods[[input$selectMethod]]$WL.fun[["wash.method"]],
                       splitWash = Methods[[input$selectMethod]]$WL.fun[["splitWash"]],
                       #    final.wash.method.1 = Methods[[input$selectMethod]]$WL.fun[["final.wash.method.1"]],
                       #   final.wash.method.2 = Methods[[input$selectMethod]]$WL.fun[["final.wash.method.2"]],
                       nSTD = as.numeric(input$Standards),
                       brackets = as.numeric(input$Brackets),
                       randomise = input$randomise,
                       development = input$development,
                       full.Set = input$full.Set)
        
        return(temp)}else{
          return(data_frame("Warning: Check column headers " = ""))
        }
    } else if (values$upload_state == 'reset') {
      return(NULL)
    }
  })
  
  output$WL4MS <- renderDataTable(tempTable(), options = list(pageLength = 50))
  
  SampleNumber <- reactive({
    if (is.null(values$upload_state)) {
      return(input$SampleN)
    } else if (values$upload_state == 'uploaded') {
      temp = tempTable()
      if(all(c("Sample Name", "Sample Position")%in%colnames(temp))){
        if(is.null(Methods[[input$selectMethod]]$WL.fun[["splitWash"]])){
          return(temp %>%
                   dplyr::filter(!grepl("finalwash",`Sample Name`)) %>%
                   nrow()
          )
        }else{
          return(temp %>%
                   dplyr::filter(!grepl("finalwash",`Sample Name`)) %>%
                   nrow()/2
          )
        }
      }else{
        return(NA)
      }
    } else if (values$upload_state == 'reset') {
      return(input$SampleN)
    }
  })
  
  output$expectedRuntime = renderTable({
    exp.time = SampleNumber()*max(Methods[[input$selectMethod]]$Gradient$Time,na.rm = T)
    days = exp.time %/% (24 * 60) # days
    hours = floor((exp.time %% (24 * 60))/60) # hours
    minutes = floor(((exp.time %% (24 * 60))/60) %% 1 * 60)# mins
    exp.time = data_frame("Sample Number" = as.character(SampleNumber()),
                          "Expected Runtime (h)" = paste(days,"d",hours,"h", minutes, "min", sep = " "))
    return(exp.time)})
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(),"_",input$User, ".csv")
    },
    content = function(file) {
      write.csv(tempTable(), file, row.names = FALSE)
    }
  )
  
  output$Chromatogram <- renderPlot({
    
    p1 = Methods[[input$selectMethod]]$Gradient %>%
      tidyr::gather("Channel","Percent", Methods[[input$selectMethod]]$SolventChannels) %>%
      ggplot(aes(x = Time, y = Percent, fill = Channel)) +
      geom_area() +
      labs(x = "Time (min)", title = input$selectMethod) +
      theme_bw() 
    
    
    if(!is.null(Methods[[input$selectMethod]]$Pressure)){
      p1  +   geom_line(data = Methods[[input$selectMethod]]$Pressure, aes(x = Time, y = max(Pressure)*1.2/10),inherit.aes = F) +
        scale_y_continuous(limits = c(0,100), "Pressure (bar)", 
                           sec.axis = sec_axis(~.*max(Methods[[input$selectMethod]]$Pressure$Pressure)*1.2/100, name = derive()))
    }else{
      p1 +
        scale_y_continuous(limits = c(0,100))
    }
  })
  
  output$Reagents <- renderTable(Methods[[input$selectMethod]]$Reagents)
  output$SolventSystem <- renderTable(Methods[[input$selectMethod]]$SolventSystem)
  
  output$MethodDetails <- renderTable({
    temp = Methods[[input$selectMethod]]$Gradient %>%
      tidyr::gather("Solvent","Percent",one_of(Methods[[input$selectMethod]]$SolventChannels)) %>%
      group_by(Solvent) %>%
      dplyr::mutate(diffTime = c(0,diff(Time)),
                    Voladjust = c(rep_len(NA, 1), tail(cumsum(Percent)/100, -1) - c(0, head(cumsum(Percent)/100, -2)))/2,
                    Flowadjust = c(rep_len(NA, 1), tail(cumsum(Flow), -1) - c(0, head(cumsum(Flow), -2)))/2,
                    Voltot = Voladjust*diffTime*Flowadjust) %>%
      summarise(exact = sum(Voltot,na.rm = T)*SampleNumber()+input$AddVol)
    
    temp = bind_rows(temp,
                     data_frame(Solvent = "Needle Wash", exact = 1*SampleNumber()))
    
    Methods[[input$selectMethod]]$calcSolvents(temp) %>%
      dplyr::select(-exact)
    
  })
  
  output$Column <- renderTable(data.frame(Column = Methods[[input$selectMethod]]$Column))
  
  
}