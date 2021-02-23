library(shiny);library(tidyverse)
source("WL.R")
source("LCMethods.R")
# Define UI ----
ui <- fluidPage(
  titlePanel("LC Method Helper","topleft"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput("selectMethod", h4("Select Method"), 
                  choices = names(Methods)[!grepl("Buescher",names(Methods))],
                  selected = 1),
      
      
      fileInput(inputId = "file1", label = h4("Load Worklist"),
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")),
      
      helpText("Has to be a .csv file with columns named `Sample Name` and `Sample Position`"),
      
      actionButton('reset', 'Reset Input'),
      
      numericInput("SampleN",label = h5("Specify number of samples manually"), 
                   value = 100),
      helpText("Only when no worklist is loaded.")
    ),
    mainPanel(      
      tabsetPanel(type="tab",
                  tabPanel("Step-by-step guide",
                           includeHTML("StepByStepGuide.html")),
                  tabPanel("To prepare:", 
                           tableOutput("MethodDetails"),
                           tableOutput("Column"),
                           tableOutput("expectedRuntime"),
                           numericInput("AddVol", min = 0,
                                        h5("Additional Volume"), 
                                        value = 100)),
                  tabPanel("SolventSystem", 
                           tableOutput("SolventSystem"),
                           plotOutput("Chromatogram")),
                  tabPanel("Reagents", tableOutput("Reagents")),
                  tabPanel("Loaded worklist",dataTableOutput("WL")),
                  tabPanel("Worklist4Masshunter", 
                           fluidRow(
                             splitLayout(
                               selectInput(inputId = "AnaMethod",label = "Analysis Method",choices = NA),
                               textInput("User", "User Initials",width = "80%", value = "MM"),
                               textInput("FolderName", "Project ID",width = "80%",value = "00-0000"),
                               textInput("Standards", "Nr. of Standards",width = "80%",value = 6),
                               textInput("Brackets", "Nr. of Brackets",width = "80%",value = 20)
                             ), 
                             splitLayout(
                               checkboxInput("randomise", label = "Randomise Samples", value = FALSE),
                               checkboxInput("full.Set", label = "Full Set of Standards", value = FALSE),
                               downloadButton("downloadData", "Download"))
                           ),
                           dataTableOutput("WL4MS"))
      )
    )
  ))