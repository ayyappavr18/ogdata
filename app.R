library(shinythemes)
library(ggplot2)
library(ggmosaic)
library(productplots)
library(plotly)
library(summarytools)
library(MASS) 
library(car)
library(expss)
library(labelled)
# function
# missing value recode Na's
replace.empty <- function(a) {
  a <- as.character(a)
  a[a== ""]<- NA
  a<-as.factor(a)
  return(a)
}
remove_outliers<-function(x,na.rm=T){
  qnt<-quantile(x,probs=c(0.25,0.75),na.rm=na.rm)
  h<-1.5*(IQR(x,na.rm=na.rm))
  
  x[x<(qnt[1]-h)]<-NA
  x[x>(qnt[2]+h)]<-NA
  x
}
# libraries
library(vcd)
library(skimr)
library(DT)
library("dplyr")
library("ggplot2")
library("tidyr")
library("gridExtra")
library(naniar)
library(shiny)
library(epiDisplay)
library(epiR)
library(epitools)
options(digits=5)
library(DescTools)
library(Hmisc)
library(pastecs)
library(plyr)
library(shinyjs)
library(V8)
#---refresh text input------------
jscode <- "shinyjs.refresh = function() { history.go(0); }"
jsCode <- 'shinyjs.winprint = function(){
window.print();
}'
library(knitr)
options(knitr.table.format = function(){ if (knitr::is_latex_output()) "latex" else "pandoc"})
library(summarytools)
library(dplyr) 
library(ggplot2)
library(ggpubr)
library(reshape2)
theme_set(theme_pubclean())
library(shinythemes)
#------
ui = navbarPage(theme = shinytheme("flatly"),"ANTENATAL",
                tabPanel("Data", 
                         sidebarPanel(width = 3,
                                      
                                      fileInput("file", "Choose RDS File",
                                                accept = c("rds",".csv","text/csv","text/comma-separated-values,text/plain")),
                                      fileInput("file1", "Choose CSV File",
                                                accept = c("rds",".csv","text/csv","text/comma-separated-values,text/plain",".xlsx")),
                                     
                                      actionButton('Demo', 'Dataset(N)')
                                      #downloadButton("download", "Download")
                                      
                         ),
                         useShinyjs(),
                         extendShinyjs(text = jsCode),
                         mainPanel(
                           DT::dataTableOutput("table") 
                           
                         )
                ),
                tabPanel("Data cleaning",
                         # sidebarPanel(width = 2,
                         #   actionButton("print", "PRINT")
                         # 
                         # ),
                         mainPanel(width=12,
                                   tabsetPanel(tabPanel( h5("VARIABLE RENAME"),div(style="display:inline-block",uiOutput("colname_in2"),width=3),uiOutput("colname_in3"),div(style="width:500px;",verbatimTextOutput("variablenamechange")),actionButton("RenameColumn","Rename"),tags$hr(style="border-color: black;")),
                                               tabPanel( h5("RECLASSIFYING VARIABLE TYPE"),div(style="display:inline-block",uiOutput("colname_in"),width=3),div(style="display:inline-block",uiOutput("colname_in1"),width=3),verbatimTextOutput("classvari"),actionButton("change_class","Change"),verbatimTextOutput("reclassifyvariable"),tags$hr(style="border-color: black;")),
                                              # tabPanel( h5("RENAME FACTOR LEVELS"),div(style="display:inline-block",uiOutput("colname_in4"),width=3),uiOutput("renamedif"),actionButton("renamecol","OK"),uiOutput("colname_in5"),uiOutput("colname_in6"),actionButton("RenamelevelsColumn","Rename levels"),tags$hr(style="border-color: black;")),    
                                               tabPanel( h5("RECODE CONTINUOUS VARIABLE"),
                                                         fluidRow(
                                                           column(width = 8,
                                                                  div(style="display:inline-block",uiOutput('recodevariable'),width=1),div(style="display:inline-block",uiOutput("renewvari"),width=1),div(style="display:inline-block",actionButton("addvari","Add Variable"),width=1),
                                                         uiOutput("lowselect"),div(style="display:inline-block",uiOutput('lower'),width=1),div(style="display:inline-block",uiOutput("lowervalue"),width=1),div(style="display:inline-block",actionButton("lowerok","OK"),width=1),
                                                         uiOutput("lowselect1"),div(style="display:inline-block",uiOutput('between1'),width=1),div(style="display:inline-block",uiOutput('between2'),width=1),div(style="display:inline-block",uiOutput('bewteenvalue'),width=1),div(style="display:inline-block",actionButton("betweenok", "OK"),width=1),
                                                         verbatimTextOutput('recodesummary1'),div(style="display:inline-block",uiOutput('higher'),width=1),div(style="display:inline-block",uiOutput("highervalue"),width=1),div(style="display:inline-block",actionButton("higherok","OK"),width=1),verbatimTextOutput('recodesummary')),
                                                         # column(width = 4,
                                                         #        h5("CREATE LABEL"),verbatimTextOutput('recodesuary1'),div(style="display:inline-block",uiOutput("colname_in4"),width=3),div(style="display:inline-block",uiOutput("colname_in5"),width=3),div(style="display:inline-block",uiOutput("colname_in6"),width=3),div(style="display:inline-block",actionButton("RenamelevelsColumn","Label"),width=3),verbatimTextOutput("labelsummary")),
                                                         )
                                                         ),
                                               tabPanel( h5("RECODE CATEGORICAL VARIABLE"),div(style="display:inline-block",uiOutput("recodeui1"),width=3),uiOutput("recodeui01"),div(style="display:inline-block",actionButton("cateadd","Add Variable"),width=3),uiOutput("recodeui001"),h5("Categorical variable : 'PGE1'='PGE'; 'PGE2'='PGE'"),actionButton("recodeok01","Recode"),verbatimTextOutput('cutsummary'),verbatimTextOutput('cutsummary1')), 
                                              tabPanel( h5("LABEL"),
                                                        fluidRow(
                                                          column(width = 5,
                                                                 div(style="display:inline-block",h4("VARIABLE LABEL"),uiOutput("labelselectui"),width=3),uiOutput("labeltextui"),div(style="display:inline-block",actionButton("labeladding","Add label"),width=3),div(style="width:500px;",verbatimTextOutput('labelsummary1'))),
                                                          column(width = 7,
                                                                 h4("FACTOR LEVELS LABEL"),div(style="display:inline-block",uiOutput("levelslableselectui"),width=3),
                                                                 div(style="display:inline-block",uiOutput("colname_in5"),width=3),uiOutput("colname_in6"),div(style="display:inline-block",actionButton("RenamelevelsColumn","Label"),width=3),verbatimTextOutput("labelsummary2"))
                                                        )), 
                                               tabPanel(h5("OUTLIERS"),uiOutput('select5'),h4("Total number of outliers"),verbatimTextOutput('summary5'),h4("List of outliers"),verbatimTextOutput("outlierslist"),h5("Tukey's Rule method to remove the outliers"),actionButton("remove", "Remove!"),plotlyOutput("plotlier"),tags$hr(style="border-color: black;")),tabPanel( h5("MISSING VALUE"),tabsetPanel(tabPanel(h6("Variable"),uiOutput("miss1"),h4("Total number of missing values and percentage"),verbatimTextOutput("miss3"),h4("Missing values plot"),plotlyOutput("missplot1"))
                                                                                                                                                                                                                                                                                                                                                                                 # tabPanel(h6("Risk factor variable"),uiOutput("miss2"),h4("Total number of missing values and percentage"),verbatimTextOutput("miss4"),h4("Missing values plot"),plotlyOutput("missplot2",height=600))
                                                                                                                                                                                                                                                                                                                                                                                  ))
                                   )
                         )
                
                ),
                tabPanel("Univariate",
                         # # sidebarPanel(width = 2,
                         # #   
                         # #   actionButton("print", "PRINT")
                         #         
                         # ),
                         mainPanel(width=12,
                                   verbatimTextOutput('summary3'),downloadButton('report',label="Download Report")
                                   
                         )
                ),
                tabPanel("Bivariate",
                         sidebarPanel(width = 3,
                                      
                                      uiOutput('select'),
                                      textOutput("classtype1"),
                                      uiOutput('select1'),
                                      textOutput("classtype2")
                                      
                         ),
                         mainPanel(width = 9,
                           verbatimTextOutput("summary"),verbatimTextOutput('summary1'), verbatimTextOutput('summary2'),
                                   # fluidRow(
                                   #   column(
                                   #          #h5("Outcome variable"),
                                   #          verbatimTextOutput("sum11")),
                                   #   column(
                                   #          #h5("Independent variable"),
                                   #          verbatimTextOutput('summary2')),
                                   # ),
                                  plotlyOutput("plot4"),tags$hr(style="border-color: black;"),plotlyOutput("plot5"),tags$hr(style="border-color: black;"),plotlyOutput("plot6")
                                   
                         )
                ),
                tabPanel("Stratified",
                         mainPanel(width=11,
                                   tabsetPanel(tabPanel("Stratified",
                                                        sidebarPanel(width = 3,
                                                                     
                                                                     uiOutput('select11'),
                                                                     textOutput("classtype3"),
                                                                     uiOutput('select12'),
                                                                     textOutput("classtype4"),
                                                                     uiOutput('select13'),
                                                                     textOutput("classtype5")
                                                                     
                                                        ),
                                                        mainPanel(width=9,
                                                                  verbatimTextOutput("summary4"),verbatimTextOutput("summary7"),plotlyOutput("plot3")
                                                        )
                                   ),
                                   tabPanel("Trend chart",
                                            sidebarPanel(width = 2,
                                                         
                                                         uiOutput('select41'),
                                                         uiOutput('select42'),
                                                         uiOutput('select43')
                                                         
                                            ),
                                            mainPanel(width=10,
                                                      plotlyOutput("trend"),verbatimTextOutput("mean")
                                            )
                                   )
                                   )
                         )
                ),
                # tabPanel("Risk Factor",
                #          sidebarPanel(width = 3,
                #                       uiOutput('select31'),
                #                       uiOutput('select32'),
                #                       uiOutput('select33')
                # 
                #          ),
                #          mainPanel(
                #            verbatimTextOutput("risk1"),
                #            verbatimTextOutput("risk2"),
                #            verbatimTextOutput("risk3"),
                #            verbatimTextOutput("risk4"),
                #            verbatimTextOutput("risk5"),
                #            #verbatimTextOutput("risk6")
                #            plotOutput("riskplot41")
                #          )
                # ),
                tabPanel("Indication",
                         sidebarPanel(width = 3,
                                      uiOutput('select51'),
                                      uiOutput('select52'),
                                      uiOutput('select53')

                         ),
                         mainPanel(width=8,

                                   plotlyOutput("indiplot1"),tags$hr(style="border-color: black;"),
                                   plotlyOutput("indiplot2"),tags$hr(style="border-color: black;"),
                                   plotlyOutput("indiplot3"),tags$hr(style="border-color: black;"),
                                   textOutput("inditext1"), plotlyOutput("indiplot4"),verbatimTextOutput("indi1"),textOutput("inditext7"),plotlyOutput("indiplot7"),tags$hr(style="border-color: black;"),
                                   textOutput("inditext8"),plotlyOutput("indiplot8"),textOutput("inditext5"),plotlyOutput("indiplot5"),tags$hr(style="border-color: black;"),
                                   textOutput("inditext9"),plotlyOutput("indiplot9"),textOutput("inditext6"),plotlyOutput("indiplot6")
                         )
                ),
                tabPanel("Associate variable",
                         mainPanel(width=11,
                                   tabsetPanel(tabPanel("FLOW CHART",
                                                        mainPanel(plotOutput("flowchart"))),
                                               
                                               tabPanel("ASSOCIATE VARIABLE",
                                                        sidebarPanel(width = 3, uiOutput('select71'), uiOutput('select72'),uiOutput('select73')
                                                        ),
                                                        mainPanel(width=9,
                                                                  verbatimTextOutput("sss"))
                                               )
                                   ) 
                         )
                ),
                
                tabPanel("Multivariate",
                         sidebarPanel(width = 3,
                                      uiOutput('select21'),
                                      uiOutput('select22'),
                                      uiOutput('select23')
                                      
                         ),
                         mainPanel(
                           verbatimTextOutput("s")
                         )
                ),
                tabPanel("More",
                         
                         mainPanel(width = 12,
                                   tabsetPanel(tabPanel(h6("LABELS & DEFINITIONS"),verbatimTextOutput("labels2")),
                                               tabPanel(h6("REFERENCES"),verbatimTextOutput("labels3"))
                                               
                                               
                                               
                                   )
                                   
                         )
                ),
                #actionButton("save"),actionButton("download1"),radioButtons("type","Choose file type",choices = c('csv','xls'))
                tabPanel("Download",
                         mainPanel(width = 12,
                                   verbatimTextOutput("saved"), downloadButton("download", "Download-CSV file"),
                                   downloadButton("download1", "Download-RDS file")
                                               
                         )
                )
                
)

server = function(input, output,session) {
 
#-----import data set --------------
  v <- reactiveValues()  # store values to be changed by observers
  v$data <- data.frame()
  
  
# Observer for uploaded file
    observe({
      inFile = input$file
      # if (is.null(inFile)) return(NULL)
      # values$data <- read.csv(inFile$datapath)
      if(is.null(inFile)){
        v$data <-readRDS("anten.samp.death.19.5.rds")
      }  
      else {
        v$data <-readRDS(inFile$datapath)
      }
      
    })
  
    observe({
      inFile2 = input$file1
      # if (is.null(inFile)) return(NULL)
      # values$data <- read.csv(inFile$datapath)
       
        if(is.null(inFile2)){
          return(NULL)
        }  
      else   {
        v$data <-read.csv(inFile2$datapath)
      }
      
    })
    ## Observer for demo data button
     observe({
       if (input$Demo > 0)  # otherwise demo data shows on startup
         v$data <- readRDS("anten.18.05.dealth.rds")
     })
  #---refreshing text input-------------   
     # observeEvent(input$file, {
     #   js$refresh();
     # })
     # observeEvent(input$file1, {
     #   js$refresh();
     # })
     
  #--table output views-------
  output$table <- DT::renderDataTable(
    DT::datatable(v$data, options = list(pageLength = 25))
  )
  #------SAVE RDS file-------------
      observeEvent(input$save, {
        write.csv(v$data, file="data.csv", row.names = TRUE)
           #saveRDS(v$data,file = "v$data.rds")
         })
  #------delete variable -------   
     # output<-renderUI({
     #   selectInput(inputId = "deletevariable","Remove variable",
     #               choices = c(colnames(v$data)),
     #               multiple = T,
     #               selected = "")
     # })
     # 
     # observeEvent(input$delete, {
     #   v$data[,input$deletevariable]<-NULL
     # })
     # 
  # Downloadable csv of selected dataset ----
   output$download <- downloadHandler(
     filename = function() {
       paste("dataset", ".csv", sep = "")
     },
     content = function(file) {
       write.csv(v$data, file, row.names = FALSE)
     }
   )
       
   output$download1 <-downloadHandler(
     filename = function() {
       paste("dataset", ".rds", sep = "")
     },
     content = function(file) {
       saveRDS(v$data, file)
     }
   )
  # select variable 1st column name-------
     output$reclassifyvariable<-renderPrint({
       str(v$data)
     })     

  # select variable 1st column name-------
     output$colname_in <- renderUI({
       
       selectInput(inputId = "colname",
                   label = "Choose variable",
                   multiple = T,
                   choices = c(colnames(v$data)))
       
     })
     
     #----select variable class changes------
     output$colname_in1 <- renderUI({
       selectInput("class", "Choose variable type",
                   choices = c(" ", "factor", "numeric", "integer", "character","Date"),
                   selected = " ")
     })
     
     observeEvent(input$change_class, {

       v$data <- eval(parse(text = paste0('v$data %>% mutate(',
                                          input$colname,
                                          ' = as.',
                                          input$class,
                                          '(',
                                          input$colname,
                                          '))')
       )
       )
       
       # req(v$data[,input$colname],input$class)
       # if(input$class=="factor"){
       #   v$data[,input$colname]<-as.factor( v$data[,input$colname])
       # } else if (input$class=="integer") {
       #   v$data[,input$colname]<-as.integer( v$data[,input$colname]) 
       # } else if (input$class=="character") {
       #   v$data[,input$colname]<-as.character( v$data[,input$colname]) 
       # } else if (input$class=="numeric") {
       #   v$data[,input$colname]<-as.numeric( v$data[,input$colname]) 
       # } else if (input$class=="Date") {
       #   v$data[,input$colname]<-as.Date( v$data[,input$colname],format = "%d-%m-%y") 
       # }
       #  else if (input$class=="Date1") {
       #   v$data[,input$colname]<-as.numeric( v$data[,input$colname],format = "%y-%m-%d") 
       # }
     })
  #---rename select ui------
  output$colname_in2<-renderUI({
    selectInput(inputId = "OldColumnName", 
                label = "Select Variable",
                multiple = T, 
                choices = c(colnames(v$data)), 
                selected = "")
  })
  #---select---2--
  output$colname_in3<-renderUI({
    textInput(inputId = "NewColumnName", 
              label = "Enter New Variable Name","")
  })
  observeEvent(input$RenameColumn, {
    req(input$NewColumnName, input$OldColumnName)
    if (input$NewColumnName != "Nil") {
      colnames(v$data)[colnames(v$data) == input$OldColumnName] <- 
        input$NewColumnName
    }
  })
  
  observeEvent(input$RenameColumn, {
     output$variablenamechange<-renderPrint({
          print(paste("changed"))
        })
  
  })
  
  #---select ui for rename factor levels-------
  #---select----- 
  # output$renamedif<-renderUI({
  #   textInput(inputId = "renamedifcolumn", 
  #             label = "Enter new variable name", "BOOKED")
  # })
  # 
  #   observeEvent(input$renamecol, {
  #     v$data[,input$renamedifcolumn]<-v$data[,input$OldColumnName1]
  #     
  #   })  
  #---select---1---  
  output$levelslableselectui<- renderUI({
   
    selectInput(inputId = "OldlColumnName1", 
                label = "Choose variable ",
                multiple = T, 
                choices = c(colnames(v$data)),
                selected = "")
  })
     
  #---select---2---  
  output$colname_in5<- renderUI({
    dff1 <- v$data[,input$OldlColumnName1]
    dff1<- levels(dff1)
    selectInput(inputId = "OldlevelsColumnName1", 
                label = "Select factor levels",
                multiple = T, 
                choices = dff1,
                selected = "")
  })
  #---select---3--
  output$colname_in6<-renderUI({
    textInput(inputId = "NewColumnName1", 
              label = "Create label", "")
  })
  
  observeEvent(input$RenamelevelsColumn, {
    
    req(input$NewColumnName1, input$OldlColumnName1,input$OldlevelsColumnName1)
    if (input$NewColumnName1 != "Nil") {
      levels(v$data[,input$OldlColumnName1])[levels(v$data[,input$OldlColumnName1]) == input$OldlevelsColumnName1] <- 
        input$NewColumnName1
    }
  })
  
  
  #select variable
  output$select <- renderUI({
    df<-colnames(v$data)
    selectInput("variable1", "Outcome variable(df):",df,selected = "BABY.WEIGHT1")
  })
  
  output$classtype1<-renderText({

    dfg<-v$data[,input$variable1]
    nlevels1<-length(levels(dfg))
    if(is.factor(dfg)){
     if( nlevels1==2){
       print("Type of outcome variable: Binary")
     }
      else if (nlevels1 >=3){
        print("Type of outcome variable: Multinomial")
      }
    }
    else if (is.numeric(dfg) | is.integer(dfg)){
      print("Type of outcome variable: Continuous")
    }
  })
 
  output$classtype2<-renderText({

    dfg<-v$data[,input$variable]
    nlevels1<-length(levels(dfg))
    if(is.factor(dfg)){
      if( nlevels1==2){
        print("Type of independent variable: Binary")
      }
      else if (nlevels1 >=3){
        print("Type of independent variable: Multinomial")
      }
    }
    else if (is.numeric(dfg) | is.integer(dfg)){
      print("Type of independent variable: Continuous")
    }
  })
#--labeling variable-----------------
 output$labelselectui<-renderUI({
    selectInput(inputId = "labelselectui1",
                label = "Choose variable",
                multiple = T,
                choices = c(colnames(v$data)))
  }) 
  output$labeltextui<-renderUI({
    textInput(inputId = "labeltextui1", 
              label = "Create label", "")
  })                                                                                               
  
  observeEvent(input$labeladding, {
    
    var_label(v$data[,input$labelselectui1])<- input$labeltextui1
    
  })
  observeEvent(input$RenamelevelsColumn, {
    output$labelsummary2<-renderPrint({
      levels(v$data[,input$OldlColumnName1])
    })
    
  })
  
  observeEvent(input$labeladding, {
    output$labelsummary1<-renderPrint({
      print(paste("label created"))
      str(v$data[,input$labelselectui1])
    })
  })
  
  
#---recode into different variable for categorical variable---------
  output$recodeui1<- renderUI({
    
    selectInput(inputId = "breakvariable01",
                label = "Choose variable",
                multiple = T,
                choices = c(colnames(v$data)))
  })
  
  output$recodeui01<-renderUI({
    textInput(inputId = "catevariable", 
              label = "Create new variable", "")
  })
  
  observeEvent(input$cateadd, {
    
    v$data[,input$catevariable]<-v$data[,input$breakvariable01]
    
  })
  
  output$recodeui001<-renderUI({
    textInput(inputId = "renamefaclevels", 
              label = "Enter factor levels name", "")
  })
   
  observeEvent(input$recodeok01, {
    req(input$catevariable,input$renamefaclevels)
    v$data[,input$catevariable]<-car::recode(v$data[,input$catevariable],input$renamefaclevels)
    return(v$data[,input$catevariable])
  })
  
  observeEvent(input$cateadd, {
  output$cutsummary<-renderPrint({
    levels(v$data[,input$catevariable])
  })
  })
  
#---recode into different variable for continuous variable---------
  # select variable 1st column name-------
  output$recodevariable<- renderUI({
    
    selectInput(inputId = "recodeselectvariable1",
                label = "Choose variable",
                multiple = T,
                choices = c(colnames(v$data)),
                selected = "AGE")
  })
  output$renewvari<-renderUI({
    textInput(inputId = "entervariablename", 
              label = "Create new variable", "")
  })
  
  observeEvent(input$addvari, {
    v$data[,input$entervariablename]<-v$data[,input$recodeselectvariable1]
  })
  
  # select variable 1st column name-------
  # output$contiselect<- renderUI({
  # 
  #   selectInput(inputId = "entervariablename",
  #               label = "Choose variable",
  #               multiple = T,
  #               choices = c(colnames(v$data)),
  #               selected = "")
  # 
  # })
  output$lower<-renderUI({
    numericInput(inputId = "lowervalue1", 
              label = "Enter Lowest than ( < ) value", "")
  })
  
  
  output$lowervalue<-renderUI({
    numericInput(inputId = "lowervalue2", 
              label = "Enter new value", "")
  })
  
  observeEvent(input$lowerok, {
    req(input$entervariablename,input$lowervalue1,input$lowervalue2,input$entervariablename)
    v$data[,input$entervariablename]<- ifelse(v$data[,input$entervariablename] < input$lowervalue1,input$lowervalue2,v$data[,input$entervariablename])
    
  })
  
  output$between1<-renderUI({
    numericInput(inputId = "betweenvalue1", 
              label = "Enter range from ( >= )", "")
  })
  
  output$between2<-renderUI({
    numericInput(inputId = "betweenvalue2", 
              label = "Enter range to ( < )", "")
  })
  
  output$bewteenvalue<-renderUI({
    
    numericInput(inputId = "betweenvalue3", 
              label = "Enter new value", "")
  })
  observeEvent(input$betweenok, {
    req(input$entervariablename,input$betweenvalue1,input$betweenvalue2,input$betweenvalue3)
    v$data[,input$entervariablename]<-ifelse(v$data[,input$entervariablename] >= input$betweenvalue1 & 
                                               v$data[,input$entervariablename] < input$betweenvalue2,
                                             input$betweenvalue3,v$data[,input$entervariablename])
  
  })
  
  output$higher<-renderUI({
    numericInput(inputId = "highvalue", 
              label = "Enter highest ( >= ) value", "")
  })
  
  output$highervalue<-renderUI({
    numericInput(inputId = "highvalue1", 
              label = "Enter new value", "")
  })
  
  observeEvent(input$higherok, {
    req(input$entervariablename,input$highvalue,input$highvalue1)
    v$data[,input$entervariablename]<-ifelse(v$data[,input$entervariablename] >= input$highvalue,input$highvalue1,v$data[,input$entervariablename])
    v$data[,input$entervariablename]<-as.factor(v$data[,input$entervariablename])
    })
 
  observeEvent(input$addvari, {
    
    output$recodesummary<-renderPrint({
      describe(v$data[,input$entervariablename],paste(input$entervariablename[1]))  
      
      #table(v$data[,input$entervariablename],paste(input$entervariablename[1]))
      #class(v$data[,input$cutvariable])
    })
    
  })
 
  
  
output$labeluiselect<-renderUI({
  selectInput(inputId = "labeluiselectid", 
              label = "Select Factor level Name to rename:",
              multiple = T, 
              choices = c(colnames(v$data)),
              selected = "")
})

  output$labellevelsselect<-renderUI({
    dff1 <- v$data[,input$labeluiselectid]
    dff1<- levels(dff1)
    selectInput(inputId = "oldlabelvariable", 
                label = "Select Factor level Name to rename:",
                multiple = T, 
                choices = dff1,
                selected = "")
  })
  
  output$enternewlabel<-renderUI({
    textInput(inputId = "enternewlabelname",
              label = "Enter new value", "Nil")
  })


  observeEvent(input$lowlabelok, {
    req(input$entervariablename,input$lowervalue2,input$enternewlabelname)
    
    v$data[,input$entervariablename]<-ifelse(v$data[,input$entervariablename] == input$lowervalue2,input$enternewlabelname,v$data[,input$entervariablename])
      
  })
  output$betweenenternewlabel<-renderUI({
    textInput(inputId = "bewteenlabevalue1",
              label = "Enter new value", "Nil")
  })
  
  observeEvent(input$betweenlabelok, {
    req(input$entervariablename,input$betweenvalue3,input$bewteenlabevalue1)
    
    v$data[,input$entervariablename]<-ifelse(v$data[,input$entervariablename] == input$betweenvalue3,input$bewteenlabevalue1,v$data[,input$entervariablename])
    
  })
  
  output$higherenternewlabel<-renderUI({
    textInput(inputId = "higherlabevalue1",
              label = "label", "Nil")
  })
  
  observeEvent(input$highlabelok, {
    req(input$entervariablename,input$highvalue1,input$higherlabevalue1)
    v$data[,input$entervariablename]<-ifelse(v$data[,input$entervariablename] == input$highvalue1,input$higherlabevalue1,v$data[,input$entervariablename])
  })
  
  # output$labelsummary<-renderPrint({
  #   describe(v$data[,input$OldColumnName1],paste(input$OldColumnName1[1]))
  # })                                                                               
  # this is Bivariate ui_input functions------------------------------------
  #select variable
  output$select1 <- renderUI({
    df1 <- colnames(v$data)
    selectInput("variable", "Independent variable(df1):",df1,selected = "BABY.SEX")
  })
  
  # This is startified analysis ui input function------------------ ----------------------- 
  #select variable
  output$select11 <- renderUI({
    df11<-colnames(v$data)
    selectInput("variable11", "Outcome variable(df):",df11,selected = "BABY.SEX")
  })
  
  #select variable
  output$select12 <- renderUI({
    df12 <- colnames(v$data)
    selectInput("variable12", "Independent variable(df1):",df12,selected = "ONSET.OF.LABOUR")
  })
  #select variable
  output$select13 <- renderUI({
    df13 <- colnames(v$data)
    selectInput("variable13", "Stratified variable(df2):",df13,selected = "BLOOD.LOSS")
  })
  output$classtype3<-renderText({
    
    dfg<-v$data[,input$variable11]
    nlevels1<-length(levels(dfg))
    if(is.factor(dfg)){
      if( nlevels1==2){
        print("Type of outcome variable: Binary")
      }
      else if (nlevels1 >=3){
        print("Type of outcome variable: Multinomial")
      }
    }
    else if (is.numeric(dfg) | is.integer(dfg)){
      print("Type of outcome variable: Continuous")
    }
  })
  output$classtype4<-renderText({
    
    dfg<-v$data[,input$variable12]
    nlevels1<-length(levels(dfg))
    if(is.factor(dfg)){
      if( nlevels1==2){
        print("Type of independent variable: Binary")
      }
      else if (nlevels1 >=3){
        print("Type of independent variable: Multinomial")
      }
    }
    else if (is.numeric(dfg) | is.integer(dfg)){
      print("Type of independent variable: Continuous")
    }
  })
  output$classtype5<-renderText({
    
    dfg<-v$data[,input$variable13]
    nlevels1<-length(levels(dfg))
    if(is.factor(dfg)){
      if( nlevels1==2){
        print("Type of stratified variable: Binary")
      }
      else if (nlevels1 >=3){
        print("Type of stratified variable: Multinomial")
      }
    }
    else if (is.numeric(dfg) | is.integer(dfg)){
      print("Type of stratified variable: Continuous")
    }
  })
  # # This is multivariate analysis ui input functions---------------------  
  #   #select variable
  #   output$select21 <- renderUI({
  #     df<-colnames(v$data)
  #     selectInput("variable1", "outcome Variable(df):",df)
  #   })
  #   
  #   #select variable
  #   output$select22 <- renderUI({
  #     df1 <- colnames(v$data)
  #     selectInput("variable", "independent variable(df1):",df1)
  #   })
  #   #select variable
  #   output$select23 <- renderUI({
  #     df2 <- colnames(v$data)
  #     selectInput("variable2", "stratified variable(df2):",df2)
  #   })
  #-----------------------------------------------------------------------------------  
  
  #Risk factor--select variable
  output$select31 <- renderUI({
    df31 <- colnames(v$data[32:47])
    selectInput("variable31", "Medical risk factor(df1):",df31)
  })
  # -- Risk factor--select variable2
  output$select32 <- renderUI({
    df32 <- colnames(v$data[48:62])
    selectInput("variable32", "Obstetric risk factor(df2):",df32)
  })
  # --Risk factor--select variable2
  output$select33 <- renderUI({
    df33 <- colnames(v$data)
    selectInput("variable33", "Outcome variable(df3):",df33)
  })
  
  #----indications--select-1-ui---
  output$select51 <- renderUI({
    df51 <- colnames(v$data)[118:137]
    selectInput("variable51", "Induction indication(df1):",df51)
  })
  #----indications--select-2-ui---
  output$select52 <- renderUI({
    df52 <- colnames(v$data)[78:97]
    selectInput("variable52", "Primary indication(df2):",df52)
  })
  #----indications--select-3-ui---
  output$select53 <- renderUI({
    df53 <- colnames(v$data)[98:117]
    selectInput("variable53", "Secondary indication(df3):",df53)
  })
  
#--outlier variable---------selectui
  output$select5 <- renderUI({
    df5 <- colnames(Filter(is.numeric,v$data))
    selectInput("outliervariable", "choose variable:",df5)
  })
#---missing select--ui----- 
  # output$miss1<-renderUI({
  #   mi1<-colnames(v$data[1:31])
  #   selectInput("misvariable1","Demograph:",mi1)
  # })
  output$miss1<-renderUI({
    mi1<-colnames(v$data)
    selectInput("misvariable1","Variable:",mi1)
  })
  
  output$miss2<-renderUI({
    mi2<-colnames(v$data[41:77])
    selectInput("misvariable2","Risk factor:",mi2)
  })
  
  # chosse data type
  output$classvari<-renderPrint({
    dfg<-v$data[,input$colname]
    print(class(dfg))
    #print(class([,input$colname]))
    print(input$class)
    
  })
  
  # trend chart select variable--------------
  
  #select variable
  output$select41 <- renderUI({
    
    selectInput(
      inputId = "variable41", 
      label = "outcome Variable(df)",
      choices = c(colnames(v$data)),
      selected = "BABY.WEIGHT1")
  })  
  
  #select variable
  output$select42 <- renderUI({
    df42 <- v$data
    df42<- colnames(df42["YEAR"])
    selectInput("variable42", "time Variable(df):",df42)
  })
  
  #select variable
  output$select43 <- renderUI({
    df43 <- v$data
    df43 <- colnames(df43["unit1"])
    selectInput("variable43", "stratified Variable(df):",df43)
  })
  # associate variable select variable--------------
  output$select71<- renderUI({
    df71<- v$data
    df72<-data.frame(df71$BABY.WEIGHT1,df71$AGE1,df71$MOTHER.HEIGHT1,df71$MOTHER.HEIGHT,df71$PLACENTA.WEIGHT.Gms.1,df71$GEST.WEEKS1)
    names(df72)<-c("BABY.WEIGHT1","AGE1","MOTHER.HEIGHT1","MOTHER.WEIGHT1","PLACENTA.WEIGHT.Gms.1","GEST.WEEKS1")
    df73 <- colnames(df72)
    
    selectInput("variable73", "stratified Variable(df):",df73)
  })
  
  
  #summary--------------
  output$summary<-renderPrint({
    df<-v$data
    df<- df[,input$variable1]
    #df<- paste(names(df[,input$variable1]))
    # df<- names(antental1[input$variable1])
    #df<- paste(antental1[input$variable])
    if(is.factor(df)){
      # y1<-
      describe(df,paste(input$variable1[1]))
      # y2<-summary(df)
      # g2<-data.frame(y1$counts[1],y1$counts[2],y2[1],y2[2])
      # names(g2)<-c("n","Missing values","F")
      # rownames(g2)<-NULL
      # knitr::kable(g2,caption=paste(input$variable1[1]))
    }else {
     # describe(df,paste(input$variable1[1]))
      y<-describe(df,paste(input$variable1[1]))
      g<-data.frame(length(df),y$counts[2],y$counts[5],median(df,na.rm=T),sd(df,na.rm=T),range(df,na.rm=T)[1],range(df,na.rm=T)[2],y$counts[7],y$counts[8],y$counts[9],y$counts[10],y$counts[11],y$counts[12],y$counts[13])
      names(g)<-c("n","Missing vlaues","Mean","Median","SD","Lower.Range","upper.Range",".05",".10",".25",".50",".75",".90",".95")
      rownames(g)<-NULL
     # kable(g,format="markdown")
      knitr::kable(g,caption=paste(input$variable1[1]))
      #Desc(df,maxrows = NULL, ord = NULL, plotit = F,sep = NULL,paste(input$variable1[1]))
      
    }
  })
  # #summary-------------------------
  output$summary1<-renderPrint({
    df1<- v$data
    df1<- df1[,input$variable]
    if(is.integer(df1)|is.numeric(df1)){
      y<-describe(df1,paste(input$variable[1]))
      g<-data.frame(length(df1),y$counts[2],y$counts[5],median(df1,na.rm=T),sd(df1,na.rm=T),range(df1,na.rm=T)[1],range(df1,na.rm=T)[2],y$counts[7],y$counts[8],y$counts[9],y$counts[10],y$counts[11],y$counts[12],y$counts[13])
      names(g)<-c("n","Missing vlaues","Mean","Median","SD","Lower.Range","upper.Range",".05",".10",".25",".50",".75",".90",".95")
      # kable(g,format="markdown")
      knitr::kable(g,caption=paste(input$variable[1]))
     # Desc(df1,maxrows = NULL, ord = NULL, plotit = F,sep = NULL,paste(input$variable[1]))
    }else {
     # dfSummary(df1)
     # skim(df1,paste(input$variable[1]))
      describe(df1,paste(input$variable[1]))
    }
  })
 #--aggreate---bivariate
   # output$sum11<- renderPrint({
   #  options(digits=3)
   #  df<- v$data
   #  df1<- v$data
   #  df<- df[,input$variable1]
   #  df1<- df1[,input$variable]
   #  y1<-aggregate(r$BABY.WEIGHT1~r$DELIVERY.TYPE,FUN="length")
   #  y2<-aggregate(r$BABY.WEIGHT1~r$DELIVERY.TYPE,FUN="mean")[2]
   #  h<-data.frame(y1,y2)
   #  names(h)<-c("levels","Frequency","Mean")
   #  h
   #  
   # })
  ## #summary-----------------------
  output$summary2<- renderPrint({
    options(digits=3)
    df<- v$data
    df1<- v$data
    df<- df[,input$variable1]
    df1<- df1[,input$variable]
    nlevels<-length(levels(df1))
    nlevels1<-length(levels(df))
    if ((is.integer(df)|is.numeric(df)) & is.factor(df1)){
      if (nlevels == 2){
        t.test(df~df1)
      } 
      else if (nlevels >= 3){
        g3<-aggregate(df~df1,FUN="mean")
         g<-lm(df~df1)
         print(anova(g))
        print(TukeyHSD(aov(df~df1)))
         
      } 
    }
    else if ((is.integer(df)|is.numeric(df)) & (is.integer(df1) | is.numeric(df1))){
      cor(df,df1,use="complete.obs")
    }
    else if (is.factor(df) & is.factor(df1)) {
      tab<-table(df,df1,dnn =c(paste(input$variable1[1]),paste(input$variable[1])))
      tab1<-t(tab)
      print(tab1)
     print(chisq.test(df,df1))
      # r<-oddsratio(tab1, method = "wald")
      # print(r[c(1,2,3)],digits=5)
      
    }
    else if (is.factor(df) & (is.numeric(df1)|is.integer(df1))) {
      if (nlevels1 == 2){
        t.test(df1~df)
      } 
      else if (nlevels1 >= 3){
        g<-lm(df1~df)
        g1<-TukeyHSD(aov(df1~df))
        g2<-data.frame(g1$df)
        g3<-subset(g2,p.adj < 0.05)
        print(anova(g))
        print(g3)
      }
    }
  })  
  
  # Generate a summary of the dataset ----
  output$summary3 <- renderPrint({
    dfSummary(v$data)
    #Desc(v$data,main = NULL, maxrows = NULL, ord = NULL, plotit = F,sep = NULL,digits = NULL)
  })
  #---risk factor summary--1----
  output$risk1<-renderPrint({
    df31<-v$data
    df1<- df31[,input$variable31]
    if (is.factor(df1)){
      describe(df1)
    }
    else {
      print("NULL")
    }
  })
  #---risk factor summary--2----
  output$risk2<-renderPrint({
    df32<-v$data
    df2<- df32[,input$variable32]
    if (is.factor(df2)){
      describe(df2)
    }
    else {
      print("NULL")
    }
  })
  #---risk factor summary---3----
  output$risk3<-renderPrint({
    df31<-v$data
    df32<-v$data
    df1<- df31[,input$variable31]
    df2<- df32[,input$variable32]
    table(df1,df2,dnn =c(paste(input$variable31[1]),paste(input$variable32[1])))
  })
  #---risk factor summary---4----
  output$risk4<-renderPrint({
    options(digits=3)
    df31<-v$data
    df33<-v$data
    df1<- df31[,input$variable31]
    df3<- df33[,input$variable33]
    if((is.integer(df3)| is.numeric(df3)) & (is.factor(df1))){
      aggregate(df3 ~ df1,data=v$data,mean)
      # with(v$data, tapply(df31 ,df32, mean))
    }
    else {
      print("NULL")
    }
  })
  #---risk factor summary---5----
  output$risk5<-renderPrint({
    options(digits=3)
    df32<-v$data
    df33<-v$data
    df2<- df32[,input$variable32]
    df3<- df33[,input$variable33]
    if((is.integer(df3)| is.numeric(df3)) & (is.factor(df2))){
      aggregate(df3 ~ df2,data=v$data,mean)
      # with(v$data, tapply(df31 ,df32, mean))
    }
    else {
      print("NULL")
    }
  })
  #---risk factor summary--INDICATION----
  # output$risk6<-renderPrint({
  #   df31<-v$data
  #   df32<-v$data
  #   df333<-v$data
  #   df1<- df31[,input$variable31]
  #   df2<- df32[,input$variable32]
  #   df422<- v$data[83]
  #   df423<- v$data[84]
  #   df424<- v$data[85]
  #   d1<-data.frame(df422,df1)
  #   d2<-data.frame(df423,df1)
  #   d3<-data.frame(df424,df1)
  #   names(d1)<-c("INDUCTION.INDICATIONS","risk.factor1")
  #   names(d2)<-c("PRIMARY.INDICATIONS","risk.factor2")
  #   names(d3)<-c("SECONDARY.INDICATIONS","risk.factor3")
  #   l1<-subset(d1,risk.factor1=="1")
  #   l2<-subset(d2,risk.factor2=="1")
  #   l3<-subset(d3,risk.factor3=="1")
  #   f1<- names(sort(table(l1$INDUCTION.INDICATIONS),decreasing=TRUE)[1:10])
  #   f2<- names(sort(table(l2$PRIMARY.INDICATIONS),decreasing=TRUE)[1:10])
  #   f3<- names(sort(table(l3$SECONDARY.INDICATIONS),decreasing=TRUE)[1:10])
  #   a<-data.frame(f1,f2,f3)
  #   names(a)<-c("df3:INDUCTION_INDICATION","df3:PRIMARY_INDICATION","df3:SECONDARY_INDICATION")
  #   a
  #   # cat("INDUCTION_INDICATION:",f1, sep = "\n")
  #   # cat("PRIMARY_INDICATION:",f2, sep = "\n")
  #   # cat("SECONDARY_INDICATION:",f3, sep = "\n")
  # })
  # #---polt----------------
  output$indiplot1<-renderPlotly({
    df63<-v$data[118:137]
    mylist <- data.frame(mapply(table, df63))
  #  mylist<-mylist1[c(F,T)]
  #  names(mylist)<-c("I.CHTN","I.ELDERLY","I.GDM","I.GHTN","I.HYDROCEPHA",  
   #                    "I.THYROID","I.INFERTILITY","I.IUD","I.IUGR","I.Low.AFI",     
   #                    "I.MILD.PE","I.OBESE","I.POST.DATES","I.PPROM","I.PREVIOUS.IUD",
    #                   "I.PROM","I.RH.NEGATIVE","I.SGA","I.SPE","I.TWINS")
    a1<-mylist[2,]
    r2<-sort(a1,decreasing = T)
    r3<-names(r2)
    r4<-as.numeric(r2)
    p<-sum(r4)
    p1<-(r4/p)*100
    da<-data.frame(r3,p1)
    da$r3<-factor(da$r3,levels = unique(da$r3)[order(da$p1,decreasing = T)])
    plot_ly(da, x = ~r3, y = ~p1, type = "bar")%>%
      layout(xaxis = list(title = 'induction_indication'),yaxis = list(title = 'patients(percentage)'),title = "induction indication(df1)")

  })
  #---polt----------------
  output$indiplot2<-renderPlotly({
    df63<-v$data[78:97]
    mylist <- data.frame(mapply(table, df63))
    a1<-mylist[2,]
    r2<-sort(a1,decreasing = T)
    r3<-names(r2)
    r4<-as.numeric(r2)
    p<-sum(r4)
    p1<-(r4/p)*100
    da<-data.frame(r3,p1)
    da$r3<-factor(da$r3,levels = unique(da$r3)[order(da$p1,decreasing = T)])
    plot_ly(da, x = ~r3, y = ~p1, type = "bar")%>%
      layout(xaxis = list(title = 'primary_indication'),yaxis = list(title = 'patients(percentage)'),title = "Primary indication(df2)")
    
  })
  #---polt----------------
  output$indiplot3<-renderPlotly({
    df63<-v$data[98:117]
    mylist <- data.frame(mapply(table, df63))
    a1<-mylist[2,]
    r2<-sort(a1,decreasing = T)
    r3<-names(r2)
    r4<-as.numeric(r2)
    p<-sum(r4)
    p1<-(r4/p)*100
    da<-data.frame(r3,p1)
    da$r3<-factor(da$r3,levels = unique(da$r3)[order(da$p1,decreasing = T)])
    plot_ly(da, x = ~r3, y = ~p1, type = "bar")%>%
      layout(xaxis = list(title = 'secondary_indication'),yaxis = list(title = 'patients(percentage)'),title = "secondary indication(df3)")
    
  })
  #---indi text output--------------
  output$inditext1<-renderText({
    df62<-v$data[,input$variable51]
    p<-paste0("selected indication is:", input$variable51)
    p
  })
  
  #---indication plot--medical-----
  output$indiplot4<-renderPlotly({
    df61<-v$data[65:68]
    df62<-v$data[,input$variable51]
    da<-data.frame(df61,df62)
    names(da)<-c("risk1","risk2","risk3","risk4","df1")
    d1<-subset(da,df1=="1")
    d3<-d1
    rows<-seq(1:nrow(d3))
    d2<-data.frame(rows,d3)
    d2$df1<-NULL
    dat3 <- reshape2::melt(d2, id.var = "rows")
    df3 <- dat3 %>% 
      group_by(variable, value) %>% 
      tally() %>% 
      complete(value, fill = list(n = 0)) %>% 
      mutate(percentage = n / sum(n) * 100)
    p<-ggplot(df3,aes(x=variable,y=percentage,fill=value)) + 
      geom_bar(stat = "identity",position="fill")+
      scale_fill_manual(values = c("Anemia Mild"="green", "Anemia severe" = "green","Anemia Moderate"="green","BIOHAZARD-HCV"="skyblue","BIOHAZARD-HBsAg"="skyblue","BIOHAZARD-HIV"="skyblue",
                                   "BRONCHIAL ASTHMA" = "slateblue1",  "CARDIAC DISEASE-CONGENITAL HEART DISEASE" = "purple3","CARDIAC DISEASE-RHEUMATIC HEART DISEASE" ="purple3","ESSENTIAL/CHRONIC HYPERTENSION"="blue2",
                                   "GDM Diet control" = "firebrick4",  "GDM Insulin"="firebrick4", "GDM OHA"="firebrick4",
                                   "GHTN Drugs" = "coral2", "GHTN Home BP" = "coral2", "LIVER DISORDER-AFLP" = "palevioletred", "LIVER DISORDER-HELLP" = "palevioletred","None" = "black",
                                   "Preeclampsia Mild" = "violetred" ,"Preeclampsia Severe" = "violetred" ,"Eclampsia"= "violetred" , "PREGESTATIONAL DIABETES MELLITUS"="red2", "SEIZURE DISORDERS" = "springgreen2",
                                   "THYROID DISORDERS-HYPERTHYROIDISM" = "darkgoldenrod2", "THYROID DISORDERS-HYPOTHYROIDISM" = "darkgoldenrod2"), 
                        na.value ="grey")+ggtitle(paste("INDUCTION_INDICATION FOR MEDICAL RISK FACTOR",colnames(v$data[,input$variable51])))
    
    
    ggplotly(p)
    
  })
  
  #---indi text output--------------
  output$inditext7<-renderText({
    df62<-v$data[,input$variable51]
    p<-paste0("selected indication is:", input$variable51)
    p
  })
  
  #---indication plot-obs--
  output$indiplot7<-renderPlotly({
    df61<-v$data[70:73]
    df62<-v$data[,input$variable51]
    da<-data.frame(df61,df62)
    names(da)<-c("risk1","risk2","risk3","risk4","df1")
    d1<-subset(da,df1=="1")
    d3<-d1
    rows<-seq(1:nrow(d3))
    d2<-data.frame(rows,d3)
    d2$df1<-NULL
  
    dat3 <- reshape2::melt(d2, id.var = "rows")
    df3 <- dat3 %>% 
      group_by(variable, value) %>% 
      tally() %>% 
      complete(value, fill = list(n = 0)) %>% 
      mutate(percentage = n / sum(n) * 100)
    p<-ggplot(df3,aes(x=variable,y=percentage,fill=value)) + 
      geom_bar(stat = "identity",position="fill")+
      scale_fill_manual(values = c("APH-ABRUPTION GRADE 1"="pink1", "APH-ABRUPTION GRADE 2" = "pink1","APH-ABRUPTION GRADE 3"="pink1","APH-PLACENTA PREVIA"="pink1","APH-UNCLASSIFIED" ="pink1","BIOHAZARD-HCV"="violet","BREECH"="firebrick4",
                                   "ELDERLY PRIMI" = "slateblue1",  "Fibroid complicating pregnancy" = "yellow","GRAND MULTIPARA" ="purple3",
                                   "IUGR"  = "green",
                                   "IVF PREGNANCY" =  "blue2", "MACROSOMIA" = "navyblue", "None" = "black",
                                   "Multiple pregnancy Triplets"  = "tomato" ,"Multiple pregnancy Twins"= "coral2"  , "PPROM"="palevioletred", "Preterm labour"  = "violetred" ,
                                   "PREVIOUS LSCS" =  "red2", "PREVIOUS NEONATAL DEATH" = "springgreen2", "PREVIOUS PRETERM DELIVERY" = "yellowgreen", "PREVIOUS STILLBIRTH" = "palegreen4",
                                   "PRIMARY INFERTILITY" ="darkgoldenrod2","Rh isoimmunisation"="tan" ,"SHORT STATURE"="tan3"),
                        na.value = "grey")+ggtitle("INDUCTION_INDICATION FOR OBSTETRICS RISK FACTOR")
    
    
    ggplotly(p)
  })    
  #---indi text output--------------
  output$inditext8<-renderText({
    df62<-v$data[,input$variable51]
    p<-paste0("selected indication is:", input$variable52)
    p
  })
  
  #---indication plot---medical------
  output$indiplot8<-renderPlotly({
    df61<-v$data[70:73]
    df62<-v$data[,input$variable52]
    da<-data.frame(df61,df62)
    names(da)<-c("risk1","risk2","risk3","risk4","df1")
    d1<-subset(da,df1=="1")
    d3<-d1
    rows<-seq(1:nrow(d3))
    d2<-data.frame(rows,d3)
    d2$df1<-NULL
    
    dat3 <- reshape2::melt(d2, id.var = "rows")
     df3 <- dat3 %>% 
      group_by(variable, value) %>% 
      tally() %>% 
      complete(value, fill = list(n = 0)) %>% 
      mutate(percentage = n / sum(n) * 100)
    p<-ggplot(df3,aes(x=variable,y=percentage,fill=value)) + 
      geom_bar(stat = "identity",position="fill")+
      scale_fill_manual(values = c("APH-ABRUPTION GRADE 1"="pink1", "APH-ABRUPTION GRADE 2" = "pink1","APH-ABRUPTION GRADE 3"="pink1","APH-PLACENTA PREVIA"="pink1","APH-UNCLASSIFIED" ="pink1","BIOHAZARD-HCV"="violet","BREECH"="firebrick4",
                                   "ELDERLY PRIMI" = "slateblue1",  "Fibroid complicating pregnancy" = "yellow","GRAND MULTIPARA" ="purple3",
                                   "IUGR"  = "green",
                                   "IVF PREGNANCY" =  "blue2", "MACROSOMIA" = "navyblue", "None" = "black",
                                   "Multiple pregnancy Triplets"  = "tomato" ,"Multiple pregnancy Twins"= "coral2"  , "PPROM"="palevioletred", "Preterm labour"  = "violetred" ,
                                   "PREVIOUS LSCS" =  "red2", "PREVIOUS NEONATAL DEATH" = "springgreen2", "PREVIOUS PRETERM DELIVERY" = "yellowgreen", "PREVIOUS STILLBIRTH" = "palegreen4",
                                   "PRIMARY INFERTILITY" ="darkgoldenrod2","Rh isoimmunisation"="tan" ,"SHORT STATURE"="tan3"),
                        na.value = "grey")+ggtitle("PRIMARY_INDICATION FOR OBSTETRICS RISK FACTOR")
    
    
    ggplotly(p)
    
  })   
  #---indi text output--------------
  output$inditext5<-renderText({
    df62<-v$data[,input$variable51]
    p<-paste0("selected indication is:", input$variable52)
    p
  })
  
  output$indiplot5<-renderPlotly({
    df61<-v$data[65:68]
    df62<-v$data[,input$variable52]
    da<-data.frame(df61,df62)
    names(da)<-c("risk1","risk2","risk3","risk4","df1")
    d1<-subset(da,df1=="1")
    d3<-d1
    rows<-seq(1:nrow(d3))
    d2<-data.frame(rows,d3)
    d2$df1<-NULL
   
    dat3 <- reshape2::melt(d2, id.var = "rows")
  
    df3 <- dat3 %>% 
      group_by(variable, value) %>% 
      tally() %>% 
      complete(value, fill = list(n = 0)) %>% 
      mutate(percentage = n / sum(n) * 100)
    p<-ggplot(df3,aes(x=variable,y=percentage,fill=value)) + 
      geom_bar(stat = "identity",position="fill")+
      scale_fill_manual(values = c("Anemia Mild"="green", "Anemia severe" = "green","Anemia Moderate"="green","BIOHAZARD-HCV"="skyblue","BIOHAZARD-HBsAg"="skyblue","BIOHAZARD-HIV"="skyblue",
                                   "BRONCHIAL ASTHMA" = "slateblue1",  "CARDIAC DISEASE-CONGENITAL HEART DISEASE" = "purple3","CARDIAC DISEASE-RHEUMATIC HEART DISEASE" ="purple3","ESSENTIAL/CHRONIC HYPERTENSION"="blue2",
                                   "GDM Diet control" = "firebrick4",  "GDM Insulin"="firebrick4", "GDM OHA"="firebrick4",
                                   "GHTN Drugs" = "coral2", "GHTN Home BP" = "coral2", "LIVER DISORDER-AFLP" = "palevioletred", "LIVER DISORDER-HELLP" = "palevioletred","None" = "black",
                                   "Preeclampsia Mild" = "violetred" ,"Preeclampsia Severe" = "violetred" ,"Eclampsia"= "violetred" , "PREGESTATIONAL DIABETES MELLITUS"="red2", "SEIZURE DISORDERS" = "springgreen2",
                                   "THYROID DISORDERS-HYPERTHYROIDISM" = "darkgoldenrod2", "THYROID DISORDERS-HYPOTHYROIDISM" = "darkgoldenrod2"), 
                        na.value ="grey")+ggtitle(paste("PRIMARY_INDICATION FOR MEDICAL RISK FACTOR",colnames(v$data[,input$variable51])))
    
    
    ggplotly(p)
    
  })
  
  #---indi text output--------------
  output$inditext9<-renderText({
    df62<-v$data[,input$variable51]
    p<-paste0("selected indication is:", input$variable53)
    p
  })
  
  #---indication plot---
  output$indiplot9<-renderPlotly({
    df61<-v$data[70:73]
    df62<-v$data[,input$variable53]
    da<-data.frame(df61,df62)
    names(da)<-c("risk1","risk2","risk3","risk4","df1")
    d1<-subset(da,df1=="1")
    d3<-d1
    rows<-seq(1:nrow(d3))
    d2<-data.frame(rows,d3)
    d2$df1<-NULL
    dat3 <- reshape2::melt(d2, id.var = "rows")
    df3 <- dat3 %>% 
      group_by(variable, value) %>% 
      tally() %>% 
      complete(value, fill = list(n = 0)) %>% 
      mutate(percentage = n / sum(n) * 100)
    p<-ggplot(df3,aes(x=variable,y=percentage,fill=value)) + 
      geom_bar(stat = "identity",position="fill")+
      scale_fill_manual(values = c("APH-ABRUPTION GRADE 1"="pink1", "APH-ABRUPTION GRADE 2" = "pink1","APH-ABRUPTION GRADE 3"="pink1","APH-PLACENTA PREVIA"="pink1","APH-UNCLASSIFIED" ="pink1","BIOHAZARD-HCV"="violet","BREECH"="firebrick4",
                                   "ELDERLY PRIMI" = "slateblue1",  "Fibroid complicating pregnancy" = "yellow","GRAND MULTIPARA" ="purple3",
                                   "IUGR"  = "green",
                                   "IVF PREGNANCY" =  "blue2", "MACROSOMIA" = "navyblue", "None" = "black",
                                   "Multiple pregnancy Triplets"  = "tomato" ,"Multiple pregnancy Twins"= "coral2"  , "PPROM"="palevioletred", "Preterm labour"  = "violetred" ,
                                   "PREVIOUS LSCS" =  "red2", "PREVIOUS NEONATAL DEATH" = "springgreen2", "PREVIOUS PRETERM DELIVERY" = "yellowgreen", "PREVIOUS STILLBIRTH" = "palegreen4",
                                   "PRIMARY INFERTILITY" ="darkgoldenrod2","Rh isoimmunisation"="tan" ,"SHORT STATURE"="tan3"),
                        na.value = "grey")+ggtitle("SECONDARY_INDICATION FOR OBSTETRICS RISK FACTOR")
    
    
    ggplotly(p)
  })
  #---indi text output--------------
  output$inditext6<-renderText({
    df62<-v$data[,input$variable51]
    p<-paste0("selected indication is:", input$variable53)
    p
  })
  
  output$indiplot6<-renderPlotly({
    df61<-v$data[65:68]
    df62<-v$data[,input$variable53]
    da<-data.frame(df61,df62)
    names(da)<-c("risk1","risk2","risk3","risk4","df1")
    d1<-subset(da,df1=="1")
    d3<-d1
    rows<-seq(1:nrow(d3))
    d2<-data.frame(rows,d3)
    d2$df1<-NULL
    dat3 <- reshape2::melt(d2, id.var = "rows")
    # g<- ggplot(data = dat3) +
    #   geom_mosaic(aes(x = product(variable), fill = value)) +
    #   labs(x='', title='mosaic plot') 
    # 
    df3 <- dat3 %>% 
      group_by(variable, value) %>% 
      tally() %>% 
      complete(value, fill = list(n = 0)) %>% 
      mutate(percentage = n / sum(n) * 100)
    p<-ggplot(df3,aes(x=variable,y=percentage,fill=value)) + 
      geom_bar(stat = "identity",position="fill")+
      scale_fill_manual(values = c("Anemia Mild"="green", "Anemia severe" = "green","Anemia Moderate"="green","BIOHAZARD-HCV"="skyblue","BIOHAZARD-HBsAg"="skyblue","BIOHAZARD-HIV"="skyblue",
                                   "BRONCHIAL ASTHMA" = "slateblue1",  "CARDIAC DISEASE-CONGENITAL HEART DISEASE" = "purple3","CARDIAC DISEASE-RHEUMATIC HEART DISEASE" ="purple3","ESSENTIAL/CHRONIC HYPERTENSION"="blue2",
                                   "GDM Diet control" = "firebrick4",  "GDM Insulin"="firebrick4", "GDM OHA"="firebrick4",
                                   "GHTN Drugs" = "coral2", "GHTN Home BP" = "coral2", "LIVER DISORDER-AFLP" = "palevioletred", "LIVER DISORDER-HELLP" = "palevioletred","None" = "black",
                                   "Preeclampsia Mild" = "violetred" ,"Preeclampsia Severe" = "violetred" ,"Eclampsia"= "violetred" , "PREGESTATIONAL DIABETES MELLITUS"="red2", "SEIZURE DISORDERS" = "springgreen2",
                                   "THYROID DISORDERS-HYPERTHYROIDISM" = "darkgoldenrod2", "THYROID DISORDERS-HYPOTHYROIDISM" = "darkgoldenrod2"), 
                        na.value ="grey")+ggtitle(paste("SECONDARY_INDICATION FOR MEDICAL RISK FACTOR",colnames(v$data[,input$variable51])))
    
    
    ggplotly(p)
  })  
  #--univariate download report----   
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      library(summarytools)
      params <-  dfSummary(v$data)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
# stratified tabs----summary-------------------------
  output$summary4<-renderPrint({
    df11<- v$data
    df12<-v$data
    df13<-v$data
    df11<- df11[,input$variable11]
    df12<- df12[,input$variable12]
    df13<- df13[,input$variable13]
    
    if ((is.factor(df11)) & (is.factor(df12) | is.numeric(df12) | is.integer(df12)) & (is.factor(df13) | is.numeric(df13) | is.integer(df13))){
      
      table(df11,df12,df13,dnn =c(paste(input$variable11[1]),paste(input$variable12[1]),paste(input$variable13[1])))
      # mhor(df,df1,df2)
      # epi.2by2(table(df,df1,df2))
      #crosstab(da,row.vars = c("df","df1"),col.vars = "df2",type = "f")
    }
    else if ((is.integer(df11) | is.numeric(df11)) & (is.factor(df12) | is.numeric(df12) | is.integer(df12)) & (is.factor(df13) | is.numeric(df13) | is.integer(df13))){
      library(data.table)
      #df11<- v$data[,input$variable11]
      #df12<- v$data[,input$variable12]
      #df13<- v$data[,input$variable13]
      #aggregate(df11 ~ df12 + df13, data=v$data, mean)
      with(v$data, tapply(df11, list(df1=df12,df2=df13), mean,na.rm=T) )
    }
  })
  
  
#mantel haenszel chi-sqaure----summary-------------------------
  output$summary7<-renderPrint({
    df11<- v$data
    df12<-v$data
    df13<-v$data
    df11<- df11[,input$variable11]
    df12<- df12[,input$variable12]
    df13<- df13[,input$variable13]
    df<-df11
    df1<-df12
    df2<-df13
    if (is.factor(df) & is.factor(df1) & is.factor(df2)){
      
      # df<- names((input$variable1[1])
      # df1<-paste(input$variable[1])
      # table(df,df1,df2,dnn =c(paste(input$variable1[1]),paste(input$variable[1]),paste(input$variable[1])))
      mhor(df,df1,df2,decimal = 2,graph = F)
      # epi.2by2(table(df,df1,df2))
      #crosstab(da,row.vars = c("df","df1"),col.vars = "df2",type = "f")
    }
    else {
      print("Must be categorical variable(2x2xN form)")
    }
  })
  
  
#outlier remove summary 5------------

  observeEvent(input$remove, {
    
    v$data[,input$outliervariable]<-remove_outliers(v$data[,input$outliervariable])
    v$data<-v$data[complete.cases(v$data[,input$outliervariable]),]
   
  })
#outlier summary---------------  
  output$summary5<-renderPrint({
    outliers <- boxplot(v$data[,input$outliervariable], plot=FALSE)$out
    length(outliers)
  })
#--list of outlier summary---------------  
  output$outlierslist<-renderPrint({
    outliers <- boxplot(v$data[,input$outliervariable], plot=FALSE)$out
   table(outliers)
  })  
#----missing value---summary------
  output$miss3<- renderPrint({
    df7<-v$data
    df7<-df7[,input$misvariable1]
    if (is.integer(df7) | is.numeric(df7)){
      
      count<-sum(is.na(df7))
      Total=length(df7)
      percentage<-(count/length(df7))*100
      percent<-paste(round(percentage,2),"%",sep="")
      mis<-data.frame(count,percent)
      names(mis)<-c("Missing vlaue(Total)","percentage")
      mis
      
      
    }
    else if(is.factor(df7)){
      df7<-replace.empty(df7)
      count<-sum(is.na(df7))
      Total=length(df7)
      percentage<-(count/length(df7))*100
      percent<-paste(round(percentage,2),"%",sep="")
      
      mis<-data.frame(count,percent)
      names(mis)<-c("Missings value(Total)","percentage")
      mis
    } 
  })
  #missing value
  output$miss4<- renderPrint({
    df7<-v$data
    df7<-df7[,input$misvariable2]
    if (is.integer(df7) | is.numeric(df7)){
      
      count<-sum(is.na(df7))
      Total=length(df7)
      percentage<-(count/length(df7))*100
      percent<-paste(round(percentage,2),"%",sep="")
      mis1<-data.frame(count,percent)
      names(mis1)<-c("Missing values(Total)","Percentage")
      mis1
    }
    else if(is.factor(df7)){
      df7<-replace.empty(df7)
      count<-sum(is.na(df7))
      Total=length(df7)
      percentage<-(count/length(df7))*100
      percent<-paste(round(percentage,2),"%",sep="")
      data.frame(count,percent)
      mis1<-data.frame(count,percent)
      names(mis1)<-c("Missing values(Total)","Percentage")
      mis1
    } 
  })
  
  # #removing variable
  # output$summary8<-renderPrint({
  #   df8<-myData()
  #   df9<-df8[,input$variable8]
  #   df9<-NULL
  #   df9
  # })
  
  #Trend chart summary------------------------  
  # output$mean<-renderPrint({
  #   options(digits=4)
  #   df41<- v$data
  #   df42<- v$data
  #   df43<-v$data
  #   df41<- df41[,input$variable41]
  #   df42<- df42[,input$variable42]
  #   df43<- df43[,input$variable43]
  #   
  #   if((is.integer(df41) | is.numeric(df41)) & (is.integer(df42) | is.numeric(df42)|is.factor(df42) | is.character(df43)) & (is.integer(df43) | is.numeric(df43)|is.factor(df43)| is.character(df43))){
  #     
  #     ds<-aggregate(df41 ~ df42+df43,data=v$data,mean,na.rm=T)
  #     names(ds)<-c("year","unit1","df")
  #     ds
  #   }
  #   else if((is.integer(df41) | is.numeric(df41)| is.factor(df41)| is.character(df41)) & (is.integer(df42) | is.numeric(df42)|is.factor(df42)| is.character(df43)) & (is.integer(df43) | is.numeric(df43)|is.factor(df43)| is.character(df43))){
  #     df<-data.frame(df41,df42,df43)
  #     names(df)<-c("df1","year","unit1")
  #     
  #     a<-as.data.frame(table(df[,c("df1","year","unit1")]))
  #     a
  #   }
  #   
  # })  
  
  # #----labels---
  #   output$labels1 <- renderPrint({
  #     l1<-v$data[40:70]  
  #     l2<-v$data[86:145]
  #     l<-cbind(l1,l2)
  #     l5<-label(l)
  #     transform(l5)
  #  })
  #----labels---
  output$labels3 <- renderPrint({
    cat("link:","https://www.ncbi.nlm.nih.gov/pmc/articles.", sep = "\n")
    cat("link(2):","https://emedicine.medscape.com/article.", sep = "\n")
    cat("link(3):","https://www.ncbi.nlm.nih.gov/pubmed/25185379.", sep = "\n")
    cat("link(4):","https://care.diabetesjournals.org/content/26/suppl_1/s103.", sep = "\n")
    cat("link(5):","https://www.hopkinsmedicine.org/health/conditions-and-diseases/staying-healthy-during-pregnancy/hypothyroidism-and-pregnancy.", sep = "\n")
    cat("link(6):","https://www.medicinenet.com/script/main/art.asp?articlekey=4063.", sep = "\n")
    cat("link(7):","https://www.webmd.com/baby/iugr-intrauterine-growth-restriction#1.", sep = "\n")
    cat("link(8):","https://www.mayoclinic.org/healthy-lifestyle/pregnancy-week-by-week/expert-answers/low-amniotic-fluid/faq-20057964.", sep = "\n")
    cat("link(9):","https://www.urmc.rochester.edu/encyclopedia/content.aspx?ContentTypeID=90&ContentID=P02496.", sep = "\n")
    cat("link(10):","https://emedicine.medscape.com/article/261137-overview.", sep = "\n")
    cat("link(11):","https://www.ncbi.nlm.nih.gov/pubmed/12972014.", sep = "\n")
    
  })
  
 
  # outliers plot-------------------------
  output$plotlier <- renderPlotly({
    df5<-v$data
    df6<-df5[,input$outliervariable]
    
    if (is.integer(df6) |is.numeric(df6) ){
      r<- ggplot(v$data, aes(x = "", y = df6)) +geom_boxplot(fill="lightblue")+labs(title = "Outliers Plot",x="outliers",y=names(df6))
      r<- ggplotly(r)
      r  
    }
    else {
      df6<-df5[,input$variable5]
      plot_ly(x=names(table(df6)),y= as.numeric(table(df6)),type="bar")
    }
  })
  #missing plot1---------
  output$missplot1<-renderPlotly({
    df7<-v$data
    # df7<-df7[,input$misvariable1]
    # df7<-data.frame(df7)[1:31]
    g<-naniar::gg_miss_var(df7)
    ggplotly(g)
  })
  #missing plot--2---------
  output$missplot2<-renderPlotly({
    df17<-v$data
    # df17<-df17[,input$misvariable2]
    # df17<-data.frame(df17)[41:77]
    g<-naniar::gg_miss_var(df17)
    ggplotly(g)
  })
  
  
  #
  output$plot4<-renderPlotly({
    df<- v$data
    df<- df[,input$variable1]
    if(is.integer(df) |is.numeric(df) ){
      plot_ly(x=df,type="histogram")%>%
        layout(xaxis = list(title = 'df'),yaxis = list(title = 'Frequency'),title = "outcome variable(df)")
      
    }
    else {
      plot_ly(x=names(table(df)),y= as.numeric(table(df)),type="bar")%>%
        layout(xaxis = list(title = 'df'),yaxis = list(title = 'Count'),title = "outcome variable(df)")
    }
  })
  
  output$plot5<-renderPlotly({
    df1<- v$data
    df1<- df1[,input$variable]
    if(is.factor(df1)){
      plot_ly(x=names(table(df1)),y= as.numeric(table(df1)),type="bar")%>%
        layout(xaxis = list(title = 'df1'),yaxis = list(title = 'Count'),title = "independent variable(df1)")
    }
    else {
      plot_ly(x=df1,type="histogram")%>%
        layout(xaxis = list(title = 'df1'),yaxis = list(title = 'Frequency'),title = "independent variable(df1)")
    }
  })
  #--bivariate tab plots----  
  output$plot6 <- renderPlotly({
    df<-v$data
    df1<-v$data
    df <- df[,input$variable1]
    df1<- df1[,input$variable]  
    if(is.factor(df) & is.factor(df1)){
      #mosaic(~ df1+df, data = v$data)
      an<-data.frame(df,df1)
      
      names(an)<-c("df","df1")
      g<- ggplot(data = an) +
        geom_mosaic(aes(x = product(df), fill = df1), na.rm=TRUE) +
        labs(x='', y=" ",title='mosaic plot') 
      g<- ggplotly(g)
      g
      
    }
    else if (is.factor(df) & (is.integer(df1)|is.numeric(df1))){
      wr1<- data.frame(df,df1)
      wr<-na.omit(wr1)
      names(wr)<-c("df","df1")
      p <- ggplot(wr, aes(x=df, y=df1, fill=df)) + geom_boxplot()
      p <- ggplotly(p)
      p
    }
    else if ((is.integer(df)|is.numeric(df))& (is.integer(df1) | is.numeric(df1))){
      wr<- data.frame(df,df1)
      names(wr)<-c("df","df1")
      plot_ly(wr,x = ~df,y= ~df1,type = "scatter",mode = 'markers')%>%
        layout(title = "scatter plot")
      
    }
    else if ((is.integer(df)|is.numeric(df)) & is.factor(df1)) {
      wr1<- data.frame(df,df1)
      wr<-na.omit(wr1)
      names(wr)<-c("df","df1")
      p <- ggplot(wr, aes(x=df1, y=df, fill=df1)) + geom_boxplot()
      p <- ggplotly(p)
      p
      
    }
  })
  #--stratified tabs--plot----  
  output$plot3<-renderPlotly({
    df11<- v$data
    df11<- df11[,input$variable11]
    df12<- v$data
    df12<- df12[,input$variable12]
    df13<- v$data
    df13<- df13[,input$variable13]
    if(is.factor(df11) & is.factor(df12) & is.factor(df13)){
      w<-data.frame(df11,df12,df13)
      names(w)<-c("df","df1","df2")
      a<- ggplot(data = w) +
        geom_mosaic(aes(x = product(df, df1), fill=df2), na.rm=TRUE) +
        labs(x = "", title='mosaic plot')+theme(axis.text.x = element_text(angle = 55, hjust = 1))
      a<-ggplotly(a)
      a
    }
    else if (is.factor(df11) & is.factor(df12) & (is.integer(df13) | is.numeric(df13)) ){
      
      w<-data.frame(df11,df12,df13)
      f<-na.omit(w)
      
      names(f)<-c("df","df1","df2")
      t<- ggplot(f, aes(x = df, y = df2, fill = df1) ) +
        geom_bar(stat="identity", position = 'dodge') +theme(axis.text.x = element_text(angle = 55, hjust = 1))
      t<-ggplotly(t)
      t
    }
    else if (is.factor(df11) & (is.numeric(df12) | is.integer(df12)) & is.factor(df13)) {
      
      ae<-data.frame(df11,df12,df13)
      f<-na.omit(ae)
      names(f)<-c("df","df1","df2")
      pq<- ggplot(f, aes(df, df1)) +
        geom_boxplot(aes(color = df2 ))+
        facet_wrap(~df2)
      pq<-ggplotly(pq)
      pq
    }
    else if ((is.integer(df11)| is.numeric(df11)) & is.factor(df12) & is.factor(df13)){
      aa<-data.frame(df11,df12,df13)
      f<-na.omit(aa)
      names(f)<-c("df","df1","df2")
      pq<- ggplot(f, aes(df1, df)) +
        geom_boxplot(aes(color = df2 ))+
        facet_wrap(~df2)
      pq<-ggplotly(pq)
      pq
    }
    
    else if ((is.integer(df11) | is.numeric(df11)) & (is.integer(df12) | is.numeric(df12)) & is.factor(df13)){
      e<-data.frame(df11,df12,df13)
      f<-na.omit(e)
      names(f)<-c("df","df1","df2")
      q <- ggplot(f, aes(df, df1)) +
        geom_point(aes(color = df2), size = 3, alpha = 0.6) +
        facet_wrap(~df2)
      q<-ggplotly(q)
      q
      
    }
    else if((is.integer(df11) | is.numeric(df11)) & is.factor(df12) & (is.integer(df13) | is.numeric(df13))){
      w<-data.frame(df11,df12,df13)
      f<-na.omit(w)
      names(f)<-c("df","df1","df2")
      
      p<-plot_ly(f, x = ~df, y = ~interaction(df1, df2)) %>%
        add_boxplot(color = ~df1) %>%
        layout(yaxis = list(title = ""))
      p
    }
    else if (is.factor(df11) & (is.integer(df12) | is.numeric(df12)) & (is.integer(df13) | is.numeric(df13))){
      w<-data.frame(df11,df12,df13)
      f<-na.omit(w)
      names(f)<-c("df","df1","df2")
      
      p<-plot_ly(f, x = ~df2, y = ~interaction(df1, df)) %>%
        add_boxplot(color = ~df) %>%
        layout(yaxis = list(title = ""))
      p
    }
    else {
      print("NULL")
    }
  })
  #   else if ((is.integer(df11) | is.numeric(df11)) & (is.integer(df12) | is.numeric(df12)) & (is.integer(df13) | is.numeric(df13))){
  #     w<-data.frame(df11,df12,df13)
  #     names(w)<-c("df","df1","df2")
  #     p <- plot_ly(w, x = ~df, y = ~df1, z = ~df2, colors = c('#BF382A', '#0C4B8E')) %>%
  #       add_markers() %>%
  #       layout(title = "scatter plot")
  #     p
  #     
  #     
  #   }
  #trend plot-------------------------------
  output$trend<-renderPlotly({
    options(digits=4)
    df41<- v$data
    df42<- v$data
    df43<-v$data
    df41<- df41[,input$variable41]
    df42<- df42[,input$variable42]
    df43<- df43[,input$variable43]
    
    if((is.integer(df41) | is.numeric(df41)) & (is.integer(df42) | is.numeric(df42)|is.factor(df42)| is.character(df43)) & (is.integer(df43) | is.numeric(df43)|is.factor(df43)| is.character(df43))){
      
      ds<-aggregate(df41~df42+df43,data=v$data,mean)
      names(ds)<-c("year","unit1","df")
      p<- ggplot(ds, aes(x = year, y = df)) + geom_line(aes(color = unit1), size = 1)
      p<-ggplotly(p)
      p
      
    }
    else if((is.integer(df41) | is.numeric(df41)| is.factor(df41)| is.character(df41)) & (is.integer(df42) | is.numeric(df42)|is.factor(df42)| is.character(df43)) & (is.integer(df43) | is.numeric(df43)|is.factor(df43)| is.character(df43))){
      
      df<-data.frame(df41,df42,df43)
      names(df)<-c("df1","year","unit1")
      
      a<-as.data.frame(table(df[,c("df1","year","unit1")]))
      q <- ggplot(a,aes(x=year, y=Freq, colour=df1, group=df1)) +
        geom_line() +
        facet_wrap(~unit1, scales="free_y")
      
      q<-ggplotly(q)
      q
      
    }
  })
  #--risk plot----  
  output$riskplot1<-renderPlotly({
    df31<- v$data
    df32<- v$data
    df31<- df31[,input$variable31]
    df32<- df32[,input$variable32]
    ee<-data.frame(df31,df32)
    names(ee)<-c("df1","df2")
    g<- ggplot(data = ee) +
      geom_mosaic(aes(x = product(df1), fill = df2), na.rm=TRUE) +
      labs(x='', title='mosaic plot') 
    g<- ggplotly(g)
    g
  })
  #--Associate tabs--t.test--  
  output$sss<-renderPrint({
    dg<-v$data
    df71<-v$data
    options(digits = 5)
    df72<-data.frame(df71$BABY.WEIGHT1,df71$AGE1,df71$MOTHER.HEIGHT1,df71$MOTHER.HEIGHT,df71$PLACENTA.WEIGHT.Gms.1,df71$GEST.WEEKS1)
    names(df72)<-c("BABY.WEIGHT1","AGE1","MOTHER.HEIGHT1","MOTHER.WEIGHT1","PLACENTA.WEIGHT.Gms.1","GEST.WEEKS1")
    
    df73<-df72[,input$variable73]
    
    t<-t.test(df73~dg$BABY.SEX)
    t1<-t.test(df73~dg$BOOKED)
    t2<-t.test(df73~dg$ONSET.OF.LABOUR)
    t3<-t.test(df73~dg$LSCS.TYPE)
    p<-data.frame(A = round(t$estimate[[1]], digits = 4),
                  B = round(t$estimate[[2]], digits = 4),
                  CI = paste('(',round(t$conf.int[[1]], 
                                       digits = 4),', ',
                             round(t$conf.int[[2]], 
                                   digits = 4), ')',
                             sep=""), pvalue = t$p.value,
                  A1 = round(t1$estimate[[1]], digits = 4),
                  B1 = round(t1$estimate[[2]], digits = 4),
                  CI1 = paste('(',round(t1$conf.int[[1]], 
                                        digits = 4),', ',
                              round(t1$conf.int[[2]], 
                                    digits = 4), ')',
                              sep=""), pvalue1 = t1$p.value,
                  
                  A2 = round(t2$estimate[[1]], digits = 4),
                  B2 = round(t2$estimate[[2]], digits = 4),
                  CI2 = paste('(',round(t2$conf.int[[1]], 
                                        digits = 4),', ',
                              round(t2$conf.int[[2]], 
                                    digits = 4), ')',
                              sep=""), pvalue2 = t2$p.value,
                  A3 = round(t3$estimate[[1]], digits = 4),
                  B3 = round(t3$estimate[[2]], digits = 4),
                  CI3 = paste('(',round(t3$conf.int[[1]], 
                                        digits = 4),', ',
                              round(t3$conf.int[[2]], 
                                    digits = 4), ')',
                              sep=""), pvalue3 = t3$p.value)
    
    
    p$CI<-as.character(p$CI)
    p$CI1<-as.character(p$CI1)
    p$CI2<-as.character(p$CI2)
    p$CI3<-as.character(p$CI3)
    
    variable1<-c("SEX","","BOOKED","","ONSET.OF.LABOUR","","LSCS.TYPE","")
    label1<-c("M","F","I","O","INDUCED","SPONTANEOUS","ELECTIVE","EMERGENCY")
    
    mean1<-c(1,2,3,4,5,6,7,8)
    
    pvalue2<-c(1,2,3,4,5,6,7,8)
    
    diff<-c("","","","","","","","")
    
    confidence.interval1<-c("_","_","_","_","_","_","_","_")
    
    adj_pvalue<-c("_","_","_","_","_","_","_","_")
    
    df11<-data.frame(variable1,label1,mean1,pvalue2,diff,confidence.interval1,adj_pvalue)
    names(df11)<-c("Variable","Levels","mean","Pvalue","Difference","Confidence.interval","Adj_pvalue")
    
    df11$mean<-c(p$A,p$B,p$A1,p$B1,p$A2,p$B2,p$A3,p$B3)
    
    df11$Pvalue<-c(p$pvalue,"",p$pvalue1,"",p$pvalue2,"",p$pvalue3,"")
    
    df11$Confidence.interval<-c(p$CI,"",p$CI1,"",p$CI2,"",p$CI3,"")
    
    #df11<-data.frame(variable1,label1,mean1,pvalue2,diff,confidence.interval1,adj_pvalue)
    
    df22<-df11
    
    #---
    
    p1<-aov(df73~dg$MODE.OF.INDUCTION)
    p2<-summary(p1)[[1]][["Pr(>F)"]]
    a<-aggregate(df73~dg$MODE.OF.INDUCTION,FUN="mean")
    names(a)<-c("mode.of.induction","weight")
    variable1<-c("MODE.OF.INDUCTION","","","")
    
    label1<-c("FOLEYS","NONE","PGE1","PGE2")
    
    mean1<-c(a$weight[1],a$weight[2],a$weight[3],a$weight[4])
    
    pvalue2<-c(p2[1],"","","")
    
    diff<-c("","","","")
    
    confidence.interval1<-c("_","_","_","_")
    adj_pvalue<-c("_","_","_","_")
    
    df2<-data.frame(variable1,label1,mean1,pvalue2,diff,confidence.interval1,adj_pvalue)
    names(df2)<-c("Variable","Levels","mean","Pvalue","Difference","Confidence.interval","Adj_pvalue")
  
    p1<-aov(df73~dg$unit1)
    
    p2<-summary(p1)[[1]][["Pr(>F)"]]
    
    p22<-round(p2[1],digits = 4)
    t<-TukeyHSD(p1)
    
    p3<-aov(df73~dg$BLOOD.LOSS)
    
    p4<-summary(p3)[[1]][["Pr(>F)"]]
    
    p44<-round(p4[1],digits = 4)
    t1<-TukeyHSD(p3)
    a<-aggregate(df73~dg$unit1,FUN="mean")
    names(a)<-c("unit","weight")
    
    a1<-aggregate(df73~dg$BLOOD.LOSS,FUN="mean")
    names(a1)<-c("blood","weight")
    
    Cf = paste('(',round(t$`dg$unit1`[4], 
                         digits = 4),', ',
               round(t$`dg$unit1`[7], 
                     digits = 4), ')',
               sep="")
    
    Cf1 = paste('(',round(t$`dg$unit1`[5], 
                          digits = 4),', ',
                round(t$`dg$unit1`[8], 
                      digits = 4), ')',
                sep="")
    
    Cf2 = paste('(',round(t$`dg$unit1`[6], 
                          digits = 4),', ',
                round(t$`dg$unit1`[9], 
                      digits = 4), ')',
                sep="")
    
    
    variable1<-c("UNIT","","","BLOOD LOSS","","")
    
    label1<-c("OG3","OG4","OG5","< 500 M","> 1000 ML","500 - 1000 ML")
    
    mean1<-c(a$weight[1],a$weight[2],a$weight[3],a1$weight[1],a1$weight[2],a1$weight[3])
    
    pvalue2<-c(p22,"","",p44,"","")
    l1<-round(t$`dg$unit1`[1],digits = 4)
    l2<-round(t$`dg$unit1`[2],digits = 4)
    l3<-round(t$`dg$unit1`[3],digits = 4)
    l4<-round(t$`dg$unit1`[10],digits = 4)
    l5<-round(t$`dg$unit1`[11],digits = 4)
    l6<-round(t$`dg$unit1`[12],digits = 4)
    
    diff<-c("_","_","_","_","_","_")
    
    confidence.interval1<-c("_","_","_","_","_","_")
    adj_pvalue<-c("_","_","_","_","_","_")
    
    df1<-data.frame(variable1,label1,mean1,pvalue2,diff,confidence.interval1,adj_pvalue)
    names(df1)<-c("Variable","Levels","mean","Pvalue","Difference","Confidence.interval","Adj_pvalue")
    
    p1<-aov(df73~dg$PERINIUM)
    p2<-summary(p1)[[1]][["Pr(>F)"]]
    a<-aggregate(df73~dg$PERINIUM,FUN="mean")
    names(a)<-c("perineum","weight")
    variable1<-c("PERINEUM","","","","","")
    
    label1<-c("Degree of tear I","Degree of tear II","Degree of tear III","Degree of tear IV","Intact","RMLE")
    
    mean1<-c(a$weight[1],a$weight[2],a$weight[3],a$weight[4],a$weight[5],a$weight[6])
    
    pvalue2<-c(p2[1],"","","","","")
    
    diff<-c("","","","","","")
    
    confidence.interval1<-c("_","_","_","_","_","_")
    adj_pvalue<-c("_","_","_","_","_","_")
    
    df3<-data.frame(variable1,label1,mean1,pvalue2,diff,confidence.interval1,adj_pvalue)
    names(df3)<-c("Variable","Levels","mean","Pvalue","Difference","Confidence.interval","Adj_pvalue")
   
    p1<-aov(df73~dg$STILLBORN)
    p1
    p2<-summary(p1)[[1]][["Pr(>F)"]]
    a<-aggregate(df73~dg$STILLBORN,FUN="mean")
    names(a)<-c("STILLBORN","weight")
    variable1<-c("STILLBORN","","","","","")
    
    label1<-c("0","1","2","3","4","5")
    
    mean1<-c(a$weight[1],a$weight[2],a$weight[3],a$weight[4],a$weight[5],a$weight[6])
    
    pvalue2<-c(p2[1],"","","","","")
    
    diff<-c("","","","","","")
    
    confidence.interval1<-c("_","_","_","_","_","_")
    adj_pvalue<-c("_","_","_","_","_","_")
    
    df4<-data.frame(variable1,label1,mean1,pvalue2,diff,confidence.interval1,adj_pvalue)
    names(df4)<-c("Variable","Levels","mean","Pvalue","Difference","Confidence.interval","Adj_pvalue")
    #-----
    p1<-aov(df73~dg$ABORTION)
    p2<-summary(p1)[[1]][["Pr(>F)"]]
    a<-aggregate(df73~dg$ABORTION,FUN="mean")
    names(a)<-c("ABORTION","weight")
    variable1<-c("ABORTION","","","","","","","","")
    
    label1<-c("0","1","2","3","4","5","6","7","8")
    
    mean1<-c(a$weight[1],a$weight[2],a$weight[3],a$weight[4],a$weight[5],a$weight[6],a$weight[7],a$weight[8],a$weight[9])
    
    pvalue2<-c(p2[1],"","","","","","","","")
    
    diff<-c("","","","","","","","","")
    
    confidence.interval1<-c("_","_","_","_","_","_","_","_","_")
    adj_pvalue<-c("_","_","_","_","_","_","_","_","_")
    
    df5<-data.frame(variable1,label1,mean1,pvalue2,diff,confidence.interval1,adj_pvalue)
    names(df5)<-c("Variable","Levels","mean","Pvalue","Difference","Confidence.interval","Adj_pvalue")
   
    p1<-aov(df73~dg$DELIVERY.TYPE)
    p2<-summary(p1)[[1]][["Pr(>F)"]]
    a<-aggregate(df73~dg$DELIVERY.TYPE,FUN="mean")
    names(a)<-c("DELIVERY.TYPE","weight")
    variable1<-c("DELIVERY.TYPE","","","","","","","","","","")
    
    label1<-c("Assisted breech","Breech extraction","Hysterotomy","Laparotomy","Low mid forceps","Normal","Outlet Forceps","Primary LSCS","Repeat LSCS","Sponteneous Breech","Vaccum")
    
    mean1<-c(a$weight[1],a$weight[2],a$weight[3],a$weight[4],a$weight[5],a$weight[6],a$weight[7],a$weight[8],a$weight[9],a$weight[10],a$weight[11])
    
    pvalue2<-c(p2[1],"","","","","","","","","","")
    
    diff<-c("","","","","","","","","","","")
    
    confidence.interval1<-c("_","_","_","_","_","_","_","_","_","_","_")
    adj_pvalue<-c("_","_","_","_","_","_","_","_","_","_","_")
    
    df6<-data.frame(variable1,label1,mean1,pvalue2,diff,confidence.interval1,adj_pvalue)
    names(df6)<-c("Variable","Levels","mean","Pvalue","Difference","Confidence.interval","Adj_pvalue")
    #-----
    p1<-aov(df73~dg$LIVING)
    p2<-summary(p1)[[1]][["Pr(>F)"]]
    
    a<-aggregate(df73~dg$LIVING,FUN="mean")
    names(a)<-c("LIVING","weight")
    variable1<-c("LIVING","","","","","","","","","","")
    
    label1<-c("0","1","2","3","4","5","6","7","8","9","13")
    
    mean1<-c(a$weight[1],a$weight[2],a$weight[3],a$weight[4],a$weight[5],a$weight[6],a$weight[7],a$weight[8],a$weight[9],a$weight[10],a$weight[11])
    
    pvalue2<-c(p2[1],"","","","","","","","","","")
    
    diff<-c("","","","","","","","","","","")
    
    confidence.interval1<-c("_","_","_","_","_","_","_","_","_","_","_")
    adj_pvalue<-c("_","_","_","_","_","_","_","_","_","_","_")
    
    df7<-data.frame(variable1,label1,mean1,pvalue2,diff,confidence.interval1,adj_pvalue)
    names(df7)<-c("Variable","Levels","mean","Pvalue","Difference","Confidence.interval","Adj_pvalue")
    #-----
    p1<-aov(df73~dg$PARA)
    p2<-summary(p1)[[1]][["Pr(>F)"]]
    a<-aggregate(df73~dg$PARA,FUN="mean")
    names(a)<-c("PARA","weight")
    variable1<-c("PARA","","","","","","","","","","")
    
    label1<-c("0","1","2","3","4","5","6","7","8","9","13")
    
    mean1<-c(a$weight[1],a$weight[2],a$weight[3],a$weight[4],a$weight[5],a$weight[6],a$weight[7],a$weight[8],a$weight[9],a$weight[10],a$weight[11])
    
    pvalue2<-c(p2[1],"","","","","","","","","","")
    
    diff<-c("","","","","","","","","","","")
    
    confidence.interval1<-c("_","_","_","_","_","_","_","_","_","_","_")
    adj_pvalue<-c("_","_","_","_","_","_","_","_","_","_","_")
    
    df8<-data.frame(variable1,label1,mean1,pvalue2,diff,confidence.interval1,adj_pvalue)
    names(df8)<-c("Variable","Levels","mean","Pvalue","Difference","Confidence.interval","Adj_pvalue")
    #-----
    p1<-aov(df73~dg$GRAVIDA)
    p2<-summary(p1)[[1]][["Pr(>F)"]]
    a<-aggregate(df73~dg$GRAVIDA,FUN="mean")
    names(a)<-c("GRAVIDA","weight")
    variable1<-c("GRAVIDA","","","","","","","","","","","")
    
    label1<-c("1","2","3","4","5","6","7","8","9","10","11","13")
    
    mean1<-c(a$weight[1],a$weight[2],a$weight[3],a$weight[4],a$weight[5],a$weight[6],a$weight[7],a$weight[8],a$weight[9],a$weight[10],a$weight[11],a$weight[12])
    
    pvalue2<-c(p2[1],"","","","","","","","","","","")
    
    diff<-c("","","","","","","","","","","","")
    
    confidence.interval1<-c("_","_","_","_","_","_","_","_","_","_","_","_")
    adj_pvalue<-c("_","_","_","_","_","_","_","_","_","_","_","_")
    
    df9<-data.frame(variable1,label1,mean1,pvalue2,diff,confidence.interval1,adj_pvalue)
    names(df9)<-c("Variable","Levels","mean","Pvalue","Difference","Confidence.interval","Adj_pvalue")
    #---
    variable1<-c("variable")
    
    label1<-c("label")
    mean1<-c("mean")
    pvalue2<-c("pvalue")
    diff<-c("correlation")
    confidence.interval1<-c("confidence.interval")
    adj_pvalue<-c("adj_pvalue")
    df10<-data.frame(variable1,label1,mean1,pvalue2,diff,confidence.interval1,adj_pvalue)
    names(df10)<-c("Variable","Levels","mean","Pvalue","Difference","Confidence.interval","Adj_pvalue")
    
    r<-cor.test(df73,dg$AGE1)
    k<-mean(dg$AGE1,na.rm=T)
    k1<-round(r$estimate[[1]],digits = 4)
    variable1<-"MOTHERS AGE"
    label1<-""
    mean1<-k
    pvalue2<-r$p.value
    k1<-cor(df73,dg$AGE1,use="complete.obs")
    
    diff<-paste('(',round(k1, 
                          digits = 4),')',sep="")
    CI2 = paste('(',round(r$conf.int[[1]], 
                          digits = 4),', ',
                round(r$conf.int[[2]], 
                      digits = 4), ')',
                sep="")
    
    confidence.interval1<-CI2
    adj_pvalue<-""
    df11<-data.frame(variable1,label1,mean1,pvalue2,diff,confidence.interval1,adj_pvalue)
    names(df11)<-c("Variable","Levels","mean","Pvalue","Difference","Confidence.interval","Adj_pvalue")
    
    #---
    r<-cor.test(df73,dg$MOTHER.HEIGHT1)
    k<-mean(dg$MOTHER.HEIGHT1,na.rm=T)
    k1<-round(r$estimate[[1]],digits = 4)
    variable1<-"MOTHERS HEIGHT"
    label1<-""
    mean1<-k
    pvalue2<-r$p.value
    
    k1<-cor(df73,dg$MOTHER.HEIGHT1,use="complete.obs")
    
    diff<-paste('(',round(k1, 
                          digits = 4),')',sep="")
    
    CI2 = paste('(',round(r$conf.int[[1]], 
                          digits = 4),', ',
                round(r$conf.int[[2]], 
                      digits = 4), ')',
                sep="")
    
    confidence.interval1<-CI2
    adj_pvalue<-""
    df12<-data.frame(variable1,label1,mean1,pvalue2,diff,confidence.interval1,adj_pvalue)
    names(df12)<-c("Variable","Levels","mean","Pvalue","Difference","Confidence.interval","Adj_pvalue")
    #---
    r<-cor.test(df73,dg$MOTHER.WEIGHT1)
    k<-mean(dg$MOTHER.WEIGHT1,na.rm=T)
    k1<-round(r$estimate[[1]],digits = 4)
    variable1<-"MOTHERS WEIGHT"
    label1<-""
    mean1<-k
    pvalue2<-round(r$p.value,digits = 10)
    
    k1<-cor(df73,dg$MOTHER.WEIGHT1,use="complete.obs")
    
    diff<-paste('(',round(k1, 
                          digits = 4),')',sep="")
    CI2 = paste('(',round(r$conf.int[[1]], 
                          digits = 4),', ',
                round(r$conf.int[[2]], 
                      digits = 4), ')',
                sep="")
    
    confidence.interval1<-CI2
    adj_pvalue<-""
    df13<-data.frame(variable1,label1,mean1,pvalue2,diff,confidence.interval1,adj_pvalue)
    names(df13)<-c("Variable","Levels","mean","Pvalue","Difference","Confidence.interval","Adj_pvalue")
    #---
    r<-cor.test(df73,dg$PLACENTA.WEIGHT.Gms.1)
    k<-mean(dg$PLACENTA.WEIGHT.Gms.1,na.rm=T)
    
    variable1<-"PLACENTA.WEIGHT"
    label1<-""
    mean1<-k
    pvalue2<-round(r$p.value,digits = 10)
    k1<-cor(df73,dg$PLACENTA.WEIGHT.Gms.1,use="complete.obs")
    
    diff<-paste('(',round(k1, 
                          digits = 4),')',sep="")
    
    CI2 = paste('(',round(r$conf.int[[1]], 
                          digits = 4),', ',
                round(r$conf.int[[2]], 
                      digits = 4), ')',
                sep="")
    
    confidence.interval1<-CI2
    adj_pvalue<-""
    df14<-data.frame(variable1,label1,mean1,pvalue2,diff,confidence.interval1,adj_pvalue)
    names(df14)<-c("Variable","Levels","mean","Pvalue","Difference","Confidence.interval","Adj_pvalue")
    #---
    
    r<-cor.test(df73,dg$GEST.WEEKS1)
    k<-mean(dg$GEST.WEEKS1,na.rm=T)
    
    variable1<-"GESTATIONAL WEEKS"
    label1<-""
    mean1<-k
    pvalue2<-round(r$p.value,digits = 10)
    k1<-cor(df73,dg$GEST.WEEKS1,use="complete.obs")
    
    diff<-paste('(',round(k1, 
                          digits = 4),')',sep="")
    
    CI2 = paste('(',round(r$conf.int[[1]], 
                          digits = 4),', ',
                round(r$conf.int[[2]], 
                      digits = 4), ')',
                sep="")
    
    confidence.interval1<-CI2
    adj_pvalue<-""
    df15<-data.frame(variable1,label1,mean1,pvalue2,diff,confidence.interval1,adj_pvalue)
    names(df15)<-c("Variable","Levels","mean","Pvalue","Difference","Confidence.interval","Adj_pvalue")
    
    
    new <- rbind(df22,df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12,df13,df14,df15)
  #knitr::kable(new)
  kable(new,format="markdown")
  
    
  })
#--flow chart for antenatal (sample size)----  
  output$flowchart<-renderPlot({
    data <- tibble(x= 1:100, y= 1:100)
    data %>% 
      ggplot(aes(x, y)) +
      scale_x_continuous(minor_breaks = seq(10, 100, 10)) +
      scale_y_continuous(minor_breaks = seq(10, 100, 10)) +
      theme_linedraw() ->
      p
    p +
      geom_rect(xmin = 36, xmax=64, ymin=98, ymax=104, color='black',
                fill='white', size=0.25, size=0.25) +
      annotate('text', x= 50, y=100,label= 'N = 1,00,109', size=3.5) ->
      p
    
    p +
      geom_rect(xmin = 36, xmax=64, ymin=79, ymax=85, color='black',
                fill='white', size=0.25) +
      annotate('text', x= 50, y=83,label= 'N = 98,298', size=3.5) +
      geom_rect(xmin = 70, xmax=100, ymin=87, ymax=100, color='black',
                fill='white', size=0.25) +
      annotate('text', x= 85.5, y=95,label= '(BABY DEATHS)  \n LIVING STATUS VARIABLE \n 1811 ', size=3.5) ->
      p
    
    
    p +
      geom_segment(
        x=50, xend=50, y=98, yend=85, 
        size=0.15, linejoin = "mitre", lineend = "butt",
        arrow = arrow(length = unit(1, "mm"), type= "closed")) +
      geom_segment(
        x=50, xend=69.7, y=92, yend=92, 
        size=0.15, linejoin = "mitre", lineend = "butt",
        arrow = arrow(length = unit(1, "mm"), type= "closed")) ->
      p
    
    
    
    p +
      geom_rect(xmin = 36, xmax=64, ymin=63, ymax=69, color='black',
                fill='white', size=0.25) +
      annotate('text', x= 49, y=66,label= 'N = 89,779', size=3.5)+
      geom_rect(xmin = 2, xmax=30, ymin=60, ymax=89, color='black',
                fill='white', size=0.25) +
      annotate('text', x= 16, y=75,label= 'DEMOGRAPHIC VARIABLES:\n MOTHERS AGE:278 \n GRAVIDA:0 \n PARA:0 \n MOTHERS HEIGHT:5471 \n MOTHERS WEIGHT:2769 \n LIVING:0 \n ABORTION:0 \n STILL BORN:1', size=3.1) ->
      p
    
    
    p +
      geom_segment(
        x=50, xend=50, y=79, yend=69, 
        size=0.15, linejoin = "mitre", lineend = "butt",
        arrow = arrow(length = unit(1, "mm"), type= "closed")) +
      geom_segment(
        x=50, xend=30.2, y=74, yend=74, 
        size=0.15, linejoin = "mitre", lineend = "butt",
        arrow = arrow(length = unit(1, "mm"), type= "closed")) ->
      p
    
    
    p +
      geom_rect(xmin = 36, xmax=64, ymin=42, ymax=48, color='black',
                fill='white', size=0.25) +
      annotate('text', x= 49, y=46,label= 'N = 85,513', size=3.5)+
      geom_rect(xmin = 70, xmax=100, ymin=44, ymax=70, color='black',
                fill='white', size=0.25) +
      annotate('text', x= 81.5, y=58,label= 'LABORS: \nONSET LABOR:0 \n MODE OF INDUCTION:0  \n DELIVERY TYPE:1 \n PERINEUM:3 \n BLOOD LOSS:3 \n PLACENTA WEIGHT:4259 \n APGAR1:0 \nAGPAR5:0 ', size=3.1) ->
      p
    
    
    p +
      geom_segment(
        x=50, xend=50, y=62, yend=48, 
        size=0.15, linejoin = "mitre", lineend = "butt",
        arrow = arrow(length = unit(1, "mm"), type= "closed")) +
      geom_segment(
        x=50, xend=69.7, y=53, yend=53, 
        size=0.15, linejoin = "mitre", lineend = "butt",
        arrow = arrow(length = unit(1, "mm"), type= "closed")) ->
      p
    
    
    
    p +
      geom_rect(xmin = 36, xmax=64, ymin=23, ymax=29, color='black',
                fill='white', size=0.25) +
      annotate('text', x= 49, y=27,label= 'N = 82,900', size=3.5)+
      geom_rect(xmin = 3, xmax=30, ymin=22, ymax=48, color='black',
                fill='white', size=0.25) +
      annotate('text', x= 18, y=35,label= 'BABY WEIGHT:2301 \n BABY SEX:33 \n GESTATIONAL WEEKS:279 ', size=3.1) ->
      p
    
    
    p +
      geom_segment(
        x=50, xend=50, y=42, yend=29, 
        size=0.15, linejoin = "mitre", lineend = "butt",
        arrow = arrow(length = unit(1, "mm"), type= "closed")) +
      geom_segment(
        x=50, xend=30, y=35, yend=35, 
        size=0.15, linejoin = "mitre", lineend = "butt",
        arrow = arrow(length = unit(1, "mm"), type= "closed")) ->
      p
    
    
    p +
      geom_rect(xmin = 36, xmax=64, ymin=3, ymax=9, color='black',
                fill='white', size=0.25) +
      annotate('text', x= 49, y=6,label= 'N = 82,900', size=3.5)+
      geom_rect(xmin = 70, xmax=99, ymin=7, ymax=23, color='black',
                fill='white', size=0.25) +
      annotate('text', x= 83.5, y=15,label= 'MEDICAL RISK FACTOR: \n ANEMIA,\n GDM,\n GHTN,\n PREECLAMPSIA,\n ECLAMPSIA', size=3.1) ->
      p
    
    
    
    p +
      geom_segment(
        x=50, xend=50, y=23, yend=9, 
        size=0.15, linejoin = "mitre", lineend = "butt",
        arrow = arrow(length = unit(1, "mm"), type= "closed")) +
      geom_segment(
        x=50, xend=69.7, y=16, yend=16, 
        size=0.15, linejoin = "mitre", lineend = "butt",
        arrow = arrow(length = unit(1, "mm"), type= "closed")) ->
      p
    
    
    
    p + theme_void()
    
    
  }, height = 700, width = 800)  
  
  
} 

shinyApp(ui, server)

