
packages   <- c('shiny', 'shinydashboard', 'smbinning', 'data.table', "htmltools", 
                "shinyWidgets", "htmltools", "ggplot2", "dplyr", "DT")

### Install the packages ###
if(length(setdiff(packages,rownames(installed.packages())))>0){
  install.packages(setdiff(packages,rownames(installed.packages())), dependencies = T)
}

### Load the packages in R ###
sapply(packages, require, character.only =T)


### Dashboard
UI <- 
  shinydashboard::dashboardPage(
    dashboardHeader(title = h4("WoE & IV Analysis")),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Data Import ", tabName = "Import",icon = icon("table")),
        menuItem("WoE & IV", tabName = "WoE", icon = icon("wrench", lib="glyphicon")
        ), width = 250)
      ),
    
    dashboardBody( 
      tabItems(
        tabItem(fluidRow(
          box(width=4,
              title = "Import Data & Enter Parameters",
              collapsible = T,
              collapsed = F,
              solidHeader = T,
              background = "light-blue",
              status = "primary",
              tags$head(tags$style(".progress-bar{background-color:#FFA500;}")),
              fileInput("file1", "Choose CSV File",
                                           multiple = F,
                                           accept = c("text/csv",
                                                      "text/comma-separated-values,text/plain",
                                                      ".csv")),
              tags$hr(),
              # Input: Checkbox if file has header ----
              checkboxInput("header", "Header", TRUE),
              
              tags$hr(),
              # Enter the Target variable
              textInput("Dependent.Var", "Enter the Dependent Variable Name")
          ),
          fluidRow(
            box(width=7,
                title = "View of the Table",
                collapsible = T,
                collapsed = F,
                solidHeader = T,
                background = "light-blue",
                status = "primary",DT::dataTableOutput("contents"))))
          , tabName = "Import")
        
        ,tabItem(
          h2("Weight of Evidence"),
                        sidebarPanel(
                          uiOutput("binscol"),
                          tags$hr(),
                          tags$hr(),
                          h4("Fill the below options for Automatic Binning"),
                          textInput("Rec.per.bin", "Enter the min Perc. of Records per bin", placeholder = "0.05"),
                          actionButton("Binn.auto", "Create Bins Automatically"),
                          
                          tags$hr(),
                          h4("Fill the below options for Manual Binning"),
                          textOutput("min"), textOutput("max"),
                          textInput("Cuts", "Enter the Cuts separated with single comma",placeholder = "24,48,70,100" ),
                          actionButton("Binn.manual", "Create Bins Manually")
                        ),
                        mainPanel(
                          fluidRow("Automatic binning of Variables",
                                   tabsetPanel(
                                     tabPanel( "WoE & IV Table" , DT::dataTableOutput("WoE.IV.auto")),
                                     tabPanel( "WoE Chart"      , plotOutput("WoE.auto.plot"))
                                   )),
                          tags$hr(),
                          tags$hr(),
                          tags$hr(),
                          tags$hr(),
                          fluidRow("Manual binning of Variables",
                                   tabsetPanel(
                                     tabPanel( "WoE & IV Table" , DT::dataTableOutput("WoE.IV.man")),
                                     tabPanel( "WoE Chart"      , plotOutput("woe.plot.man"))
                                   ))),tabName="WoE")
        )
    ))


########################################################################
########################## Server Part #################################

server <- shinyServer(function(input,output, session){
  
  # Importing the file #
  data_       <- reactive({
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header)
    df <- as.data.table(df)
      return(df)
  })
  
  output$contents <- DT::renderDataTable(datatable(
   data_(),
    options = list(
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
        "}"
      ), pageLength = 10, width="100%", scrollX = TRUE, hover=F))%>%
    formatStyle(columns = names(data_()),fontSize = '12px',color = 'black'))

 
  ###################################################################
  #######           Weight of Evidence
  
  var.data                      <- reactive({
    type                        <- data_()[ , sapply(data_(), function(x) class(x) %in% c("integer","numeric", "integer64")), with=F]
    all.names.num               <- names(type)
    return(all.names.num)
  })
  
  
  # A dropdown for getting the Numeric Columns #
  output$binscol               <- renderUI({
    selectInput("bincols", "Select a column to create bins:", choices = var.data())
  })
  
  output$min                  <- renderText({
    req(input$bincols)
    all.vals                  <- as.numeric(unlist(data_()[ ,input$bincols, with=F]))
    min_                      <- min(all.vals)
    min_message               <- paste("Min Value: ",min_)
    return(min_message)
  })
  
  output$max                  <- renderText({
    req(input$bincols)
    all.vals                  <- as.numeric(unlist(data_()[ ,input$bincols, with=F]))
    max_                      <- max(all.vals)
    max_message               <- paste("Max Value: ",max_)
    return(max_message)
  })
  
  
  
  ######################           Automatic Bins                  ###################
  #       Creating a reactive function to calculate Bins and WoE                 #
  bins.woe                     <- eventReactive( input$Binn.auto,{
    
    req(input$Dependent.Var)
    req(input$bincols)
    Dependent_var_name       <<- paste0(input$Dependent.Var)
    Independet_var_name      <<- paste0(input$bincols)
    records.per.bin          <<- paste0(input$Rec.per.bin)
    #print(records.per.bin)
    if(input$Rec.per.bin != ""){
      #print(paste0("Yay!",is.null(records.per.bin)))
      
      
      withProgress(message = 'Creating Auto Bins', 
                   detail = 'This may take a while....', {
                     N   <- 20
                     for(i in 1:N){
                       
                       # Update progress
                       incProgress(1/i)
                       
                       # Long Running Task
                       Sys.sleep(0.7)
                     }
                     
                     bins_auto           <<- smbinning::smbinning(df=data_()[ , c(Dependent_var_name,Independet_var_name), with=F], y= Dependent_var_name,  x=Independet_var_name, p= as.numeric(records.per.bin))
                   })
      
      cols_bins.auto      <- c('Cutpoint','CntCumGood','CntCumBad','GoodRate','BadRate','WoE','IV')
      bins_auto1          <- bins_auto$ivtable[,cols_bins.auto ]
      bins_auto1          <- bins_auto1[ 1:(nrow(bins_auto1)-2) , ]
      
      return(bins_auto1)
    }else{return(NULL)}
  })
  
  # This is to create Automatic Bins and calculate WoE & IV
  output$WoE.IV.auto        <- DT::renderDataTable(datatable(
    bins.woe(),
    
    options = list(
      initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});","}"),
      paging=FALSE, searching=FALSE, scrollX=F, scrollY=F))%>%
      formatStyle(columns = names(bins.woe()),fontSize = '12px',color = 'black')
  )
  
  # This is to create Graphs from the Automatic Binning
  output$WoE.auto.plot        <- renderPlot({
    
    gg           <- ggplot( data = bins.woe() , aes(x = bins.woe()$Cutpoint, y=bins.woe()$WoE))+
      geom_bar(aes(fill = bins.woe()$WoE < 0), stat = "identity",width = 0.2)+
      geom_hline( aes(yintercept = 0), color="white", linetype = "dashed", size=1)+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            
            # Change plot and panel background
            panel.background = element_rect(fill = 'black'),
            panel.border = element_rect(colour = "black", fill=NA, size=1))+
      scale_fill_manual(guide = FALSE, breaks = c(TRUE, FALSE), values=c("green", "red"))+
      scale_y_continuous(trans = "reverse") +
      coord_flip() +
      xlab("Bins") +
      ylab("Weight Of Evidence") +
      ggtitle("WoE Plots")
    return(gg)
  })  
  
  ################################################################################
  ################                    Manual Bins                 ################
  
  bins.woe.manual           <- eventReactive(input$Binn.manual,{
    cuts                    <<- paste0(input$Cuts)
    print(cuts)
    print(grepl(",", cuts))
    if(grepl(",", cuts)==TRUE){
      updateTextInput(session, inputId = "Cuts")
      cuts                    <<- paste0(input$Cuts)
      print(cuts)
      cuts.value              <<- as.integer(strsplit(cuts, ",")[[1]])
      print(cuts.value) 
      
      withProgress(message = 'Creating Bins Manually', 
                   detail = 'This may take a while....', {
                     N   <- 20
                     for(i in 1:N){
                       
                       # Update progress
                       incProgress(1/i)
                       
                       # Long Running Task
                       Sys.sleep(0.7)
                     }
                     bin             <<- smbinning.custom(df= data_()[ , c(Dependent_var_name,Independet_var_name), with=F], y=Dependent_var_name ,x=Independet_var_name, cuts = cuts.value)
                     
                   })
      
      cols_bins       <- c('Cutpoint','CntCumGood','CntCumBad','GoodRate','BadRate','WoE','IV')
      bins_man        <- data.frame(bin$ivtable[ ,cols_bins])
      bins_man        <- bins_man[ 1:(nrow(bins_man)-2) , ]
      return(bins_man)
    }else{return(NULL)}
    
  })
  
  
  #################################################################
  ####       A reactive function to save the Manual file      #####
  output$WoE.IV.man        <- DT::renderDataTable(datatable(
    bins.woe.manual(),
    
    options = list(
      initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});","}"),
      paging=FALSE, searching=FALSE, scrollX=F, scrollY=F))%>%
      formatStyle(columns = names(bins.woe.manual()),fontSize = '12px',color = 'black')
  )
  
  
  #################################################################
  ####          Creating a WoE Plot                            ####
  
  output$woe.plot.man      <- renderPlot({
    gg.manual           <- ggplot( data = bins.woe.manual() , aes(x = bins.woe.manual()$Cutpoint, y=bins.woe.manual()$WoE))+
      geom_bar(aes(fill = bins.woe.manual()$WoE < 0), stat = "identity",width = 0.2)+
      geom_hline( aes(yintercept = 0), color="white", linetype = "dashed", size=1)+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            # Change plot and panel background
            panel.background = element_rect(fill = 'black'),
            panel.border = element_rect(colour = "black", fill=NA, size=1))+
      scale_fill_manual(guide = FALSE, breaks = c(TRUE, FALSE), values=c("green", "red"))+
      scale_y_continuous(trans = "reverse") +
      coord_flip() +
      xlab("Bins") +
      ylab("Weight Of Evidence") +
      ggtitle("WoE Plots")
    
    return(gg.manual)
  })
  
}) 

shinyApp(ui = UI, server = server)
     
  