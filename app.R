

library(shiny)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(shinydashboard)
library(DT)
library(vtable)
library(skimr)
library(modeldata)
library(lmerTest)
library(arm)


df <- read.csv('dataset.csv')
df



# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Housing Exploration"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Descriptive Stats and Info about the Data" , tabName = "mod_1_tab" , icon = icon("dashboard")),
      menuItem("Visualization" , tabName = "mod_2_tab" , icon = icon("dashboard")),
      menuItem("Regression" , tabName = "mod_3_tab" , icon = icon("dashboard"))
    )
    
  ),
  dashboardBody(
    tabItems(
      #----mod1---
      tabItem(tabName = "mod_1_tab" , 
              fluidRow(
                box( 
                  sliderInput("x_1_1", label = h3("Price Range"), min = 30000, #price
                              max = 755000, value = c(100000, 300000)),
                  selectInput("x_2_1d", label = h4("Neighborhood"), 
                              choices = unique(ames$Neighborhood), multiple = TRUE),
                  actionButton("filterbtn", "Update Filter"))
                
                
              ),
              fluidRow( box(width = 12, verbatimTextOutput("stattable"))),
              fluidRow( box(width = 12 ,DT::dataTableOutput("search_match")))
              
              
      ),
      #---------Scatter plots---------------------
      tabItem(tabName = "mod_2_tab",
              fluidRow( 
                box( width = 12, 
                     textInput("title1", label = h3("Title Your Graph"), value = "Enter"),
                     selectInput("y_1xaxis", label = h3("Select X Axis Variable"), 
                                 choices = c( "Lot Area" = "LotArea",
                                              "Above Ground Sq ft " = "GrLivArea", "Price" = "SalePrice"),
                                 selected = "LotArea" ),
                     selectInput("y_1yaxis", label = h3("Select Y Axis Variable"), 
                                 choices = c("Lot Area" = "LotArea",
                                             "Above Ground Sq ft " = "GrLivArea", "Price" = "SalePrice"),
                                 selected = "Price" ),
                     selectInput("y_1color", label = h3("Choose Neighborhoods"), 
                                 choices = unique(df$Neighborhood), multiple = TRUE ),
                     plotOutput(outputId = "scatterplot"))),
              #--------- box plots-------------------
              fluidRow(        
                box( width = 12,   textInput("title2", label = h3("Title Your Graph"), value = "Enter"),
                                 selectInput("y_2xaxis", label = h3("Select X Axis Variable"), 
                                 choices = c("Zoning" = "MS_Zoning", 
                                             "Utilities"= "Utilities", "Building Type" = "Bldg_Type",
                                             "House Style" = "House_Style", "Exterior" = "Exterior_1st",
                                            "Central AC" = "Central_Air",
                                             "FullBath"= "Full_Bath", "Bedrooms" = "Bedroom_AbvGr","GarageType" = "Garage_Type"),
                                 selected = "Zoning" ),
                     selectInput("y_2yaxis", label = h3("Select Y Axis Variable"), 
                                 choices = c("Lot Area" = "Lot_Area",
                                             "Above Ground Sq ft " = "Gr_Liv_Area",
                                             "Price" = "Sale_Price"),
                                 selected = "Sale_Price" ),
                     sliderInput("y_2lim", label = h3("Boxplot Limit"), min =0 , #price
                                 max = 375000 , value = c(0, 375000)),
                plotOutput(outputId = "box"))),
              
              #---------Histograms-----
              fluidRow( 
                box(  width = 12, textInput("title3", label = h3("Title Your Graph"), value = "Enter"),
                      selectInput("y_3xaxis", label = h3("Select X Axis Variable"), 
                                  choices = c( "Lot Area" = "Lot_Area",
                                               "Above Ground Sq ft " = "Gr_Liv_Area", "Price" = "Sale_Price"),
                                  selected = "Sale_Price" ),
                      sliderInput(
                        "y_3bins", label = "Number of bins:",
                        min = 1, value = 30, max = 200
                      ),
                      plotOutput(outputId = "hist"))),
              
              #---------------Barcharts---------------------
              
              fluidRow( 
                box(  width = 12, textInput("title4", label = h3("Title Your Graph"), value = "Enter"),
                  selectInput("y_4xaxis", label = h3("Select X Axis Variable"), 
                                 choices = c("Zoning" = "Zoning", 
                                             "Utilities"= "Utilities", "Building Type" = "BldgType",
                                             "House Style" = "HouseStyle", "Exterior" = "Exterior1st",
                                             "Basement" = "Bsmt", "Central AC" = "CeNotralAir",
                                             "FullBath"= "FullBath", "Bedrooms" = "BedroomAbvGr","GarageType" = "GarageType"),
                                 selected = "Zoning" ),
                     selectInput("y_4fill", label = h3("Choose Neighborhoods"), 
                                 choices = unique(df$Neighborhood), multiple = TRUE ),
                     plotOutput(outputId = "bar"))),
          
              #---------------Counts---------------------
              
              fluidRow( 
                box( width = 12, textInput("title5", label = h3("Title Your Graph"), value = "Enter"),
                     selectInput("y_5xaxis", label = h3("Select X Axis Variable"), 
                                 choices = c("Zoning" = "Zoning", 
                                             "Utilities"= "Utilities", "Building Type" = "BldgType",
                                             "House Style" = "HouseStyle", "Exterior" = "Exterior",
                                             "Basement" = "Bsmt", "Central AC" = "CeNotralAir",
                                             "FullBath"= "FullBath", "Bedrooms" = "BedroomAbvGr","GarageType" = "GarageType"),
                                 selected = "Zoning" ),
                     selectInput("y_5yaxis", label = h3("Select Y Axis Variable"), 
                                 choices = c("Zoning" = "Zoning", 
                                             "Utilities"= "Utilities", "Building Type" = "BldgType",
                                             "House Style" = "HouseStyle", "Exterior" = "Exterior1st",
                                             "Basement" = "Bsmt", "Central AC" = "CeNotralAir",
                                             "FullBath"= "FullBath", "Bedrooms" = "BedroomAbvGr","GarageType" = "GarageType"),
                                 selected = "Zoning" )),
                plotOutput(outputId = "tile"))
              
              
      ),
      tabItem(tabName = "mod_3_tab",
              fluidRow(
                box( 
                  actionButton("reg1btn", "Run Regression Model Continous "),
                  actionButton("reg2btn", "Run Regression Model for Categorical"))
                
                
              ),
              fluidRow( box(verbatimTextOutput(outputId = "reg1")),
              fluidRow( box(verbatimTextOutput(outputId = "reg2"))))))))
    
  


server <- function(input, output,session) {
  
 
    output$search_match <- DT::renderDT(df, filter = 'top', options = list(
      pageLength = 10, fillContainer= TRUE, scrollX = TRUE))
  
  
  #-----filter function 

    filter1 <- eventReactive(eventExpr = input$filterbtn, 
                  valueExpr = {
                    filter(ames,      
                    Sale_Price >= input$x_1_1[1] & Sale_Price<= input$x_1_1[2],
                    Neighborhood ==input$x_2_1d)
                     })
    
    
    output$stattable <- renderPrint({skim(filter1())})
      

  #---------Scatter plot------
  output$scatterplot <- renderPlot({ ggplot(data = scatter_subset(), aes_string(x = input$y_1xaxis, y = input$y_1yaxis)) +
      geom_point(aes(color =  Neighborhood ))+
      ggtitle(input$title1)+
      theme(plot.title = element_text(size = 20))+
      scale_y_continuous(trans= "log10")+
      scale_x_continuous(trans = "log10")
  })
  #-------------------Boxplot--------
  output$box <- renderPlot({ ggplot(data = ames, aes_string(x = input$y_2xaxis, y = input$y_2yaxis)) +
      geom_boxplot(outlier.shape = NA)+
      coord_cartesian(ylim=c(input$y_2lim[1],input$y_2lim[2]))+
    ggtitle(input$title2)+
      theme(plot.title = element_text(size = 20))
    
  })
  #----Hist
    
    output$hist <- renderPlot({ ggplot(data = ames, aes_string(x = input$y_3xaxis)) +
        geom_histogram(bins = input$y_3bins)+
        ggtitle(input$title4)+
        theme(plot.title = element_text(size = 20))
    
    })
  #-------bar-----
  output$bar <- renderPlot({ ggplot(data = bar_subset(), aes_string(x = input$y_4xaxis)) +
      geom_bar(aes(fill = Neighborhood), position = "dodge")+
      ggtitle(input$title4)+
      theme(plot.title = element_text(size = 20))
  })
  
  #-------Tile---
     output$tile <- renderPlot({ ggplot(data = df , aes_string(x = input$y_5xaxis, y= input$y_5yaxis)) +
        geom_count()+
        ggtitle(input$title5)+
        theme(plot.title = element_text(size = 20))
     })
  
  #------reactive for scatter color
  scatter_subset <- reactive({
    req(input$y_1color)
    filter(df, Neighborhood %in% input$y_1color)
  })
  
  #------reactive for bar fill
  bar_subset <- reactive({
    req(input$y_4fill)
    filter(df, Neighborhood %in% input$y_4fill)
  })
  
  #-----Regression1 ---

  output$reg1 <- renderPrint({summary(M1a)})


  reg1result <- eventReactive(eventExpr = input$reg1btn, 
                           valueExpr = {
                             ames1 <- filter(ames, Sale_Condition == "Normal") %>%
                             M1 <- lmerTest::lmer(Sale_Price~ 1+ Neighborhood + Year_Remod_Add + Year_Built+ Lot_Area + Gr_Liv_Area+ (1|Misc_Feature), data = ames1 ) %>%
                             M1a <- arm::standardize(M1) %>%
                             summary(M1a)
                           })

  #--------Regression 2-----
  
  output$reg2 <- renderPrint({summary(M2a)})
  
  
  reg2result <- eventReactive(eventExpr = input$reg2btn, 
                              valueExpr = {
                                ames1 <- filter(ames, Sale_Condition == "Normal") %>%
                                  ames3 <- ames %>% mutate( Cond = recode(Functional, 
                                                                          "Typ" = 1,
                                                                          "Min1" = 1,
                                                                          "Min2" = 1,
                                                                          "Mod" = 0,
                                                                          "Maj1" = 0,
                                                                          "Maj2" = 0,
                                                                          "Sev" = 0,
                                                                          "Sal" = 0)) %>%
                                    ames3$Cond <- as.integer(ames3$Cond)%>%
                                    ames3$Cond <- as.factor(ames3$Cond)%>%
                                    M2a <- glm(Cond~ 1+ Neighborhood+ Gr_Liv_Area+ Total_Bsmt_SF , data = ames3, family="binomial") %>%
                                    Summary(M2a)
                              })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
