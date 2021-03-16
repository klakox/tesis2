library(shinydashboard)
library(readr)
library(tidyverse)
library(lubridate)
library(reshape)
library(MASS)
library(condMVNorm)
library(norm)
library(corpcor)
library(plotly)

ui <- dashboardPage(
  dashboardHeader(title = "Imputación"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Gráficas", tabName = "dashboard", icon = icon("dashboard"))
      # ,
      # menuItem("Gaussian", tabName = "widgets", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
          
                box(width =12,
                    title = "Días de la semana", solidHeader = TRUE, status = "primary",
                    
                    selectInput("input_type", "Seleccionar día",
                                c("lunes","martes","miércoles","jueves","viernes","sábado","domingo")
                    )
              )

              ),
              
            fluidRow(
              
              # tabBox(
              #   title = "Tablas de datos",
              #   # The id lets us use input$tabset1 on the server to find the current tab
              #   id = "tabset1", height = NULL, width = 6,
              #   tabPanel("Datos por hora", DT::dataTableOutput("contents")),
              #   tabPanel("Datos por día y hora",  div(style = 'overflow-x: scroll',DT::dataTableOutput("contents2"))) 
              # ),
              
              box(width =6,
                  title = "Boxplot imputación - MV", solidHeader = TRUE, status = "primary",
                  
                  plotlyOutput("plot2")
              ),
              box(width =6,
                  title = "Boxplot imputación - ES", solidHeader = TRUE, status = "primary",
                  plotlyOutput("plot33")
              )
              
              
              
           
              
            )
      )
      # ,
      # 
      # # Second tab content
      # tabItem(tabName = "widgets",
      # 
      #         fluidRow(
      # 
      #           box(width =12,
      #               title = "Días de la semana", solidHeader = TRUE, status = "primary",
      # 
      #               selectInput("input_type2", "Seleccionar día",
      #                           c("lunes","martes","miércoles","jueves","viernes","sábado","domingo")
      #               ),
      # 
      #               selectInput("input_type3", "Seleccionar",
      #                           c(1:20)
      #               )
      # 
      # 
      # 
      # 
      #           )
      # 
      # 
      #         ),
      # 
      #         fluidRow(
      # 
      #           # tabBox(
      #           #   title = "Tablas de datos",
      #           #   # The id lets us use input$tabset1 on the server to find the current tab
      #           #   id = "tabset1", height = NULL, width = 6,
      #           #   tabPanel("Datos por hora", DT::dataTableOutput("contents")),
      #           #   tabPanel("Datos por día y hora",  div(style = 'overflow-x: scroll',DT::dataTableOutput("contents2")))
      #           # ),
      # 
      #           box(width =6,
      #               title = "Prior", solidHeader = TRUE, status = "primary",
      # 
      #               plotOutput("plot3")
      #           ),
      #           box(width =6,
      #               title = "Posterior", solidHeader = TRUE, status = "primary",
      # 
      #               plotOutput("plot4")
      #           )
      # 
      # 
      # 
      # 
      # 
      #         ),
      #         fluidRow(
      # 
      #           box(width =12,
      #               title = "Hiper parametros", solidHeader = TRUE, status = "primary",
      # 
      #               sliderInput("etha", "Etha:",
      #                           min = 0.1, max = 10,
      #                           value = 0.5, step = 0.1),
      # 
      #               sliderInput("rho", "Rho:",
      #                           min = 0.01, max = 5,
      #                           value = 0.5, step = 0.05),
      # 
      #               sliderInput("sigma", "Sigma:",
      #                           min = 0.1, max = 10,
      #                           value = 0.5, step = 0.1)
      # 
      #           )
      # 
      # 
      #         )
      # 
      # 
      # 
      # 
      # 
      # 
      # 
      #         )
    )
  )
)


