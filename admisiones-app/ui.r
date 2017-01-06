library(shiny)

years_vector <- c("2013-2014", "2012-2013", "2011-2012", "2010-2011", "2009-2010")
campus_vector <- c("AGUADILLA" = "AGUADILLA", "ARECIBO" = "ARECIBO", "BAYAMÓN" = "BAYAMÓN", 
                   "CAROLINA" = "CAROLINA", "CAYEY" = "CAYEY", "HUMACAO" = "HUMACAO",
                   "PONCE" = "PONCE", "RÍO PIEDRAS" = "RÍO PIEDRAS", "RUM" = "RUM", "UTUADO" = "UTUADO")

shinyUI(fluidPage(
  titlePanel(h3("Admisiones a Sistema UPR"), windowTitle = "Admisiones a Sistema UPR"),
  sidebarLayout (
    sidebarPanel(
           selectInput("campus", label = h4("Campus"), campus_vector, selected = "AGUADILLA"),
           selectInput("year", label = h4("Año Académico"), years_vector, selected = "2013-2014"),
           HTML('<b style="color: #337ab7;"><a href="mailto:carlosgg123@gmail.com" target="_blank">carlosgg123@gmail.com</a></b>'),
           width = 3
    ),
    mainPanel(
        tabsetPanel(
            tabPanel("Campus", plotOutput("campus", width = "auto", height="640px")),
            tabPanel("Populares - F", plotOutput("populares_f", width = "auto", height="800px")),
            tabPanel("Populares - M", plotOutput("populares_m", width = "auto", height="800px")),
            tabPanel("Selectivos", plotOutput("selectivos", width = "auto", height="800px")),
            tabPanel("Escuelas", plotOutput("escuelas", width = "auto", height="640px")),
            tabPanel("Escuelas Destacadas", plotOutput("escuelas_e", width = "auto", height="640px")),
            tabPanel("Tabla", dataTableOutput("DataTable")),
            tabPanel("Referencias", htmlOutput("references"))
            #tabPanel("Debug", verbatimTextOutput("debug"))
        )
    )
)))