library(shiny)
fluidPage(
  titlePanel(""),
  
  tabPanel("Graf",
           sidebarPanel(
             selectInput("Kmetije", label = "Izberi vrsto kmetijskih gospodarstev", 
                         choices = unique(nov_data_napoved_cela$Kmetije))),
           mainPanel(plotOutput("graf1"))),
    
  tabPanel("Graf",
           sidebarPanel(
             selectInput("Area", label = "Izberi območje", 
                         choices = unique(Tabela_svet$Area))),
           mainPanel(plotOutput("graf2")))
)
