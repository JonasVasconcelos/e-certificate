library("shiny")
library("devtools")
library("shinydashboard")
library("shinyWidgets")
library("waiter")
library("shinyalert")


title <- a(href="https://github.com/JonasVasconcelos",
           style = "font-family: monospace; color: white;",
           strong('CertificadoOnline'))
           

sidebar <- {dashboardSidebar(
  sidebarMenu(id = "tabs", 
              menuItem("Planilhas", tabName = "home", 
                       icon = icon("home")),
              
              menuItem("Parâmetros Gráficos", tabName = "graph", 
                       icon = icon("bar-chart-o"))
              )
  )
}

LWRInput <- {tabItem(tabName = "home",
                    fluidRow(
                      box(title = "Entrada dos dados",
                          status = "primary",
                          solidHeader = T,
                          collapsible = TRUE,
                          
                          fileInput("lwrfile",
                                    "Upload", 
                                    multiple = T),
                          helpText("Selecione a planilha que deseja trabalhar"),
                      )
                    ),
                    fluidRow(
                      box(title = "Data table",
                            status = "primary",
                            solidHeader = T,
                            width = 2000,
                            uiOutput("lwrtb"),
                        )
                      )
                    )}

GraphInputLWR <- {tabItem(tabName = "graph",
                        fluidRow(
                          tabBox(
                           title = "Configuração",
                           tabPanel(
                               title = "",
                               status = "primary",
                               solidHeader = T,
                               collapsible = TRUE,
                               
                               helpText("Configure os parâmetros"),
                               sliderInput("itera", "0. Aluno:",
                                           min = 1, max = 1000, value = 1, step = 1),
                               sliderInput("MargemVert", "1. Margem vertical:",
                                           min = 0, max = 105, value = 10, step = 0.1),
                               sliderInput("MargemHoriz", "2. Margem horizontal:",
                                           min = 0, max = 150, value = 10, step = 0.1),
                               sliderInput("Espacamento", "3. Espaçamento:",
                                           min = 0, max = 50, value = 6, step = 0.1),
                               sliderInput("FontSize", "4. Tamanho da fonte:",
                                           min = 0.1, max = 2, value = 1, step = 0.05),
                               sliderInput("FontSizeAluno", "5. Tamanho da fonte (Concluinte):",
                                           min = 0.1, max = 3, value = 2, step = 0.05),
                            )
                          ),
                           
                          box(
                            title = "Prévia",
                            tabPanel(
                              title = "",
                              status = "primary",
                              solidHeader = T,
                              collapsible = TRUE,
                            
                              plotOutput("plotlwr"),
                              downloadButton("downLWRplot", 
                                            "Download")
                            )
                          )
                        )
)}


shinyUI(
  dashboardPage(
    dashboardHeader( title = title),
    dashboardSidebar(sidebar),
    dashboardBody( 
      shinyjs::useShinyjs(),
      autoWaiter(html =   spin_loaders(id = 15,
                                       color = rgb(0.25, 0.25, 0.25, 0.5)),
                 color = rgb(0.1, 0.1, 0.1, 0),
                 fadeout = T),
      
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"),
      tabItems(LWRInput,
               GraphInputLWR
      )
    )
  )
)
