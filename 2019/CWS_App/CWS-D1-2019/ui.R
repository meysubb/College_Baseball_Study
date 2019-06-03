#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shinydashboard)
library(shinycssloaders)
library(highcharter)


sidebar <- dashboardSidebar(sidebarMenu(
  menuItem("Overview", tabName = "dashboard", icon = icon("dashboard")),
  menuItem("Player Impact - Hitting",tabName="team",icon=icon("bar-chart-o")),
  menuItem("Additional Plots (Under Development)",tabName="add_plots",icon=icon("train")),
  menuItem("Information",tabName="description",icon=icon("info-circle"))
))

dashboard <- tabItem(tabName = "dashboard",
                     fluidRow(
                       box(title = "Conference Overview",width=12,status = "danger", 
                           solidHeader = TRUE,column(1),
                           div(class="row",
                               div(style="display:inline-block; padding-right: 20px;",
                                   selectizeInput("conf", "Choose Conference:",choices = conf_list,width='150px',
                                                  options = list(placeholder = "Type,e.g Big 12")))),
                           tags$style(type="text/css", '#foo {width: 60px; padding-left:50px;}'),
                           tags$style(type="text/css", '#bar {width: 60px;}'),
                           withSpinner(plotOutput("plot2"))
                       )),
                     fluidRow(
                       box(title="Standings",width=12,status = "danger", solidHeader = TRUE,
                           withSpinner(DT::dataTableOutput("mytable")))
                     )
)

player_hit <- tabItem("team",
                      fluidRow(
                        box(title="wOBA Consistency (3 game rolling intervals)",
                            width=12, status = "danger", solidHeader = TRUE,collapsible = TRUE,column(1),
                            div(class="row",
                                div(style="display:inline-block; padding-right: 20px;",
                                    radioButtons("select", h3("View type"),
                                                 choices=list("Top players in Conference"=1,
                                                              "Top players in team"=2))
                                ),
                                div(style="display:inline-block; padding-right: 20px;",
                                    selectizeInput("conf2", "Choose Conference:",choices = conf_list,
                                                   options = list(placeholder = "Type,e.g Big 12"))),
                                div(style="display:inline-block; padding-right: 20px;",
                                    selectizeInput("team","Choose Team",choices=team_list,
                                                   options=list(placeholder="Type,e.g Texas")))
                            ),
                            tags$style(type="text/css", '#foo {width: 60px; padding-left:50px;}'),
                            tags$style(type="text/css", '#bar {width: 60px;}'),
                            withSpinner(plotOutput("plot4")))
                      ),
                      fluidRow(
                        box(width=12,solidHeader = TRUE,title="Measuring Player Similarity via OPG",
                            status="danger",withSpinner(highchartOutput("plot5"))
                            )
                            )
)

extra_plots <- tabItem(tabName = "add_plots",
                     fluidRow(
                       box(title="Offensive Overview",width=12,status = "danger", 
                          solidHeader = TRUE,
                           withSpinner(plotOutput("plot3")))
                     ),
                     fluidRow(
                       box(title="Pitching Overview",width=12,status = "danger", solidHeader = TRUE,
                           withSpinner(withSpinner(plotOutput("plot7")))))
                     )

des_item <- tabItem(tabName = "description",
                    includeMarkdown("info.md"))


dashboardPage(skin = "red",
  dashboardHeader(title = "CWS 2019"),
  sidebar,
  dashboardBody(
    tabItems(
     dashboard,
     player_hit,
     extra_plots,
     des_item)))
    