##shiny template ui
#row-based layout using shiny dashboard

library(shinydashboard)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Example",tabName="example",icon=icon("stats",lib="glyphicon")),
    menuItem("test",tabName="test"),br()
  ) 
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName="example",
            fluidRow(
              box(title = "title1", width=4
                  ),
              box(title = "title2", width=4,solidHeader = TRUE,status="primary",
                  column(width=5),
                  column(width=5)
              ),
              tabBox(id = "plots",width = 4,
                     tabPanel("tab1"),
                     tabPanel("tab2")
              )
            ),
            fluidRow(
              box(title="title here"
              ),
              box(title="title3", width=6, status="primary"))
            ),
    tabItem(tabName="test",
            h2("tab2 content")
    )
  )

)
# Put them together into a dashboardPage
dashboardPage(skin = "red",
              dashboardHeader(title = "dash title"),
              sidebar,
              body
)