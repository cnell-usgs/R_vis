##shiny template ui
#row-based layout using shiny dashboard

library(shinydashboard)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Species Richness",tabName="sprich",icon=icon("bird",lib="glyphicon")),
    menuItem("Literature",tabName="lit",icon=icon("book", lib="glyphicon")),br(),br()
    
  ) 
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName="sprich",
            fluidRow(
              column(width=8,
                box(title = "Species Accumulation Curve",width=NULL,
                    plotOutput("rarefaction")
                ),
                box(title = "Communities",solidHeader = TRUE,width=NULL,
                    column(width=4,
                      sliderInput("srA","Species Richness A",min=0,max=100,round=TRUE,value=25,width=200),
                      sliderInput("srB","Species Richness B",min=0,max=100,round=TRUE,value=45,width=200)),
                    column(width=4,
                      sliderInput("abun_A","Abundance A",min=0,max=500,round=TRUE,value=150,width=200),
                      sliderInput("abun_B","Abundance B",min=0,max=500,round=TRUE,value=250,width=200)),
                    column(width=4,
                      sliderInput("eff_A","Sampling Effort A",min=0,max=300,round=TRUE,value=90,width=200),
                      sliderInput("eff_B","Sampling Effort B",min=0,max=300,round=TRUE,value=90,width=200),
                      actionButton("rarefy","Rarefy!"),br())
                )
              ),
              column(width=4,
                tabBox(id = "interextra",width=NULL,
                       tabPanel("Interpolate"),
                       tabPanel("Extrapolate")
                      ),
                box(title="What is species richness?", width=NULL,
                    p("Species richness is the total number of unique species in a defined area."),
                    h4("How do we compare species richness?"),
                    p("With a greater abundance of individuals, species richness increases
                      as well NEEDS WORDS. This makes it difficult to compare species richness among areaNEW WORD. To account for abundance/occurance
                      species accumulation curves can be used to look at species richness for equal abundance."),br(),
                    p("Use the controls to see how the species richness and abundance of individuals in an area interact with sampling effort, or the 
                      number of observations, and impact results estimates of species richness.GARBAGE"),
                    p("Words here that describe how/way a species accumulation curve is what it is. Should I say something about
                      rarefaction and using it as a tool of sampling effort? yes")
                )
              )
            )
          ),
    tabItem(tabName="test",
            h2("tab2 content")
    )
  )
)
# Put them together into a dashboardPage
dashboardPage(skin = "purple",
              dashboardHeader(title = "Measuring Species Richness"),
              sidebar,
              body
)