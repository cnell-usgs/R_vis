library(shinydashboard)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Island Biogeography",tabName="topic",icon=icon("globe",lib="glyphicon")),
    menuItem("Example",tabName="example",icon=icon("twitter", lib="font-awesome")),
    menuItem("Exercises",tabName="exercises",icon=icon("line-chart", lib="font-awesome")),
    menuItem("Learn More",tabName="lit",icon=icon("book", lib="glyphicon"))
    ,br(),br()
    
  ) 
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName="topic",
            fluidRow(
              column(width=4,
                     box(title=tags$b("Theory"), width=NULL,collapsible = TRUE,
                         p(""),
                         p("This application demonstrates how island distance and size determine island species richness and composition.")),
                     box(title = tags$b("Create Island system"),solidHeader = TRUE,width=NULL,
                         p("Consider 4 theoretical islands, use the sliders to set their distance to the mainland and size."),
                         column(width=6,
                           h4("Island Size: "),
                           sliderInput("size1","Size 1",min=1,max=30,value=3,step=TRUE,round=TRUE),
                           sliderInput("size2","Size 2",min=1,max=30,value=30,step=TRUE,round=TRUE),
                           sliderInput("size3","Size 3",min=1,max=30,value=25,step=TRUE,round=TRUE),
                           sliderInput("size4","Size 4",min=1,max=30,value=17,step=TRUE,round=TRUE)),
                         column(width=6,
                           h4("Distance from mainland: "),
                           sliderInput("dist1","Distance 1",min=1,max=50,value=5,step=FALSE),
                           sliderInput("dist2","Distance 2",min=1,max=50,value=15,step=FALSE),
                           sliderInput("dist3","Distance 3",min=1,max=50,value=40,step=FALSE),
                           sliderInput("dist4","Distance 4",min=1,max=50,value=32,step=FALSE)),br(),
                         p("Press 'Emigrate' to populate the islands with new species"))
                     ),
                     box(title=tags$b("title3"),width=NULL,collapsible = TRUE,
                         p("content 3")
                         )),
              column(width=8,
                     box(title = tags$b("Island System"),width=NULL,
                         p(""),
                         plotOutput("islands")),
                    box(title=tags$b("Species establishment"), width=NULL,collapsible = TRUE,
                        p("content 6"))
                     )
              )
             
            )
    ),
    tabItem(tabName="lit",
            fluidRow(
              column(width=6,
                     box(title=tags$b("Literature"),width=NULL,
                         p("MacArthur and Wilson. 1963. "),
                         a("See pdf", href="web address",target="_blank"))),
              column(width=6,
                     box(title=tags$b("R packages"),width=NULL,
                         p("Several R packages have been developed for XXXXX.
                           The following were used in the development of this app."),
                         h4("package"),
                         p("description."),
                         a("Documentation",href="docadd",target="_blank")),
                     box(title=tags$b("This app"), solidHeader=TRUE,status="warning",width=NULL,
                         p("This app was made by",a("Colleen Nell",href="www.collnell.com",target="_blank"),"with support from the", a("UCI Data Science Initiative summer fellowship (2016)",
                                                                                                                href="http://datascience.uci.edu/2016/06/27/2016-data-science-summer-fellow/",target="_blank")),
                         p(tags$b("Get R script for these analyses:")),br(),
                         a(href="",target="_blank"),br(),
                         p("If you are new to R, check out this",a("Intro to R cookbook for Ecologists",href="http://rpubs.com/mooneyk/213152",target="_blank")),
                         a("See code for application",href="githublink",target="_blank")
                         )
                     )
            )
    ),
    tabItem(tabName="exercises",
            column(width=6,
                   box(title=tags$b("Using the app to understand XXXX"),width=NULL,
                       h5(tags$b("steps") ),
                       p("questions"),
                       h5(tags$b("5. Go to the 'Example' tab")),
                       p("questions"),
                       p("What do you conclude from the data shown? Why?"))),
            column(width=6,
                   box(title=tags$b("Questions"),width=NULL,
                       p("here")),
                   box(title=tags$b("R-based"),width=NULL,
                       p("The following questions pertain to using the 'XXX' dataset. To access this dataset run 'data(XXX)' in your R console.
                         To answer the questions use the R script provided on the 'Learn More' tab under 'This app' to look at the data in R. Read the ",
                         a("documentation",href="XXX doc",target="_blank"), "on the XXX data set to learn more"),br(),br(),
                       p("")
                       ))),
    tabItem(tabName="example",
            fluidRow(
              column(width=5,
                     box(title=tags$b("Experimental Design"),width=NULL,
                     p("description"),br(),
                     h4(tags$b("Question:")), h4("Does tree diversity (monoculture vs polyculture) had an effect on the species richness 
                       of birds."),
                     h4(tags$b("Hypothesis:")), h4("Polyculture plots contain more species of birds than monoculture plots")
                     ), ##incldue plot images
                     box(title=tags$b("XXX"),width=NULL,br(),br(),
                         p("content,"),br()
                     )
            ),
              column(width=7,
                     box(title=tags$b("titles"),width=NULL,br(),
                         p("content")),
                    box(title=tags$b("title 9"),width=NULL,
                        p("Talk about takeaway etc"))
                    
                     )
            )
    )
    
    
  )
)
# Put them together into a dashboardPage
dashboardPage(skin = "black",
              dashboardHeader(title = "Title", titleWidth=350),
              sidebar,
              body
)