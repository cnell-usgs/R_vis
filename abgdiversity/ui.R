library(shinydashboard)
library(VennDiagram)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Measuring Diversity",tabName="abg",icon=icon("star",lib="glyphicon")),
    menuItem("Example",tabName="example",icon=icon("twitter", lib="font-awesome")),
    menuItem("Exercises",tabName="exercises",icon=icon("line-chart", lib="font-awesome")),
    menuItem("Learn More",tabName="lit",icon=icon("book", lib="glyphicon"))
    ,br(),br()
    
  ) 
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName="abg",
            fluidRow(
              column(width=4,
                     box(title=tags$b("Diversity"), width=NULL,collapsible = TRUE,
                         p("In ecology, species diversity is a measurement that reflects the number of different species represented in a given community. Species diversity is composed 
                           of two elements : species richness and species evenness,"),
                         p("Species diversity can be broken into 3 main types:"),
                         p(tags$b("Alpha diversity: ")),p(tags$b("Beta diversity:")),p(tags$b("Gamma diversity:"))),
                     box(title = tags$b("Alpha diversity"),solidHeader = TRUE,width=NULL,
                         p("Alpha diversity is the diversity of a single site, or the local species pool."),
                         p("Diversity can be calculated as Shannon diversity or richness, shanoon is abudnance weighted"),
                         p("Set the diversity for 2 different communities:"),
                         column(width=6,
                            numericInput("rich_A","Species richness A:",value=5, min=0, max=NA)),
                         column(width=6,
                            numericInput("rich_B","Species richness B:",value=15, min=0, max=NA)),
                         numericInput("share_AB","Number of shared species between A & B:",value=3, min=0, max=NA),
                         p("Set sp richness, evenness for 2 communities?"),
                         
                         p("Or, upload a .'csv' file"),
                         fileInput("getcsv","Upload data",multiple=FALSE,accept=c("text/csv","text/comma-separated-values,text/plain",".csv"))
                     ),
                     box(title=tags$b("Calculation of diversity"),width=NULL,collapsible = TRUE,
                         p("calculating shannon diversity, shoe equationa nd have diversity output for both communities")
                         )),
              column(width=8,
                     box(title = tags$b("Alpha, Beta, and Gamma Diversity"),width=NULL,
                         p("comparing diversity for alpha, beta, and gamma, based on what degisn? show a design fig also."),
                         plotOutput("vend",height=700)),
                     column(width=6,
                            box(title=tags$b("Beta"),width=NULL, collapsible=TRUE,
                                p("Beta diversity is the diversity along a gradient, a measure of heterogeneity in species. "),
                                p("Measures as the ratio of total number of species in a collection of sites S and the average richness per one site. Beta is 0 when there are no excess species
                                  or heterogenity between sites.  "),
                                p("Pairwise comparison of sites within the sme community"),
                                p("a is shared species, and b c unique species. =2a+b+c/2") #S=a+b+c. this is sorenson index of similarity
                            )),
                     column(width=6,
                            box(title=tags$b("Gamma diversity"), width=NULL,collapsible = TRUE,
                                p("content 6"))
                     )
              )
             
            )
    ),
    tabItem(tabName="lit",
            fluidRow(
              column(width=6,
                     box(title=tags$b("Literature"),width=NULL,
                         p("Whittaker, R.H. 1960. Vegetation of the Siskiyou Mountains, Oregon and California. Ecological Monographs, 30, 279-338."),
                         p("Whittaker, R.J. et al. 2001. Scale and species richness: towards a general, hierarchical theory of spcies diversity. Journal of Biogeography, 28: 453-470"),
                         a("See publication", href="http://onlinelibrary.wiley.com/doi/10.1046/j.1365-2699.2001.00563.x/abstract",target="_blank"))),
              column(width=6,
                     box(title=tags$b("R packages"),width=NULL,
                         p("Several R packages have been developed for measuring different aspects of diversity.
                           The following were used in the development of this app."),
                         h4("vegan"),
                         p("description."),
                         h4("betapart"),
                         p("an R package for the study of beta diversity"),
                         a("Documentation",href="http://onlinelibrary.wiley.com/doi/10.1111/j.2041-210X.2012.00224.x/full",target="_blank")),
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
dashboardPage(skin = "green",
              dashboardHeader(title = "Title", titleWidth=350),
              sidebar,
              body
)