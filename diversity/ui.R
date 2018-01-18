library(shinydashboard)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Calculating Diversity",tabName="topic",icon=icon("calculator",lib="font-awesome")),
    menuItem("Learn More",tabName="lit",icon=icon("book", lib="glyphicon")),
    br()
    
  ) 
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName="topic",
            fluidRow(
              column(width=4,
                     box(title=tags$b("What is diversity?"), width=NULL,collapsible = TRUE,background="navy",
                         p("Species diversity is a measure of the species within a community. Diversity incorporates both species richness and the
                           species evenness. There are several indices which express different aspects of diversity."),
                         p("This application demonstrates how diversity measures are calcualted using 2 theoretical communities controlled in the 'Measuring Diversity' tab.")),
                     box(title = tags$b("Measuring Diversity"),width=NULL,
                         p("Enter abundances of 6 species below found for 2 communities. Pressing 'Calculate Diversity'
                           will show several ways community diversity can be quantified."),
                         column(width=2,br(),br(),br(),
                                h5(tags$b("Sp1:")),br(),br(),
                                h5(tags$b("Sp2:")),br(),br(),
                                h5(tags$b("Sp3:")),br(),
                                h5(tags$b("Sp4:")),br(),br(),
                                h5(tags$b("Sp5:")),br(),br(),
                                h5(tags$b("Sp6:"))),
                                
                         column(width=5,
                                h4(tags$b("Community A")),
                           numericInput("sp1_a"," ",value=1,min=0),
                           numericInput("sp2_a"," ",value=1,min=0),
                           numericInput("sp3_a"," ",value=1,min=0),
                           numericInput("sp4_a"," ",value=1,min=0),
                           numericInput("sp5_a"," ",value=1,min=0),
                           numericInput("sp6_a"," ",value=1,min=0)),
                         column(width=5,
                                h4(tags$b("Community B")),
                                numericInput("sp1_b"," ",value=1,min=0),
                                numericInput("sp2_b"," ",value=1,min=0),
                                numericInput("sp3_b"," ",value=1,min=0),
                                numericInput("sp4_b"," ",value=1,min=0),
                                numericInput("sp5_b"," ",value=1,min=0),
                                numericInput("sp6_b"," ",value=1,min=0)),
                         actionButton("shann","Calculate Diversity")
                     ),
                     box(title=tags$b("Diversity Indices"),width=NULL,background="navy",
                         p("A diversity index is used to quantify species diversity in an ecological community. An index goes beyond species richness 
                           by incorporating species evenness, or the relative abundance of species. In doing so, indices inform us about species rarity and community structure."))
                         ),
              column(width=8,
                     box(title = tags$b("Species Relative Abundance"),width=NULL,status="primary",
                         plotOutput("plot1",height=200),br(),br(),br(),
                         dataTableOutput("divtable"),
                         p("Table 1: Diveristy metrics for 2 communities including total abundance, species richness (S), Shannon diversity (H), Simpson diversity (D), and evenness (E) for two communities.")),
                            tabBox(title=tags$b("Metrics"),width=NULL,id="calcbox",
                             tabPanel("Shannon Diversity", 
                                     h4(tags$b("Shannon (H): ")), 
                                     uiOutput("H"),
                                     p(tags$b("Shannon diversity"),"incorporates both species richness and evenness, where",tags$i("p",tags$sub("i")),"is the proportion of species",tags$i("i")),
                                     p(tags$b("Scale:"), "typical values are between 1.5 and 3.5, rarely greater than 4. Larger H reflects greater richness and evenness."),
                                     dataTableOutput("shantable"),
                                     p("Table 2: Species richness (S), species proportions (p",tags$sub('1-6'),") for each species (Sp",tags$sub('1-6'),"), and Shannon diversity (H) for the two communities."),
                                     column(width=6,
                                            htmlOutput("h_a"),htmlOutput("shan_calca")),
                                     column(width=6,
                                            htmlOutput("h_b"),htmlOutput("shan_calcb")),br(),br(),br(),
                                     checkboxInput("showcalch","Show calculation",value=FALSE),
                                     p("Using the 'vegan' package in R:",tags$code("diversity(x, index='shannon')"),br(),"Where ",tags$code("x"), "is a ",tags$a("site x species community matrix",
                                                                                                                                                                 href="https://collnell.shinyapps.io/dissimilarity/",target="_blank")),
                                     a("See R code for these analyses",href="https://raw.githubusercontent.com/biRdandfRog/diversity/master/Diversity_indices.R",target="_blank")),
                             tabPanel("Simpson Diversity",
                                      h4(tags$b("Simpson (D):")),
                                      uiOutput("D"),
                                      p(tags$b("Simpson's D")," is a measure of species dominance and richness. This cna be thought of as the probability that two individuals randomly selected
                                        will belong to different species."),
                                      p(tags$b("Scale:"),"D increases with evenness (E) and richness (S) ranging from 0 to 1. Simpson is less sensitive to S than H is."),
                                      dataTableOutput("simptable"),
                                      p("Table 3: Species richness (S), species proportions (p",tags$sub('1-6'),") for each species (Sp",tags$sub('1-6'),"), and Simpson diversity (H) for the two communities."),
                                      column(width=6,
                                             htmlOutput("d_a"),htmlOutput("simp_calca")),
                                      column(width=6,
                                             htmlOutput("d_b"),htmlOutput("simp_calcb")),br(),br(),br(),
                                      checkboxInput("showcalcd","Show calculation",value=FALSE),
                                      p("Using the 'vegan' package in R:",tags$code("diversity(x, index='simpson')"),br(),"Where ",tags$code("x"), " is a",tags$a("site x species community matrix",
                                                                                                                                                                  href="https://collnell.shinyapps.io/dissimilarity/",target="_blank")),
                                      a("See R code for these analyses",href="https://raw.githubusercontent.com/biRdandfRog/diversity/master/Diversity_indices.R",target="_blank")),
                             tabPanel("Evenness",
                                      h4(tags$b("Evenness (E)")),
                                      uiOutput('E'), 
                                      p(tags$b("Evenness")," is a measure of relative species abundance. This can be calculated by multiplying H (shannon diversity), by S (species richness)"),
                                      p(tags$b("Scale:"),"0 to 1; 1 indicating species present in equal proportion, 0 representative of a community with a few common species and several rare species"),
                                      dataTableOutput("eventable"),
                                      HTML(paste("Table 4: Relative species abundances for Sp",tags$sub('1-6')," as P",tags$sub('1-6'),". Species proportions for each community total 1.",sep="")),br(),
                                      column(width=6,
                                             htmlOutput("e_a"),htmlOutput("even_calca")),
                                      column(width=6,
                                             htmlOutput("e_b"),htmlOutput("even_calcb")),br(),br(),br(),
                                      checkboxInput("showcalce","Show calculation",value=FALSE),
                                      p("Using the 'vegan' package in R:",tags$code("diversity(x, index='shannon')/log(specnumber(x))"),br(),"Where ",tags$code("x"), " is a ",tags$a("site x species community matrix",
                                                                                                                                                                                      href="https://collnell.shinyapps.io/dissimilarity/",target="_blank"),
                                      a("See R code for these analyses",href="https://raw.githubusercontent.com/collnell/R_vis/master/diversity/diversity_indices",target="_blank")))
                            ))
              )
             
            
    ),
    tabItem(tabName="lit",
            fluidRow(
              column(width=6,
                     box(title=tags$b("This app"), solidHeader=TRUE,status="warning",width=NULL,
                         p("This app was made by",a("Colleen Nell",href="www.collnell.com",target="_blank"),"with support from the", a("UCI Data Science Initiative summer fellowship (2016)",
                                                                                                                                       href="http://datascience.uci.edu/2016/06/27/2016-data-science-summer-fellow/",target="_blank")),
                         p("It is one part to a series of applications on", tags$b("Measuring and comparing ecological communities"), "using R."),
                         p(tags$b("Get R script for these analyses:")),
                         a("Diversity_indices",href="https://raw.githubusercontent.com/collnell/R_vis/master/diversity/diversity_indices",target="_blank"),br(),br(),
                         p("What is a",tags$a("site x species community matrix?",href="https://collnell.shinyapps.io/diversity/",target="_blank")),br(),
                         p("If you are new to R, check out this",a("Intro to R cookbook for Ecologists",href="http://rpubs.com/mooneyk/213152",target="_blank")),
                         a("See code for application",href="https://github.com/biRdandfRog/diversity",target="_blank")
                     ),
                     box(title=tags$b("Literature"),width=NULL,
                         p("Magurran, A. E. 1988. Ecological Diversity and its Measurement. Princeton University Press, Princeton, NJ."),
                         p(""),
                         a("See pdf", href="web address",target="_blank"))),
              column(width=6,
                     box(title=tags$b("R packages"),width=NULL,
                         p("Several R packages have been developed for XXXXX.
                           The following were used in the development of this app."),
                         h4("vegan"),
                         p("description."),
                         a("Documentation",href="docadd",target="_blank"))
                     )
            )
    )
    
    
  )
)
# Put them together into a dashboardPage
dashboardPage(skin = "yellow",
              dashboardHeader(title = "Measuring Diversity", titleWidth=250),
              sidebar,
              body
)