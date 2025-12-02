library(shiny)

fluidPage(
  
  titlePanel("NBA Rookies Dataset Analysis (1979-2019)", windowTitle = "NBA Rookies Dataset Analysis"),
             p("Wrigley Heide - DATA 824"),
  navlistPanel(
    "Sections:",
    
    
    tabPanel("Variable Overviews",
             h3("Descriptions of All Rookie Variables"),
p("Name: Name of the player"),
p("Team: Team where the player debuted (Some players were traded on draft night)"),
p("Year: Year of debut"),
p("Conf: Conference of the team drafted to"),
p("Age: Age of the player when drafted"),
p("Career: Number of seasons played in the NBA (up to season 2021-2022)"),
p("Games: Total games played in rookie season"),
p("MP:Minutes played"),
p("PTS: Total points scored"),
p("FG: Total field goals scored"),
p("FGA: Total field goal attempts"),
p("FGperc: Field goals' percentage of efficacy"),
p("thrP: Total 3-point field goal scored"),
p("thrPA: Total 3-point field goal attempts"),
p("thrPperc: 3-point field goals' percentage of efficacy"),
p("FT:Total free throws"),
p("FTA: Total free throw attempts"),
p("FTperc: Free throw's percentage of efficacy"),
p("ORB: Total offensive rebounds"),
p("DRB: Total defensive rebounds"),
p("TRB: Total rebounds"),
p("AST: Total assists"),
p("STL:Total steals"),
p("BLK: Total blocks"),
p("TOV: Total turnovers"),
p("Total turnovers"),
p("PF:Total personal fouls"),
p("MPpg:Average minutes played"),
p("PTSpg:Average points scored"),
p("FGpg:Average field goals scored"),
p("thrPpg: Average 3-points field goals scored"),
p("FTpg: Average free throws scored"),
p("ORBpg: Average offensive rebounds"),
p("DRBpg: Average defensive rebounds"),
p("TRBpg: Average total rebounds"),
p("ASTpg: Average assists"),
p("STLpg: Average steals"),
p("BLKpg: Average blocks"),
p("TOVpg: Average turnovers"),
p("PFpg: Average personal turnovers"),
p("More information about this particular dataset can be found here:"),
a("Dataset", href = "https://www.kaggle.com/datasets/ignaciovinuales/nba-rookies-stay-longer-than-2-years ")),
    
    
    tabPanel("Search Information by Rookie",
             textInput(inputId = "player",
                       label = "Enter Name (e.g., 'LeBron James'):",
                       value = ""),
             actionButton(inputId = "searchButton",
                          label = "Search"),
             h3("Rookie Season Numbers for Chosen Player:"),
             DTOutput("result")),

    
    
    tabPanel("Variable Summaries",tableOutput("var_sum")),
    
    
    tabPanel("Single Variable Displays",
             tags$style(type="text/css",".shiny-output-error { visibility: hidden; }",
                        ".shiny-output-error:before { visibility: hidden; }"),
             selectInput("variable", "Choose a variable for your analysis:",
                        choices = c("Age", "Career", "Games","MP","PTS","FG","FGA","FGperc","thrP","thrPA","thrPpg","FT","FTA","FTperc",
                                    "ORB","DRB","TRB","AST","STL","BLK","TOV","PF","MPpg","PTSpg","FGpg","thrPpg","FTpg","ORBpg","DRBpg",
                                    "TRBpg","ASTpg","STLpg","BLKpg","TOVpg","PFpg")),
             sliderInput("bins", "Number of Bins:", 
                         min = 5, max = 25, value = 15), 
             verbatimTextOutput("bins"),
             plotOutput("chosen_hist"),plotOutput("chosen_box")),
    
    
    tabPanel("Associations of Two Variables",
             tags$style(type="text/css",".shiny-output-error { visibility: hidden; }",
                         ".shiny-output-error:before { visibility: hidden; }"),
             selectInput("variable_x", "Choose an x variable for your plot:",
                         choices = c("Age", "Career", "Games","MP","PTS","FG","FGA","FGperc","thrP","thrPA","thrPpg","FT","FTA","FTperc",
                                     "ORB","DRB","TRB","AST","STL","BLK","TOV","PF","MPpg","PTSpg","FGpg","thrPpg","FTpg","ORBpg","DRBpg",
                                     "TRBpg","ASTpg","STLpg","BLKpg","TOVpg","PFpg")),
             selectInput("variable_y", "Choose a y variable for your plot:",
                                     choices = c("Age", "Career", "Games","MP","PTS","FG","FGA","FGperc","thrP","thrPA","thrPpg","FT","FTA","FTperc",
                                                 "ORB","DRB","TRB","AST","STL","BLK","TOV","PF","MPpg","PTSpg","FGpg","thrPpg","FTpg","ORBpg","DRBpg",
                                                 "TRBpg","ASTpg","STLpg","BLKpg","TOVpg","PFpg")),           
              plotOutput("chosen_scatterplot")),
    
    
    tabPanel("Variable Distributions by Team",
             tags$style(type="text/css",".shiny-output-error { visibility: hidden; }",
                        ".shiny-output-error:before { visibility: hidden; }"),
             
             selectInput("variable", "Choose a variable to compare among the teams:",
                         choices = c("Age", "Career", "Games","MP","PTS","FG","FGA","FGperc","thrP","thrPA","thrPpg","FT","FTA","FTperc",
                                     "ORB","DRB","TRB","AST","STL","BLK","TOV","PF","MPpg","PTSpg","FGpg","thrPpg","FTpg","ORBpg","DRBpg",
                                     "TRBpg","ASTpg","STLpg","BLKpg","TOVpg","PFpg")),
             plotOutput("comp_box")),
    
    
    tabPanel("Correlation and PCA",
             fluidRow(
               splitLayout(cellWidths = c("50%", "50%"), plotOutput("corrs"), plotOutput("pca_2d"))),
             plotOutput("pca_bars"),tableOutput("loadings"),
             fluidRow(
               splitLayout(cellWidths = c("50%", "50%"),plotOutput("dim1"), plotOutput("dim2"))),
             plotOutput("two_dims_plot")),
    
    
    tabPanel("Clustering Draft Classes",
             selectInput("year", "Choose a draft class to cluster:",
                         choices = bb$Year),
             selectInput("method", "Choose a clustering method:",
                         choices = c("single", "complete","average")),
             p("Labels 1,2,... correspond to draft order"),
             plotOutput("clust"),
              sliderInput("cutpoint", "Cut-off Point:", 
                min = 0, max = 2500, value = 100),
             p("Number of players in each cluster at chosen cutpoint:"),
            tableOutput("cut"))
             
)
)

