# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(boastUtils)
library(shinyMatrix)
library(igraph)
library(dplyr)
library(shinyjs)
library(shinyWidgets)
library(ggplot2)

options(dplyr.summarise.inform = FALSE) # Prevents annoying warning message

# Define global constants and functions, load data ----
## Names for each node
nodeNames <- LETTERS[1:13]

## Set up question banks for challenge levels
bank1 <- read.csv(
  file = "Context_Bank_L1.csv",
  header = TRUE,
  stringsAsFactors = FALSE
)
bank2 <- read.csv(
  file = "Context_Bank_L2.csv",
  header = TRUE,
  stringsAsFactors = FALSE
)
bank3 <- read.csv(
  file = "Context_Bank_L3.csv",
  header = TRUE,
  stringsAsFactors = FALSE)

# Define UI for App ----
ui <- list(
  useShinyjs(),
  dashboardPage(
    skin = "blue",
    ## Header ----
    dashboardHeader(
      titleWidth = 250,
      title = "Tree Diagrams",
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        tags$a(target = "_blank", icon("comments"),
               href = "https://pennstate.qualtrics.com/jfe/form/SV_7TLIkFtJEJ7fEPz?appName=Tree_Diagrams"
        )
      ),
      tags$li(class = "dropdown",
              tags$a(href = 'https://shinyapps.science.psu.edu/',
                     icon("home")))
    ),
    ## Sidebar ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "Overview", icon = icon("tachometer-alt")),
        menuItem("Prerequisites", tabName = "Prerequisites", icon = icon("book")),
        menuItem("Explore", tabName = "Explore", icon = icon("wpexplorer")),
        menuItem("Challenge", tabName = "Challenge", icon = icon("cogs")),
        menuItem("References", tabName = "References",icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::psu_eberly_logo("reversed")
      )
    ),
    ## Body ----
    dashboardBody(
      tabItems(
        ### Set up the Overview Page ----
        tabItem(
          tabName = "Overview",
          withMathJax(),
          h1("Tree Diagrams"),
          p("In this app, you will explore tree diagrams and their uses in
            solving probability problems."),
          h2("Instructions"),
          tags$ol(
            tags$li("If necessary, review prerequisite ideas using the
                    Prerequisites tab."),
            tags$li("Explore the effects of changing probabilities in the tree
                    using the Explore tab."),
            tags$li("Challenge yourself to build trees for various contexts in
                    order to solve problems in the Challenge tab."),
          ),
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "go1",
              label = "GO!",
              size = "large",
              icon = icon("bolt")
            )
          ),
          br(),
          br(),
          h2("Acknowledgements"),
          p(
            "This app was developed and coded by Leah Hunt.",
            "The question bank was written primarily by Dennis Pearl.",
            br(),
            br(),
            br(),
            div(class = "updated", "Last Update: 11/24/2020 by LMH.")
          )
        ),
        ### Set up the Prerequisites Page ----
        tabItem(
          tabName = "Prerequisites",
          withMathJax(),
          h2("Prerequisites"),
          p("In order to get the most out of this app, please review the
            following:"),
          tags$ul(
            tags$li(
              "The basic rules of probability, which are reviewed in ",
              tags$a(href = "https://online.stat.psu.edu/stat100/lesson/7/7.1",
                     "these notes.",
                     class = "bodylinks")),
            tags$li(
              "The basic rules of conditional probability, which are reviewed in ",
              tags$a(
                href = "https://online.stat.psu.edu/stat200/lesson/2/2.1/2.1.3/2.1.3.2/2.1.3.2.5",
                "these notes.",
                class = "bodylinks")),
            tags$li(
              "The Law of Total Probability: the probability that an event
            occurs is equivalent to the sum of the probabilities of the event
            occurring along with each possibility in a partition of the sample
            space, i.e.
            \\[P(B) = \\sum_{i = 1}^j P\\left(B \\cap A_{i}\\right) = \\sum_{i = 1}^jP
            \\left(B|A_{i}\\right)P(A_{i})\\]  for events \\(A\\) and \\(B\\)
            where the \\(A_i\\)'s form a partition of \\(S\\).")
          ),
          br(),
          div(
            style = "text-align: center",
            bsButton(
              inputId = "go2",
              label = "GO!",
              size = "large",
              icon = icon("bolt")
            )
          ),
        ),
        ### Set up Explore Page ----
        tabItem(
          tabName = "Explore",
          h2("Explore"),
          p("In this section you will explore how to interpret tree diagrams.
          First, read the context and answer the question posed. Once you are
          ready to move on to the next step, click the Next Step button when you
          will be able to move the prevalence slide to change the number of
          people in the population that truly have the disease and observe the
          effects of these changes."),
          hr(),
          h3("Context"),
          p("Through the winter and spring of 2020, COVID-19 spread over the
          United States so that by the start of the summer, approximately 3% of
          all Americans had been infected with the coronavirus that causes it.
          In order to detect whether a person had been infected, a laboratory
          created a test to detect antibodies to the virus in the blood.  The
          test is able to detect antibodies in 98% of people who had been
          infected and does not detect the antibodies in 97% of people who have
            never been infected."),
          sidebarLayout(
            sidebarPanel(
              # First (and only) question for explore tab
              p("What is the probability that a randomly selected American
                           would show a positive antibody test?"),
              fluidRow(
                column(
                  width = 9,
                  numericInput(
                    inputId = "exploreAns",
                    label = "Enter your answer for the probability",
                    value = NA,
                    min = 0,
                    max = 1,
                    step = .01
                  )
                ),
                column(
                  width = 3,
                  br(),
                  uiOutput("exploreCorrectnessPic")
                )
              ),
              bsButton(
                inputId = "checkExplore",
                label = "Check Answer",
                size = "large"
              ),
              bsButton(
                inputId = "nextStep",
                label = "Next Step",
                size = "large"
              ),
              # Prevalence slider
              conditionalPanel(
                condition = "input.nextStep",
                br(),
                p("Of course, the true prevalence of the antibodies in the
                  American population is unknown and the 3% figure would also
                  vary greatly from state-to-state. Move the prevalence slider
                  to see how the probability of getting a positive test changes
                  as the prevalence moves away from 3%."),
                sliderInput(
                  inputId = "prevalence",
                  label = "Prevalence",
                  min = 0,
                  max = 100,
                  value = 3,
                  step = 1,
                  post = "%",
                  animate = animationOptions(interval = 1500)
                )
              )
            ),
            mainPanel(
              # Show Tree
              plotOutput("exploreGraph"),
              htmlOutput("exploreAlt"),
              # Outputted text for part 2
              conditionalPanel(
                condition = "input.nextStep",
                plotOutput("exploreProbPlot", height = 300),
                htmlOutput("exploreBarAlt")
              )
            )
          ),
          br(),
          div(
            style = "text-align: right;",
            bsButton(
              inputId = "exToCh",
              label = "Go to Challenge",
              size = "large",
              icon = icon("bolt")
            )
          )
        ),
        ### Set up Challenge Page ----
        tabItem(
          tabName = "Challenge",
          tabsetPanel(
            id = "levels",
            # Instructions ----
            tabPanel(
              title = "Instructions",
              value = "instructions",
              br(),
              h2("Instructions"),
              p("The following challenges will allow you to build your own trees
                from context in order to answer questions. The section is divided
                into three levels:"),
              tags$ul(
                tags$li(tags$strong("Level 1: "), "You will be given a tree
                        structure and will have to enter the probabilities for
                        each edge of the tree then answer questions based on the
                        tree."),
                tags$li(tags$strong("Level 2: "), "You will create the tree from
                        scratch, deciding both the structure of the tree and the
                        appropriate probabilities before answering questions."),
                tags$li(tags$strong("Level 3: "), "You will develop a tree from
                        scratch, but the problems will include added twists such
                        as recursive trees and edge probabilities that require
                        calculations to find. This level also disables the
                        validation messages given as hints in the first two
                        levels.")
              )
            ),
            ## Challenge Level 1 ----
            ## Give structure, make them input all probabilities,
            ## questions are "simple"
            tabPanel(
              title = "Level 1",
              value = "level1",
              br(),
              h2("Challenge: Create and Interpret Trees from Context"),
              p("In this section, you will build a probability tree from a given
                context. First choose the number of options (branches exiting)
                for each node. Then input the probabilities associated with each
                of these options. Displaying the edge labels would be helpful in
                clarifying which option should be associated with which node.
                Once you have created your tree make sure to check your work."),
              p("Next, answer several questions about the tree you created. You
                can choose whether to show the ending probabilities or challenge
                yourself to calculate them on your own."),
              p("You can try various different contexts, each of which has a set
                of questions."),
              hr(),
              h3("Context"),
              textOutput("context1"),
              br(),
              bsButton(
                inputId = "newContext1",
                label = "Next Context",
                size = "large"
              ),
              conditionalPanel(
                condition = "output.contextNumber1",
                p("Note: This is the final context currently implemented.
                  Clicking New Context will return you to the first context.")
              ),
              br(),
              br(),
              sidebarLayout(
                sidebarPanel(
                  width = 4,
                  textOutput("nodesWith2Child1"),
                  uiOutput("uMat21"),
                  textOutput("nodesWith3Child1"),
                  uiOutput("uMat31"),
                  # Check entries for edge weights
                  bsButton(
                    inputId = "checkMat1",
                    label = "Check Your Probabilities",
                    size = "large"
                  ),
                  fluidRow(
                    column(
                      width = 2,
                      uiOutput("correctnessPic1")
                    ),
                    column(
                      width = 10,
                      textOutput("correctnessText1")
                    )
                  )
                ),
                # Outputs: plot of states visited and the matrix to the n-steps power
                mainPanel(
                  width = 8,
                  # Options for displaying probabilities and edge labels
                  fluidRow(
                    column(
                      width = 6,
                      checkboxInput(
                        inputId = "displayProbs1",
                        label = "Display Leaf Node Probabilities"
                      ),
                    )
                  ),
                  # Plot tree
                  plotOutput("graph1"),
                  tags$script(HTML(
                    "$(document).ready(function() {
                    document.getElementById('graph1').setAttribute('aria-label',
                    `This plot shows the tree diagram that you created.`)
                    })"
                  )),
                  htmlOutput("graph1Alt"),
                  bsButton(
                    inputId = "newQuestion1",
                    label = "Next Question",
                    size = "large"
                  ),
                  textOutput("question1"),
                  conditionalPanel(
                    condition = "output.questionNumber1",
                    p("Note: This is the final question for this context.
                      Clicking Next Question will return you to the first
                      question.")
                  ),
                  # Answer questions about scenario
                  fluidRow(
                    column(
                      width = 3,
                      numericInput(
                        inputId = "comboProb1",
                        label = "Enter Answer",
                        value = NA,
                        min = 0,
                        max = 1,
                        step = .01,
                        width = "100%"
                      )
                    ),
                    column(
                      width = 2,
                      br(),
                      uiOutput("correctnessPicCombo1")
                    )
                  ),
                  fluidRow(
                    div(
                      id = "question1",
                      column(
                        width = 4,
                        bsButton(
                          inputId = "comboCheck1",
                          label = "Check Answer",
                          size = "large"
                        )
                      ),
                      conditionalPanel(
                        condition = "output.showAnsButton1",
                        column(
                          width = 8,
                          bsButton(
                            inputId = "showAns1",
                            label = "Show Answer",
                            size = "large"
                          ),
                          textOutput("answer1")
                        )
                      )
                    )
                  )
                )
              )
            ),
            ## Challenge Level 2 ----
            ## Student must form entire tree structure themselves
            ## Questions/probabilities are still relatively straightforward
            tabPanel(
              title = "Level 2",
              value = "level2",
              br(),
              h2("Challenge: Create and Interpret Trees from Context"),
              p("In this section, you will build a probability tree from a given
                context. First choose the number of options (branches exiting)
                for each node. Then input the probabilities associated with each
                of these options. Displaying the edge labels would be helpful in
                clarifying which option should be associated with which node.
                Once you have created your tree make sure to check your work."),
              p("Next, answer several questions about the tree you created. You
                can choose whether to show the ending probabilities or challenge
                yourself to calculate them on your own."),
              p("You can try various different contexts, each of which has a set
                of questions."),
              hr(),
              h3("Context"),
              textOutput("context2"),
              br(),
              bsButton(
                inputId = "newContext2",
                label = "Next Context",
                size = "large"
              ),
              conditionalPanel(
                condition = "output.contextNumber2",
                p("Note: This is the final context currently implemented.
                  Clicking New Context will return you to the first context.")
              ),
              br(),
              br(),
              sidebarLayout(
                sidebarPanel(
                  width = 4,
                  conditionalPanel(
                    condition = "!output.nextStep2",
                    p("Set up the tree: Choose the variables to split by and
                      number of resulting children for each node. When you think
                      you have the answer, click the check answer button. If your
                      answer is correct, you will then move on to the next part."),
                    p("Note: Some labels may be used more than once."),
                    # Inputs for all number of children
                    p("Node A"),
                    fluidRow(
                      column(
                        width = 8 ,
                        uiOutput("lab12")
                      ),
                      column(
                        width = 4,
                        selectInput(
                          inputId = "nchildA2",
                          label = "Children",
                          choices = c(2, 3),
                          width =  "90%"
                        )
                      )
                    ),
                    p("Node B"),
                    fluidRow(
                      column(
                        width = 8,
                        uiOutput("lab22")
                      ),
                      column(
                        width = 4,
                        selectInput(
                          inputId = "nchildB2",
                          label = "Children",
                          choices = c(2, 3),
                          width = '90%'
                        ),
                      )
                    ),
                    p("Node C"),
                    fluidRow(
                      column(
                        width = 8,
                        uiOutput("lab32")
                      ),
                      column(
                        width = 4,
                        selectInput(
                          inputId = "nchildC2",
                          label = "Children",
                          choices = c(2, 3),
                          width = '90%'
                        )
                      )
                    ),
                    # Only display an option for D if A has 3 children
                    conditionalPanel(
                      condition = "input.nchildA2 == 3",
                      p("Node D"),
                      fluidRow(
                        column(
                          width = 8,
                          uiOutput("lab42")
                        ),
                        column(
                          width = 4,
                          selectInput(
                            inputId = "nchildD2",
                            label = "Children",
                            choices = c(2, 3),
                            width = '90%'
                          )
                        )
                      )
                    ),
                    bsButton(
                      inputId = "checkStep2",
                      label = "Check Answer",
                      size = "large"
                    ),
                    bsButton(
                      inputId = "skip",
                      label = "Next Step",
                      size = "large"
                    ),
                    fluidRow(
                      column(
                        width = 2,
                        uiOutput("stepCheckPic")
                      ),
                      column(
                        width = 10,
                        textOutput("stepCheckFeedback")
                      ),
                    )
                  ),
                  conditionalPanel(
                    condition = "output.nextStep2",
                    # Matrices to let the user enter edge weights
                    conditionalPanel(
                      condition = "input.nchildA2 == 2 || input.nchildB2 == 2
                                  || input.nchildC2 == 2 || input.nchildD2 == 2",
                      p("Input edge probabilities below nodes with 2 children:"),
                      uiOutput("uMat22")
                    ),
                    conditionalPanel(
                      condition = "!(input.nchildA2 == 2 &&  input.nchildB2 == 2
                                   && input.nchildC2 == 2)",
                      p("Input edge probabilities below nodes with 3 children:"),
                      uiOutput("uMat32")
                    ),
                    # Check entries for edge weights
                    bsButton(
                      inputId = "checkMat2",
                      label = "Check Weights",
                      size = "large"
                    ),
                    fluidRow(
                      column(
                        width = 2,
                        uiOutput("correctnessPic2")
                      ),
                      column(
                        width = 10,
                        textOutput("correctnessText2")
                      )
                    )
                  )
                ),
                # Outputs: plot of states visited and the matrix to the n-steps power
                mainPanel(
                  width = 8,
                  conditionalPanel(
                    condition = "!output.nextStep2",
                    p("Current tree structure you have designed:"),
                    plotOutput("tempGraph2"),
                    tags$script(HTML(
                      "$(document).ready(function() {
                      document.getElementById('tempGraph2').setAttribute('aria-label',
                      `This plot shows the structure of the tree diagram your
                      responses suggest.`)
                      })"
                    )),
                  ),
                  conditionalPanel(
                    condition = "output.nextStep2",
                    # Options for displaying probabilities and edge labels
                    checkboxInput(
                      inputId = "displayProbs2",
                      label = "Display Leaf Node Probabilities"
                    ),
                    # Plot tree
                    plotOutput("graph2"),
                    htmlOutput("graph2Alt"),
                    bsButton(
                      inputId = "newQuestion2",
                      label = "Next Question",
                      size = "large"
                    ),
                    textOutput("question2"),
                    conditionalPanel(
                      condition = "output.questionNumber2",
                      p("Note: This is the final question for this context.
                        Clicking Next Question will return you to the first
                        question.")
                    ),
                    br(),
                    # Answer questions about scenario
                    fluidRow(
                      column(
                        width = 3,
                        numericInput(
                          inputId = "comboProb2",
                          label = "Enter Answer",
                          value = NA,
                          min = 0,
                          max = 1,
                          step = .01,
                          width = "100%"
                        )
                      ),
                      column(
                        width = 2,
                        br(),
                        uiOutput("correctnessPicCombo2")
                      )
                    ),
                    fluidRow(
                      div(
                        id = "question2",
                        column(
                          width = 4,
                          bsButton(
                            inputId = "comboCheck2",
                            label = "Check Answer",
                            size = "large"
                          )
                        ),
                        conditionalPanel(
                          condition = "output.showAnsButton2",
                          column(
                            width = 8,
                            bsButton(
                              inputId = "showAns2",
                              label = "Show Answer",
                              size = "large"),
                            textOutput("answer2")
                          )
                        )
                      )
                    )
                  )
                )
              )
            ),
            ## Challenge Level 3 ----
            ## Student has no initial scaffolding
            tabPanel(
              title = "Level 3",
              value = "level3",
              br(),
              p("You can try various different contexts, each of which has a set
                of questions."),
              hr(),
              h3("Context"),
              textOutput("context3"),
              bsButton(
                inputId = "newContext3",
                label = "Next Context",
                size = "large"
              ),
              conditionalPanel(
                condition = "output.contextNumber3",
                p("Note: This is the final context currently implemented.
                   Clicking New Context will return you to the first context.")
              ),
              br(),
              br(),
              sidebarLayout(
                sidebarPanel(
                  width = 4,
                  checkboxInput(
                    inputId = "shownChildOptions",
                    label = "Show options for number of children",
                    value = TRUE
                  ),
                  # Inputs for all number of children
                  conditionalPanel(
                    condition = "input.shownChildOptions",
                    fluidRow(
                      column(
                        width = 8,
                        paste("Options for node", nodeNames[1])
                      ),
                      column(
                        width = 4,
                        selectInput(
                          inputId = "nchildA3",
                          label = NULL,
                          choices = c(2, 3),
                          selected = 2,
                          width = '90%'
                        )
                      )
                    ),
                    fluidRow(
                      column(
                        width = 8,
                        paste("Options for node", nodeNames[2])
                      ),
                      column(
                        width = 4,
                        selectInput(
                          inputId = "nchildB3",
                          label = NULL,
                          choices = c(0, 2, 3),
                          selected = 2,
                          width = '90%'
                        )
                      )
                    ),
                    fluidRow(
                      column(
                        width = 8,
                        paste("Options for node", nodeNames[3])
                      ),
                      column(
                        width = 4,
                        selectInput(
                          inputId = "nchildC3",
                          label = NULL,
                          choices = c(0, 2, 3),
                          selected = 2,
                          width = '90%'
                        )
                      )
                    ),
                    # Only display an option for D if A has 3 children
                    conditionalPanel(
                      condition = "input.nchildA3 == 3",
                      fluidRow(
                        column(
                          width = 8,
                          paste("Options for node", nodeNames[4])
                        ),
                        column(
                          width = 4,
                          selectInput(
                            inputId = "nchildD3",
                            label = NULL,
                            choices = c(0, 2, 3),
                            selected = 2,
                            width = '90%'
                          )
                        )
                      )
                    )
                  ),
                  checkboxInput(
                    inputId = "showLabelOptions",
                    label = "Show options for labels",
                    value = TRUE
                  ),
                  conditionalPanel(
                    condition = "input.showLabelOptions",
                    # Matrices to let the user enter edge labels
                    conditionalPanel(
                      condition = "input.nchildA3 == 2 || input.nchildB3 == 2 ||
                                   input.nchildC3 == 2 || input.nchildD3 == 2",
                      p("Input edge labels below nodes with 2 children:"),
                      uiOutput("labelMat23"),
                    ),
                    conditionalPanel(
                      condition = "input.nchildA3 == 3 || input.nchildB3 == 3 ||
                                   input.nchildC3 == 3",
                      p("Input edge probabilities below nodes with 3 children:"),
                      uiOutput("labelMat33")
                    )
                  ),
                  conditionalPanel(
                    condition = "output.showLabelWarning",
                    p("Caution: Lengthy labels may overlap on the graph.
                      Consider using shorter labels.",
                      class = "redtext"
                    )
                  ),
                  checkboxInput(
                    inputId = "showWeightOptions",
                    label = "Show options for edge probabilities",
                    value = TRUE
                  ),
                  conditionalPanel(
                    condition = "input.showWeightOptions",
                    # Matrices to let the user enter edge weights
                    conditionalPanel(
                      condition = "input.nchildA3 == 2 || input.nchildB3 == 2 ||
                                   input.nchildC3 == 2 || input.nchildD3 == 2",
                      p("Input edge probabilities below nodes with 2 children:"),
                      uiOutput("uMat23"),
                    ),
                    conditionalPanel(
                      condition = "input.nchildA3 == 3 || input.nchildB3 == 3 ||
                                   input.nchildC3 == 3",
                      p("Input edge probabilities below nodes with 3 children:"),
                      uiOutput("uMat33")
                    )
                  ),
                  conditionalPanel(
                    condition = "output.showRecursive",
                    checkboxInput(
                      inputId = "recursionOptions",
                      label = "Show options for recursive nodes",
                      value = FALSE
                    ),
                    conditionalPanel(
                      condition = "input.recursionOptions",
                      uiOutput("recursiveButtons")
                    )
                  )
                ),
                mainPanel(
                  conditionalPanel(
                    condition = "(input.nchildB3 == 0 && input.nchildC3 != 0) ||
                                 (input.nchildA3 == 3 && input.nchildD3 != 0 &&
                                 (input.nchildB3 == 0 || input.nchildC3 == 0) )",
                    p("Caution: Filling the tree in an order that is not left to
                      right may lead nodes being rearranged causing the left and
                      right labels to be inconsistent with the graph.",
                      class = "redtext"
                    )
                  ),
                  plotOutput("graph3"),
                  tags$script(HTML(
                    "$(document).ready(function() {
                    document.getElementById('graph3').setAttribute('aria-label',
                    `This plot shows the tree diagram that you created using
                    the inputs with node A as the root node.`)
                    })"
                  )),
                  bsButton(
                    inputId = "newQuestion3",
                    label = "Next Question",
                    size = "large"
                  ),
                  textOutput("question3"),
                  conditionalPanel(
                    condition = "output.questionNumber3",
                    p("Note: This is the final question for this context.
                      Clicking Next Question will return you to the first
                      question.")
                  ),
                  # Answer questions about scenario
                  fluidRow(
                    column(
                      width = 3,
                      numericInput(
                        inputId = "comboProb3",
                        label = "Enter Answer",
                        value = NA,
                        min = 0,
                        max = 1,
                        step = .01,
                        width = "100%"
                      )
                    ),
                    column(
                      width = 2,
                      br(),
                      uiOutput("correctnessPicCombo3")
                    )
                  ),
                  fluidRow(
                    div(
                      id = "question3",
                      column(
                        width = 3,
                        bsButton(
                          inputId = "comboCheck3",
                          label = "Check Answer",
                          size = "large"
                        ),
                      ),
                      conditionalPanel(
                        condition = "output.showHintButton3",
                        column(
                          width = 2,
                          bsButton(
                            inputId = "showHint3",
                            label = "Hint",
                            size = "large"
                          )
                        ),
                        column(
                          width = 7,
                          bsButton(
                            inputId = "showAns3",
                            label = "Show Answer",
                            size = "large"
                          ),
                          textOutput("answer3")
                        ),
                        br(),
                        textOutput("tryAgain3")
                      )
                    )
                  )
                )
              )
            )
          )
        ),
        ### Set up the References Page ----
        tabItem(
          tabName = "References",
          withMathJax(),
          h2("References"),
          p(
            class = "hangingindent",
            "Attali, D. (2020), shinyjs: Easily Improve the User Experience of
            Your Shiny Apps in Seconds, R package. Available from
            https://CRAN.R-project.org/package=shinyjs"
          ),
          p(
            class = "hangingindent",
            "Bailey, E. (2015), shinyBS: Twitter bootstrap components for shiny,
            R package. Availablem from https://CRAN.R-project.org/package=shinyBS"
          ),
          p(
            class = "hangingindent",
            "Carey, R. (2019), boastUtils: BOAST Utilities, R Package. Available
            from https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p(
            class = "hangingindent",
            "Chang, W. and Borges Ribeio, B. (2018), shinydashboard: Create
            dashboards with 'Shiny', R Package. Available from
            https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(
            class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J. (2019),
            shiny: Web application framework for R, R Package. Available from
            https://CRAN.R-project.org/package=shiny"
          ),
          p(
            class = "hangingindent",
            "Csardi, G. and Nepusz, T. (2006), igraph: The igraph software
            package for complex network research, InterJournal, Complex Systems
            1695. http://igraph.org"
          ),
          p(
            class = "hangingindent",
            "Neudecker, A. (2019), shinyMatrix: Shiny Matrix Input Field, R
            package. Available from https://CRAN.R-project.org/package=shinyMatrix"
          ),
          p(
            class = "hangingindent",
            "Penn State University. 2.1.3.2.5 - Conditional Probability: STAT 200.
            Penn State: Statistics Online Courses. Available from
            https://online.stat.psu.edu/stat200/lesson/2/2.1/2.1.3/2.1.3.2/2.1.3.2.5"
          ),
          p(
            class = "hangingindent",
            "Penn State University. 7.1 - The Rules of Probability: STAT 100.
            Penn State: Statistics Online Courses. Available from
            https://online.stat.psu.edu/stat100/lesson/7/7.1"
          ),
          p(
            class = "hangingindent",
            "Perrier, V., Meyer, F., and Granjon, D. (2020), shinyWidgets:
            Custom Inputs Widgets for Shiny, R package. Available from
            https://CRAN.R-project.org/package=shinyWidgets"
          ),
          p(
            class = "hangingindent",
            "Wickham, H., François, R., Henry L., and Müller, K. (2020), dplyr:
            A Grammar of Data Manipulation, R package. Available from
            https://CRAN.R-project.org/package=dplyr"
          ),
          p(
            class = "hangingindent",
            "Wickham, W. (2016), ggplot2: Elegant graphics for data analysis,
            R Package. Springer-Verlag New York. Available from
            https://ggplot2.tidyverse.org"
          )
        )
      )
    )
  )
)


# Define server logic ----
server <- function(input, output, session) {

  ## Define reactive values ----
  # Tracks context number, question number, and when to reset the show answer options
  index <- reactiveValues(
    context1 = 1,
    question1 = 1,
    context2 = 1,
    question2 = 1,
    context3 = 1,
    question3 = 1
  )
  reset <- reactiveValues(
    weight = FALSE,
    question = FALSE,
    answer = FALSE,
    setUp2 = FALSE,
    setUpAns2 = FALSE,
    hint3 = FALSE,
    ans1 = FALSE,
    ans2 = FALSE,
    ans3 = FALSE
  )

  ## Program Info and Go Buttons ----
  ### Info button
  observeEvent(input$info, {
    sendSweetAlert(
      session = session,
      title = "Instructions",
      text = "In this app you will explore tree diagrams. First, review the
      content in the Prerequisites then explore how changing probabilities
      affect the tree. Once you are comfortable with the concepts,
      go to the challenge section to create your own trees to solve problems.",
      type = "info"
    )
  })

  ### Go button on Overview page
  observeEvent(
    eventExpr = input$go1,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "Explore"
      )
    })

  #### Go button on prereqs page
  observeEvent(
    eventExpr = input$go2,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "Explore"
      )
    })

  #### Go button on explore page
  observeEvent(
    eventExpr = input$exToCh,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "Challenge"
      )
    })

  ## Exploration Tab ----
  # Only let Next button be hit once
  observeEvent(
    eventExpr = input$nextStep,
    handlerExpr = {
      updateButton(
        session = session,
        inputId = "nextStep",
        disabled = TRUE)
    })

  edgeLabels <- eventReactive(
    eventExpr = input$prevalence,
    valueExpr = {
      c(
        input$prevalence / 100,
        1 - (input$prevalence / 100),
        0.98,
        0.02,
        0.03,
        0.97)
    },
    ignoreNULL = FALSE,
    ignoreInit = FALSE
  )
  pPos <- eventReactive(
    eventExpr = input$prevalence,
    valueExpr = {
      edgeLabels()[1] * edgeLabels()[3] + edgeLabels()[2] * edgeLabels()[5]
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )

  pNeg <- eventReactive(
    eventExpr = input$prevalence,
    valueExpr = {
      edgeLabels()[1] * edgeLabels()[4] + edgeLabels()[2] * edgeLabels()[6]
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )

  ### Explore Page's Condition Probabilities Plot ----
  output$exploreProbPlot <- renderPlot({
    data <- data.frame(
      "Name" = c(
        '"P(infected|R"^{"+"}~")"',
        '"P(not|R"^{"+"}~")"',
        '"P(infected|R"^{"\u2013"}~")"',
        '"P(not|R"^{"\u2013"}~")"'
      ),
      "Value" = round(
        x = c(
          (edgeLabels()[3] * edgeLabels()[1]) / pPos(),
          (edgeLabels()[5] * edgeLabels()[2]) / pPos(),
          (edgeLabels()[4] * edgeLabels()[1]) / pNeg(),
          (edgeLabels()[6] * edgeLabels()[2]) / pNeg()
        ),
        digits = 4
      )
    )
    nameLabels <- parse(text = sort(data$Name))
    ggplot(
      data = data,
      mapping = aes(x = Name, y = Value)
    ) +
      geom_bar(stat = "identity") +
      scale_x_discrete(labels = nameLabels) +
      geom_text(
        mapping = aes(
          label = format(x = Value, scientific = FALSE),
          y = Value + .06
        ),
        position = position_dodge(width = .9),
        vjust = -.25,
        size = 5) +
      ylim(0, 1.06) +
      coord_flip() +
      xlab("") +
      ylab("Probability") +
      labs(
        title = "Conditional Probabilities",
        subtitle = expression(
          bold(R^'+')~': Pos. Result; '~bold(R^'\u2013')~': Neg. Result '
        )
      ) +
      theme_bw() +
      theme(
        axis.text = element_text(size = 18),
        plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 18),
        panel.background = element_rect(fill = "white", color = "black"),
        legend.position = c(.89,1.07),
        legend.text = element_text(size = 14),
        plot.subtitle = element_text(size = 16)
      )
  })

  ## ARIA Label for Conditional Probability Plot ----
  output$exploreBarAlt <- renderUI({
    value <- round(
      c(
        (edgeLabels()[3] * edgeLabels()[1]) / pPos(),
        (edgeLabels()[5] * edgeLabels()[2]) / pPos(),
        (edgeLabels()[4] * edgeLabels()[1]) / pNeg(),
        (edgeLabels()[6] * edgeLabels()[2]) / pNeg()
      ),
      digits = 4
    )
    # Returned alt text
    tags$script(HTML(
      paste0("$(document).ready(function() {
             document.getElementById('exploreProbPlot').setAttribute('aria-label',
             `Bar plot showing",
             "the probability of infected given a positive result is ", value[1],
             ", the probability of not infected given a positive result is ", value[2],
             ", the probability of infected given a negative result is ", value[3],
             ", the probability of not infected given a negative result is ", value[4],
             "`)})"
      )
    )
    )
  })

  ### Explore Page Tree Diagram ----
  #### Create data frame for explore tab
  exploreDF <- reactive({
    # Creates the 6 edges of the graph
    from <- c(nodeNames[1], nodeNames[1], nodeNames[2],
              nodeNames[2], nodeNames[3], nodeNames[3])
    to <- c(nodeNames[2], nodeNames[3], nodeNames[5],
            nodeNames[6], nodeNames[8], nodeNames[9])

    # Adjusts the vertex labels for the leaf nodes if weights are being shown
    probs <- c(
      edgeLabels()[1] * edgeLabels()[3],
      edgeLabels()[1] * edgeLabels()[4],
      edgeLabels()[2] * edgeLabels()[5],
      edgeLabels()[2] * edgeLabels()[6]
    )
    probs <- format(
      x = round(x = probs, digits = 4),
      scientific = F
    )
    edgeLabelsNice <- format(
      x = round(x = edgeLabels(),  digits = 2),
      scientific = F
    )
    # Paste probabilities at bottom of tree
    for(i in 3:6){
      to[i] <- paste("\n", to[i], "\n", probs[i-2])
    }
    # Make actual data frame
    data.frame(from = from, to = to, weight = edgeLabelsNice)
  })

  # Create weighted graph for explore section
  output$exploreGraph <- renderPlot({
    df <- exploreDF()
    # Add labels
    labels <- c("Infected", "Not Infected", "Positive",
                "Negative", "Positive", "Negative")
    # Adjust spacing (making it consistent with challenge side where this is
    # necessary to give labels more room)
    for (edge in 1:6) {
      if (edge %% 2 == 1) {
        df$weight[edge] <- paste(
          "\n",
          labels[edge],
          "      \n",
          df$weight[edge],
          "      "
        ) # Move right labels further right
      } else {
        df$weight[edge] <- paste(
          "\n      ",
          labels[edge],
          "\n      ",
          df$weight[edge]
        ) # Move left labels further left
      }
    }

    # Make actual plot (the plot command is overridden by igraph)
    par(mar = c(0.5, 0, 0.5, 0))
    plot(
      igraph::graph_from_data_frame(df, directed = F),
      label = TRUE,
      edge.label = df$weight,
      edge.color = "#000000",
      edge.width = 1.5,
      vertex.label.color = "#000000",
      edge.label.color = "#000000",
      label.cex = 1.2,
      edge.label.cex = 1.2,
      vertex.color = rep(x = "#E69F0080", times = 6), # Assigns the correct color to all 6 nodes
      layout = igraph::layout_as_tree(
        igraph::graph_from_data_frame(df),
        root = 1
      )
    )
  })

  ### Explore Check Answer ----
  observeEvent(
    eventExpr = input$checkExplore,
    handlerExpr = {
      output$exploreCorrectnessPic <- renderIcon(
        icon = ifelse(
          test = !is.na(input$exploreAns) && input$exploreAns == .0585,
          yes = "correct",
          no = "incorrect"
        ),
        width = 60
      )
    }
  )

  ## Challenge Tab ----
  # Values for the selectInput so that the order puts the current context first
  # When the values are changed via the buttons
  # Naming convention: Generally names end in the level that they apply to

  ### Level 1 left panel ----
  # Text label to go with level 1's 2-child matrix (if it exists)
  output$nodesWith2Child1 <- renderText({
    if (bank1$nCA[index$context1] == 2 || bank1$nCB[index$context1] == 2 ||
        bank1$nCC[index$context1] == 2 || bank1$nCD[index$context1] == 2) {
      "Input edge probabilities below nodes with 2 children:"
    } else {
      ""
    }
  })

  # Text label to go with level 1's 3-child matrix (if it exists)
  output$nodesWith3Child1 <- renderText({
    if (bank1$nCA[index$context1] == 3 || bank1$nCB[index$context1] == 3 ||
        bank1$nCC[index$context1] == 3) {
      "Input edge probabilities below nodes with 3 children:"
    } else {
      ""
    }
  })

  ### Level 2 left panel ----
  # Select input for labels for level 2:
  output$lab12 <- renderUI({
    selectInput(
      inputId = "label12",
      label = "Label",
      choices = c(
        bank2$label1[index$context2],
        bank2$label2[index$context2],
        bank2$label3[index$context2],
        bank2$label4[index$context2]
      )
    )
  })
  output$lab22 <- renderUI({
    selectInput(
      inputId = "label22",
      label = "Label",
      choices = c(
        bank2$label1[index$context2],
        bank2$label2[index$context2],
        bank2$label3[index$context2],
        bank2$label4[index$context2]
      )
    )
  })
  output$lab32 <- renderUI({
    selectInput(
      inputId = "label32",
      label = "Label",
      choices = c(
        bank2$label1[index$context2],
        bank2$label2[index$context2],
        bank2$label3[index$context2],
        bank2$label4[index$context2]
      )
    )
  })
  output$lab42 <- renderUI({
    selectInput(
      inputId = "label42",
      label = "Label",
      choices = c(
        bank2$label1[index$context2],
        bank2$label2[index$context2],
        bank2$label3[index$context2],
        bank2$label4[index$context2]
      )
    )
  })

  # Allows the user to skip the level dealing with the labels
  observeEvent(input$skip, {
    # Sets the reset for the setup answers
    # (make sure that proper feedback always shows)
    reset$setUp2 <- TRUE
    reset$setUpAns2 <- FALSE
    # Updates select inputs to correct label answers
    updateSelectInput(
      session = session,
      inputId = "nchildA2",
      selected = bank2$nCA[index$context2]
    )
    updateSelectInput(
      session = session,
      inputId = "nchildB2",
      selected = bank2$nCB[index$context2]
    )
    updateSelectInput(
      session = session,
      inputId = "nchildC2",
      selected = bank2$nCC[index$context2]
    )
    updateSelectInput(
      session = session,
      inputId = "nchildD2",
      selected = bank2$nCD[index$context2]
    )
  })

  # Display the answer answer for label choices
  observeEvent(input$checkStep2, {
    reset$setUpAns2 <- TRUE
  })

  # Level 2 Scoring Logic ----
  #if all children numbers (excluding D) match the bank
  feedback <- eventReactive(
    eventExpr = input$checkStep2,
    valueExpr = {
      if (input$nchildA2 == bank2$nCA[index$context2] &&
          input$nchildB2 == bank2$nCB[index$context2] &&
          input$nchildC2 == bank2$nCC[index$context2]) {
        labels <- c(
          bank2$label1[index$context2],
          bank2$label2[index$context2],
          bank2$label3[index$context2],
          bank2$label4[index$context2]
        )
        if (input$nchildA2 == 3 && input$nchildD2 == bank2$nCD[index$context2]) {
          # Check labels
          if (input$label12 == labels[bank2$correctLabel1[index$context2]] &&
              input$label22 == labels[bank2$correctLabel2[index$context2]] &&
              input$label32 == labels[bank2$correctLabel2[index$context2]] &&
              input$label42 == labels[bank2$correctLabel2[index$context2]]) {
            output$stepCheckPic <- renderIcon(icon = "correct", width = 36)
            return(
              "Good Job! You are Correct! Click the Next Step button to proceed
              to the next part of the question."
            )
          } else if (bank2$nCA[index$context2] == bank2$nCB[index$context2]) {
            # If labels are swapped in the case where there are the same number
            # of children at both levels
            if (input$label12 == labels[bank2$correctLabel2[index$context2]] &&
                input$label22 == labels[bank2$correctLabel1[index$context2]] &&
                input$label32 == labels[bank2$correctLabel1[index$context2]] &&
                input$label42 == labels[bank2$correctLabel1[index$context2]]) {
              output$stepCheckPic <- renderIcon(icon = "correct", width = 36)
              return(
                "This is a valid set up for this problem, but the structure of
                the context makes a different form of the tree easier to work
                with in this case."
              )
            } else {
              output$stepCheckPic <- renderIcon(icon = "incorrect", width = 36)
              return(
                "You have a correct structure for the tree, but the labels you
                have chosen are not correct. There is another form of the tree
                that would be easier to work with for this problem."
              )
            }
          } else {
            output$stepCheckPic <- renderIcon(icon = "incorrect", width = 30)
            return("Check the labels you have chosen.")
          }
        } else {
          # Check labels
          if (input$label12 == labels[bank2$correctLabel1[index$context2]] &&
              input$label22 == labels[bank2$correctLabel2[index$context2]] &&
              input$label32 == labels[bank2$correctLabel2[index$context2]]) {
            output$stepCheckPic <- renderIcon(icon = "correct", width = 30)
            return(
              "Good Job! You are Correct! Click the Next Step button to proceed
              to the next part of the question."
            )
          } else if (bank2$nCA[index$context2] == bank2$nCB[index$context2]) {
            # Label check where levels have same number
            if (input$label12 == labels[bank2$correctLabel2[index$context2]] &&
               input$label22 == labels[bank2$correctLabel1[index$context2]] &&
               input$label32 == labels[bank2$correctLabel1[index$context2]]) {
              output$stepCheckPic <- renderIcon(icon = "correct", width = 30)
              return(
                "This is a valid set up for this problem, but the structure of
                the context makes a different form of the tree easier to work
                with in this case."
              )
            } else {
              output$stepCheckPic <- renderIcon(icon = "incorrect", width = 30)
              return(
                "You have a correct structure for the tree, but the labels you
                have chosen are not correct. There is another form of the tree
                that would be easier to work with for this problem."
              )
            }
          } else {
            output$stepCheckPic <- renderIcon(icon = "incorrect", width = 30)
            return("Check the labels you have chosen.")
          }
        }
      } else{
        if (input$nchildA2 == bank2$nCB[index$context2] &&
           input$nchildB2 == bank2$nCA[index$context2] &&
           input$nchildC2 == bank2$nCA[index$context2]) {
          # If A-C don't match
          # Check if tree just has order swapped (assumes a full tree, which is true
          # for this level)
          labels <- c(
            bank2$label1[index$context2],
            bank2$label2[index$context2],
            bank2$label3[index$context2],
            bank2$label4[index$context2]
          )
          # Check if D is relevant
          if (input$nchildA2 == 3 && input$nchildD2 == bank2$nCA[index$context2]) {
            if (input$label12 == labels[bank2$correctLabel2[index$context2]] &&
               input$label22 == labels[bank2$correctLabel1[index$context2]] &&
               input$label32 == labels[bank2$correctLabel1[index$context2]] &&
               input$label42 == labels[bank2$correctLabel1[index$context2]]) {
              output$stepCheckPic <- renderIcon(icon = "correct", width = 30)
              return(
                "This is a valid set up for this problem, but the structure of
                the context makes a different form of the tree easier to work
                with in this case."
              )
            } else {
              output$stepCheckPic <- renderIcon(icon = "incorrect", width = 30)
              return(
                "You have a correct structure for the tree, but the labels you
                have chosen are not correct. There is another form of the tree
                that would be easier to work with for this problem."
              )
            }
          } else {
            # Not swapped case
            if (input$label12 == labels[bank2$correctLabel2[index$context2]] &&
               input$label22 == labels[bank2$correctLabel1[index$context2]] &&
               input$label32 == labels[bank2$correctLabel1[index$context2]]) {
              output$stepCheckPic <- renderIcon(icon = "correct", width = 30)
              return(
                "This is a valid set up for this problem, but the structure of
                the context makes a different form of the tree easier to work
                with in this case."
              )
            } else {
              output$stepCheckPic <- renderIcon(icon = "incorrect", width = 30)
              return(
                "You have a correct structure for the tree, but the labels you
                have  chosen are not correct. There is another form of the tree
                that would be easier to work with for this problem."
              )
            }
          }
        } else {
          output$stepCheckPic <- renderIcon(icon = "incorrect", width = 30)
          return("Check the structure of your tree.")
        }
      }
    })

  # Prints feedback for set up (labels/numbers)
  output$stepCheckFeedback <- renderText({
    if (reset$setUpAns2) {
      return(feedback())
    } else {
      output$stepCheckPic <- renderUI({NULL})
      return("")
    }
  })

  # General Context numbers setup ----
  # Possible context numbers for level 1
  # Order has the current first followed by numeric order so that correct one is
  # always selected
  contNums1 <- reactive({
    # Note that the endpoints are special cases
    if (index$context1 == 1) {
      1:nrow(bank1)
    } else if (index$context1 == nrow(bank1)) {
      c(nrow(bank1), 1:(nrow(bank1) - 1))
    } else {
      c(index$context1, 1:(index$context1 - 1), (index$context1 + 1):nrow(bank1))
    }
  })

  # Possible context numbers for level 2
  # Order has the current first followed by numeric order so that correct one is
  # always selected
  contNums2 <- reactive({
    # Note that the endpoints are special cases
    if (index$context2 == 1) {
      1:nrow(bank2)
    } else if (index$context2 == nrow(bank2)) {
      c(nrow(bank2), 1:(nrow(bank2) - 1))
    } else {
      c(index$context2, 1:(index$context2 - 1), (index$context2 + 1):nrow(bank2))
    }
  })

  # Possible context numbers for level 3
  # Order has the current first followed by numeric order so that correct one is
  # always selected
  contNums3 <- reactive({
    # Note that the endpoints are special cases
    if (index$context3 == 1) {
      1:nrow(bank3)
    } else if (index$context3 == nrow(bank3)) {
      c(nrow(bank3), 1:(nrow(bank3)-1))
    } else{
      c(index$context3, 1:(index$context3 - 1), (index$context3 + 1):nrow(bank3))
    }
  })
  #Select inputs for context number for all 3 levels
  output$contNum1 <- renderUI({
    selectInput(
      inputId = "contextNum1",
      label = "Context number",
      choices = contNums1(),
      selectize = FALSE,
      width = "150px"
    )
  })
  output$contNum2 <- renderUI({
    selectInput(
      inputId = "contextNum2",
      label = "Context number",
      choices = contNums2(),
      selectize = FALSE,
      width = "150px"
    )
  })
  output$contNum3 <- renderUI({
    selectInput(
      inputId = "contextNum3",
      label = "Context number",
      choices = contNums3(),
      selectize = FALSE,
      width = "150px"
    )
  })

  # If changing the question number using the select input
  observeEvent(input$contextNum1, {
    index$context1 <- as.numeric(input$contextNum1)
    index$question1 <- 1
    # Re-hide all shown answers and feedback
    reset$weight <- FALSE
    reset$question <- FALSE
    reset$answer <- FALSE
    reset$ans1 <- FALSE
  })

  # If changing the question number using the select input
  observeEvent(input$contextNum2, {
    index$context2 <- as.numeric(input$contextNum2)
    index$question2 <- 1
    reset$setUp2 <- FALSE
    # Re-hide all shown answers and feedback
    reset$weight <- FALSE
    reset$question <- FALSE
    reset$answer <- FALSE
    reset$ans2 <- FALSE
  })

  # If changing the question number using the select input
  observeEvent(input$contextNum3, {
    index$context3 <- as.numeric(input$contextNum3)
    index$question3 <- 1
    # Re-hide all shown answers and feedback
    reset$weight <- FALSE
    reset$question <- FALSE
    reset$answer <- FALSE
    reset$hint3 <- FALSE
    reset$ans3 <- FALSE
  })

  # Generating input matricies for probabilities ----
  # Function to create starting matrix for nodes with 2 children
  baseMatrix2 <- function(context, bank){
    # Compile a list of which nodes should be included (i.e. has 2 children)
    goodNodes <- c()
    if (bank$nCA[context] == 2) {
      goodNodes <- c(goodNodes, nodeNames[1])
    } else {
      if(bank$nCD[context] == 2) {
        goodNodes <- c(goodNodes, nodeNames[4])
      }
    }
    if (bank$nCB[context] == 2) {
      goodNodes <- c(goodNodes, nodeNames[2])
    }
    if (bank$nCC[context] == 2) {
      goodNodes <- c(goodNodes, nodeNames[3])
    }
    # Sort the labels
    goodNodes <- sort(goodNodes)

    # Create a matrix where left probability is always 1 of the right size
    matrix(
      data = c(
        rep(x = 1, times = length(goodNodes)),
        rep(x = 0, times = length(goodNodes))
      ),
      nrow = length(goodNodes),
      dimnames = list(goodNodes, c("Left", "Right"))
    )
  }

  #Matrix structure for the initial input of matrix of nodes with 2 children
  baseMat21 <- reactive({
    # Figure out which nodes need to be included as labels in the size 2 matrix
    baseMatrix2(
      context = index$context1,
      bank = bank1
    )
  })

  #Matrix structure for the initial input of matrix of nodes with 2 children
  baseMat22 <- reactive({
    # Figure out which nodes need to be included as labels in the size 2 matrix
    baseMatrix2(
      context = index$context2,
      bank = bank2
    )
  })

  #Matrix structure for the initial input of matrix of nodes with 2 children
  baseMat23 <- reactive({
    # Figure out which nodes need to be included as labels in the size 2 matrix
    baseMatrix2(
      context = index$context3,
      bank = bank3
    )
  })

  # Function to create starting matrix for nodes with 3 children
  baseMatrix3 <- function(context, bank){
    # Compile a list of which nodes should be included (i.e. has 3 children)
    goodNodes <- c()
    if (bank$nCA[context] == 3) {
      goodNodes <- c(goodNodes, nodeNames[1])
      if (bank$nCD[context] == 3) {
        goodNodes <- c(goodNodes, nodeNames[4])
      }
    }
    if (bank$nCB[context] == 3) {
      goodNodes <- c(goodNodes, nodeNames[2])
    }
    if (bank$nCC[context] == 3) {
      goodNodes <- c(goodNodes, nodeNames[3])
    }
    # Sort the nodes that will become labels in the matrix
    goodNodes <- sort(goodNodes)
    # Create a matrix of the appropriate size with left probability always 1
    matrix(
      data = c(
        rep(x = 1, times = length(goodNodes)),
        rep(x = 0, times = 2*length(goodNodes))
      ),
      nrow = length(goodNodes),
      dimnames = list(goodNodes, c("Left", "Center", "Right"))
    )
  }

  # Matrix structure for matrix for nodes with 3 children
  # For level 1
  baseMat31 <- reactive({
    # Figure out which nodes should be included in matrix
    baseMatrix3(
      context = index$context1,
      bank = bank1
    )
  })
  # For level 2
  baseMat32 <- reactive({
    # Figure out which nodes should be included in matrix
    baseMatrix3(
      context = index$context2,
      bank = bank2
    )
  })
  # For level 3
  baseMat33 <- reactive({
    # Figure out which nodes should be included in matrix
    baseMatrix3(
      context = index$context3,
      bank = bank3
    )
  })

  # Calculations to make tree ----

  # A function to create a matrix of all of the probabilities (combining the two matrices)
  # Only use with levels 1 and 2  (because 3 requires special cases for 0s and labels)
  calcPMat <- function(context, probabilities2, probabilities3, bank){
    # If not in the case where all nodes have 3 children
    if (bank$nCA[context] == 2 || bank$nCB[context] == 2 ||
       bank$nCC[context] == 2 || bank$nCD[context] == 2) {
      # If not in the case where all nodes have 2 children
      if (!(bank$nCA[context] == 2 && bank$nCB[context] == 2 &&
           bank$nCC[context] == 2) && !is.null(probabilities2)) {
        # This is the case where the 2 and 3 tables must be merged
        # Format the 2 table to have an extra column of 0s (so it can join with the 3)
        p2 <- probabilities2
        p2 <- as.data.frame(p2)
        p2$Center <- c(rep( x = 0, times = nrow(probabilities2)))
        colnames(p2) <- c("Left", "Center", "Right")

        # Bind the reformatted 2 table with the 3 table and sort the row names
        # appropriately
        probs <- rbind(p2, probabilities3)
        probs <- probs[order(row.names(probs)), ]
        probs <- as.matrix(probs)
      } else {
        # If case where all nodes have 2 children, just return the 2 child matrix
        probs <- probabilities2
      }
    } else {
      # If case where all nodes have 3 children, just return the 3 child matrix
      probs <- probabilities3
    }
    return(probs)
  }

  # Matrix of all probabilities for level 1
  probabilities1 <- reactive({
    calcPMat(
      context = index$context1,
      probabilities2 = input$probabilities21,
      probabilities3 = input$probabilities31,
      bank = bank1
    )
  })
  # Matrix of all probabilities for level 2
  probabilities2 <- reactive({
    calcPMat(
      context = index$context2,
      probabilities2 = input$probabilities22,
      probabilities3 = input$probabilities32,
      bank = bank2
    )
  })

  # Special case for probabilities for level 3
  # Argument numeric dictates whether working with probabilities or labels
  # (defaults to probabilities)
  probabilities3 <- function(numeric = TRUE){
    # If dealing with probabilities
    if (numeric) {
      matrix2 <- input$probabilities23
      matrix3 <- input$probabilities33
    } else {
      # If dealing with labels
      matrix2 <- input$uEdgeLabels23
      matrix3 <- input$uEdgeLabels33
    }

    # If not in the case where all nodes have 3 children
    if ((input$nchildA3 == 2 || input$nchildB3 == 2 ||
        input$nchildC3 == 2 || input$nchildD3 == 2)) {
      # If not in the case where all nodes have 2 children
      if (!(input$nchildA3!= 3 &&input$nchildB3!= 3 && input$nchildC3!= 3)) {
        # This is the case where the 2 and 3 tables must be merged
        # Format the 2 table to have an extra column of 0s
        p2 <- matrix2
        p2 <- as.data.frame(p2)
        # Adds "dummy" rows for any nonexistent rows for probs case
        if (numeric) {
          p2$Center <- c(rep(x = 0, times = nrow(matrix2)))
          if (input$nchildB3 == 0) {
            p2["B", ] = rep(x = 0, times = 3)
          }
          if (input$nchildC3 == 0) {
            p2["C", ] = rep(x = 0, times = 3)
          }
          if (input$nchildD3 == 0) {
            p2["D", ] = rep(x = 0, times = 3)
          }
        } else {
          # Adds "dummy" rows for any nonexistent rows for labels case
          p2$Center <- c(rep("", nrow(matrix2)))
          if (input$nchildB3 == 0) {
            p2["B", ] = rep(x = "", times = 3)
          }
          if (input$nchildC3 == 0) {
            p2["C", ] = rep(x = "", times = 3)
          }
          if (input$nchildD3 == 0) {
            p2["D", ] = rep(x = "", times = 3)
          }
        }
        colnames(p2) <- c("Left", "Center", "Right")

        # Bind the reformmatted 2 table with the 3 table and sort the row names
        # appropriately
        probs <- rbind(p2, matrix3)
        probs <- probs[order(row.names(probs)), ]
        probs <- as.matrix(probs)
      } else {
        # If case where all nodes have 2 or fewer children, just return the 2
        # child matrix
        # Adds "dummy" rows for any nonexistent rows for probs case
        p2 <- as.data.frame(matrix2)
        if (numeric) {
          if (input$nchildB3 == 0) {
            p2$Center <- c(rep(x = 0, times = nrow(p2)))
            p2["B", ] = rep(x = 0, times = 3)
          }
          if (input$nchildC3 == 0) {
            p2$Center <- c(rep(x = 0, times = nrow(p2)))
            p2["C", ] = rep(x = 0, times = 3)
          }
          if (input$nchildD3 == 0) {
            p2$Center <- c(rep(x = 0, times = nrow(p2)))
            p2["D", ] = rep(x = 0, times = 3)
          }
        } else {
        # Adds "dummy" rows for any nonexistent rows for labels case
          if (input$nchildB3 == 0) {
            p2$Center <- c(rep(x = "", times = nrow(p2)))
            p2["B", ] <- rep(x = "", times = 3)
          }
          if (input$nchildC3 == 0) {
            p2$Center <- c(rep(x = "", times = nrow(p2)))
            p2["C", ] <- rep(x = "", times = 3)
          }
          if (input$nchildD3 == 0) {
            p2$Center <- c(rep(x = "", times = nrow(p2)))
            p2["D", ] <- rep(x = "", times = 3)
          }
        }
        probs <- p2[order(row.names(p2)), ]
        probs <- as.matrix(probs)
      }
    } else {
    # If case where all nodes have 3 children, just return the 3 child matrix
      probs <- as.data.frame(matrix3)
      if (numeric) {
        if (input$nchildB3 == 0) {
          probs["B", ] <- rep(x = 0, times = 3)
        }
        if (input$nchildC3 == 0) {
          probs["C", ] <- rep(x = 0, times = 3)
        }
        if (input$nchildD3 == 0) {
          probs["D", ] <- rep(x = 0, times = 3)
        }
      } else {
        if (input$nchildB3 == 0) {
          probs["B", ] <- rep(x = "", times = 3)
        }
        if (input$nchildC3 == 0) {
          probs["C", ] <- rep(x = "", times = 3)
        }
        if (input$nchildD3 == 0) {
          probs["D", ] <- rep(x = "", times = 3)
        }
        probs <- probs[order(row.names(probs)), ]
        probs <- as.matrix(probs)
      }
    }
    return(probs)
  }

  # Nodes that aren't in the tree (only works for ones working off the bank)
  badNodes <- function(context, bank){
    irrelevantNumbers <- c()
    # Removes children related to A's third child if A only has 2 children
    if (bank$nCA[context] == 2) {
      irrelevantNumbers <- c(irrelevantNumbers, 4, 11:13)
    } else if (bank$nCD[context] == 2) {
      # If a third child of A exists, remove its last option if it only has 2 children
      irrelevantNumbers <- c(irrelevantNumbers, 13)
    } else if (bank$nCD[context] == 0) {
      # Case where D exists but has no children
      irrelevantNumbers <- c(irrelevantNumbers, 11:13)
    }

    # Check root node's first child's number of children
    if (bank$nCB[context] == 2) {
      irrelevantNumbers <- c(irrelevantNumbers, 7)
    } else if (bank$nCB[context] == 0) {
      irrelevantNumbers <- c(irrelevantNumbers, 5:7)
    }
    # Check root node's second child's number of children
    if (bank$nCC[context] == 2) {
      irrelevantNumbers <- c(irrelevantNumbers, 10)
    } else if (bank$nCC[context] == 0) {
      irrelevantNumbers <- c(irrelevantNumbers, 8:10)
    }
    return(irrelevantNumbers)
  }

  # Gets all nodes that are currently not in the tree
  # For level 1
  irrelevantNodes1 <- reactive({
    badNodes(
      context = index$context1,
      bank = bank1
    )
  })
  # For level 2
  irrelevantNodes2 <- reactive({
    badNodes(
      context = index$context2,
      bank = bank2
    )
  })
  # For level 3
  irrelevantNodes3 <- reactive({
    badNodes(
      context = index$context3,
      bank = bank3
    )
  })

  # Returns all leaf nodes for the scenario (must be working off bank)
  lNodes <- function(context, bank){
    leaves <- c() # Nodes that are always leaves
    # Add appropriate children based on each node (3 leaves if 3 children, 2 if
    # 2 children, or the node itself if 0)
    if (bank$nCB[context] == 3) {
      leaves <- c(leaves, 5:7)
    } else if (bank$nCB[context] == 2) {
      leaves <- c(leaves, 5:6)
    } else if (bank$nCB[context] == 0) {
      leaves <- c(leaves, 2)
    }
    if (bank$nCC[context] == 3) {
      leaves <- c(leaves, 8:10)
    } else if (bank$nCC[context] == 2) {
      leaves <- c(leaves, 8:9)
    } else if (bank$nCC[context] == 3) {
      leaves <- c(leaves, 3)
    }
    # Special case for A's third child (if exists)
    if (bank$nCA[context] == 3) {
      if (bank$nCD[context] == 3) {
        leaves <- c(leaves, 11:13)
      } else if (bank$nCD[context] == 2) {
        leaves <- c(leaves, 11:12)
      } else if (bank$nCD[context] == 0) {
        leaves <- c(leaves, 4)
      }
    }
    return(leaves)
  }

  # Gets numbers of leaf nodes:
  # For level 1
  leafNodes1 <- reactive({
    lNodes(
      context = index$context1,
      bank = bank1
    )
  })
  # For level 2
  leafNodes2 <- reactive({
    lNodes(
      context = index$context2,
      bank = bank2
    )
  })
  # For level 3
  leafNodes3 <- reactive({
    lNodes(
      context = index$context3,
      bank = bank3
    )
  })

  # Convert matrix probs to list of weights
  calcWeights <- function(probs, numeric = T){
    # First 2 ifs catch temporary errors when switching between contexts
    if (!is.null(probs)) {
      if (nrow(probs) >= 3) {
        # Weights that always exist
        weight <- c(probs[1, ], probs[2, ], probs[3, ])
        # Case for at least one node having 3 children
        if (ncol(probs) == 3) {
          # Check case for number of rows
          if (nrow(probs) == 4) {
            weight <- c(weight, probs[4, ])
          } else {
            # Appends 0s for the fourth row if none exists
            if (numeric) {
              weight <- c(weight, rep(x = 0, times = 3))
              #round(weight, 2)
            } else {
              weight <- c(weight, rep(x = 0, times = 3))
            }
          }
        }
      } else {
        weight <- rep(x = 0, times = 12)
      }
    } else {
      weight <- rep(x = 0,  times = 6)
    }
    return(weight)
  }

  # Gives all weights as a single vector
  # For level 1
  weights1 <- reactive({
    calcWeights(probs = probabilities1())
  })
  # For level 2
  weights2 <- reactive({
    calcWeights(probs = probabilities2())
  })
  # For level 3
  weights3 <- reactive({
    calcWeights(probs = probabilities3())
  })

  # Defines the matrix of nodes with 2 children for user to define edge weights
  output$uMat21 <- renderUI({
    if ((bank1$nCA[index$context1] == 3 && bank1$nCB[index$context1] == 3 &&
         bank1$nCC[index$context1] == 3 && bank1$nCD[index$context1] == 3) ||
        is.null(baseMat21())) {
      NULL
    }
    else{
      matrixInput(inputId = "probabilities21",
                  value = baseMat21(),
                  rows = list(names = TRUE),
                  cols = list( names = TRUE),
                  class = "numeric"
      )}
  })
  # Defines the matrix of nodes with 2 children for user to define edge weights
  output$uMat22 <- renderUI({
    if((bank2$nCA[index$context2] == 3 && bank2$nCB[index$context2] == 3 &&
        bank2$nCC[index$context2] == 3 && bank2$nCD[index$context2] == 3) ||
       is.null(baseMat22()) ){
      NULL
    } else {
      matrixInput(
        inputId = "probabilities22",
        value = baseMat22(),
        rows = list(names = TRUE),
        cols = list( names = TRUE),
        class = "numeric"
      )
    }
  })
  # Defines the matrix of nodes with 2 children for user to define edge weights
  output$uMat23 <- renderUI({
    if (is.null(bMat23(numeric = TRUE)) ||
        (bank3$nCA[index$context3] == 3 && bank3$nCB[index$context3] == 3 &&
         bank3$nCC[index$context3] == 3 && bank3$nCD[index$context3] == 3)) {
      NULL
    } else {
      matrixInput(
        inputId = "probabilities23",
        value = bMat23(numeric = TRUE),
        rows = list(names = TRUE),
        cols = list( names = TRUE),
        class = "numeric"
      )
    }
  })

  # Defines the matrix of nodes with 3 children for user to define edge weights for level 1
  output$uMat31 <- renderUI({
    if (bank1$nCA[index$context1] == 2 && bank1$nCB[index$context1] == 2 &&
        bank1$nCC[index$context1] == 2) {
      NULL
    } else {
      matrixInput(
        inputId = "probabilities31",
        value = baseMat31(),
        rows = list(names = TRUE),
        cols = list( names = TRUE),
        class = "numeric"
      )
    }
  })

  # Defines the matrix of nodes with 3 children for user to define edge weights
  # for level 2
  output$uMat32 <- renderUI({
    if ((bank2$nCA[index$context2] == 2 && bank2$nCB[index$context2] == 2 &&
         bank2$nCC[index$context2] == 2)) {
      NULL
    } else {
      matrixInput(
        inputId = "probabilities32",
        value = baseMat32(),
        rows = list(names = TRUE),
        cols = list( names = TRUE),
        class = "numeric"
      )
    }
  })

  # Starting matrix for nodes with 2 children for level 3 (works for probs and labels)
  bMat23 <- function(numeric){
    # Get nodes that should be featured in the 2 case
    goodNodes <- c()
    if (input$nchildA3 == 2) {
      goodNodes <- c(goodNodes, nodeNames[1])
    } else {
      if (input$nchildD3 == 2) {
        goodNodes <- c(goodNodes, nodeNames[4])
      }
    }
    if (input$nchildB3 == 2) {
      goodNodes <- c(goodNodes, nodeNames[2])
    }
    if (input$nchildC3 == 2) {
      goodNodes <- c(goodNodes, nodeNames[3])
    }
    # Sort the labels
    goodNodes <- sort(goodNodes)

    if (length(goodNodes > 0)) {
      # Create a matrix where left probability is always 1 of the right size
      if (numeric) {
        matrix(
          data = c(
            rep(x = 1, times = length(goodNodes)),
            rep(x = 0, times = length(goodNodes))
          ),
          nrow = length(goodNodes),
          dimnames = list(goodNodes, c("Left", "Right"))
        )
      } else {
        # Create a matrix defaulted to Lab 1 and Lab 2 for all labels
        matrix(
          data = rep(x = c("Lab 1", "Lab 2"), times = length(goodNodes)),
          byrow = TRUE,
          nrow = length(goodNodes),
          dimnames = list(goodNodes, c("Left", "Right"))
        )
      }
    } else {
      NULL
    }
  }

  # Creates the initial matrix for the 3 children nodes for level 3 specifically
  bMat33 <- function(numeric){
    # Figure out which nodes apply
    goodNodes <- c()
    if (input$nchildA3 == 3) {
      goodNodes <- c(goodNodes, nodeNames[1])
      if (input$nchildD3 == 3) {
        goodNodes <- c(goodNodes, nodeNames[4])
      }
    }
    if (input$nchildB3 == 3) {
      goodNodes <- c(goodNodes, nodeNames[2])
    }
    if (input$nchildC3 == 3) {
      goodNodes <- c(goodNodes, nodeNames[3])
    }
    # Sort the nodes that will become labels in the matrix
    goodNodes <- sort(goodNodes)
    # Create a matrix of the appropriate size with left probability always 1
    if (length(goodNodes > 0)) {
      # If dealing with probabilities
      if (numeric) {
        matrix(
          data = c(
            rep(x = 1, times = length(goodNodes)),
            rep(x = 0, times = 2*length(goodNodes))
          ),
          nrow = length(goodNodes),
          dimnames = list(goodNodes, c("Left", "Center", "Right"))
        )
      } else {
        # if dealing with labels
        matrix(
          data = rep(x = c("Lab 1", "Lab 2", "Lab 3"), times = length(goodNodes)),
          byrow = T,
          nrow = length(goodNodes),
          dimnames = list(goodNodes, c("Left", "Center", "Right"))
        )
      }
    } else {
      NULL
    }
  }

  # Output for user to enter probs for nodes with 3 children for level 3
  output$uMat33 <- renderUI({
    if (!is.null(bMat33(numeric = TRUE))) {
      matrixInput(
        inputId = "probabilities33",
        value = bMat33(numeric = TRUE),
        rows = list(names = TRUE),
        cols = list( names = TRUE),
        class = "numeric"
      )
    }
  })

  # Output for user to enter labels for nodes with 3 children for level 3
  output$labelMat33 <- renderUI({
    if (!is.null(bMat33(numeric = FALSE))) {
      matrixInput(
        inputId = "uEdgeLabels33",
        value = bMat33(numeric = FALSE),
        rows = list(names = TRUE),
        cols = list( names = TRUE)
      )
    }
  })

  # Output for user to enter labels for nodes with 2 children for level 3
  output$labelMat23 <- renderUI({
    if (!is.null(bMat23(numeric = FALSE))) {
      matrixInput(
        inputId = "uEdgeLabels23",
        value = bMat23(numeric = FALSE),
        rows = list(names = TRUE),
        cols = list( names = TRUE)
      )
    }
  })

  # Defines the matrix for user to input leaf node probabilities
  output$uGMat1 <- renderUI({
    matrixInput(
      inputId = "userGuesses1",
      value = matrix(
        data = rep(x = 0, times = length(leafNodes1())),
        nrow = length(leafNodes1()),
        dimnames = list(
          nodeNames[sort(leafNodes1())],
          c("Probability of Reaching the State")
        )
      ),
      rows = list(names = TRUE),
      cols = list( names = TRUE),
      class = "numeric")
  })

  # Defines the matrix for user to input leaf node probabilities
  output$uGMat2 <- renderUI({
    matrixInput(
      inputId = "userGuesses2",
      value = matrix(
        data = rep(x = 0, times = length(leafNodes2())),
        nrow = length(leafNodes2()),
        dimnames = list(
          nodeNames[sort(leafNodes2())],
          c("Probability of Reaching the State"))
      ),
      rows = list(names = TRUE),
      cols = list( names = TRUE),
      class = "numeric")
  })

  # Defines the matrix for user to input leaf node probabilities
  output$uGMat3 <- renderUI({
    matrixInput(
      inputId = "userGuesses3",
      value = matrix(
        data = rep(x = 0, times = length(leafNodes3())),
        nrow = length(leafNodes3()),
        dimnames = list(
          nodeNames[sort(leafNodes3())],
          c("Probability of Reaching the State"))
      ),
      rows = list(names = TRUE),
      cols = list( names = TRUE), class = "numeric")
  })

  # Checks if input is valid (i.e. has probabilities that sum to 1)
  validInput <- function(context, probs, bank){
    # If all probabilities arent in [0, 1], immediately return false
    # First condition prevents min or max from calculating if probs is null
    # This prevents temporary error between switching contexts
    if (!(is.null(probs)) && min(probs) >= 0 && max(probs) <= 1) {
      # Get all numbers of children
      lengths <- c(as.numeric(bank$nCA[context]),
                   as.numeric(bank$nCB[context]),
                   as.numeric(bank$nCC[context]))
      if (lengths[1] == 3) {
        lengths <- c(lengths, as.numeric(bank$nCD[context]))
      }
      isValidInput <- TRUE
      # Check each valid row to make sure that the sum of probabilities is 1
      for (i in 1:length(lengths)) {
        # First 3 conditions prevent temporary error on switch between contexts
        if (!is.null(probs) && nrow(probs) >= i && ncol(probs) >= lengths[i] &&
           sum(probs[i, 1:lengths[i]]) != 1) {
          isValidInput <- FALSE
        }
      }
    } else{
      isValidInput = FALSE
    }
    return(isValidInput)
  }

  # Checks whether the current inputted probabilities are valid.
  # For level 1
  isValidInput1 <- reactive({
    validInput(
      context = index$context1,
      probs = probabilities1(),
      bank = bank1
    )
  })
  # For level 2
  isValidInput2 <- reactive({
    validInput(
      context = index$context2,
      probs = probabilities2(),
      bank = bank2
    )
  })
  # For level 3
  isValidInput3 <- reactive({
    validInput(
      context = index$context3,
      probs = probabilities3(),
      bank = bank3
    )
  })

  # Creates data frame for the graph
  # For level 1
  makeGraphDataFrame1 <- reactive({
    graphDF(
      context = index$context1,
      bank = bank1,
      isValid = isValidInput1(),
      weight = weights1(),
      irrelevantNodes = irrelevantNodes1(),
      probabilities = probabilities1(),
      displayProbs = input$displayProbs1,
      leafNodes = leafNodes1(),
      correctProbabilities = correctProbabilities1()
    )
  })
  # For level 2
  makeGraphDataFrame2 <- reactive({
    graphDF(
      context = index$context2,
      bank = bank2,
      isValid = isValidInput2(),
      weight = weights2(),
      irrelevantNodes = irrelevantNodes2(),
      probabilities = probabilities2(),
      displayProbs = input$displayProbs2,
      leafNodes = leafNodes2(),
      correctProbabilities = correctProbabilities2()
    )
  })
  # For level 3 (different case than other 2)
  makeGraphDataFrame3 <- reactive({
    # Starting case (edges that always exist)
    from <- c(nodeNames[1], nodeNames[1])
    to <- c(nodeNames[2], nodeNames[3])

    # Add edges that exist for specific cases
    # If B has 3rd child
    if (input$nchildB3 == 3) {
      from <- c(from, nodeNames[2], nodeNames[2], nodeNames[2])
      to <- c(to, nodeNames[5], nodeNames[6], nodeNames[7])
    } else if (input$nchildB3 == 2) {
      from <- c(from, nodeNames[2], nodeNames[2])
      to <- c(to, nodeNames[5], nodeNames[6])
    }
    # If C has 3rd Child
    if (input$nchildC3 == 3) {
      from <- c(from, nodeNames[3], nodeNames[3], nodeNames[3])
      to <- c(to , nodeNames[8], nodeNames[9], nodeNames[10])
    }
    if (input$nchildC3 == 2) {
      from <- c(from, nodeNames[3], nodeNames[3])
      to <- c(to , nodeNames[8], nodeNames[9])
    }
    # If A has 3rd child
    if (input$nchildA3 == 3) {
      from <- c(from, nodeNames[1])
      to <- c(to, nodeNames[4])
      # If D has 3rd child
      if (input$nchildD3 == 3) {
        from <- c(from, nodeNames[4], nodeNames[4], nodeNames[4])
        to <- c(to, nodeNames[11], nodeNames[12], nodeNames[13])
      } else if (input$nchildD3 == 2) {
        from <- c(from, nodeNames[4], nodeNames[4])
        to <- c(to, nodeNames[11], nodeNames[12])
      }
    }

    irrelevantNumbers <- c() # Will be all edge numbers that aren't valid
    # Removes children related to A's third child if A only has 2 children
    if (input$nchildA3 == 2) {
      irrelevantNumbers <- c(irrelevantNumbers, 3, 10:12)
    } else if (input$nchildD3 == 2) {
      # If a third child of A exists, remove its last option if it only has 2 children
      irrelevantNumbers <- c(irrelevantNumbers, 12)
    } else if (input$nchildD3 == 0) {
      irrelevantNumbers <- c(irrelevantNumbers, 10:12)
    }
    # Check root node's first child's number of children
    if (input$nchildB3 == 2) {
      irrelevantNumbers <- c(irrelevantNumbers, 6)
    } else if (input$nchildB3 == 0) {
      irrelevantNumbers <- c(irrelevantNumbers, 4:6)
    }
    # Check root node's second child's number of children
    if (input$nchildC3 == 2) {
      irrelevantNumbers <- c(irrelevantNumbers, 9)
    } else if (input$nchildC3 == 0) {
      irrelevantNumbers <- c(irrelevantNumbers, 7:9)
    }

    # Adjust for irrelevant nodes if necessary (no irrelavant nodes and not the all 2s case)
    weight <- calcWeights(
      probs = probabilities3(numeric = TRUE),
      numeric = TRUE
    )
    if (length(irrelevantNumbers) > 0 && ncol(probabilities3(numeric = TRUE)) == 3) {
      weight <- weight[-(irrelevantNumbers)]
    }

    # Make actual data frame
    df <- data.frame(from = from, to = to)
    df <- df[order(df$from), ]
    # Every now and again, I see an error (which only lasts about half a second)
    # come from here; I have not seen it since implementing the trycatch and
    # never figured out how to consistently reproduce it
    tryCatch(
      expr = {df$weight <- weight},
      warning = function(war){},
      error = function(err){
        df$weight <- rep(x = 0, times = nrow(df))},
      finally = {})
    return(df)
  })

  # Function to create the graph from a data frame (only for levels 1 and 2)
  graphDF <- function(context, bank, isValid, weight, irrelevantNodes,
                      probabilities, displayProbs, leafNodes, correctProbabilities){
    # Make sure input is valid before continuing
    validate(
      need(isValid, "Be Careful: Probabilities at each node (each matrix row)
           must add to 1 and be individually between 0 and 1.")
    )
    # Starting case (edges that always exist)
    from <- c(nodeNames[1], nodeNames[1], nodeNames[2],
              nodeNames[2], nodeNames[3], nodeNames[3])
    to <- c(nodeNames[2], nodeNames[3], nodeNames[5],
            nodeNames[6], nodeNames[8], nodeNames[9])

    # Add edges that exist for specific cases
    # If B has 3rd child
    if (bank$nCB[context] == 3) {
      from <- c(from, nodeNames[2])
      to <- c(to, nodeNames[7])
    }
    # If C has 3rd Child
    if (bank$nCC[context] == 3) {
      from <- c(from, nodeNames[3])
      to <- c(to, nodeNames[10])
    }
    # If A has 3rd child
    if (bank$nCA[context] == 3) {
      from <- c(from, nodeNames[1], nodeNames[4], nodeNames[4])
      to <- c(to, nodeNames[4], nodeNames[11], nodeNames[12])
      # If D has 3rd child
      if (bank$nCD[context] == 3) {
        from <- c(from, nodeNames[4])
        to <- c(to, nodeNames[13])
      }
    }
    # Adjust for irrelevant nodes if necessary (no irrelevant nodes and not the all 2s case)
    if (!is.null(probabilities) && length(irrelevantNodes) > 0 &&
        ncol(probabilities) == 3) {
      badNodes <- irrelevantNodes - 1
      weight <- weight[-(irrelevantNodes - 1)]
    }

    # Adjusts the vertex labels for the leaf nodes if weights are being shown
    if (displayProbs) {
      weightIndex <- 1
      for (i in 1:length(to)) {
        if (to[i] %in% nodeNames[leafNodes]) {
          # The extra enter at the beginning lets the label remain in the center
          to[i] <- paste("\n", to[i], "\n",
                         correctProbabilities[weightIndex, 1])
          weightIndex <- weightIndex + 1
        }
      }
    }
    # Make actual data frame
    weight <- round(weight, 4)
    df <- data.frame(from = from, to = to)
    df <- df[order(df$from), ]
    df$weight <- weight
    return(df)
  }

  # Create layout for output graph
  layout1 <- reactive({
    layout_as_tree(
      graph = graph_from_data_frame(makeGraphDataFrame1()),
      root = 1
    )
  })
  layout2 <- reactive({
    layout_as_tree(
      graph = graph_from_data_frame(makeGraphDataFrame2()),
      root = 1
    )
  })
  layout3 <- reactive({
    layout_as_tree(
      graph = graph_from_data_frame(makeGraphDataFrame3()),
      root = 1
    )
  })

  # Create plot from data frame with extra formatting
  graph <- function(df, bank, context, layout, showProbs = TRUE, colors = NULL){
    # Create a dataframe that gives all from nodes and how many times they occur
    dfTemp <- df %>% group_by(from) %>% summarize(count = n())
    if (!("B" %in% df$from)) {
      dfTemp[nrow(dfTemp) + 1, ] <- list("B", 0)
    }
    if (!("C" %in% df$from)) {
      dfTemp[nrow(dfTemp) + 1, ] <- list("C", 0)
    }
    if (!("D" %in% df$from)) {
      dfTemp[nrow(dfTemp) + 1, ] <- list("D", 0)
    }
    dfTemp <- dfTemp[order(dfTemp$from), ]
    goodLabels <- c(1, 2) # Edges that definitely exist
    # Add all other edges that also exist
    if (dfTemp$count[1] == 3) {
      goodLabels <- c(goodLabels, 3)
      if (dfTemp$count[4] == 3) {
        goodLabels <- c(goodLabels, 10, 11, 12)
      } else if (dfTemp$count[4] == 2) {
        goodLabels <- c(goodLabels, 10, 11)
      }
    }
    if (dfTemp$count[2] == 3) {
      goodLabels <- c(goodLabels, 4, 5, 6)
    } else if (dfTemp$count[2] == 2) {
      goodLabels <- c(goodLabels, 4, 5)
    }
    if (dfTemp$count[3] == 3) {
      goodLabels <- c(goodLabels, 7, 8, 9)
    } else if (dfTemp$count[3] == 2) {
      goodLabels <- c(goodLabels, 7, 8)
    }
    goodLabels <- sort(goodLabels) # Sorts good edges
    edgeLabels <- bank[context, 2:13] # pull edge labels from bank
    edgeLabels <- edgeLabels[goodLabels] # Take only relavent edge labels
    if (is.null(colors)) {
      colors <- rep(x = "#E69F0080", times = length(c(1, goodLabels)))
    }
    # Adjusts edge labels so that lefts are slightly left and rights are
    # slightly right of center
    if (showProbs) { # If you are showing probabilities on the edge labels
      for (edge in 1:length(edgeLabels)) {
        if (goodLabels[edge] %% 3 == 1) {
          # Formatting that moves both label and probability over to right (if a right node)
          df$weight[edge] <- paste(
            "\n",
            edgeLabels[edge],
            "      \n", df$weight[edge],
            "      "
          )
        } else if (goodLabels[edge] %% 3 == 2) {
          if (dfTemp$count[as.integer((goodLabels[edge] + 2) / 3)] == 2) {
            # Formatting that moves both label and probability over to left (if a left node)
            df$weight[edge] <- paste(
              "\n      ",
              edgeLabels[edge],
              "\n      ",
              df$weight[edge]
            )
          } else {
            # Formatting that moves both label and probability over to left (if a left node)
            df$weight[edge] <- paste(
              "\n",
              edgeLabels[edge],
              "\n",
              df$weight[edge]
            )
          }
        } else {
          # Formatting that moves both label and probability over to left (if a left node)
          df$weight[edge] <- paste(
            "\n      ",
            edgeLabels[edge],
            "\n      ",
            df$weight[edge]
          )
        }
      }
    } else {
      for (edge in 1:length(edgeLabels)) {
        if (goodLabels[edge] %% 3 == 1) {
          # Formatting that moves label over to right (if a right node)
          df$weight[edge] <- paste("\n", edgeLabels[edge], "      \n ")
        } else if (goodLabels[edge] %% 3 == 2) {
          if (dfTemp$count[as.integer((goodLabels[edge] + 2) / 3)] == 2) {
            # Formatting that moves label over to left (if a left node)
            df$weight[edge] <- paste("\n      ", edgeLabels[edge], "\n      ")
          } else {
            df$weight[edge] <- paste("\n", edgeLabels[edge], "\n")
          }
        } else {
          # Formatting that moves label over to left (if a left node)
          df$weight[edge] <- paste("\n      ", edgeLabels[edge], "\n      ")
        }
      }
    }
    # Make actual plot using igraph
    par(mar = c(0.5,0,0.5,0))
    plot <- plot(
      graph_from_data_frame(df, directed = F),
      label = TRUE,
      edge.label = df$weight,
      edge.color = "#000000",
      edge.width = 1.5,
      vertex.label.color = "#000000",
      edge.label.color = "#000000",
      label.cex = 1.2,
      edge.label.cex = 1.2,
      vertex.color = colors,
      layout = layout,
      asp = 0.2,
      vertex.size = 8
    )
    return(plot)
  }

  # Creates output graph
  # For level 1
  output$graph1 <- renderPlot({
    graph(
      df = makeGraphDataFrame1(),
      bank = bank1,
      context = index$context1,
      layout = layout1()
    )
  })
  # For level 2
  output$graph2 <- renderPlot({
    graph(
      df = makeGraphDataFrame2(),
      bank = bank2,
      context = index$context2,
      layout = layout2()
    )
  })
  # For level 3
  output$graph3 <- renderPlot({
    df <- makeGraphDataFrame3()
    # Create a dataframe that gives all from nodes and how many times they occur
    dfTemp <- df %>% group_by(from) %>% summarize(count = n())
    goodLabels <- c(1, 2) # Edges that definitely exist
    # Add all other edges that also exist
    if (input$nchildA3 == 3) {
      goodLabels <- c(goodLabels, 3)
      if (input$nchildD3 == 3) {
        goodLabels <- c(goodLabels, 10:12)
      } else if (input$nchildD3 == 2) {
        goodLabels <- c(goodLabels, 10:11)
      } else if (input$nchildD3 == 0) {
        dfTemp[nrow(dfTemp) + 1, ] = list("D", 0)
      }
    }

    if (input$nchildB3 == 3) {
      goodLabels <- c(goodLabels, 4:6)
    } else if (input$nchildB3 == 2) {
      goodLabels <- c(goodLabels, 4:5)
    } else if (input$nchildB3 == 0) {
      dfTemp[nrow(dfTemp) + 1, ] = list("B", 0)
    } else if (input$nchildC3 == 3) {
      goodLabels <- c(goodLabels, 7:9)
    } else if (input$nchildC3 == 2) {
      goodLabels <- c(goodLabels, 7:8)
    } else if (input$nchildC3 == 0) {
      dfTemp[nrow(dfTemp) + 1, ] = list("C", 0)
    }
    goodLabels <- sort(goodLabels) # Sorts good edges

    dfTemp <- dfTemp[order(dfTemp$from), ] # order by from
    # pull edge labels from bank
    edgeLabels <- calcWeights(
      probs = probabilities3(numeric = FALSE),
      numeric = FALSE
    )

    # only subset edge labels if necessary
    if (length(edgeLabels) != 6) {
      edgeLabels <- edgeLabels[goodLabels]
    }

    # Adjusts edge labels so that lefts are slightly left and rights are
    # slightly right of center
    for (edge in 1:length(goodLabels)) {
      if (goodLabels[edge] %% 3 == 1) {
        # Move left labels further left
        df$weight[edge] <- paste(
          "\n",
          edgeLabels[edge],
          "      \n",
          df$weight[edge],
          "      "
        )
      } else if (goodLabels[edge] %% 3 == 2) {
        if (dfTemp$count[as.integer((goodLabels[edge] + 2) / 3)] == 2) {
          # Move right labels further right
          df$weight[edge] <- paste(
            "\n      ",
            edgeLabels[edge],
            "\n      ",
            df$weight[edge]
          )
        } else {
          df$weight[edge] <- paste(
            "\n",
            edgeLabels[edge], "\n",
            df$weight[edge]
          )
        }
      } else {
        # Move right labels further right
        df$weight[edge] <- paste(
          "\n      ",
          edgeLabels[edge], "\n      ",
          df$weight[edge]
        )
      }
    }

    colors <- rep(x = "#E69F0080", times = length(c(1, goodLabels))) # initialize color list
    countA <- 1
    countB <- 1
    countC <- 1
    countD <- 1
    forCount <- 1

    possibleInputs <- list(input$rButtonsB, input$rButtonsC, input$rButtonsD,
                           input$rButtonsE, input$rButtonsF, input$rButtonsG,
                           input$rButtonsH, input$rButtonsI, input$rButtonsJ,
                           input$rButtonsK, input$rButtonsL, input$rButtonsM)
    possibleInputs <- possibleInputs[sort(choices3()) - 1]
    for (node in (sort(choices3()))) { # For each potential leaf
      if (possibleInputs[forCount] == "A") { # If recurses to A
        colors[1] <- "#56B4E980" # Change A's color
        df[df == nodeNames[node]] <- paste0("A", countA) # Add number to node name
        # If the nodes are in the wrong order on the graph, this fixes them
        if (grep(nodeNames[node],
                 nodeNames[c(1, goodLabels + 1)]) %in% reorderBCD()) {
          colors[reorderBCD()[grep(nodeNames[node], nodeNames[c(1, goodLabels + 1)]) - 1]] <- "#56B4E980"
        } else {
          colors[grep(nodeNames[node], nodeNames[c(1, goodLabels + 1)])] <- "#56B4E980" # Change node's color
        }
        countA <- countA + 1
      } else if (possibleInputs[forCount] == "B") { # If recursive to B
        if (grep(nodeNames[node],
                 nodeNames[c(1, goodLabels + 1)]) %in% reorderBCD()) {
          colors[reorderBCD()[
            grep(nodeNames[node], nodeNames[c(1, goodLabels + 1)]) - 1]] <- "#ce77a880"
        } else {
          colors[grep(nodeNames[node], nodeNames[c(1, goodLabels + 1)])] <- "#ce77a880" # Change node's color
        }
        colors[reorderBCD()[1]] <- "#ce77a880" # Change B's color
        df[df == nodeNames[node]] <- paste0("B", countB) # Add number after B

        countB <- countB + 1
      } else if (possibleInputs[forCount] == "C") { # If recursive to C
        if (grep(nodeNames[node],
                 nodeNames[c(1, goodLabels + 1)]) %in% reorderBCD()) {
          colors[reorderBCD()[
            grep(nodeNames[node], nodeNames[c(1, goodLabels + 1)]) - 1]] <- "#99CC0080"
        } else {
          colors[ grep(nodeNames[node], nodeNames[c(1, goodLabels + 1)])] <- "#99CC0080" # Change node's color
        }
        colors[reorderBCD()[2]] <- "#99CC0080"
        df[df == nodeNames[node]] <- paste0("C", countC) # Add number after C
        countC <- countC + 1
      } else if (possibleInputs[forCount] == "D") { # If recursive to D
        if (grep(nodeNames[node],
                 nodeNames[c(1, goodLabels + 1)]) %in% reorderBCD()) {
          colors[reorderBCD()[
            grep(nodeNames[node], nodeNames[c(1, goodLabels + 1)]) - 1]] <- "#D55E0080"
        } else {
          colors[grep(nodeNames[node], nodeNames[c(1, goodLabels + 1)])] <- "#D55E0080" # Change node's color
        }
        colors[reorderBCD()[3]] <- "#D55E0080" # Change D's color
        df[df == nodeNames[node]] <- paste0("D", countD) # Add number to end of D
        countD <- countD + 1
      }
      forCount <- forCount + 1
    }

    # Make plot using igraph
    par(mar = c(0.5,0,0.5,0))
    plot(
      graph_from_data_frame(df, directed = F),
      label = TRUE,
      edge.label = df$weight,
      layout = layout3(),
      asp = .2,
      vertex.size = 8,
      edge.color = "#000000",
      edge.width = 1.5,
      vertex.label.color = "#000000",
      edge.label.color = "#000000",
      label.cex = 1.2,
      edge.label.cex = 1.2,
      vertex.color = colors
    )
  })

  # Graph for the first part of level 2
  output$tempGraph2 <- renderPlot({
    numNodes <- 7
    # Starting case (edges that always exist)
    from <- c(nodeNames[1], nodeNames[1], nodeNames[2],
              nodeNames[2], nodeNames[3], nodeNames[3])
    to <- c(nodeNames[2], nodeNames[3], nodeNames[5],
            nodeNames[6], nodeNames[8], nodeNames[9])

    # Add edges that exist for specific cases
    # If B has 3rd child
    if (input$nchildB2 == 3) {
      from <- c(from, nodeNames[2])
      to <- c(to, nodeNames[7])
      numNodes <- numNodes + 1
    }
    # If C has 3rd Child
    if (input$nchildC2 == 3) {
      from <- c(from, nodeNames[3])
      to <- c(to, nodeNames[10])
      numNodes <- numNodes + 1
    }
    # If A has 3rd child
    if (input$nchildA2 == 3) {
      from <- c(from, nodeNames[1], nodeNames[4], nodeNames[4])
      to <- c(to, nodeNames[4], nodeNames[11], nodeNames[12])
      numNodes <- numNodes + 3
      # If D has 3rd child
      if (input$nchildD2 == 3) {
        from <- c(from, nodeNames[4])
        to <- c(to, nodeNames[13])
        numNodes <- numNodes + 1
      }
    }
    # Create and order data frame for graph
    df <- data.frame(from = from, to = to)
    df <- df[order(df$from), ]
    # Plot using igraph
    par(mar = c(0.5,0,0.5,0))
    plot(
      graph_from_data_frame(df, directed = F),
      label = TRUE,
      edge.color = "#000000",
      edge.width = 1.5,
      vertex.label.color = "#000000",
      edge.label.color = "#000000",
      label.cex = 1.2,
      edge.label.cex = 1.2,
      vertex.color = rep(x = "#E69F0080", times = numNodes),
      layout = layout_as_tree(
        graph = graph_from_data_frame(df),
        root = 1
      ),
      asp = .2,
      vertex.size = 8
    )
  })
  # Check if entered probabilities are correct
  correct <- function(context, bank, probs){
    if (!any(is.na(probs))) {
      # Split case by whether A has 2 or 3 children
      if (bank[context, 26] == 2) {
        # Split case by whether in the all 2s case
        if (bank[context, 27] == 2 && bank[context, 28] == 2) {
          user <- c(probs[1, ], probs[2, ], probs[3, ])
          correct <- bank[context, c(14, 15, 17, 18, 20, 21)]
        } else {
          user <- c(probs[1, ], probs[2, ], probs[3, ])
          correct <- bank[context, 14:22]
        }
      } else {
        user <- c(probs[1, ], probs[2, ], probs[3, ], probs[4, ])
        correct <- bank[context, 14:25]
      }
      isSame = TRUE
      # Loop to check whether entered values are same as correct ones (defined above)
      for (i in 1:length(correct)) {
        # The as.numeric shouldn't be necessary but prevents type issues in bank
        if (round(as.numeric(user[i]), 2) != round(as.numeric(correct[i]), 2)) {
          isSame = FALSE
        }
      }
      return(isSame)
    } else {
      return(FALSE)
    }
  }
  # Determines whether the transition matrix is correct
  # For Level 1
  isCorrect1 <- eventReactive(
    eventExpr = input$checkMat1,
    valueExpr = {
      reset$weight <- TRUE
      correct(
        context = index$context1,
        bank = bank1,
        probs = probabilities1()
      )
    })
  # For Level 2
  isCorrect2 <- eventReactive(
    eventExpr = input$checkMat2,
    valueExpr = {
      reset$weight <- TRUE
      correct(
        context = index$context2,
        bank = bank2,
        probs = probabilities2()
      )
    })
  # For Level 3
  isCorrect3 <- eventReactive(
    eventExpr = input$checkMat3,
    valueExpr = {
      reset$weight <- TRUE
      correct(
        context = index$context3,
        bank = bank3,
        probs = probabilities3()
      )
    })

  # Level 1; Puts checkmark or X depending on if answer was correct and if
  # weight was reset since last button push
  output$correctnessPic1 <- renderUI({
    if (isCorrect1()) {
      if (reset$weight) {
        renderIcon(icon = "correct", html = TRUE)
      } else {
        NULL
      }
    } else {
      if (reset$weight) {
        renderIcon(icon = "incorrect", html = TRUE, width = 30)
      }
    }
  })

  # Level 2; Puts checkmark or X depending on if answer was correct and if
  # weight was reset since last button push
  output$correctnessPic2 <- renderUI({
    if (isCorrect2()) {
      if (reset$weight) {
        renderIcon(icon = "correct", html = TRUE)
      } else {
        NULL
      }
    } else {
      if (reset$weight) {
        renderIcon(icon = "incorrect", html = TRUE, width = 30)
      }
    }
  })

  # Level 1; Writes either good job or check work depending on if answer was
  # correct and if it was reset since last button push
  output$correctnessText1 <- renderText({
    if (reset$weight) {}
    if (isCorrect1()) {
      if (reset$weight) {
        "Good Job! You are Correct!"
      } else {
        ""
      }
    } else if (reset$weight) {
      "Not quite. Pay close attention to what each branch represents."
    } else {
      ""
    }
  })

  # Level 2; Writes either good job or check work depending on if answer was
  # correct and if it was reset since last button push
  output$correctnessText2 <- renderText({
    if (reset$weight) {}
    if (isCorrect2()) {
      if (reset$weight) {
        "Good Job! You are Correct!"
      } else {
        ""
      }
    } else {
      # Gives different feedback depending on the error
      isolate(
        if (reset$weight) {
          "Not quite. Check your probabilities."
        }
      )
    }
  })

  # Level 3; Writes either good job or check work depending on if answer was
  # correct and if it was reset since last button push
  output$correctnessText3 <- renderText({
    if (reset$weight) {}
    if (isCorrect3()) {
      if (reset$weight) {
        "Good Job! You are Correct!"
      } else {
        ""
      }
    } else {
      # Gives different feedback depending on the error
      isolate(
        if (bank3[index$context3, 26] == bank3$nCA[index$context3] &&
            bank3[index$context3, 27] == bank3$nCB[index$context3] &&
            bank3[index$context3, 28] == bank3$nCD[index$context3] &&
            ifelse(bank3$nCA[index$context3] == 3,
                   bank3[index$context3, 29] == bank3$nCD[index$context3],
                   TRUE)) {
          if (reset$weight) {
            "You are close. Your tree structure is correct, but the
            probabilities are not."
          } else {
            ""
          }
        } else {
          if (reset$weight) {
            "The structure of your tree is not correct. Check if you have any
            extra branches or if any branches are missing."
          } else {
            ""
          }
        }
      )
    }
  })

  # Calculate correct probabilities to put under leaf nodes
  cProbs <- function(weights, probabilities, leafNodes){
    probs <- c()
    if (length(weights) == 6) {
      probs <- c(weights[1]*weights[3], weights[1]*weights[4],
                 weights[2]*weights[5], weights[2]*weights[6])
      names <- c("E", "F", "H", "I")
    } else {
      atEnd <- c()
      names <- c()
      namesAtEnd <- c()
      for (node in leafNodes) {
        if (node > 4) {
          if (node %in% c(7, 10, 11, 12, 13)) {
            atEnd <- c(atEnd, as.numeric(weights[node - 1]) *
                         as.numeric(weights[as.integer((node - 2) / 3)]))
            namesAtEnd <- c(namesAtEnd, nodeNames[node])
          } else {
            probs <- c(probs, as.numeric(weights[node - 1]) *
                         as.numeric(weights[as.integer((node - 2) / 3)]))
            names <- c(names, nodeNames[node])}
        } else {
          probs <- c(probs, weights[node - 1])
          names <- c(names, nodeNames[node])
        }
      }
      probs <- c(probs, atEnd)

      names <- c(names, namesAtEnd)
    }
    matrix(
      data = probs,
      nrow = length(leafNodes),
      ncol = 1,
      dimnames = list(names, NULL)
    )
  }
  # Calculates the correct probabilities
  # For Level 1
  correctProbabilities1 <- reactive({
    cProbs(
      weights = weights1(),
      probabilities = probabilities1(),
      leafNodes = leafNodes1()
    )
  })
  # For Level 2
  correctProbabilities2 <- reactive({
    cProbs(
      weights = weights2(),
      probabilities = probabilities2(),
      leafNodes = leafNodes2()
    )
  })
  # For Level 3
  correctProbabilities3 <- reactive({
    cProbs(
      weights = weights3(),
      probabilities = probabilities3(),
      leafNodes = choices3()
    )
  })

  # Calculates correct probability of combination of states
  # For Level 1
  calcCombo1 <- reactive({
    sum(correctProbabilities1()[as.numeric(input$leafChoices1), 1])
  })
  # For Level 2
  calcCombo2 <- reactive({
    sum(correctProbabilities2()[as.numeric(input$leafChoices2), 1])
  })
  # For Level 3
  calcCombo3 <- reactive({
    sum(correctProbabilities3()[as.numeric(input$leafChoices3), 1])
  })

  # Creates list for the choices for leaf nodes in uComb
  # For Level 1
  choices1 <- reactive({
    choices <- 1:length(leafNodes1())
    names(choices) <- nodeNames[leafNodes1()]
    choices
  })
  # For Level 2
  choices2 <- reactive({
    choices <- 1:length(leafNodes2())
    names(choices) <- nodeNames[leafNodes2()]
    choices
  })
  # For Level 3
  choices3 <- reactive({
    leaves <- c() # Nodes that are always leaves
    # Add the third leaf for any node that has a third child
    if (input$nchildB3 == 3) {
      leaves <- c(leaves, 5:7)
    } else if (input$nchildB3 == 2) {
      leaves <- c(leaves, 5:6)
    } else if (input$nchildB3 == 0) {
      leaves <- c(leaves, 2)
    }
    if (input$nchildC3 == 3) {
      leaves <- c(leaves, 8:10)
    } else if (input$nchildC3 == 2) {
      leaves <- c(leaves, 8:9)
    } else if (input$nchildC3 == 0) {
      leaves <- c(leaves, 3)
    }
    # Special case for A's third child (if exists)
    if (input$nchildA3 == 3) {
      if (input$nchildD3 == 3) {
        leaves <- c(leaves, 11:13)
      } else if (input$nchildD3 == 2) {
        leaves <- c(leaves, 11:12)
      } else if (input$nchildD3 == 0) {
        leaves <- c(leaves, 4)
      }
    }
    return(leaves)
  })

  # Check if question answer is correct
  # For Level 1
  isCorrectCombo1 <- eventReactive(
    eventExpr = input$comboCheck1,
    valueExpr = {
      reset$question <- TRUE
      correctness <- ifelse(
        any(is.na(input$comboProb1)),
        FALSE,
        round(input$comboProb1, 2) == round(x = bank1[index$context1,
                                                      2 * index$question1 + 30],
                                            digits = 2))

      if (!correctness) {
        reset$ans1 <- TRUE
      }
      return(correctness)
    })
  # For Level 2
  isCorrectCombo2 <- eventReactive(
    eventExpr = input$comboCheck2,
    valueExpr = {
      reset$question <- TRUE
      correctness <- ifelse(
        any(is.na(input$comboProb2)),
        FALSE,
        round(x = input$comboProb2,
              digits = 2) == round(x = bank2[index$context2,
                                             2 * index$question2 + 30],
                                   digits = 2))
      if (!correctness) {
        reset$ans2 <- TRUE
      }
      return(correctness)
    })
  # For Level 3
  isCorrectCombo3 <- eventReactive(
    eventExpr = input$comboCheck3,
    valueExpr = {
      reset$question <- TRUE
      correctness <- ifelse(
        any(is.na(input$comboProb3)),
        FALSE,
        round(x = input$comboProb3,
              digits = 2) == round(x = bank3[index$context3,
                                             2 * index$question3 + 30],
                                   digits = 2))
      if (!correctness) {
        reset$hint3 <- TRUE
      }
      return(correctness)
    })

  # generates either a check or x depending on condition passed as boolean
  cPic <- function(correct){
    if (correct) {
      if (reset$question) {
        renderIcon(icon = "correct", html = TRUE)
      } else {
        NULL
      }
    } else {
      if (reset$question) {
        renderIcon(icon = "incorrect", html = TRUE, width = 30)
      } else {
        NULL
      }
    }
  }

  # Puts checkmark or X depending on if answer was correct
  # For Level 1
  output$correctnessPicCombo1 <- renderUI({
    cPic(isCorrectCombo1())
  })
  # For Level 2
  output$correctnessPicCombo2 <- renderUI({
    cPic(isCorrectCombo2())
  })
  # For Level 3
  output$correctnessPicCombo3 <- renderUI({
    output$tryAgain3 <- renderText({
      ifelse(isCorrectCombo3(),
             "",
             "Check the hint and try again.")})
    cPic(isCorrectCombo3())
  })

  # Gives appropriate text feedback depending on if answer was correct
  cText <- function(correct){
    if (correct) {
      if (reset$question) {
        "Good Job! You are Correct!"
      } else {
        ""
      }
    } else {
      if (reset$question) {
        "Check Your Work for Errors."
      } else {
        ""
      }
    }
  }
  # Writes either good job or check work depending on if answer was correct
  # For Level 1
  output$correctnessTextCombo1 <- renderText({
    cText(isCorrectCombo1())
  })
  # For Level 2
  output$correctnessTextCombo2 <- renderText({
    cText(isCorrectCombo2())
  })
  # For Level 3
  output$correctnessTextCombo3 <- renderText({
    cText(isCorrectCombo3())
  })

  # Show answer
  # For Level 1
  ans1 <- eventReactive(input$showAns1, {
    reset$answer <- TRUE # Makes sure answer will show once button is clicked
    bank1[index$context1, 2 * index$question1 + 30]})
  # For Level 2
  ans2 <- eventReactive(input$showAns2, {
    reset$answer <- TRUE # Makes sure answer will show once button is clicked
    bank2[index$context2, 2 * index$question2 + 30]})
  # For Level 3
  ans3 <- eventReactive(input$showAns3, {
    reset$answer <- TRUE # Makes sure answer will show once button is clicked
    bank3[index$context3, 2 * index$question3 + 30]})
  # For Level 1
  output$answer1 <- renderText({
    response <- ans1()
    if (reset$answer) {
      response
    } else {
      ""
    }
  })
  # For Level 2
  output$answer2 <- renderText({
    response <- ans2()
    if (reset$answer) {
      response
    } else {
      ""
    }
  })
  # For Level 3
  output$answer3 <- renderText({
    response <- ans3()
    if (reset$answer) {
      response
    } else {
      ""
    }
  })

  # New context button
  # For Level 1
  observeEvent(input$newContext1, {
    index$question1 <- 1
    # Move index to next context
    if (index$context1 < nrow(bank1)) {
      index$context1 <- index$context1 + 1
    } else {
      index$context1 <- 1
    }
    reset$setUp2 <- FALSE
    # Re-hide all shown answers and feedback
    reset$weight <- FALSE
    reset$question <- FALSE
    reset$answer <- FALSE
    reset$ans1 <- FALSE
    updateNumericInput(session, "comboProb1", value = 0)
  })
  # For Level 2
  observeEvent(input$newContext2, {
    index$question2 <- 1
    # Move index to next context
    if (index$context2 < nrow(bank2)) {
      index$context2 <- index$context2 + 1
    } else {
      index$context2 <- 1
    }
    index$question2 <- 1
    # Re-hide all shown answers and feedback
    reset$weight <- FALSE
    reset$question <- FALSE
    reset$answer <- FALSE
    reset$ans2 <- FALSE
    reset$setUp2 <- FALSE
    updateSelectInput(session, "nchildA2", selected = 2)
    updateSelectInput(session, "nchildB2", selected = 2)
    updateSelectInput(session, "nchildC2", selected = 2)
    updateSelectInput(session, "nchildD2", selected = 2)
    updateNumericInput(session, "comboProb2", value = NA)

  })
  # For Level 3
  observeEvent(input$newContext3, {
    index$question3 <- 1
    # Move index to next context
    if (index$context3 < nrow(bank3)) {
      index$context3 <- index$context3 + 1
    } else {
      index$context3 <- 1
    }
    # Re-hide all shown answers and feedback
    reset$weight <- FALSE
    reset$question <- FALSE
    reset$answer <- FALSE
    reset$hint3 <- FALSE
    reset$ans3 <- FALSE
    updateNumericInput(
      session = session,
      inputId = "comboProb3",
      value = 0
    )
    # Reset values
    count <- 1
    for (node in sort(choices3())) {
      updateRadioButtons(
        session = session,
        inputId = paste0("rButtons", nodeNames[node]),
        selected = "None"
      )
      count <- count + 1
    }
    updateSelectInput(
      session = session,
      inputId = "nchildA3",
      selected = 2
    )
    updateSelectInput(
      session = session,
      inputId = "nchildB3",
      selected = 2
    )
    updateSelectInput(
      session = session,
      inputId = "nchildC3",
      selected = 2)
    updateSelectInput(
      session = session,
      inputId = "nchildD3",
      selected = 2
    )

    # Defines the matrix of nodes with 2 children for user to define edge weights
    output$uMat23 <- renderUI({
      if (is.null(bMat23(numeric = TRUE)) ||
          (bank3$nCA[index$context3] == 3 && bank3$nCB[index$context3] == 3 &&
           bank3$nCC[index$context3] == 3 && bank3$nCD[index$context3] == 3)) {
        NULL
      } else {
        matrixInput(
          inputId = "probabilities23",
          value = bMat23(numeric = TRUE),
          rows = list(names = TRUE),
          cols = list( names = TRUE),
          class = "numeric"
        )
      }
    })

    # Output for user to enter probs for nodes with 3 children for level 3
    output$uMat33 <- renderUI({
      if (!is.null(bMat33(numeric = TRUE))) {
        matrixInput(
          inputId = "probabilities33",
          value = bMat33(numeric = TRUE),
          rows = list(names = TRUE),
          cols = list( names = TRUE),
          class = "numeric"
        )
      }
    })

    # Output for user to enter labels for nodes with 3 children for level 3
    output$labelMat33 <- renderUI({
      if (!is.null(bMat33(numeric = FALSE))) {
        matrixInput(
          inputId = "uEdgeLabels33",
          value = bMat33(numeric = FALSE),
          rows = list(names = TRUE),
          cols = list( names = TRUE)
        )
      }
    })

    # Output for user to enter labels for nodes with 2 children for level 3
    output$labelMat23 <- renderUI({
      if (!is.null(bMat23(numeric = FALSE))) {
        matrixInput(
          inputId = "uEdgeLabels23",
          value = bMat23(numeric = FALSE),
          rows = list(names = TRUE),
          cols = list( names = TRUE)
        )
      }
    })

    # Defines the matrix for user to input leaf node probabilities
    output$uGMat3 <- renderUI({
      matrixInput(
        inputId = "userGuesses3",
        value = matrix(
          data = rep(x = 0, times = length(leafNodes3())),
          nrow = length(leafNodes3()),
          dimnames = list(
            nodeNames[sort(leafNodes3())],
            c("Probability of Reaching the State")
          )
        ),
        rows = list(names = TRUE),
        cols = list( names = TRUE),
        class = "numeric"
      )
    })
  })

  # Button to get a new question
  # For Level 1
  observeEvent(input$newQuestion1, {
    # Shift question index
    if (index$question1 < bank1[index$context1, 30]) {
      index$question1 <- index$question1 + 1
    } else {
      index$question1 <- 1
    }
    # Reset all question dependent feedback/shown responses
    updateNumericInput(
      session = session,
      inputId = "comboProb1",
      value = 0
    )
    reset$question <- FALSE
    reset$answer <- FALSE
    reset$ans1 <- FALSE
  })
  # For Level 2
  observeEvent(input$newQuestion2, {
    # Shift question index
    if (index$question2 < bank2[index$context2, 30]) {
      index$question2 <- index$question2 + 1
    } else {
      index$question2 <- 1
    }
    # Reset all question dependent feedback/shown responses
    reset$question <- FALSE
    reset$answer <- FALSE
    reset$ans2 <- FALSE
    updateNumericInput(
      session = session,
      inputId = "comboProb2",
      value = 0
    )
  })
  # For Level 3
  observeEvent(input$newQuestion3, {
    # Shift question index
    if (index$question3 < bank3[index$context3, 30]) {
      index$question3 <- index$question3 + 1
    } else {
      index$question3 <- 1
    }
    # Reset all question dependent feedback/shown responses
    reset$question <- FALSE
    reset$answer <- FALSE
    reset$hint3 <- FALSE
    reset$ans3 <- FALSE
    updateNumericInput(
      session = session,
      inputId = "comboProb3",
      value = 0
    )
  })

  # Output for the context
  output$context1 <- renderText({bank1[index$context1, 1]})
  output$context2 <- renderText({bank2[index$context2, 1]})
  output$context3 <- renderText({bank3[index$context3, 1]})

  # Output for the questions
  output$question1 <- renderText({bank1[index$context1, index$question1 * 2 + 29]})
  output$question2 <- renderText({bank2[index$context2, index$question2 * 2 + 29]})
  output$question3 <- renderText({bank3[index$context3, index$question3 * 2 + 29]})


  # Specific to Level 3----
  # Check the structure of the tree (i.e. if right numbers of children/nodes present)
  structureCheck3 <- reactive({
    ret <- T
    if (input$nchildA3 != bank3$nCA[index$context3]) {
      ret <- F
    }
    if (input$nchildB3 != bank3$nCB[index$context3]) {
      ret <- F
    }
    if (input$nchildC3 != bank3$nCC[index$context3]) {
      ret <- F
    }
    if (input$nchildA3 == 3 && input$nchildD3 != bank3$nCD[index$context3]) {
      ret <- F
    }
    ret
  })

  # In the case where the graph rearranges itself, reorders nodes B, C, and D to
  # send them to the right locations
  reorderBCD <- reactive({
    order <- c(2, 3, 4)
    # Checks for a problem
    if ((input$nchildB3 == 0 && input$nchildC3 != 0) ||
        (input$nchildA3 == 3 && (input$nchildD3 != 0 &&
                                 (input$nchildB3 == 0 || input$nchildC3 == 0)))) {
      # If only 3 nodes and problem, switch B and C
      if (input$nchildA3 == 2) {
        order <- c(3, 2, 4)
      } else { # D exists
        # 4 cases: (- refers to any non-0); A is always non-zero
        # (1) -0-0
        # (2) -0--
        # (3) --0-
        # (4) -00-
        if (input$nchildB3 != 0) { # Case 3
          order <- c(2, 4, 3)
        } else if (input$nchildC3 == 0) { # Case 4
          order <- c(3, 4, 2)
        } else if (input$nchildD3 == 0) {# Case 1
          order <- c(3, 2, 4)
        } else {# Case 2
          order <- c(4, 2, 3)
        }
      }}
    order
  })

  # Creates a plot of a sample tree for level 3
  output$sampleAns3 <- renderPlot({
    # Start with the full tree
    df <- data.frame(
      from = c("A", "A", "A", "B", "B", "B", "C", "C", "C", "D", "D", "D"),
      to = c("B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M"))
    df$weight <- as.list(bank3[index$context3, 14:25])
    goodLabels <- which(df$weight > 0) # Note which nodes are about to be removed
    df <- df[df$weight > 0, ] # Remove weights of 0

    colors <- rep(x = "#E69F0080",
                  times = length(c(1, goodLabels))) # initialize color list
    if (bank3$Recursive[index$context3]) { # if in recursive case
      countA <- 1
      countB <- 1
      countC <- 1
      countD <- 1
      for (x in 42:50) {# For each potential leaf
        if (bank3[index$context3, x] == "A") {# If recurses to A
          colors[
            grep(colnames(bank3)[x], nodeNames[c(1, goodLabels + 1)])] <- "#56B4E980" # Change node's color
          colors[grep("A", nodeNames[c(1, goodLabels + 1)])] <- "#56B4E980" # Change A's color
          df[df == colnames(bank3)[x]] <- paste0("A", countA) # Add number to node name
          countA <- countA + 1
        } else if (bank3[index$context3, x] == "B") { # If recursive to B
          colors[grep(colnames(bank3)[x], nodeNames[c(1, goodLabels + 1)])] <- "#ce77a880" # Change node's color
          colors[grep("B", nodeNames[c(1, goodLabels + 1)])] <- "#ce77a880" # Change B's color
          df[df == colnames(bank3)[x]] <- paste0("B", countB) # Add number after B
          countB <- countB + 1
        } else if (bank3[index$context3, x] == "C") { # If recursive to C
          colors[grep(colnames(bank3)[x], nodeNames[c(1, goodLabels + 1)])] <- "#99CC0080" # Change node's color
          colors[grep("C", nodeNames[c(1, goodLabels + 1)])] <- "#99CC0080" # Change C's color
          df[df == colnames(bank3)[x]] <- paste0("C", countC) # Add number after C
          countC <- countC + 1
        } else if (bank3[index$context3, x] == "D") { # If recursive to D
          # Change D's color
          colors[grep(colnames(bank3)[x], nodeNames[c(1, goodLabels + 1)])] <- "#D55E0080"
          colors[grep("D",nodeNames[c(1, goodLabels + 1)])] <- "#D55E0080" # Change D's color
          df[df == colnames(bank3)[x]] <- paste0("D", countD) # Add number to end of D
          countD <- countD + 1
        }
      }
    }
    # Decide which version of a hint to give
    full <- df
    full$label <- bank3[index$context3, 2:13][bank3[index$context3, 2:13] != "-"]

    # If structure is wrong, just show the structure
    if (!structureCheck3()) {
      graph <- graph(
        df = df,
        bank = bank3,
        context = index$context3,
        layout = layout_as_tree(graph_from_data_frame(df), root = 1),
        showProbs = F,
        colors = colors)
      # Adjust corresponding alt text
      output$sampleAns3Alt <- renderUI({
        sentences <- "This plot shows the tree."
        for (row in 1:nrow(full)) {
          sentences <- paste0(
            sentences,
            " Edge ",
            row,
            " goes from ",
            full$from[row],
            " to ",
            full$to[row],
            " and is labeled ",
            full$label[row], "."
          )
        }
        tags$script(HTML(
          paste0(
            "$(document).ready(function() {
          document.getElementById('sampleAns3').setAttribute('aria-label',`",
            sentences,
            "`)})"
          )
        ))
      })
    } else if (!correct(index$context3, bank3, probabilities3())) {
      # If structure is right but probabilities off, show just up to probabilities
      graph <- graph(
        df = df,
        bank = bank3,
        context = index$context3,
        layout = layout_as_tree(graph_from_data_frame(df), root = 1),
        showProbs = T,
        colors = colors
      )
      # Adjust corresponding alt text
      output$sampleAns3Alt <- renderUI({
        sentences <- "This plot shows the tree."
        for (row in 1:nrow(full)) {
          sentences <- paste0(
            sentences,
            " Edge ",
            row,
            " goes from ",
            full$from[row],
            " to ",
            full$to[row],
            " and is labeled ",
            full$label[row],
            " with weight ",
            df$weight[row],
            ".")
        }
        tags$script(HTML(
          paste0(
            "$(document).ready(function() {
            document.getElementById('sampleAns3').setAttribute('aria-label', `",
            sentences,
            "`)})"
          )
        ))
      })
    } else {
      # If probabilities are right too, include leaf node probabilities
      # Adjust corresponding alt text
      output$sampleAns3Alt <- renderUI({
        sentences <- "This plot shows the tree."
        for (row in 1:nrow(full)) {
          sentences <- paste0(
            sentences,
            " Edge ",
            row,
            " goes from ",
            full$from[row],
            " to ",
            full$to[row],
            " and is labeled ",
            full$label[row],
            " with weight ",
            df$weight[row],
            ".")
        }
        tags$script(HTML(
          paste0(
            "$(document).ready(function() {
            document.getElementById('sampleAns3').setAttribute('aria-label', `",
            sentences,
            "`)})"
          )
        ))
      })
      weightIndex <- 1
      pasteAtEnd <- c()
      probs <- correctProbabilities3()[order(rownames(correctProbabilities3())), 1]
      for (i in 1:length(df$to)) {
        if ((df$to[i] %in% nodeNames[choices3()]) || regexpr("[A-Z][0-9]",
                                                             df$to[i]) != -1) {
          df$to[i] <- paste("\n", df$to[i], "\n", probs[weightIndex])
          weightIndex <- weightIndex + 1
        }
      }

      graph <- graph(
        df = df,
        bank = bank3,
        context = index$context3,
        layout = layout_as_tree(graph_from_data_frame(df),
                                root = 1),
        showProbs = T,
        colors = colors)
      reset$ans3 <- TRUE
    }
    graph
  })

  # Show hint button for level 3
  observeEvent(input$showHint3, {
    sendSweetAlert(
      session = session,
      title = "Hint: Does your tree look similar to the one below?",
      text = tags$div(
        plotOutput("sampleAns3"),
        htmlOutput("sampleAns3Alt")
      )
    )
  })

  # Nodes that could be recursed to for level 3:
  nonLeaves <- reactive({
    nonLeaves <- c("A")
    if (input$nchildB3 != 0) {
      nonLeaves <- c(nonLeaves, "B")
    }
    if (input$nchildC3 != 0) {
      nonLeaves <- c(nonLeaves, "C")
    }
    if (input$nchildA3 == 3 && input$nchildD3 != 0) {
      nonLeaves <- c(nonLeaves, "D")
    }
    nonLeaves
  })

  # New recursive inputs:
  output$recursiveButtons <- renderUI({
    buttons <- list()
    count <- 1
    for (node in sort(choices3())) {
      buttons[[count]] <- radioButtons(
        inputId = paste0("rButtons", nodeNames[node]),
        label = paste("Node", nodeNames[node], "Recurses To"),
        choices = c("None", nonLeaves()),
        inline = T
      )
      count <- count + 1
    }
    buttons
  })

  # Buttons that don't actually exist----
  # Output values for the context and question number to be used for conditional statements
  output$contextNumber1 <- reactive({
    index$context1 == nrow(bank1)}) # Current context number level 1
  output$questionNumber1 <- reactive({
    index$question1 == bank1[index$context1, 30]}) # current question number level 1
  output$contextNumber2 <- reactive({
    index$context2 == nrow(bank2)}) # Current context number level 2
  output$questionNumber2 <- reactive({
    index$question2 == bank2[index$context2, 30]}) # current question number level 2
  output$contextNumber3 <- reactive({
    index$context3 == nrow(bank3)}) # Current context number level 3
  output$questionNumber3 <- reactive({
    index$question3 == bank3[index$context3, 30]}) # current question number level 3
  output$nextStep2 <- reactive({
    reset$setUp2}) # Allows ui to read reset$setUp2
  output$showAnsButton1 <- reactive({
    reset$ans1}) # Allows ui to read reset$ans3
  output$showAnsButton2 <- reactive({
    reset$ans2}) # Allows ui to read reset$ans3
  output$showHintButton3 <- reactive({
    reset$hint3}) # Allows ui to read reset$hint3
  output$showRecursive <- reactive({
    bank3$Recursive[index$context3] == 1})

  # Decides when to show label warning (level 3: that labels are too long)
  output$showLabelWarning <- reactive({
    show <- FALSE
    # Check the 2 children edges for long (>5 chars) names
    if (!is.null(input$uEdgeLabels23)) {
      for (x in 1:nrow(input$uEdgeLabels23)) {
        for (y in 1:ncol(input$uEdgeLabels23)) {
          if (nchar(input$uEdgeLabels23[x, y]) > 5) {
            show <- TRUE
          }
        }
      }
    }
    # Check the 3 children edges for long (>5 chars) names
    if (!(is.null(input$uEdgeLabels33))) {
      for (x in 1:nrow(input$uEdgeLabels33)) {
        for (y in 1:ncol(input$uEdgeLabels33)) {
          if (nchar(input$uEdgeLabels33[x, y]) > 5) {
            show <- TRUE
          }
        }
      }
    }
    show
  })

  # Creates alt text from a data frame of tree info
  labelFromDF <- function(df){
    sentences <- "This plot shows the tree."
    # If in a /n node /n probability case, takes only letter
    df$to <- regmatches(x = df$to,
                        m = regexpr(pattern = "[ABCDEFGHIJKLM]",
                                    text = df$to))
    # Creates actual text
    for (row in 1:nrow(df)) {
      sentences <- paste0(
        sentences,
        " Edge ",
        row,
        " goes from ",
        df$from[row],
        " to ",
        df$to[row],
        " and is labeled ",
        df$label[row],
        " with weight ",
        df$weight[row],
        ".")
    }
    sentences
  }

  # Alt text for explore page
  output$exploreAlt <- renderUI({
    # Create data frame including labels
    full <- exploreDF()
    full$label <- c("Infected", "Not Infected", "Positive", "Negative",
                    "Positive", "Negative")
    # Returned alt text
    tags$script(HTML(
      paste0("$(document).ready(function() {
              document.getElementById('exploreGraph').setAttribute('aria-label',
             `", labelFromDF(full),"`)
              })"
      )
    ))
  })

  # Level 1 alt text
  output$graph1Alt <- renderUI({
    # Make data frame including labels
    full <- makeGraphDataFrame1()
    full$label <- bank1[index$context1, 2:13][bank1[index$context1, 2:13] != "-"]
    # Returned alt text
    tags$script(HTML(
      paste0("$(document).ready(function() {
                    document.getElementById('graph1').setAttribute('aria-label', `",
             labelFromDF(full),"`)
                  })"
      )))
  })

  # Level 2 alt text
  output$graph2Alt <- renderUI({
    # make data frame including labels
    full <- makeGraphDataFrame2()
    full$label <- bank2[index$context2, 2:13][bank2[index$context2, 2:13] != "-"]
    # Returned alt text
    tags$script(HTML(
      paste0("$(document).ready(function() {
                    document.getElementById('graph2').setAttribute('aria-label', `",
             labelFromDF(full),"`)
                  })"
      )))
  })

  # Makes sure that all of the buttons that "don't exist" still run
  outputOptions(x = output,
                name = "questionNumber1",
                suspendWhenHidden = FALSE)
  outputOptions(x = output,
                name = "contextNumber1",
                suspendWhenHidden = FALSE)
  outputOptions(x = output,
                name = "questionNumber2",
                suspendWhenHidden = FALSE)
  outputOptions(x = output,
                name = "contextNumber2",
                suspendWhenHidden = FALSE)
  outputOptions(x = output,
                name = "questionNumber3",
                suspendWhenHidden = FALSE)
  outputOptions(x = output,
                name = "contextNumber3",
                suspendWhenHidden = FALSE)
  outputOptions(x = output,
                name = "nextStep2",
                suspendWhenHidden = FALSE)
  outputOptions(x = output,
                name = "uMat23",
                suspendWhenHidden = FALSE)
  outputOptions(x = output,
                name = "uMat33",
                suspendWhenHidden = FALSE)
  outputOptions(x = output,
                name = "labelMat23",
                suspendWhenHidden = FALSE)
  outputOptions(x = output,
                name = "labelMat33",
                suspendWhenHidden = FALSE)
  outputOptions(x = output,
                name = "showLabelWarning",
                suspendWhenHidden = FALSE)
  outputOptions(x = output,
                name = "showHintButton3",
                suspendWhenHidden = FALSE)
  outputOptions(x = output,
                name = "showAnsButton1",
                suspendWhenHidden = FALSE)
  outputOptions(x = output,
                name = "showAnsButton2",
                suspendWhenHidden = FALSE)
  outputOptions(x = output,
                name = "recursiveButtons",
                suspendWhenHidden = FALSE)
  outputOptions(x = output,
                name = "showRecursive",
                suspendWhenHidden = FALSE)
  outputOptions(x = output,
                name = "contNum1",
                suspendWhenHidden = FALSE)
  outputOptions(x = output,
                name = "contNum2",
                suspendWhenHidden = FALSE)
  outputOptions(x = output,
                name = "contNum3",
                suspendWhenHidden = FALSE)
}

# boastApp Call ----
boastUtils::boastApp(ui = ui, server = server)
