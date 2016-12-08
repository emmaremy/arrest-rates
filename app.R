library(plotly)
library(shiny)
library(ggplot2)
library(dplyr)

#Goal:
#to allow people to explore how demographics
#affect arrest rates
#over different time ranges

#Created by Stat 31 group 5
#December 2015
#(Many apologies for the general inelegance of this code)

#Server Side
server<-function(input,output){
  
  #Loading the dataset
  #and making the age variable into something useable
  #and adding an all ages category
  arrests <- read.csv("./arrest-rates.csv")
  arrests$Juvenile.indicator <- factor(arrests$Juvenile.indicator)
  juv <- arrests %>%
    group_by(Year, sex, race, Arrest.type) %>%
    summarize("Juvenile.indicator" = "2",
              "Total.Arrests" = sum(Total.Arrests), 
              "Population" = sum(Population))
  juvs <- juv %>%
    mutate("Arrests.per.100.000" = Total.Arrests/(Population/100000))
  timesi <- rbind(arrests,juvs)
  timesp <- timesi
  
  #Many apologies for incomprehensible variable naming
  #Creating the trend graph (with loess curves)
  #based on input variables, demographics
  output$trend <- renderPlotly({
    coln <- which(colnames(timesp) == input$dem)
    timesp$Group <- timesi[,coln]
    #conditional data frame creation
    #and color palette assignment
    if (input$dem == "race") {
      timesk <- timesp %>%
        filter(Group %in% input$dynamic) %>%
        filter(Arrest.type == input$type) %>%
        filter(sex == input$dyn2) %>%
        filter(Juvenile.indicator == input$dyn1)
      colo <- scale_color_brewer(type="qual", palette=6)}
    if (input$dem == "sex") {
      timesk <- timesp %>%
        filter(Group %in% input$dynamic) %>%
        filter(Arrest.type == input$type) %>%
        filter(race == input$dyn1) %>%
        filter(Juvenile.indicator == input$dyn2)
      colo <- scale_color_brewer(type="qual", palette=7)
      }
    if (input$dem == "Juvenile.indicator") {
      timesk <- timesp %>%
        filter(Group == input$dynamic) %>%
        filter(Arrest.type == input$type) %>%
        filter(sex == input$dyn2) %>%
        filter(race == input$dyn1)
      levels(timesk$Group) <- c("Adult","Juvenile", "Overall")
      colo <- scale_color_brewer(type="qual", palette=3)}
    #Actual plot
    ggplot(timesk, aes(x=Year, y=Arrests.per.100.000, color=Group))+
      geom_point(size=2) + stat_smooth(method="loess", se=FALSE) + xlim(input$time) +
      labs(y="Arrests") + colo
  })
  
  #Bar graph of averages
  #based on input, time range and demographics
  output$bar <- renderPlot({
    coln <- which(colnames(timesp) == input$dem)
    timesp$demog <- timesi[,coln]
    #Conditional data frame creation and color assignment
    if (input$dem == "race") {
      timesk <- timesp %>%
        filter(Arrest.type == input$type) %>%
        filter(sex == input$dyn2) %>%
        filter(Juvenile.indicator == input$dyn1)
      colo <- scale_fill_brewer(type="qual", palette=6, name="Time period")
      qlabs = labs(x="Race", y="Average arrests")}
    if (input$dem == "sex") {
      timesk <- timesp %>%
        filter(Arrest.type == input$type) %>%
        filter(race == input$dyn1) %>%
        filter(Juvenile.indicator == input$dyn2)
      colo <- scale_fill_brewer(type="qual", palette=7, name="Time period")
      qlabs = labs(x="Sex", y="Average arrests")}
    if (input$dem == "Juvenile.indicator") {
      levels(timesp$demog) <- c("Adult","Juvenile", "Overall")
      timesk <- timesp %>%
        filter(Arrest.type == input$type) %>%
        filter(sex == input$dyn2) %>%
        filter(race == input$dyn1)
      colo <- scale_fill_brewer(type="qual", palette=3, name="Time period")
      qlabs = labs(x="Age group", y="Average arrests")}
    #filtering for time period
    #and creating the appropriate new dataframe
    timesm <- timesk %>%
      group_by(demog) %>%
      summarize("Arrests.per.100.000" = mean(Arrests.per.100.000)) %>%
      mutate(Period = "Full")
    timesd <- timesk %>%
      filter(Year %in% input$time[1]:input$time[2]) %>%
      group_by(demog) %>%
      summarize("Arrests.per.100.000" = mean(Arrests.per.100.000)) %>%
      mutate(Period = "Partial")
    timesz <- rbind(timesm, timesd)
    #actual plot
    ggplot(timesz, aes(demog, Arrests.per.100.000, fill=Period)) + 
      geom_bar(stat="identity", position="dodge") + qlabs + colo
  })
  
  #Creates input options based on initial input
  output$ui <- renderUI({
    if (is.null(input$dem))
      return()
    switch(input$dem,
           "race" = checkboxGroupInput(inputId = "dynamic", 
                                  label = "Select groups to consider:",
                                  choices = list("Asian" = "Asian", 
                                        "Black" = "Black",
                                        "Hispanic" = "Hispanic",
                                        "White" = "White",
                                        "Other" = "Other"),
                                  selected = "Asian"),
           "sex" = checkboxGroupInput(inputId = "dynamic", 
                                label = "Select groups to consider:",
                                choices = list("Female" = "Female", 
                                               "Male" = "Male"),
                                selected = "Female"),
           "Juvenile.indicator" = checkboxGroupInput(inputId = "dynamic", 
                                    label = "Select groups to consider:",
                                    choices = list("Adult" = "0",
                                                   "Juvenile" = "1"),
                                       selected = "0")
           )
  })
  
  #Another responsive input thing
  output$ui2 <- renderUI({
    if (is.null(input$dem))
      return()
    switch(input$dem,
           "sex" = radioButtons(inputId = "dyn1", 
                                       label = "Specify demographics further:",
                                       choices = list("All races" = "All Combined",
                                                      "Asian" = "Asian", 
                                                      "Black" = "Black",
                                                      "Hispanic" = "Hispanic",
                                                      "White" = "White",
                                                      "Other" = "Other"),
                                       selected = "All Combined"),
           "race" = radioButtons(inputId = "dyn1", 
                                 label = "Specify demographics further:",
                                 choices = list("All ages" = "2",
                                                "Adult" = "0",
                                                "Juvenile" = "1"),
                                 selected = "2"),
           "Juvenile.indicator" = radioButtons(inputId = "dyn1", 
                                  label = "Specify demographics further:",
                                  choices = list("All races" = "All Combined",
                                            "Asian" = "Asian", 
                                            "Black" = "Black",
                                            "Hispanic" = "Hispanic",
                                            "White" = "White",
                                            "Other" = "Other"),
                                  selected = "All Combined")
    )
  })
  
  #And another responsive input thing
  #(I really wish I knew how to reindent automatically)
  output$ui3 <- renderUI({
    if (is.null(input$dem))
      return()
    switch(input$dem,
           "sex" = radioButtons(inputId = "dyn2", 
                                label = "and further:",
                                choices = list("All ages" = 2,
                                               "Adult" = 0,
                                               "Juvenile" = 1),
                                selected = 2),
           "race" = radioButtons(inputId = "dyn2", 
                                 label = "and further:",
                                 choices = list("Both sexes" = "Both Combined",
                                                "Female" = "Female", 
                                                "Male" = "Male"),
                                 selected = "Both Combined"),
           "Juvenile.indicator" = radioButtons(inputId = "dyn2", 
                                  label = "and further:",
                                  choices = list("Both sexes" = "Both Combined",
                                                "Female" = "Female", 
                                                "Male" = "Male"),
                                  selected = "Both Combined")
    )
  })
}  

#UI Side
ui<-fluidPage(
  #Title
  titlePanel("How do arrest rates differ for different demographics? (Group 5)"),
  sidebarLayout(
    sidebarPanel(
      #Initial input
        selectizeInput("dem",
                   label="Select a category to analyze:",
                   choices=c("Race" = "race",
                                "Sex" = "sex",
                                "Age" = "Juvenile.indicator"
                                ),
                   selected="race"),
        
        #Conditional input 1 (as coded in the server side)
        #This one selects subgroups from the initial demographic
        uiOutput("ui"),
        
        #Conditional input 2
        #Selects subgroups from alternative demographic
        uiOutput("ui2"),
        
        #Conditional input 3
        #Selects subgroups from other alternative demographic
        uiOutput("ui3"),
        
        #Arrest type
        radioButtons(inputId = "type", 
                    label = "Select a type of arrest:",
                    choices = list("All arrests" = "All Combined",
                                        "Drug-related felony" = "Felony - Drug", 
                                        "Property-related felony" = "Felony - Property",
                                        "Violence/sex-related felony" = "Felony - Violent/Sex",
                                        "Other felony" = "Felony - Other",
                                        "Misdemeanor" = "Misdemeanor",
                                        "Juvenile status-related (e.g. curfew, truancy)" = "Status"),
                              selected = "All Combined"),
        
        #Some advice
        helpText("We do not recommend selecting juvenile status-related arrests
                 if focusing on age demographics, as only juvenile are 
                 arrested for juvenile status-related arrests."),
      
        #Time period
        sliderInput("time", "Select a time period:",
                  min = 1980, max = 2013, value = c(1985,2010), sep=""),
      
        #From the data documentation
        helpText("A note: people within the other race category
                 are either mixed race or of a race that does not
                 fall into the other categories."),     

        #Not a very formal citation but easily Google-able
        helpText("This visualization was created by Swarthmore College's 
                  Stat 31 group 5
                  with data from the", a("California Department 
                                         of Justice via OpenJustice.", 
                                         href="http://openjustice.doj.ca.gov/"))
      
    ),
    mainPanel(
      
      #Lots of explanatory text, and calling the plot outputs
      
        p("We will be exploring arrest data from California, 1980-2013. Please
      begin by selecting a demographic to consider. You can compare subcategories 
      of this demographic by specifying further in the sidebar."),
        p("The following graph displays arrests per 100,000 members of the 
          population for
          the population specified by your selections in the sidebar."),
        plotlyOutput("trend"), 
        
        #We really really wanted to fix this but it was hard :(
        p("(We apologize
           that the years appear as decimals when hovering over the graph.)"),
        
        p("The following chart shows average number of arrests per 100,000 
          members of the population (as determined by your selections) over 
          the entire possible date range of 1980-2013 against a partial
          date range, as selected in the sidebar."),
        plotOutput("bar"))
  )  
)
shinyApp(ui=ui, server=server)