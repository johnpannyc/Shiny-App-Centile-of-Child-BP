library(shiny)
library(ggplot2)
library(stringr)
library(dplyr)
library(DT)
library(tools)

source("cent.R")
source("bp.R")



#05/13/2019 revision (ui + server)

#Define UI with tags
ui <- fluidPage(
     
     titlePanel("Calculator of Bp,Height, Percentile for Children"),
          
     fluidRow(
          column(3,                                
                    img(src = "./bp_logo.png", height = 100, width = 120),
                    
                    selectInput('child_sex', 'Sex',  c("Male" = "male", "Female" = "female"),
                                                  selectize = FALSE),
                 
                    dateInput('child_dob',
                              label = 'Age:input DOB (mm/dd/yyyy)',
                              value = Sys.Date(),
                              format = "mm/dd/yyyy"),
                    
                    sliderInput("child_height", 
                                 label = 'Height (cm)', 
                                 value = 60, min = 30, max = 200),
                    
                    sliderInput("child_sys_bp", 
                                 label = 'Systolic Blood Pressure (mmHg)', 
                                 value = 100, min = 40, max = 200),
                    
                    sliderInput("child_dia_bp", 
                                 label = 'Diastolic Blood Pressure (mmHg)', 
                                 value = 60, min = 40, max = 150),  
                    selectInput("plot_type", "Plot:",
                             c("Height" = "height",
                               "Systolic BP" = "sysbp",
                               "Diastolic BP" = "diabp")),
                    actionButton("plot_now", "Plot")
          ),
          column(7,
                 tabsetPanel(
                      
                      tabPanel(h4("Percentile Plot"), plotOutput("plot")),                       
                      tabPanel(h4("Percentile"), htmlOutput("summary")),
                      tabPanel(h4("About us"), htmlOutput("aboutApp"))
                 )
                 
          )
     )
)


#Define server function
#List of centiles to plot
a <- c(2,5,8,25,50,75,92,95,98);

today <- Sys.Date()

#stature data(generate centile data frame for plotting)
centile <- a

height_boy   <- get_cent_df( stat_boy, centile );   
height_girl  <- get_cent_df( stat_girl, centile );   

#Blood pressure parameters
bp.centile  <- get_bp();

prepare_plot <- function( plot_type, input ){ 
  if( input$child.sex == "male" ) {
    D <- height_boy;
  } 
  else {
    D <- height_girl;
  }
  
  
  if( plot_type == "height") {
    plot_str <- sprintf("Age %.1f Years \nHeight %.1f \nCentile %.0f", 
                        input$child.months / 12, input$child.height, input$height.cent )
    this.plot <- plotCentile( D, input$child.months, input$child.height, 
                              input$child.sex, plot_str, "Age (Months)", "Height (cms)",
                              paste("Height for Age (", input$child.sex ,")" , sep = ""))     
    print( this.plot );     
  }
  
  if( plot_type == "sysbp" ) {
    plot_str <- sprintf("Age %.1f Years \nSys. BP %.1f \nCentile %.0f", 
                        input$child.months / 12, input$child.sys.bp, input$child.sys.cent )               
    plotTitle <- sprintf("Systolic BP for Age (%s) given Height %.1f (%.0f Centile)", 
                         input$child.sex, input$child.height, input$height.cent )               
    sysPlot <- generateBP("male", input$height.statz, bp.centile, centile )$sysTab;               
    this.plot <- plotCentile( sysPlot, input$child.months, input$child.sys.bp, 
                              input$child.sex, plot_str, "Age (Months)", "Systolic BP",plotTitle)
    print( this.plot )                                 
  }             
  
  if( plot_type == "diabp" ) {               
    plot_str <- sprintf("Age %.1f Years \nDia. BP %.1f \nCentile %.0f", 
                        input$child.months / 12, input$child.dia.bp, input$child.dia.cent )               
    plotTitle <- sprintf("Diastolic BP for Age (%s) given Height %.1f (%.0f Centile)", 
                         input$child.sex, input$child.height, input$height.cent)               
    diaPlot <- generateBP("male", input$height.statz, bp.centile, centile )$diaTab;               
    this.plot <- plotCentile( diaPlot, input$child.months, input$child.dia.bp, 
                              input$child.sex, plot_str, "Age (Months)", "Diastolic BP",plotTitle)
    print( this.plot )                                
  }      
  
}

server <- function(input, output) 
{
  dataInput.All <- reactive({
    #read variables relevant to height centile calculations                    
    heightStatz <- statsZ(timeInMonths( today, input$child_dob ),
                          input$child_sex, 
                          input$child_height)
    
    bps <- ZBP(heightStatz, bp.centile, 
               input$child_sex, timeInMonths( today, input$child_dob ), 
               input$child_sys_bp, input$child_dia_bp )
    
    this.sys.cent <- bps$c.Sys
    this.dia.cent <- bps$c.Dia
    
    list(
      child.sex = input$child_sex,
      child.dob = input$child_dob,
      child.months = timeInMonths( today, input$child_dob ),
      child.height = input$child_height,
      
      height.statz = heightStatz,
      
      height.cent = centileFromZ( heightStatz ),
      
      child.sys.bp = input$child_sys_bp,
      child.dia.bp = input$child_dia_bp,
      child.sys.cent  = this.sys.cent,
      child.dia.cent  = this.dia.cent               
    )
  })
  
  
  output$summary <- renderUI({
    
    params  <- dataInput.All();
    
    str1 <- paste("Gender : ", params$child.sex );
    str2 <- paste("DOB : ",  format(params$child.dob,'%d-%m-%Y') );
    str3 <- sprintf("Age : %.0f months, %.1f years", params$child.months, params$child.months / 12);
    str4 <- sprintf("Height : %.1f centimeters (%.1f metres) = %.0f Centile", 
                    params$child.height, params$child.height / 100, params$height.cent);
    str5 <- sprintf("Systolic BP : %.2f = %.0f Centile",params$child.sys.bp, params$child.sys.cent); 
    str6 <- sprintf("Diastolic BP : %.2f = %.0f Centile",params$child.dia.bp, params$child.dia.cent); 
    
    HTML(paste(str1,str2,str3,str4,str5,str6,sep = '<br/>'))
  })
  
  
  
  output$plot <- renderPlot({
    input$plot_now
    isolate({   
      params <- dataInput.All();
      thisP <- prepare_plot( input$plot_type, params ); 
    })               
  })
  
  
  output$aboutApp <- renderUI({
    includeHTML("about_app.html")           
  })
}



#Create the app object
shiny::shinyApp(ui = ui, server  =server)







