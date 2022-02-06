
#con <- dbConnect(RPostgres::Postgres(), host='web0.eecs.uottawa.ca', port='15432', dbname='clubi035', user='clubi035',password='')

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DBI)
library(odbc)


companyNames <- dbGetQuery(con,'select distinct employername 
                                               from form1 where employername not like \'%inactive%\' and 
                                               employername not like \'%Inactive%\'  order by employername')$employername

ui <- dashboardPage(
   dashboardHeader(title = "Candev"),
   dashboardSidebar(
     sidebarMenu(
       selectInput(inputId = 'companypicker',label = 'Company:',choices = companyNames ,selected = 'UPS Canada',multiple = FALSE),
       uiOutput("sectorPicker"),
       menuItem("Retention and Growth",tabName = "tab1", icon = icon("trophy",lib = 'font-awesome')),
       menuItem("Salary", tabName = 'tab2', icon = icon("money-bill-alt", lib = 'font-awesome')),
       menuItem("Advice",tabName = "tab3", icon = icon("lightbulb",lib = 'font-awesome'))
     )
   ),
   dashboardBody(
     
     tabItems(
       tabItem("tab1",
               fluidPage(
                 h1("Retention and Growth"),
                 fluidRow(
                   h3('Data Specification'),
                   box(
                     selectInput(inputId = 'occgroup',
                                   label = 'Occupational group name :',
                                   choices = dbGetQuery(con, 'select distinct occgroup from form4'),
                                   selected = 'Semi-Skilled Manual Workers',
                                   multiple = FALSE),
                       selectInput(inputId = 'location',
                                   label = 'Location identification name :',
                                   choices = dbGetQuery(con, 'select distinct location_ from form4'),
                                   selected = 'Canada',
                                   multiple = FALSE),
                       width = 12)
                   ),
                 h3('Hired vs. terminated analysis of the 4 designated groups'),
                 fluidRow(
                   box(
                     plotOutput("nethirewomen"),
                     textOutput("nodata1"), width =12
                     )
                 )
                 ,
                 fluidRow(
                   box(
                     plotOutput("nethireIP"),
                     textOutput("nodata2"), width =12
                   )
                 ),
                 fluidRow(
                   box(
                     plotOutput("nethirePWD"),
                     textOutput("nodata3"), width =12
                   )
                 ),
                 fluidRow(
                   box(
                     plotOutput("nethireVM"),
                     textOutput("nodata4"), width =12
                   )
                 )
                 )
               )
       # ,
       # tabItem('tab2',
       #         box(plotOutput('salarytrend'),
       #             width =12)
       #         )
     )

   )

)


server <- function(input, output) {
  
  substrleft <- function(x, n){
    substr(x, 1, 2)
  }

  output$sectorPicker <- renderUI({
    selectInput("sector", "Sector:",
                choices =
                  substrleft(
                    as.vector(
                      na.exclude(
                        dbGetQuery(con,
                                   paste0('SELECT DISTINCT naicsid from form4 where employername = \'',input$companypicker,'\'')
                        )
                      )$naicsid
                    ),2
                  ),
                selected = '49',
                multiple = FALSE
                )
  })
  
  netHireEvents <- reactive({
    list(input$occgroup, input$location, input$sector)
  })
  
  
  
  observeEvent(netHireEvents(),{
    
    queryH <- paste0(
      'with secSum as (
    	select calendaryear, sum(allwomencount) as wo, 
    	sum(aborigallcount) as IP,
    	sum(pwdallcount) as PWD,
    	sum(visminallcount) as VM
    
    	from form4
    	where left( cast(naicsid as varchar),2) = \'',input$sector,'\' And 
    	occgroup = \'',input$occgroup,'\' And
    	location_ = \'',input$location,'\'
    	group by calendaryear
    ),secNum as (
    	select calendaryear, count(allwomencount) as WO,
    	count(aborigallcount) as IP,
    	count(pwdallcount) as PWD,
    	count(visminallcount) as VM
    	
    	from form4
    	where left( cast(naicsid as varchar),2) = \'',input$sector,'\' and allwomencount IS NOT NULL And 
    	aborigallcount IS NOT NULL And 
    	pwdallcount IS NOT NULL And 
    	visminallcount IS NOT NULL And 
    	occgroup = \'',input$occgroup,'\' And
    	location_ = \'',input$location,'\'
    	group by calendaryear
    ) 
    select secSum.calendaryear,secSum.WO/secNum.WO as avgWO,secSum.IP/secNum.IP as avgIP, secSum.PWD/secNum.PWD as avgPWD,secSum.VM/secNum.VM as avgVM
    from secSum, SecNum
    where secSum.calendaryear = secNum.calendaryear'
    
    )
    
    
    
    tableIndustryH <- dbGetQuery(con,queryH)
    validate(
      need(!is.null(tableIndustryH), "Something fishy with this data.")
    )
    
    
    queryT <- paste0(
      'with secSum as (
    	select calendaryear, sum(allwomencount) as wo, 
    	sum(aborigallcount) as IP,
    	sum(pwdallcount) as PWD,
    	sum(visminallcount) as VM
    
    	from form6
    	where left( cast(naicsid as varchar),2) = \'',input$sector,'\' And 
    	occgroup = \'',input$occgroup,'\' And
    	location_ = \'',input$location,'\'
    	group by calendaryear
    ),secNum as (
    	select calendaryear, count(allwomencount) as WO,
    	count(aborigallcount) as IP,
    	count(pwdallcount) as PWD,
    	count(visminallcount) as VM
    	
    	from form6
    	where left( cast(naicsid as varchar),2) = \'',input$sector,'\' and allwomencount IS NOT NULL And 
    	aborigallcount IS NOT NULL And 
    	pwdallcount IS NOT NULL And 
    	visminallcount IS NOT NULL And 
    	occgroup = \'',input$occgroup,'\' And
    	location_ = \'',input$location,'\'
    	group by calendaryear
    ) 
    select secSum.calendaryear,secSum.WO/secNum.WO as avgWO,secSum.IP/secNum.IP as avgIP, secSum.PWD/secNum.PWD as avgPWD,secSum.VM/secNum.VM as avgVM
    from secSum, SecNum
    where secSum.calendaryear = secNum.calendaryear'
      
    )
    
    
    
    
    tableIndustryT <- dbGetQuery(con,queryT)
    validate(
      need(!is.null(tableIndustryT), "Something fishy with this data.")
    )
    
    
    
    companyWO <- dbGetQuery(con,paste0(
      'select form4.employername,form4.calendaryear, form4.allwomencount as numHired, form6.allwomencount as numFired,
      (form4.allwomencount - form6.allwomencount) as diff
      from form4, form6
      where form4.employername = \'',input$companypicker,'\' AND
    	form4.occgroup = \'',input$occgroup,'\' AND
    	form4.location_ = \'',input$location,'\' AND
		form4.employmentstatus = \'Full-Time\' AND
    	form4.employername = form6.employername And
    	form4.calendaryear = form6.calendaryear And
    	form4.geography = form6.geography And
    	form4.location_ = form6.location_ And
    	form4.occgroup = form6.occgroup And
    	form4.employmentstatus = form6.employmentstatus
    	order by calendaryear'
      
    ))
    validate(
      need(!is.null(companyWO), "Something fishy with this data.")
    )
    companyIP <- dbGetQuery(con,paste0(
      'select form4.employername,form4.calendaryear, form4.aborigallcount as numHired, form6.aborigallcount as numFired,
      (form4.aborigallcount - form6.aborigallcount) as diff
      from form4, form6
      where form4.employername = \'',input$companypicker,'\' AND
    	form4.occgroup = \'',input$occgroup,'\' AND
    	form4.location_ = \'',input$location,'\' AND
		form4.employmentstatus = \'Full-Time\' AND
    	form4.employername = form6.employername And
    	form4.calendaryear = form6.calendaryear And
    	form4.geography = form6.geography And
    	form4.location_ = form6.location_ And
    	form4.occgroup = form6.occgroup And
    	form4.employmentstatus = form6.employmentstatus
    	order by calendaryear'
      
    ))
    validate(
      need(!is.null(companyIP), "Something fishy with this data.")
    )
    companyPWD <- dbGetQuery(con,paste0(
      'select form4.employername,form4.calendaryear, form4.pwdallcount as numHired, form6.pwdallcount as numFired,
      (form4.pwdallcount - form6.pwdallcount) as diff
      from form4, form6
      where form4.employername = \'',input$companypicker,'\' AND
    	form4.occgroup = \'',input$occgroup,'\' AND
    	form4.location_ = \'',input$location,'\' AND
		form4.employmentstatus = \'Full-Time\' AND
    	form4.employername = form6.employername And
    	form4.calendaryear = form6.calendaryear And
    	form4.geography = form6.geography And
    	form4.location_ = form6.location_ And
    	form4.occgroup = form6.occgroup And
    	form4.employmentstatus = form6.employmentstatus
    	order by calendaryear'
      
    ))
    validate(
      need(!is.null(companyPWD), "Something fishy with this data.")
    )
    companyVM <- dbGetQuery(con,paste0(
      'select form4.employername,form4.calendaryear, form4.visminallcount as numHired, form6.visminallcount as numFired,
      (form4.visminallcount - form6.visminallcount) as diff
      from form4, form6
      where form4.employername = \'',input$companypicker,'\' AND
    	form4.occgroup = \'',input$occgroup,'\' AND
    	form4.location_ = \'',input$location,'\' AND
		form4.employmentstatus = \'Full-Time\' AND
    	form4.employername = form6.employername And
    	form4.calendaryear = form6.calendaryear And
    	form4.geography = form6.geography And
    	form4.location_ = form6.location_ And
    	form4.occgroup = form6.occgroup And
    	form4.employmentstatus = form6.employmentstatus
    	order by calendaryear'
      
    ))
    validate(
      need(!is.null(companyVM), "Something fishy with this data.")
    )
    
    ## women
    
    Years  <- as.vector(companyWO$calendaryear)
    `Company-Number of women hired` <- as.vector(companyWO$numhired)
    `Company-Number of women terminated` <- -as.vector(companyWO$numfired)
    `Industry-Number of women hired` <- as.vector(tableIndustryH$avgwo[which(tableIndustryH$calendaryear %in% Years)])
    `Industry-Number of women terminated` <- -as.vector(tableIndustryT$avgwo[which(tableIndustryT$calendaryear %in% Years)])
    # validate(
    #   need( length(Years) = 
    #           length(`Company-Number of women hired`) = 
    #           length(`Company-Number of women terminated`) = 
    #           length(`Industry-Number of women hired`) = 
    #           length(`Industry-Number of women terminated`)
    #     , "Something fishy with this data.")
    # )
    
    output$nethirewomen <- renderPlot({
      barplot(
        cbind(`Company-Number of women hired`,
              `Industry-Number of women hired`,
              `Company-Number of women terminated`,
              `Industry-Number of women terminated`)~Years,legend = TRUE,
        beside = TRUE, width = 05, col = c('slateblue4','skyblue2','sienna4','rosybrown1'),
        main = "Hiring and termination trends of women in the selected company it's sector",
        xlim= c(min(Years),max(Years)))
    })
    
    
  ## Aboriginal peoples
    Years  <- as.vector(companyIP$calendaryear)
    `Company-Number of Aboriginal peoples hired` <- as.vector(companyIP$numhired)
    `Industry-Number of Aboriginal peoples hired` <- as.vector(tableIndustryH$avgip[which(tableIndustryH$calendaryear %in% Years)])
    `Company-Number of Aboriginal peoples terminated` <- -as.vector(companyIP$numfired)
    `Industry-Number of Aboriginal peoples terminated` <- -as.vector(tableIndustryT$avgip[which(tableIndustryT$calendaryear %in% Years)])
    # validate(
    #   need( length(Years) = 
    #           length(`Company-Number of Aboriginal peoples hired`) = 
    #           length(`Company-Number of Aboriginal peoples terminated`) = 
    #           length(`Industry-Number of Aboriginal peoples hired`) = 
    #           length(`Industry-Number of Aboriginal peoples terminated`)
    #         , "Something fishy with this data.")
    # )
    
    output$nethireIP <- renderPlot({
      barplot(
        cbind(`Company-Number of Aboriginal peoples hired`,
              `Industry-Number of Aboriginal peoples hired`,
              `Company-Number of Aboriginal peoples terminated`,
              `Industry-Number of Aboriginal peoples terminated`)~Years,
        legend = TRUE, beside = TRUE,col = c('slateblue4','skyblue2','sienna4','rosybrown1'),
        main = "Hiring and termination trends of Indigenous peoples in the selected company it's sector",
        xlim= c(min(Years),max(Years)) )
    })
    
    ## PWD
    Years  <- as.vector(companyPWD$calendaryear)
    `Company-Number of persons with disabilities hired` <- as.vector(companyPWD$numhired)
    `Industry-Number of persons with disabilities hired` <- as.vector(tableIndustryH$avgpwd[which(tableIndustryH$calendaryear %in% Years)])
    `Company-Number of persons with disabilities terminated` <- -as.vector(companyPWD$numfired)
    `Industry-Number of persons with disabilities terminated` <- -as.vector(tableIndustryT$avgpwd[which(tableIndustryT$calendaryear %in% Years)])
    # validate(
    #   need( length(Years) = 
    #           length(`Company-Number of persons with disabilities hired`) = 
    #           length(`Company-Number of persons with disabilities terminated`) = 
    #           length(`Industry-Number of persons with disabilities hired`) = 
    #           length(`Industry-Number of persons with disabilities terminated`)
    #         , "Something fishy with this data.")
    # )
    
    output$nethirePWD <- renderPlot({
      barplot(
        cbind(`Company-Number of persons with disabilities hired`,
              `Industry-Number of persons with disabilities hired`,
              `Company-Number of persons with disabilities terminated`,
              `Industry-Number of persons with disabilities terminated`)~Years,legend = TRUE,
        beside = TRUE,col = c('slateblue4','skyblue2','sienna4','rosybrown1'),
        main = "Hiring and termination trends of persons with disabilities in the selected company it's sector",
        xlim= c(min(Years),max(Years)) )
    })
    
    ## VM
    Years  <- as.vector(companyVM$calendaryear)
    `Company-Number of members of visible minorities hired` <- as.vector(companyVM$numhired)
    `Industry-Number of members of visible minorities hired` <- as.vector(tableIndustryH$avgvm[which(tableIndustryH$calendaryear %in% Years)])
    `Company-Number of members of visible minorities terminated` <- -as.vector(companyVM$numfired)
    `Industry-Number of members of visible minorities terminated` <- -as.vector(tableIndustryT$avgvm[which(tableIndustryT$calendaryear %in% Years)])
    # validate(
    #   need( length(Years) = 
    #           length(`Company-Number of members of visible minorities hired`) = 
    #           length(`Company-Number of members of visible minorities terminated`) = 
    #           length(`Industry-Number of members of visible minorities hired`) = 
    #           length(`Industry-Number of members of visible minorities terminated`)
    #         , "Something fishy with this data.")
    # )
    
    output$nethireVM <- renderPlot({
      barplot(
        cbind(`Company-Number of members of visible minorities hired`,
              `Industry-Number of members of visible minorities hired`,
              `Company-Number of members of visible minorities terminated`,
              `Industry-Number of members of visible minorities terminated`)~Years,legend = TRUE, 
        beside = TRUE,col = c('slateblue4','skyblue2','sienna4','rosybrown1'),
        main = "Hiring and termination trends of members of visible minorities in the selected company it's sector",
        xlim= c(min(Years),max(Years)))
    })
    
    
  },ignoreNULL = TRUE)
  
  
  # observeEvent(input$companypicker,{
  #   query <- paste0('select salaryrange, sum(allmencount) as allmencount, sum(allwomencount) as allwomencount from form3
  #   where employername = \'',input$companypicker,'\' group by salaryrange')
  # 
  #   table <- dbGetQuery(con,query)
  # 
  #   output$salarytrend <- renderPlot({
  #     barplot(
  #       cbind()
  #     )
  #     
  #   })
  #   
  #   
  #   
  # })
   
}
shinyApp(ui = ui, server)

