#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#install.packages("shinydashboard")

## app.R ##
library(shinydashboard)
#install.packages("baRcodeR")
library(baRcodeR)
source(file = "custom_create_PDF_sub.R")
source(file = "custom_qrcode_make.R")

generate_labels_per_visit <- function(proj, 
                                      patient,
                                      type,
                                      aliquotes,
                                      last_patient,
                                    #  visit_nr,
                                    #  visit_type,
                                      date){
  patients <- as.numeric(patient):as.numeric(last_patient)
  p <- stringr::str_pad(patients, width = 3,pad = "0", side = "left")
  p <-paste0(p,".",type)
  #v <- as.character(visit_nr) # levels 10, 11, 12, 20, 30, 40, 41, 42
  static <- paste0(proj,"\n",p,"."#,ifelse(visit_type=="scheduled","V","U"),visit_nr,"."
                   ) 
  samples <- 1:as.numeric(aliquotes)
  o <- lapply(static,paste0,samples,"\n",date) 
  o <- unlist(o)
  o <- gsub(x = o,  pattern=".*bla.*",replacement = "- - -")
  return(data.frame(label = o))
}


#generate_excel <- function(proj, 
#                       patient,
#                       visit_nr,
#                       visit_type,
#                       date){
# 
#   p <- stringr::str_pad(patient, width = 3,pad = "0", side = "left")
#   p <-paste0(substring(proj,1,1),p)
#   #v <- as.character(visit_nr) # levels 10, 11, 12, 20, 30, 40, 41, 42
#   static <- paste0(p,".",ifelse(visit_type=="scheduled","V","U"),visit_nr,".")
#   samples <- c(rep("15",10),  
#                rep("16",10)  
#   )
#   o <- paste0(static,samples)
#   oo<-data.frame(a=rep("food@",length(o)),
#                  b = o,
#                  c = rep(c("BAT+unstim",
#                            "BAT+aIgE",
#                            "BAT+All",
#                            rep("InflCircs",3),
#                            "Bcells",
#                            "Tcells",
#                            rep("PBMC",2)),2),
#                  d = format(date,"%Y"))
#   return(oo)
# }


ui <- dashboardPage(
  dashboardHeader(title = "QR code Labels generator for clinical samples"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(p("This app will help you generate labels for clinical samples."),
          p("Please provide your Project number from the list"),
          p("Please provide the patient's number (screening number), and the date the sample was acquired. Also please select the type of the sample you are gathering. The list describing the sample types can be found below."),
          p("Note that if you are gathering HEALTHY CONTROLS - regardles of the project you should put them in the HEALTHY CONTROL project"),
            p ("You may find the paper for printing the labels:"), 
          a("here under this link",href= "https://www.bueromarkt-ag.de/universaletiketten_herma_4212_movables_weiss,p-4212,l-google-prd,pd-b2c.html?gclid=EAIaIQobChMI8oq1y4uB6AIVkh0YCh3CzgbVEAQYASABEgId4_D_BwE"))
    ),
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(selectizeInput("project", label = "Project name",choices = c(#"Infant","Child",
        "Healthy controls" = "HeCon",
        "Food Challenge"="FoodC",
        "Venom Induced Anaphylaxis"="VIANA",
        "Biologicals in Atopic Dermatitis" = "BioAD")),
        textInput("patient_n", label = "First Patient number",value = "1"),
        textInput("patient_last", label = "Last Patient number",value = "1"),
        selectizeInput("type", label = "Sample type ",choices = c("serum"="01",
                                                                  "plasma"='02',
                                                                  "Skin biopsy"="03",
                                                                  "urine" = '04',
                                                                  "saliva" = '05',
                                                                  "microbiome" = '05',
                                                                  'Full blood' = '06',
                                                                  'PBMC' = '07')),
        width = 6,
        textInput("aliquotes", label = "number of aliquotes per sample",value = "1"),
        selectizeInput("paper", label = "Paper type",choices = c("deep freeze: 4388","movables: 10000")),
        
          dateInput("visit_date", label = "Visit date", value = Sys.Date()),
      textInput("row_n", label = "If reusing paper, where should we start printing? row number",value = "0")
      )
    ),
    fluidRow(
     # box(actionButton("generateB", "Generate labels")),
      #conditionalPanel("output.test",
      box(downloadButton("downloadLabels", "Download"),width =3)
      #box(downloadButton("downloadExcel","Excel"),width =3)
      #)
    )
  )
)

server <- function(input, output) {
  
  #observeEvent(input$generateB, {
  #  cat("button pressed")
   
   # cat(list.files())
  #  output$test <-  reactive({
  #    cat("testing")
  #    any(list.files() == "LabelsOut.pdf")
  #  })
  #})
  
 
  #outputOptions(output, "test", suspendWhenHidden = FALSE)
  
  
  output$downloadLabels <- downloadHandler(
    filename = function(){
      paste0("labels-",input$patient_n,".pdf")
    },
    content = function(file){
      labels_pat1 <- generate_labels_per_visit(
      proj = input$project,
      patient = input$patient_n,
      last_patient = input$patient_last,
      aliquotes = as.numeric(input$aliquotes),
      type = input$type,
      date = format(input$visit_date,format="%d.%m.%y"))
  #show the generated results. 
      cat(paste0("\nFirst Label = ",as.character(labels_pat1[1,]),
                 "\nLast Label = ",as.character(labels_pat1[nrow(labels_pat1),])))
      if(input$paper=="movables: 10000"){
        custom_create_PDF_sub(user=FALSE,
                              Labels = labels_pat1[,],
                              name = 'LabelsOut',
                              type = 'matrix',
                              ErrCorr = 'M',
                              Fsz = 4,
                              Across = T,
                              ERows = as.numeric(input$row_n),
                              ECols = 0,
                              trunc = F,
                              numrow = 27,
                              numcol = 10,
                              page_width = 8.283,
                              page_height = 11.55,
                              width_margin = 0.183,
                              height_margin = 0.46,
                              label_width = 0.683,
                              label_height = 0.3938,
                              x_space = 0,
                              y_space = 0.5)
      } else if(input$paper =="deep freeze: 4388")
      custom_create_PDF_sub(user=FALSE,
                        Labels = labels_pat1[,],
                        name = 'LabelsOut',
                        type = 'matrix',
                        ErrCorr = 'M',
                        Fsz = 4,
                        Across = T,
                        ERows = as.numeric(input$row_n),
                        ECols = 0,
                        trunc = F,
                        numrow = 26,
                        numcol = 10,
                        page_width = 8.27,
                        page_height = 11.55,
                        width_margin = 0.375,
                        height_margin = 0.4,
                        label_width = NA,
                        label_height = NA,
                        x_space = 0,
                        y_space = 0.5)
      cat(list.files())
      file.copy("LabelsOut.pdf",file)
    }
  )
  

}

shinyApp(ui, server)
