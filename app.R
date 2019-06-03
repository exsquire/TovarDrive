library(shiny)
library(ggplot2)
library(ggpubr)
library(gridExtra)
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("TovarDrive"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput("subject",
                      NULL,
                      placeholder = "Enter subject id"),
            
            numericInput("start.time", 
                         "Start time (sec)", 
                         value = 0.00),
            
            numericInput("end.time", 
                         "End time (sec)", 
                         value = 10000),
            
            fileInput("file", "File input")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("plot.out")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    #fill output$ with output
    output$plot.out <- renderPlot({
        inFile <- input$file[,"datapath"]
        
        if (is.null(inFile)){
           return(NULL) 
        }
            
        dat <- read.table(inFile, sep=" ")
        #Calculate heart rate and rename headers
        dat$HR <- 60/dat[,2]
        names(dat) <- c("time_sec","Interval_sec","HR_bpm")
        
        #Filter by start.time
        dat <- dat[dat$time_sec >= input$start.time & 
                       dat$time_sec <= input$end.time,]
        
        #Calculate outliers using median filter
        mmed <- function(x,n=5){runmed(x,n)}
        
        setWindow <- 10 #percent window size of total run
        p <- 100/setWindow
        window <- ifelse(round(max(dat[["time_sec"]])/p,0)%%2 != 0, 
                         round(max(dat[["time_sec"]])/p,0),
                         round(max(dat[["time_sec"]])/p,0)+1)
        
        dat$medFilt <- mmed(dat[["HR_bpm"]], n = window)
        
        meanRaw <- round(mean(dat$HR_bpm),2)
        maxRaw <- round(max(dat$HR_bpm),2)
        
        meanFilt <- round(mean(dat$medFilt),2)
        maxFilt <- round(max(dat$medFilt),2)
        
        resDF <- data.frame(meanRaw = meanRaw, 
                            meanFilt = meanFilt,
                            maxRaw = maxRaw,
                            maxFilt = maxFilt)
        
        p0 <- ggplot(data = dat, 
                     aes(x = time_sec, 
                         y = HR_bpm)) + 
            geom_point()+ 
            xlab("Time (sec)")+
            ylab("Heart Rate (bpm)")+
            ggtitle(paste0(input$subject,": Unfiltered"))+
            theme_bw() + 
            geom_hline(aes(yintercept=resDF[,1],colour = "red"))+
            geom_hline(aes(yintercept=resDF[,3],colour = "blue"))+
            theme(legend.position="none") + 
            annotate("text", x = -Inf, y = -Inf, 
                     hjust = -1, vjust = -1,
                     label = paste0("Raw Mean: ", 
                                    resDF$meanRaw,
                                    "\n Raw Max: ",
                                    resDF$maxRaw))

        p1 <- ggplot(data = dat, 
                     aes(x = time_sec, 
                         y = medFilt)) + 
            geom_point()+ 
            xlab("Time (sec)")+
            ylab("Heart Rate (bpm)")+
            ggtitle(paste0(input$subject,": Filtered"))+
            theme_bw() + 
            geom_hline(aes(yintercept=resDF[,2],colour = "red"))+
            geom_hline(aes(yintercept=resDF[,4],colour = "blue"))+
            theme(legend.position="none")+
            annotate("text", x = -Inf, y = -Inf, 
                     hjust = -1, vjust = -1,
                     label = paste0("Filtered Mean: ",
                                    resDF$meanFilt,
                                    "\n Filtered Max: ",
                                    resDF$maxFilt))
        
        ggarrange(p0,p1, nrow = 2, ncol = 1)
        
    })
    
    # dir
    #shinyDirChoose(input, 'dir', roots = c(name=getwd()))
    #dir <- reactive(input$dir)
    #output$dir <- renderPrint(dir())

}

# Run the application 
shinyApp(ui = ui, server = server)
