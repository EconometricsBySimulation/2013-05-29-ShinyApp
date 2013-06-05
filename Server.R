# Server.R
library(shiny)

# Load the data into memory with this somewhat odd formation.
  con = url("http://concerto4.e-psychometrics.com/media/13/Visual.Reasoning1.RData")
  load(file=con) 
  close(con) 
  nrow(individual.responses)
  
# I specify input$obs initially for debugging purposes.  Once this loads up on the server it is overwritten by the GUI.
  input = list(obs=27)

# Make a vector of values to identify the session ID for each test taker
  respondents = unique(individual.responses$sessionID)

# barplot(table(item.disp$user.answer), main="User Responses")

    
### Item Analysis

  # Create a vector of item names
  items = unique(individual.responses$item)

  # Calculate some values that will be useful later
  responses.mean  = tapply(individual.responses$anscorrect, individual.responses$item, mean)
  responses.count = tapply(individual.responses$anscorrect, individual.responses$item, length)
  
  sum.responses = data.frame(items, mean=responses.mean, count=responses.count)

  # hist(sum.responses$mean, breaks=12, col=grey(.4), main="Histogram of Item Difficulties", xlab="Probability of Correct Response")
  
  # This function takes the min of a vector and that of a scalar or two vectors of equal length.
  tmin = function(v1,v2) {
    r = NULL
    if (length(v2)==1) v2=rep(v2,length(v1))
    for (i in 1:length(v1)) r[i] = min(v1[i],v2[i])
    return(r)
  }
  # A couple of examples
  tmin(1:10,5)
  tmin(1:10,10:1)

# END SERVER STARTUP

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {

  # Generate a summary of the item
  output$summary <- renderPrint({
  
    # I want to calculate what percent of the item responses got it right and out of all responses how that compared with other items.
    correct.mean = round(mean(responses.mean[input$obs]),2)
    percentile = round(mean(responses.mean<correct.mean),2)
    
    dataset = individual.responses[individual.responses$item==input$obs,]
    loading = max(table(dataset$answer)/sum(table(dataset$answer)))
    
    # I will save a number of text bits to combine together to a single text summary of the item.
    text0 = paste0("Item ", input$obs, ":\n")
    
    text.5 = paste0("This item was taken by ", responses.count[input$obs], " respondents. ")
    
    text1 = "This was a very easy item.  As much as "
    if ((percentile<.80)) text1 = "This was an easy item.  As much as "
    if ((percentile<.60)) text1 = "This was an average item.  About "
    if ((percentile<.40)) text1 = "This was a hard item.  Only "
    if ((percentile<.20)) text1 = "This was a very hard item.  Only "
    
    text2 = paste0(round(correct.mean,2)*100,"% of people got it correct, putting it in the ", 100-percentile*100, " percentile in terms of difficulty.")
        
    text3 = ""
    if ((loading > .5) & (correct.mean<.5)) text3 = paste0(" Note that there is a large loading on a response ", round(loading,2)*100 ,"% which is not the correct one. This probably indicates that there is something wrong with this problem.")
    
    a.est = round(a$item.parms$A[a$item.parms$item==input$obs],2)
    b.est = round(a$item.parms$B[a$item.parms$item==input$obs],2)
    
    text4 = paste0("  In terms of item parameter estimates, this item had a=",a.est , " and b=", b.est, ".  ")
    
    text5 = "Having a large (a) estimate predicts the item to have good discrimination power around the mean difficulty level (b)."
    if (a.est<1.5) text5 = "Having a decent (a) estimate predicts the item to have good discrimination power around the mean difficulty level."
    if (a.est<1) text5 = "Having a small (a) estimate predicts the item to have difficulties discriminating between people who are above this level and those below."
    if (a.est<.5) text5 = "Having a very small (a) estimate predicts the item to have very little predictive power."
    
    cat(paste0(text0,text.5,text1,text2, text3, text4, text5))
  })
  
  # Plot Item Difficulty
  output$distPlot <- renderPlot({
    hist(responses.mean, xlab="Probability of Correct Response", main="Difficulty Distribution")
    abline(v=responses.mean[input$obs], lwd=3, col="red")
  })
  
  # Send item preview to the control bar
  output$preImage = renderImage({
    # When input$n is 3, filename is ./images/image3.jpeg
    filename = normalizePath(file.path('Images', paste0('Q', input$obs, '.png')))
    # Return a list containing the filename and alt text
    list(src = filename, alt = paste("Image number", input$obs))
  }, deleteFile = FALSE)

  # Graph bar graph of responses
  output$respPlot <- renderPlot({
    # Grab a subset of the item.response data to display
    dataset = individual.responses[individual.responses$item==input$obs,]
    
    # Set up the output for the plots that we would like.
    par(mfrow=c(1,2))
    
    # Select the color of the bar which if the right answer to be red.
      barcol = c("grey", "grey", "grey", "grey", "grey")
      barcol[sort(unique(individual.responses$correct))==dataset$correct[1]]="red"
    barplot(table(dataset$answer), col=barcol, 
            main=paste0("Correct Response=",dataset$correct[1]))
    
    # Calculate the average number of correct responses per ten items.
    avg.correct = tapply(dataset$anscorrect, ceiling((1:length(dataset$correct))/10), mean)
    # Plot those respones over time.
        plot(avg.correct, xaxt = "n", type="b", ylab="", xlab="", 
            main="Performance over time", ylim=c(0,1))
    # Change the x axis to have custom tick labels.
    navgs = length(avg.correct)
      axis(1, at=1:length(avg.correct), paste0(1+(1:length(avg.correct)-1)*10,"/", (tmin((1:length(avg.correct))*10,length(dataset[[1]])))))
  })
  
  # Show a table of all of the item response values.
  output$view = renderTable({
    # Select the subset of data that pertains to the item selected.
    dataset = individual.responses[individual.responses$item==input$obs,]
    rownames(dataset) <- 1:nrow(dataset)
    dataset$ip <- dataset$item <- dataset$id <- NULL
    dataset
  }, digits=0)
})

# I have these command below ready to be copied and pasted as the need arose for practice testing the shiny App.
# runApp("2013-05-29-ShinyApp")
# setwd("~/Shiny_R_Apps/")

# For me: setwd("C:\\Users\\Francis Smart\\Documents\\GitHub\\")
