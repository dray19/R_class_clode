library(shiny)
library(ggplot2)
library(DT)
library(psych)
library(doBy)
library(car)
library(e1071)
library(tidyverse)
library(mlbench)
library(psych)
library(skimr)
library(corrplot)
library(e1071)
library(plotly)
library(fmsb)
library(BaylorEdPsych)
library(gmodels)
library(lsr)
setwd("~/Desktop/Drazen.Zack_Shiny")
df <- read.csv("fram.csv")
str(df)
df1 <- df
options(scipen=999)
########## Cleaning data
df1$male <- as.factor(df1$male)
levels(df1$male) <- c("Female", "Male")
names(df1)[1] <- "sex"
df1$education <- as.factor(df1$education)
levels(df1$education) <- c("Some High School","HS and GED", "Some College", "College")
df1$currentSmoker <- as.factor(df1$currentSmoker)
levels(df1$currentSmoker) <- c("nonsmoker", "smoker")
df1$BPMeds <- as.factor(df1$BPMeds)
levels(df1$BPMeds) <- c("No BP Meds", "BP Meds")
df1$diabetes <- as.factor(df1$diabetes)
levels(df1$diabetes) <- c("No", "Yes")
df1$TenYearCHD <- as.factor(df1$TenYearCHD)
levels(df1$TenYearCHD) <- c("No","Yes")
df1$prevalentStroke <-  as.factor(df1$prevalentStroke)
levels(df1$prevalentStroke) <- c("FALSE", "TRUE")
df1$prevalentHyp <- as.factor(df1$prevalentHyp)
levels(df1$prevalentHyp) <- c("FALSE", "TRUE")
################# NA'S
summary(df1)
overview <- skim_to_wide(df1)
df1 <-  drop_na(df1)

anova_func <- function(x,y){
  r <- leveneTest(x ~ y)
  if(r$`Pr(>F)`[1] > 0.05){
    aov1 <- aov(x~y, df1)
    w <-  summary(aov1)
    t <- TukeyHSD(aov1)
    return(list(r,w, t))
  } else{
    c <- oneway.test(x~y, data=df1, var.equal=F)
    return(list(r,c))
  }
}
######################################################################
shinyServer(function(input, output,session) {
  
  output$plot <- renderPlot({
    ggplot(df1, aes_string(input$x,input$y)) + geom_point(aes_string(colour = input$col)) + labs(x = input$x, y = input$y, 
                                                                                                 title = paste0(input$x ," vs ", input$y)) +
      theme(plot.title = element_text(hjust = 0.5))
  })
  diam <- reactive({
    user_brush <- input$user_brush
    sel <- brushedPoints(df1,xvar = input$x, yvar = input$y, user_brush)
    return(sel)
  })
  output$tab <- renderDataTable(datatable(diam()[,input$show_vars, drop = FALSE]))
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("download_data", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(diam(), file, row.names = FALSE)
    }
  )
  output$cor <- renderPrint({
    cor1 <- cor(df1[,input$x], df1[,input$y])
    return(cor1)
  })
  
  output$cor_test <- renderPrint({
    k <- cor.test(df1[,input$x], df1[,input$y], method="pearson", alternative="two.sided", conf.level=0.95)
    return(k)
  })
  model <- reactive({
    brush_data <- brushedPoints(df1, input$user_brush,xvar = input$x, yvar = input$y)
    if(nrow(brush_data) < 2) {
      return(NULL)
    }
    lm(get(input$y) ~ get(input$x), data = brush_data)
  })
  output$sout <- renderText({
    if(is.null(model())) {
      "No Model Found"
    } else {
      model()[[1]][2]
    }
  })
  output$int <- renderText({
    if(is.null(model())){
      "No Model Found"
    } else {
      model()[[1]][1]
    }
  })
  output$hist1 <- renderPlot({
    ggplot(df1, aes_string(input$histx)) + geom_histogram( bins = input$binx,alpha = 0.5, fill = "red", col = "black") + 
       labs(x = input$histx,  title = paste0("Histogram of ",input$histx)) + theme(plot.title = element_text(hjust = 0.5)) +
      geom_vline(aes(xintercept=mean(df1[,input$histx], na.rm=T)),color="black", linetype="dashed", size=1) 
  })
  output$hist2 <- renderPlot({
    ggplot(df1, aes_string(input$histx, fill = input$col2)) + geom_histogram( bins = input$binx,alpha = 0.5, col = "black",position="identity") +
      labs(x = input$histx,  title = paste0("Histogram of ",input$histx)) + theme(plot.title = element_text(hjust = 0.5)) 
  })
  output$plot2 <- renderPlot({
    ggplot(df1, aes_string(input$xx, fill = input$fill)) + geom_bar() + labs(x = input$xx, y = "count", 
                                                                             title = paste0("Bar chart for ", input$xx, " by ", input$fill)) +
      theme(plot.title = element_text(hjust = 0.5))
  })
  output$view <- renderPrint({
    tab <- table(df1[,input$xx], df1[,input$fill])
    prop_tab <- prop.table(tab)
    return(list(Table = tab, Percent_Table = prop_tab))
  })
  output$cs <- renderPrint({
    w <- CrossTable(df1[,input$xx], df1[,input$fill], expected=T, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F)
    c <- w$chisq$p.value
    if(c < 0.05){
      t <- cramersV(df1[,input$xx], df1[,input$fill])
      return(list(CramersV = t))
    }else{
      return("P-value greater than 0.05")
    }
  })
  output$plot3 <- renderPlot({
    ggplot(df1, aes_string(input$x3, input$y3, fill = input$x3)) + geom_boxplot() + labs(x = input$x3, y = input$y3, 
                                                                                         title = paste0("Baxplot ", input$x3, " by ", input$y3)) +
      theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position = "none")
  })
  output$table3 <- renderTable({
    df1 %>%
      group_by(Type = get(input$x3)) %>%
      summarise(Count = n(),
                Mean = mean(get(input$y3)),
                Median = median(get(input$y3)),
                SD = sd(get(input$y3)),
                Skew = skewness(get(input$y3)),
                Kurtosis = kurtosis(get(input$y3)),
                p25 = quantile(get(input$y3), probs = 0.25),
                p50 = quantile(get(input$y3), probs = 0.5),
                p75 = quantile(get(input$y3), probs = 0.75),
                p95 = quantile(get(input$y3), probs = 0.95)
      )
  })
  output$anova_test <- renderPrint({
    f <- anova_func(df1[,input$y3], df1[,input$x3])
    return(f)
  })
  observe({
    x = input$dep
    if(x == "education"){
      updateSelectInput(session, "ref",
                        choices = unique(df1$education), selected = "College")
    } else if(x == "sex"){
      updateSelectInput(session, "ref",
                        choices = unique(df1$sex), selected = "Male")
    } else if(x == "currentSmoker"){
      updateSelectInput(session, "ref",
                        choices = unique(df1$currentSmoker), selected = "smoker")
    } else if(x == "BPMeds"){
      updateSelectInput(session, "ref",
                        choices = unique(df1$BPMeds), selected = "BP Meds")
    } else if(x == "prevalentStroke"){
      updateSelectInput(session, "ref",
                        choices = unique(df1$prevalentStroke), selected = "TRUE")
    } else if(x == "prevalentHyp"){
      updateSelectInput(session, "ref",
                        choices = unique(df1$prevalentHyp), selected = "TRUE")
    } else if(x == "diabetes"){
      updateSelectInput(session, "ref",
                        choices = unique(df1$diabetes), selected = "Yes")
    } else if(x == "TenYearCHD"){
      updateSelectInput(session, "ref",
                        choices = unique(df1$TenYearCHD), selected = "Yes")
    }
  })
  runReg <- reactive({
      df1[,input$dep] <- relevel(df1[,input$dep], input$ref)
      model <- glm(as.formula(paste(input$dep,'~',paste(input$inp, collapse= "+"))),data = df1, family = binomial())
      return(model)
    
  })
  output$sum <- renderPrint({
    summary(runReg())
  })
  output$r2 <- renderPrint({
    Values <- PseudoR2(runReg())
    r <- data.frame(Values)
    return(r)
  })
  output$ant <- renderPrint({
    antilogs <- exp(coef(runReg()))
    w <- data.frame(antilogs)
    return(w)
  })
  output$co_ant <- renderPrint({
    intexp <- exp(confint(runReg()))
    return(intexp)
  })
})
