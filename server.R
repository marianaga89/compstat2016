
library(shiny)
library(shinydashboard)
library(ggplot2)
library(stats)
library(plyr)
function(input, output, session) {
  datos1 <- eventReactive(input$go,{
    -log(1-runif(input$num1))/input$num2
  }
  )
  datos2 <- eventReactive(input$go,{
    rexp(runif(input$num1),input$num2)
  }
  )
  output$hist <- renderPlot({
    datexpsim <- datos1()
    datexpteo <- datos2()
    datoscom <- data.frame(datexpsim,datexpteo)
    cols <- c("Simulada" = "red","Teórica" = "blue")
    ggplot(datoscom,aes(y=..count..)) + 
      geom_histogram(aes(x=datexpsim,fill = "Simulada"), alpha = 0.2,bins = input$num3) +
      scale_fill_discrete(name="x")+
      xlab("Datos")+
      theme_bw()
    
  }
  )
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("simexp", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(datos1(), file)
    }
  )

bondeaj <- eventReactive(input$bo,{ 
  
    if (input$bonaj==1){
      output$hist <- renderPlot({
        datexpsim <- datos1()
        datexpteo <- datos2()
        datoscom <- data.frame(datexpsim,datexpteo)
        cols <- c("Simulada" = "red","Teórica" = "blue")
        ggplot(datoscom,aes(y=..count..)) + 
          geom_histogram(aes(x=datexpsim,fill = "Simulada"), alpha = 0.2,bins = input$num3) +
          scale_fill_discrete(name="x")+
          xlab("Datos")+
          theme_bw()
      }
      )
      datexpsim <- datos1()
      datexpteo <- datos2()
      n <- input$num1
      lambda <- input$num2
      m <- min(datexpsim)
      M <- max(datexpsim)
      rango <- M-m
      inte <- rango/5
      tab <- table(cut(datexpteo,breaks = c(m,inte,inte*2,inte*3,M)))
      FO <- c(tab[1]/n,tab[2]/n,tab[3]/n,tab[4]/n)
      exote <- rexp(runif(n),lambda)
      tab2 <- table(cut(exote,breaks = c(m,inte,inte*2,inte*3,M)))
      FE <- c(tab2[1]/n,tab2[2]/n,tab2[3]/n,tab2[4]/n)
      if (sum(((FE-FO)^2)/FE) < qchisq(.95, df=2)){
        print("La hipotesis nula no se rechaza, por lo tanto la distribución simulada sigue una distribución exponencial")
      }
      else {
        print("Rechazamos la hipotesis nula, la distrubicón no es exponencial")
      }
    }
    else if (input$bonaj==2){
      output$hist <- renderPlot({
        datexpsim <- datos1()
        datexpteo <- datos2()
        datoscom <- data.frame(datexpsim,datexpteo)
        cols <- c("Simulada" = "red","Teórica" = "blue")
        ggplot(datoscom,aes(y=..count..)) + 
          geom_histogram(aes(x=datexpsim,fill = "Simulada"), alpha = 0.2,bins = input$num3) +
          scale_fill_discrete(name="x")+
          xlab("Datos")+
          theme_bw()
        
      }
      )
      datexpsim <- datos1()
      datexpteo <- datos2()
      ks <- ks.test(datexpsim,datexpteo)
      if (ks$p.value > .05){
        print("La hipotesis nula no se rechaza, por lo tanto la distribución simulada sigue una distribución exponencial")
      }
      else {
        print("Rechazamos la hipotesis nula, la distrubicón no es exponencial")
      }
    }
    else if (input$bonaj==3){
      output$hist <- renderPlot({
        datexpsim <- datos1()
        datexpteo <- datos2()
        datoscom <- data.frame(datexpsim,datexpteo)
        cols <- c("Simulada" = "red","Teórica" = "blue")
        ggplot(datoscom,aes(y=..count..)) + 
          geom_histogram(aes(x=datexpsim,fill = "Simulada"), alpha = 0.2,bins = input$num3) +
          geom_histogram(aes(x=datexpteo,fill = "Teórica"), alpha = 0.2,bins = input$num3) +
          scale_fill_manual(values=cols,name="Datos")+
          xlab("Datos")+
          theme_bw()
        
      }
      )
    }
    
  })
  output$value <- renderPrint({ 
    bondeaj()
  })
##############################tarea2##########################################
  fun1 <- eventReactive(input$go2,{
    texto <- paste("aux <- ",input$expresion1)
    eval(parse(text = texto))
    aux
  })

  funMC <- reactive({
    N <- seq(input$numon1, 10*input$numon1, by=input$numon1)
    lapply(N, function(nsim){
      X <- runif(nsim,input$a,input$b)
      PHIX <- (input$b-input$a)*sapply(X, fun1())
      estim <- round(mean(PHIX),4)
      var <- var(PHIX)
      quant1 <- qnorm((input$alpha1)/2, lower.tail=FALSE)
      quant2 <- qnorm((input$alpha2)/2, lower.tail=FALSE)
      limsup1 <- round(estim + sqrt(var/input$numon1)*quant1,4)
      liminf1 <- round(estim - sqrt(var/input$numon1)*quant1,4)
      limsup2 <- round(estim + sqrt(var/input$numon1)*quant2,4)
      liminf2 <- round(estim - sqrt(var/input$numon1)*quant2,4)
      data.frame(N=nsim,Estimado=round(estim,2),
                 LI1=round(liminf1,2),
                 LS1=round(limsup1,2),
                 LI2=round(liminf2,2),
                 LS2=round(limsup2,2))
    })
  })
  output$Result <- renderPrint({ 
    ldply(funMC())
  })
  output$graf <- renderPlot({
    x <- sort(runif(input$numon1,input$a,input$b))
    y <- sapply(x, fun1())
    rat1 <- abs(max(x)-min(x))/abs(max(y)-min(y))
    ggplot(data.frame(x,y),aes(x,y))+geom_line(colour="#990000",size=2)+
      geom_area(fill="#FA5A57")+theme_bw()+coord_fixed(ratio = rat1)
    })
  output$grafint <- renderPlot({
    data <- data.frame(ldply(funMC()))
    integral <- integrate(fun1(),input$a,input$b)$value
    ggplot(data, aes(x=N)) +
      geom_ribbon(aes(ymin=LI1, ymax=LS1), fill="#FA5A57", alpha=.4) + 
      geom_ribbon(aes(ymin=LI2, ymax=LS2), fill="#FA5A57", alpha=.4) +
      geom_line(aes(y=Estimado), colour="#990000") + theme_bw()+
      geom_hline(aes(yintercept=integral) , colour="red", linetype="dotted", size=1)
  })
########################tarea 3 ############################
  output$contents <- renderTable({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    read.csv(inFile$datapath,header = TRUE)
  })
}
