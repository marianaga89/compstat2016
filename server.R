
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
  fun1 <- reactive({
    texto <- paste("aux <- ",input$expresion1)
    eval(parse(text = texto))
    aux
  })

  funMC <- eventReactive(input$go2,{
    N <- c(10,100,1000,10000,100000)
    lapply(N, function(nsim){
      X <- runif(nsim,input$a,input$b)
      xtr <- seq(input$a,input$b, by=((input$b-input$a)/nsim))
      fi <- sapply(xtr, fun1())
      PHIX <- (input$b-input$a)*sapply(X, fun1())
      estim <- round(mean(PHIX),4)
      estimtr <- ((input$b-input$a)/(2*nsim))*sum(fi[-1]+fi[-(nsim+1)])
      var <- var(PHIX)
      quant1 <- qnorm((input$alpha1)/2, lower.tail=FALSE)
      quant2 <- qnorm((input$alpha2)/2, lower.tail=FALSE)
      limsup1 <- round(estim + sqrt(var/nsim)*quant1,4)
      liminf1 <- round(estim - sqrt(var/nsim)*quant1,4)
      limsup2 <- round(estim + sqrt(var/nsim)*quant2,4)
      liminf2 <- round(estim - sqrt(var/nsim)*quant2,4)
      data.frame(N=nsim,
                 real = round(integrate(fun1(),input$a,input$b)$value,2),
                 Estim_MC=round(estim,2),
                 Err_MC = round(integrate(fun1(),input$a,input$b)$value,2)-round(estim,2),
                 Estim_TR=round(estimtr,2),
                 Err_TR=round(integrate(fun1(),input$a,input$b)$value,2)-round(estimtr,2),
                 MC.LI1=round(liminf1,2),
                 MC.LS1=round(limsup1,2),
                 MC.LI2=round(liminf2,2),
                 MC.LS2=round(limsup2,2))
    })
  })
  output$Result <- renderPrint({
    ldply(funMC())
  })
  output$graf <- renderPlot({
    x <- sort(runif(1000,input$a,input$b))
    y <- sapply(x, fun1())
    rat1 <- abs(max(x)-min(x))/abs(max(y)-min(y))
    ggplot(data.frame(x,y),aes(x,y))+geom_line(colour="#B6659E",size=2)+
      geom_area(fill="#EEAAD9")+theme_bw()+coord_fixed(ratio = rat1)
    })
  output$grafint <- renderPlot({
    data <- data.frame(ldply(funMC()))
    integral <- integrate(fun1(),input$a,input$b)$value
    ggplot(data, aes(x=log(N))) +
      geom_ribbon(aes(ymin=MC.LI1, ymax=MC.LS1), fill="#EEAAD9", alpha=.4) + 
      geom_ribbon(aes(ymin=MC.LI2, ymax=MC.LS2), fill="#EEAAD9", alpha=.4) +
      geom_line(aes(y=Estim_MC), colour="#B6659E") + theme_bw()+
      geom_hline(aes(yintercept=integral) , colour="red", linetype="dotted", size=1)
  })
  output$grafcomp <- renderPlot({
    res <- data.frame(ldply(funMC()))
    ggplot(res, aes(x=log(N))) + 
      geom_line(aes(y=Err_TR, colour="Trapezoidal")) + 
      geom_line(aes(y=Err_MC, colour="MonteCarlo")) + 
      theme_bw()+
      scale_colour_manual("Error", values=c("#1BF9DC", "#B6659E"))
  })
  
########################tarea 4############################
  output$table <- DT::renderDataTable(DT::datatable({
    data <- read.csv("dat_ec.csv")
    data
  }))
  output$graft4 <- renderPlot({
    data <- read.csv("dat_ec.csv")
    rat1 <- abs(max(data$PESO)-min(data$PESO))/abs(max(data$TALLA)-min(data$TALLA))
    if(input$variabley==2){
      ggplot(data,aes(x=PESO,y=TALLA))+geom_point(colour="#B6659E",size=2,shape=1)+
        theme_bw()+coord_fixed(rat1)
    }
    else{
      ggplot(data,aes(x=TALLA,y=PESO))+geom_point(colour="#B6659E",size=2,shape=1)+
        theme_bw()+coord_fixed(rat1)
    }
    
  })
}
########################tarea 5############################