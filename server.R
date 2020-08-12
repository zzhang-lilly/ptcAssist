
library(shiny)
options(scipen = 500, shiny.maxRequestSize = 10*1024^2)

getFtab <- function(ns, xs){
  nn <- length(ns)
  nx <- length(xs)
  fdfs <- data.frame(ncase=xs)
  for(k in 1:nn){
    n <- ns[k]
    z <- numeric()
    df <- data.frame()
    for(i in 1:nx){
      x <- xs[i]
      y <- as.numeric(binom.test(x=x, n=n)$conf.int)
      df <- rbind( df, data.frame(n=n, orr=x/n, lower=y[1], upper=y[2]) )
    }
    fdfs <- cbind(fdfs, df)
  }
  return(fdfs)
}

getBtab <- function(ns, rs, a, b, r0, cl, xvec, nvec, r0vec){
  nn <- length(ns)
  nr <- length(rs)
  bdfs <- data.frame(ncase=round(ns[1]*rs))
  for(k in 1:nn){
    n <- ns[k]
    z <- numeric()
    df <- data.frame()
    for(i in 1:nr){
      x <- round(n*rs[i])
      y <- qbeta(c((1-cl)/2, 1-(1-cl)/2), x+a, n-x+b)
      tmp <- data.frame(n=n, orr=x/n, lower=y[1], upper=y[2])
      if(k>1) tmp <- cbind(data.frame(ncase=x), tmp)
      df <- rbind( df, tmp)
    }
    bdfs <- cbind(bdfs, df)
    
    dfs <- data.frame()
    for(sid in 1:length(xvec)){
      x <- xvec[sid];  n <- nvec[sid];  r0 <- r0vec[sid] 
      pObs <- 100*pbinom(x,n,r0)
      pExceeds <- 100*(1-pbeta(r0, x+a, n-x+b))
      tmp <- data.frame(x=x, n=n, ORR=100*r0, pObs=pObs, pExceeds=pExceeds)
      dfs <- rbind(dfs, tmp)
    }
    inds <- which(names(dfs) %in% c('ORR', 'pObs','pExceeds'))
    names(dfs)[inds] <- paste(names(dfs)[inds],'(%)')
  }
  return(list(bdfs=bdfs, dfs=dfs))
}


shinyServer(function(input, output){
  
  dat <- reactive({
    input$goButton
    isolate({
      # if(!is.null(importData())){
      fdfs <- getFtab(as.numeric(unlist(strsplit(input$ns,","))),
                      as.numeric(unlist(strsplit(input$xs,","))))
      
      # Bayesian
      tmp <- getBtab(as.numeric(unlist(strsplit(input$bns,","))),
                     as.numeric(unlist(strsplit(input$brs,","))),
                     input$a,
                     input$b, 
                     input$r0, 
                     input$cl,
                     as.numeric(unlist(strsplit(input$xvec,","))),
                     as.numeric(unlist(strsplit(input$nvec,","))),
                     as.numeric(unlist(strsplit(input$r0vec,","))))
      bdfs <- tmp$bdfs
      dfs <- tmp$dfs
      
      return(list(fdfs=fdfs, bdfs=bdfs, dfs=dfs))
      # }
    })
  })
  
  output$ftab <- renderTable({
    if(!is.null(dat())){
      tabs <- dat()$fdfs
      tabs
    }
  }, digits=2, include.rownames=FALSE, sanitize.text.function=function(x){x}, na=""
  )
  
  output$btab <- renderTable({
    if(!is.null(dat())){
      tabs <- dat()$bdfs
      tabs
    }
  }, digits=2, include.rownames=FALSE, sanitize.text.function=function(x){x}, na=""
  )
  
  output$btab2 <- renderTable({
    if(!is.null(dat())){
      tabs <- dat()$dfs
      tabs
    }
  }, digits=2, include.rownames=FALSE, sanitize.text.function=function(x){x}, na=""
  )
  
  
}) 


