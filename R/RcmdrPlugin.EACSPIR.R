# Codigo para el desarrollo del plugin de R-Commander para el manual EACSPIR #

.onAttach <- function(libname, pkgname){
  if (!interactive()) return()
  Rcmdr <- options()$Rcmdr
  plugins <- Rcmdr$plugins
  if (!pkgname %in% plugins) {
    Rcmdr$plugins <- c(plugins, pkgname)
    options(Rcmdr=Rcmdr)
    if("package:Rcmdr" %in% search()) {
      if(!getRcmdr("autoRestart")) {
        closeCommander(ask=FALSE, ask.save=TRUE)
        Commander()
      }
    }
    else {
      Commander()
    }
  }
}

# Pendiente de introducir funcion findGlobals() desarrollada por J. Fox #
if (getRversion() >= '2.15.1') globalVariables(c('errorMessage', 'top', 'alternativaVariable', 'pruebaVariable',
    'meds', 'Rangos', 'UMW', 'alternativaFrame', 'pruebaFrame', 'buttonsFrame', '..values', 'SCuadradosVariable',
    'datos.ANOVAMR', 'descriptivo.ANOVAMR', 'tablaVariable', '.Tabla', 'tablaFrame', 'porcentajesVariable',
    'subsetVariable', '.porcentajes', '.esperadas', '.Jicuadrado', '.Componentes', '.phi', '.Coef.Contingencia',
    '.sakoda', '.chuprov', '.VCramer', '.Q', '.Y', '.V', 'lambda.a.b', 'lambda.b.a', '.lambda', 'tau.a.b', 'tau.b.a',
    'theil.a.b', 'theil.b.a', '.theil', 'porcentajesFrame', 'subsetFrame', 'covariancia', 'correlacion', 'R2pearson',
    'R2spearman', 'R2kendall', 'cond1', 'cond2', 'cond3', 'cond4', 'E.X', 'E.Y', 'E.XY', 'gammaGK', 'tau.a', 'tau.b',
    'tau.c', 'sommers.x.y', 'sommers.y.x', '.sommers', '.wilson', 'dtm', 'cond', 'ok', '.tabla', '.x', 'escalaVariable',
    '.xfit', '.yfit', '.h', 'escalaFrame', '.odds', '.moda', '.RV', '.blau', '.ni', 'indicescat', '.IVQ', '.teachman',
    'curtosis.estandar', 'valor.p', 'curtosis', 'error.curtosis', 'sim.estandar', 'sesgo', 'error.simetria', 'meddif',
    'Wilcoxon', '.groups', 'groupsFrame', 'Rcmdr.resumen.categoricas', '.TablaResumen', '.media', '.mediana',
    '.media.geom', '.trimedia', '.Finf', '.Md', '.Fsup', '.promcuartiles', '.Q1', '.Q3', '.midR', '.min', '.max',
    '.media.rec', '.dt', '.variancia', '.dt.geometrica', '.desv.media', '.CV', '.rango', '.IQR', '.mad', '.CVR', '.DQ',
    '.AC', '.ACinf', '.ACsup', '.Q2', '.Pct', '.H1', '.H3', '.AC90', '.AC10', '.beta1', '.gamma1', '.n', '.K2', '.K3',
    '.Einf', '.Esup', '.beta2', '.gamma2', 'ywttest', 'TWilcoxon'))

resumen.categoricas <- function(){
  initializeDialog(title=gettextRcmdr("Resumen distribucion de frecuencias"))
  listaVar <- variableListBox(top, Factors(), selectmode="multiple",
                              title=gettextRcmdr("Variables (escoja una o mas)"))
  opcionesFrame <- tkframe(top)
  echocodigoVariable <- tclVar("0")
  echoCheckBox <- tkcheckbutton(opcionesFrame, variable=echocodigoVariable)
  creahtmlVariable <- tclVar("0")
  htmlCheckBox <- tkcheckbutton(opcionesFrame, variable=creahtmlVariable)
  onOK <- function()
  {
    x <- getSelection(listaVar)
    if (length(x) == 0){
      errorCondition(recall=Rcmdr.resumen.categoricas, 
                     message=gettextRcmdr("Debe escoger una variable."))
      return()
    }
    .BaseDatosActiva <- ActiveDataSet()
    echocodigo <- tclvalue(echocodigoVariable)
    creahtml <- tclvalue(creahtmlVariable)
    if (creahtml == 1)
    {
      require(R2HTML)
      if (!file.exists("Informe de Resultados.html"))
        .archivo <- HTMLInitFile(file.path(getwd()),
                                 "Informe de Resultados", BackGroundColor="#FFFFCC")
      else
        .archivo <- file.path(getwd(), "Informe de Resultados.html") 
    }
    for (variable in x)
    {
      instruccion1 <- paste(".ni <- table(", .BaseDatosActiva, "$", variable, ",useNA='always')", sep="")
      instruccion2 <- ".fi <- .ni/sum(.ni)"
      instruccion3 <- ".Ni <- cumsum(.ni)"
      instruccion4 <- ".Fi <- cumsum(.fi)"
      instruccion5 <- paste(".TablaResumen <- as.data.frame(matrix(round(cbind(.ni,.Ni,.fi,.Fi),2), ",
                            "nrow=nrow(.ni), byrow=FALSE,",
                            " dimnames=list(c(levels(", .BaseDatosActiva, "$", variable,
                            " ),'NAs'),c('ni','Ni','fi','Fi'))))",sep="")
      if (echocodigo == 1)
      {
        logger(instruccion1)
        logger(instruccion2)
        logger(instruccion3)
        logger(instruccion4)
        logger(instruccion5)
      }
      justDoIt(instruccion1)
      justDoIt(instruccion2)
      justDoIt(instruccion3)
      justDoIt(instruccion4)
      justDoIt(instruccion5)
      doItAndPrint(paste(".TablaResumen # Resumen distribucion de frecuencias para", variable))
      if (creahtml == 1)
      {
        titulo <- paste("Distribucion frecuencias para variable ", 
                        variable,sep="")
        HTML(as.title(titulo),file=.archivo)
        HTML(.TablaResumen, file=.archivo)
        HTMLhr(file = .archivo)                   
      }
    }
    closeDialog()
    if (echocodigo == 1) logger("remove(list=c('.TablaResumen','.ni','.Ni','.fi','.Fi'))") 
    remove(.TablaResumen, envir=.GlobalEnv)  
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="table")
  tkgrid(getFrame(listaVar), sticky="nw") 
  tkgrid(labelRcmdr(opcionesFrame,
                    text=gettextRcmdr("Opciones"), fg="blue"), sticky="w")
  tkgrid(labelRcmdr(opcionesFrame, 
                    text=gettextRcmdr("Mostrar en pantalla el codigo de R ejecutado ")),
         echoCheckBox, sticky="w")
  tkgrid(labelRcmdr(opcionesFrame,
                    text=gettextRcmdr("Generar informe de resultados ")),
         htmlCheckBox,sticky="w")
  tkgrid(opcionesFrame, sticky="w")
  tkgrid(buttonsFrame, sticky="w")
  dialogSuffix(rows=4, columns=2)
}
    
indices.categoricas <- function(){
    initializeDialog(title=gettextRcmdr("Otros indices para variables categoricas"))
    listaVar <- variableListBox(top, Factors(), selectmode="multiple",
    title=gettextRcmdr("Variables (escoja una o mas)"))
    opcionesFrame <- tkframe(top)
    numcatFrame <- tkframe(opcionesFrame)
    kVariable <- tclVar(gettextRcmdr("<auto>"))
    kField <- ttkentry(numcatFrame, width="8", textvariable=kVariable)
    echocodigoVariable <- tclVar("0")
    echoCheckBox <- tkcheckbutton(opcionesFrame, variable=echocodigoVariable)
    oddsVariable <- tclVar("0")
    oddsCheckBox <- tkcheckbutton(opcionesFrame, variable=oddsVariable)
    modaVariable <- tclVar("0")
    modaCheckBox <- tkcheckbutton(opcionesFrame, variable=modaVariable)
    RVVariable <- tclVar("0")
    RVCheckBox <- tkcheckbutton(opcionesFrame, variable=RVVariable)
    blauVariable <- tclVar("0")
    blauCheckBox <- tkcheckbutton(opcionesFrame, variable=blauVariable)
    IVQVariable <- tclVar("0")
    IVQCheckBox <- tkcheckbutton(opcionesFrame, variable=IVQVariable)
    teachmanVariable <- tclVar("0")
    teachmanCheckBox <- tkcheckbutton(opcionesFrame, variable=teachmanVariable)
    creahtmlVariable <- tclVar("0")
    htmlCheckBox <- tkcheckbutton(opcionesFrame, variable=creahtmlVariable)
    onOK <- function(){
        x <- getSelection(listaVar)
        if (length(x) == 0){
            errorCondition(recall=indices.categoricas, message=gettextRcmdr("Debes escoger una variable."))
            return()
            }
           .BaseDatosActiva <- ActiveDataSet()
           echocodigo <- tclvalue(echocodigoVariable)
           oddsval <- tclvalue(oddsVariable)
           modaval <- tclvalue(modaVariable)
           RVval <- tclvalue(RVVariable)
           blauval <- tclvalue(blauVariable)
           IVQval <- tclvalue(IVQVariable)
           teachmanval <- tclvalue(teachmanVariable)
           creahtml <- tclvalue(creahtmlVariable)
           selec <- as.numeric(modaval) + as.numeric(oddsval) + as.numeric(RVval) +
                    as.numeric(blauval) + as.numeric(IVQval) + 
                    as.numeric(teachmanval)
           if (selec == 0){
             errorCondition(recall=indices.categoricas, 
             message=gettextRcmdr("Debe escoger algun indicador."))
             return()
           }
           if (creahtml == 1)
           {
             require(R2HTML)
             if (!file.exists("Informe de Resultados.html"))
               .archivo <- HTMLInitFile(file.path(getwd()),
                           "Informe de Resultados", BackGroundColor="#FFFFCC")
             else
               .archivo <- file.path(getwd(), "Informe de Resultados.html")
             numfilas <- as.numeric(RVval)+as.numeric(blauval)+
                         as.numeric(IVQval)+as.numeric(teachmanval)
             numcolumnas <- length(x)
             instruccion <- paste(".TablaResultados <- as.data.frame(matrix(nrow=",numfilas,",ncol=",
             numcolumnas,"))")
             justDoIt(instruccion)
             colnames(.TablaResultados) <- x
             titulo <- "Descriptivos para variables categoricas"
             HTML(as.title(titulo),file=.archivo)
           }
           if ( (IVQval == 1) && (tclvalue(kVariable) != gettextRcmdr("<auto>")) )
           {
             vars <- 0
             k <- c(gsub(" ", ",",gsub(", ", ",",tclvalue(kVariable))))
             k <- unlist(strsplit(k,","))
             for (variable in x)
             {
               vars <- vars + 1
             }
             if (length(k) < vars)
             {
               tclvalue(kVariable) <- gettextRcmdr("<auto>")
              Message(message=gettextRcmdr("Vector de categorias, invalido se utilizara '<auto>'."),
                type="warning")
             }
           }
        j <- 0
        for (variable in x)
        {
          j <- j+1
          i <- 0
          instruccion1 <- paste(".ni <- table(", .BaseDatosActiva, "$", variable, ")", sep="")
          justDoIt(instruccion1)
          if (echocodigo == 1)
          {
            logger(instruccion1)
          }
          if (oddsval == 1)
          {
            instruccion2 <- ".odds <- round(.ni/(sum(.ni)-.ni), 2)"
            justDoIt(instruccion2)
            if (echocodigo == 1) logger(instruccion2)              
            doItAndPrint(paste(".odds  # Odds para", variable))
            if (echocodigo == 1) logger("remove(.odds)")
            if (creahtml == 1)
            {
             subtitulo <- paste("Odds para variable ",variable,": ",sep="")      
             HTML(subtitulo,file=.archivo)
             .oddstabla <- as.data.frame(.odds)
	           colnames(.oddstabla) <- c("Niveles","Odds")
	           HTML(.oddstabla,file=.archivo)
            }
            remove(.odds, envir=.GlobalEnv)
          }
          if (modaval == 1)
          {
            if (is.numeric(variable))
            {
              instruccion2 <- ".moda <- as.numeric(names(.ni)[which(.ni==max(.ni))])"              
            }
            else
              instruccion2 <-".moda <- names(.ni)[which(.ni==max(.ni))]"
            justDoIt(instruccion2)
            if (echocodigo == 1) logger(instruccion2)              
            doItAndPrint(paste(".moda  # Moda para", variable))
            if (echocodigo == 1) logger("remove(.moda)")
            if (creahtml == 1)
            {
             subtitulo <- paste("Moda para variable ",variable,": ",sep="")
             .modatabla <- as.data.frame(.moda)
             colnames(.modatabla) <- "Moda"
             HTML(.modatabla,file=.archivo)                   
             HTML(subtitulo,file=.archivo)
            }
            remove(.moda, envir=.GlobalEnv)
          }
          if (RVval == 1)
          {
            instruccion2 <- ".RV <- round(1 - max(.ni)/sum(.ni),2)"
            justDoIt(instruccion2)
            if (echocodigo == 1) logger(instruccion2)             
            doItAndPrint(paste(".RV  # Indice RV para", variable))
            if (echocodigo == 1) logger("remove(.RV)")
            if (creahtml == 1)
            {
              i <- i+1
              if (j == 1) rownames(.TablaResultados)[i] <- "R. Variacion"
              .TablaResultados[i,j] <- .RV
            }
            remove(.RV, envir=.GlobalEnv)
          }
          if (blauval == 1)
          {
            instruccion2 <- ".blau <- round(1-sum(prop.table(.ni)^2),2)"
            justDoIt(instruccion2)
            if (echocodigo == 1) logger(instruccion2)              
            doItAndPrint(paste(".blau  # Indice de Blau para", variable)) 
            if (echocodigo == 1) logger("remove(.blau)")
            if (creahtml == 1)
            {
              i <- i+1
              if (j == 1) rownames(.TablaResultados)[i] <- "Blau"
              .TablaResultados[i,j] <- .blau
            }
            remove(.blau, envir=.GlobalEnv)
          }
          if (IVQval == 1)
          {            
             opts <- options(warn=-1)
             options(opts)

             if (tclvalue(kVariable) == gettextRcmdr("<auto>"))
               kval <- length(.ni)
             else kval <- as.numeric(k[j]) 
             if (is.na(kval) || kval < 1)
            {
              errorCondition(recall=indicescat,
                             message=gettextRcmdr
                             ("El numero de categorias k debe ser un numero positivo"))
              return()
            }
            if (kval < length(.ni))
            {
              errorCondition(recall=indicescat,
                             message=gettextRcmdr
                             (paste("El numero de categorias k debe ser mayor que ",length(.ni)," .")))
                             return()          
            }                 
            instruccion2 <- paste(".IVQ <- round((1-sum(prop.table(.ni)^2))/((",kval,"-1)/",
                                  kval,"),2)",sep="")
            justDoIt(instruccion2)
            if (echocodigo == 1) logger(instruccion2)            
            doItAndPrint(paste(".IVQ  # Indice IVQ para", variable))
            if (echocodigo == 1) logger("remove(.IVQ)")
            if (creahtml == 1)
            {
              i <- i+1
              if (j == 1) rownames(.TablaResultados)[i] <- "IVQ"
              .TablaResultados[i,j] <- .IVQ
            }
            remove(.IVQ, envir=.GlobalEnv)
          }
          if (teachmanval == 1)
          {
            instruccion2 <- ".teachman <- round(-sum(prop.table(.ni)*log(prop.table(.ni))),2)"
            justDoIt(instruccion2)
            if (echocodigo == 1) logger(instruccion2)              
            doItAndPrint(paste(".teachman  # Indice de Teachman para", variable))  
            if (echocodigo == 1) logger("remove(.teachman)")
            if (creahtml == 1)
            {
              i <- i+1
              if (j == 1) rownames(.TablaResultados)[i] <- "Teachman"
              .TablaResultados[i,j] <- .teachman
            }
            remove(.teachman, envir=.GlobalEnv)
          }
        closeDialog()
        if (echocodigo == 1) logger("remove(.ni)")
        remove(.ni, envir=.GlobalEnv)
        tkfocus(CommanderWindow())
        }
    if (creahtml == 1)
    {
      if (numfilas >= 1)
      {
	      HTML("Otros indices descriptivos", file=.archivo)
        HTML(.TablaResultados, file=.archivo)
      }
      HTMLhr(file = .archivo)
    }
    remove(.TablaResultados, envir=.GlobalEnv)
    }
    OKCancelHelp(helpSubject="RcmdrPlugin.EACSPIR-package")
    tkgrid(getFrame(listaVar), sticky="nw") 
    tkgrid(labelRcmdr(opcionesFrame,
    text=gettextRcmdr("Indices"), fg="blue"), columnspan=6, sticky="w")
    tkgrid(labelRcmdr(opcionesFrame, 
    text=gettextRcmdr("Odds ")), 
    oddsCheckBox, sticky="w")
    tkgrid(labelRcmdr(opcionesFrame, 
    text=gettextRcmdr("Moda ")), 
    modaCheckBox, sticky="w")
    tkgrid(labelRcmdr(opcionesFrame, 
    text=gettextRcmdr("Razon de Variacion ")), 
    RVCheckBox, sticky="w")
    tkgrid(labelRcmdr(opcionesFrame, 
    text=gettextRcmdr("Indice de Diversidad de Blau ")), 
    blauCheckBox, sticky="w")
    tkgrid(labelRcmdr(numcatFrame, text=gettextRcmdr("k = ")), kField, sticky="w")
    tkgrid(labelRcmdr(opcionesFrame, text="Indice de Variacion Cualitativa"), IVQCheckBox, numcatFrame, sticky="w")
    tkgrid(labelRcmdr(opcionesFrame, 
    text=gettextRcmdr("Indice de Diversidad de Teachman ")), 
    teachmanCheckBox, sticky="w")
    tkgrid(labelRcmdr(opcionesFrame,
    text=gettextRcmdr("Opciones"), fg="blue"), sticky="w")
    tkgrid(labelRcmdr(opcionesFrame, 
    text=gettextRcmdr("Mostrar en pantalla el codigo de R ejecutado ")), 
    echoCheckBox, sticky="w")
    tkgrid(labelRcmdr(opcionesFrame,
    text=gettextRcmdr("Generar informe de resultados ")),
    htmlCheckBox,sticky="w")
    tkgrid(opcionesFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=10, columns=2)
    }

grafico.Pareto <- function(){
    initializeDialog(title=gettextRcmdr("Diagrama de Pareto"))
    selecVar <- variableListBox(top, Factors(), title=gettextRcmdr("Variables (escoja una)"))
    opcionesFrame <- tkframe(top)
    radioButtons(top, name="tabla", buttons=c("niBoton", "fiBoton"), values=c("ni", "fi"),
    labels=gettextRcmdr(c("Frecuencias Absolutas", "Frecuencias Relativas")),
    title=gettextRcmdr("Tablas basadas en:"))
    echocodigoVariable <- tclVar("0")
    echoCheckBox <- tkcheckbutton(opcionesFrame, variable=echocodigoVariable)
    creahtmlVariable <- tclVar("0")
    htmlCheckBox <- tkcheckbutton(opcionesFrame, variable=creahtmlVariable)
    onOK <- function(){
        .BaseDatosActiva <- ActiveDataSet()
        variable <- getSelection(selecVar)
        echocodigo <- tclvalue(echocodigoVariable)
        creahtml <- tclvalue(creahtmlVariable)
        tabVariable <- as.character(tclvalue(tablaVariable))
        if (length(variable) == 0){
            errorCondition(recall=grafico.Pareto, message=gettextRcmdr("Debe escoger una variable."))
            return()
            }
        if (creahtml == 1)
        {
          require(R2HTML)
          if (!file.exists("Informe de Resultados.html"))
            .archivo <- HTMLInitFile(file.path(getwd()),
            "Informe de Resultados", BackGroundColor="#FFFFCC")
          else
            .archivo <- file.path(getwd(), "Informe de Resultados.html")
        }
        if (tabVariable == "ni")
        {
        instruccion1 <- paste(".tabla <- table(", .BaseDatosActiva, "$", variable, ")", sep="")
        justDoIt(instruccion1)    
        }
        if (tabVariable == "fi")
        {  
        instruccion1 <- paste(".tabla <- table(",.BaseDatosActiva,"$",variable,")/(sum(table(",.BaseDatosActiva,"$",variable,")))",sep="")
        justDoIt(instruccion1)
        }
        instruccion2 <- ".tabla <- .tabla[order(-.tabla)]"
        justDoIt(instruccion2)
        titulo <- paste('Diagrama de Pareto para ', variable, sep="")
        tituloy <- if (tabVariable == "ni") "Frecuencias Absolutas"
        else "Frecuencias Relativas"
        instruccion3 <- "par(mar=c(5,4,4,4))"
        instruccion4 <- paste(".x <- barplot(.tabla, main='",titulo,"',",
                        "ylab='",tituloy,"',ylim=c(0,sum(.tabla)*1.05),",
                        "col=heat.colors(length(.tabla)))",sep="")
        instruccion5 <- "lines(.x[1:length(.tabla)],cumsum(.tabla),type='b')"
        instruccion6 <- "box()"
        instruccion7 <- paste("axis(4,at=seq(0,max(cumsum(.tabla)),length=5),",
                        "labels=paste(seq(0,1,length=5)*100,'%',sep=''))",sep='')
        instruccion8 <- "mtext('Porcentaje Acumulado', 4, line=2.5, las=3)"
        justDoIt(instruccion4)
        justDoIt(instruccion3)
        justDoIt(instruccion4) 
        justDoIt(instruccion5)
        justDoIt(instruccion6)
        justDoIt(instruccion7)
        justDoIt(instruccion8)
        if (echocodigo == 1)
        {
          logger(instruccion1)
          logger(instruccion2)
          logger(instruccion3)
          logger(instruccion4)
          logger(instruccion5)
          logger(instruccion6)
          logger(instruccion7)
          logger(instruccion8)
          logger("remove(.tabla)")
          logger("remove(.x)")          
          remove(.tabla, envir=.GlobalEnv)
          remove(.x, envir=.GlobalEnv)
        }
        if (creahtml == 1)
        {
          titulo <- paste("Diagrama de Pareto para variable ",variable,sep="")
          HTML(as.title(titulo),file=.archivo)
          nombre.archivo <- paste("ParetoR",gsub(":","",substr(Sys.time(),12,19)),
          ".jpg",sep="")
          dev.print(jpeg, filename=paste(getwd(),"/",nombre.archivo,sep=""),
          width=500, height=500)
          HTMLInsertGraph(nombre.archivo,file=.archivo,append=TRUE)
          HTMLhr(file = .archivo)
        }
        closeDialog()        
        tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject="RcmdrPlugin.EACSPIR")
    tkgrid(getFrame(selecVar), sticky="nw")
    tkgrid(tablaFrame, sticky="w")
    tkgrid(labelRcmdr(opcionesFrame,
    text=gettextRcmdr("Opciones"), fg="blue"), sticky="w")
    tkgrid(labelRcmdr(opcionesFrame, 
    text=gettextRcmdr("Mostrar en pantalla el codigo de R ejecutado ")), 
    echoCheckBox, sticky="w")
    tkgrid(labelRcmdr(opcionesFrame,
    text=gettextRcmdr("Generar informe de resultados ")),
    htmlCheckBox,sticky="w")
    tkgrid(opcionesFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")        
    dialogSuffix(rows=6, columns=1)
    }

resumen.ordinales <- function(){
    initializeDialog(title=gettextRcmdr("Indices para variables ordinales"))
    listaVar <- variableListBox(top, Factors(), selectmode="multiple",
    title=gettextRcmdr("Variables (escoja una o mas)"))
    opcionesFrame <- tkframe(top)
    tcFrame <- tkframe(opcionesFrame,borderwidth=2, relief="groove")
    echocodigoVariable <- tclVar("0")
    echoCheckBox <- tkcheckbutton(opcionesFrame, variable=echocodigoVariable)
    creahtmlVariable <- tclVar("0")
    htmlCheckBox <- tkcheckbutton(opcionesFrame, variable=creahtmlVariable)
    selectodasVariable <- tclVar("0")
    selectodasCheckBox <- tkcheckbutton(opcionesFrame, variable=selectodasVariable)
    medianaVariable <- tclVar("0")
    medianaCheckBox <- tkcheckbutton(tcFrame, variable=medianaVariable)
    modaVariable <- tclVar("0")
    modaCheckBox <- tkcheckbutton(tcFrame, variable=modaVariable)
    trimediaVariable <- tclVar("0")
    trimediaCheckBox <- tkcheckbutton(tcFrame, variable=trimediaVariable)
    promcuarVariable <- tclVar("0")
    promcuarCheckBox <- tkcheckbutton(tcFrame, variable=promcuarVariable)
    midRVariable <- tclVar("0")
    midRCheckBox <- tkcheckbutton(tcFrame, variable=midRVariable)
    medrecVariable <- tclVar("0")
    medrecCheckBox <- tkcheckbutton(tcFrame, variable=medrecVariable)    
    trimFrame <- tkframe(tcFrame)
    trimVariable <- tclVar(gettextRcmdr("0.05"))
    trimField <- ttkentry(trimFrame, width="8", textvariable=trimVariable)
    dispFrame <- tkframe(opcionesFrame,borderwidth=2, relief="groove")
    rangoVariable <- tclVar("0")
    rangoCheckBox <- tkcheckbutton(dispFrame, variable=rangoVariable)    
    IQRVariable <- tclVar("0")
    IQRCheckBox <- tkcheckbutton(dispFrame, variable=IQRVariable) 
    desvcuarVariable <- tclVar("0")
    desvcuarCheckBox <- tkcheckbutton(dispFrame, variable=desvcuarVariable)
    madVariable <- tclVar("0")
    madCheckBox <- tkcheckbutton(dispFrame, variable=madVariable)
    CVRVariable <- tclVar("0")
    CVRCheckBox <- tkcheckbutton(dispFrame, variable=CVRVariable)
    ACentVariable <- tclVar("0")
    ACentCheckBox <- tkcheckbutton(dispFrame, variable=ACentVariable)    
    ACFrame <- tkframe(dispFrame)
    ACVariable <- tclVar(gettextRcmdr("0.9"))
    ACField <- ttkentry(ACFrame, width="4", textvariable=ACVariable)
    posicFrame <- tkframe(opcionesFrame,borderwidth=2, relief="groove")
    minVariable <- tclVar("0")
    minCheckBox <- tkcheckbutton(posicFrame, variable=minVariable)
    maxVariable <- tclVar("0")
    maxCheckBox <- tkcheckbutton(posicFrame, variable=maxVariable)
    Q1Variable <- tclVar("0")
    Q1CheckBox <- tkcheckbutton(posicFrame, variable=Q1Variable)
    Q2Variable <- tclVar("0")
    Q2CheckBox <- tkcheckbutton(posicFrame, variable=Q2Variable)    
    Q3Variable <- tclVar("0")
    Q3CheckBox <- tkcheckbutton(posicFrame, variable=Q3Variable)
    percentVariable <- tclVar("0")
    percentCheckBox <- tkcheckbutton(posicFrame, variable=percentVariable)    
    percentFrame <- tkframe(posicFrame)
    percentilVariable <- tclVar("0, .25, .5, .75, 1")
    percentField <- ttkentry(percentFrame, width="15", textvariable=percentilVariable)
    formaFrame <- tkframe(opcionesFrame,borderwidth=2, relief="groove")
    H1Variable <- tclVar("0")
    H1CheckBox <- tkcheckbutton(formaFrame, variable=H1Variable)
    H3Variable <- tclVar("0")
    H3CheckBox <- tkcheckbutton(formaFrame, variable=H3Variable)
    K2Variable <- tclVar("0")
    K2CheckBox <- tkcheckbutton(formaFrame, variable=K2Variable)
    K3Variable <- tclVar("0")
    K3CheckBox <- tkcheckbutton(formaFrame, variable=K3Variable)
    onOK <- function(){
        x <- getSelection(listaVar)
        if (length(x) == 0)
        {
          errorCondition(recall=resumen.ordinales, message=gettextRcmdr("Debe escoger una variable."))
          return()
        }
        for (variable in x)
        {          
        justDoIt(paste("cond <- !is.ordered(",paste(ActiveDataSet(),"$",variable,sep=""),")",sep=""))
        if (cond){
            errorCondition(recall=resumen.ordinales, message=gettextRcmdr(paste("Variable ",variable, " no es ordinal.",sep='')))
            return()
            }
        remove("cond", envir=.GlobalEnv)
        }        
        .BaseDatosActiva <- ActiveDataSet()
        echocodigo <- tclvalue(echocodigoVariable)
        creahtml <- tclvalue(creahtmlVariable)
        selectodas <- tclvalue(selectodasVariable)
        if (selectodas == 1)
        {
          medianaval = modaval = trimediaval = promcuarval = midRval = 
          medrecval = rangoval = IQRval = desvcuarval =
          madval = CVRval = ACentval = minval = maxval = Q1val = Q2val =
          Q3val = percentval = H1val = H3val = K2val = K3val = TRUE
        }
        else
        {
          medianaval <- tclvalue(medianaVariable)
          modaval <- tclvalue(modaVariable)
          trimediaval <- tclvalue(trimediaVariable)
          promcuarval <- tclvalue(promcuarVariable)
          midRval <- tclvalue(midRVariable)
          medrecval <- tclvalue(medrecVariable)
          rangoval <- tclvalue(rangoVariable)
          IQRval <- tclvalue(IQRVariable)
          desvcuarval <- tclvalue(desvcuarVariable)
          madval <- tclvalue(madVariable)
          CVRval <- tclvalue(CVRVariable)
          ACentval <- tclvalue(ACentVariable)  
          minval <- tclvalue(minVariable)
          maxval <- tclvalue(maxVariable)
          Q1val <- tclvalue(Q1Variable)
          Q2val <- tclvalue(Q2Variable)
          Q3val <- tclvalue(Q3Variable)
          percentval <- tclvalue(percentVariable)
          H1val <- tclvalue(H1Variable)
          H3val <- tclvalue(H3Variable)
          K2val <- tclvalue(K2Variable)
          K3val <- tclvalue(K3Variable)
        }        
        selec <- as.numeric(medianaval) + as.numeric(modaval) + 
                 as.numeric(trimediaval) + as.numeric(promcuarval) +
                 as.numeric(midRval) + as.numeric(medrecval)
        selec2 <- as.numeric(rangoval) +
                 as.numeric(IQRval) + as.numeric(desvcuarval) +        
                 as.numeric(madval) + as.numeric(CVRval) +
                 as.numeric(ACentval)
        selec3 <- as.numeric(minval) + as.numeric(maxval) + 
                 as.numeric(Q1val) + as.numeric(Q2val) + 
                 as.numeric(Q3val) + as.numeric(percentval)
        selec4 <- as.numeric(H1val) + as.numeric(H3val) + 
                 as.numeric(K2val) + as.numeric(K3val)
        seleccion <- selec + selec2 + selec3 + selec4
        if (seleccion == 0){
          errorCondition(recall=resumen.ordinales, 
          message=gettextRcmdr("Debe escoger algun indicador."))
          return()
        }
        if (percentval == 1)
        {
          pct <- c(gsub(" ", ",",gsub(", ", ",",tclvalue(percentilVariable))))
          pct1 <- as.numeric(unlist(strsplit(pct,",")))
          if ( is.na(pct1) || (sum(pct1<0.0)>0) || (sum(pct1>1.0)>0) || (sum(!is.numeric(pct1))>0) )
          {
            pct <- paste(seq(0.,1.,.25),collapse=",")
            Message(message=gettextRcmdr("Vector de percentiles invalido. Se utilizara vector por defecto."),
            type="warning")
          }
        }
        if (creahtml == 1)
        {
          require(R2HTML)
          if (!file.exists("Informe de Resultados.html"))
          .archivo <- HTMLInitFile(file.path(getwd()),
                      "Informe de Resultados", BackGroundColor="#FFFFCC")
          else
            .archivo <- file.path(getwd(), "Informe de Resultados.html")
          if (selec > 0)
          {
            numfilas <- selec
            numcolumnas <- length(x)
            instruccion <- paste(".TablaResTC <- as.data.frame(matrix(nrow=",numfilas,",ncol=",
            numcolumnas,"))")
            justDoIt(instruccion)
            colnames(.TablaResTC) <- x
          }
          if (selec2 > 0)
          {
            numfilas <- selec2
            numcolumnas <- length(x)
            instruccion <- paste(".TablaResDisp <- as.data.frame(matrix(nrow=",numfilas,",ncol=",
            numcolumnas,"))")
            justDoIt(instruccion)
            colnames(.TablaResDisp) <- x
          }
          if (selec3 > 0)
          {
            if (percentval == 1)
            {
              numperc <-length(as.numeric(unlist(strsplit(pct,","))))
              numfilas <- selec3 + numperc - 1
            }
            else  numfilas <- selec3
            numcolumnas <- length(x)
            instruccion <- paste(".TablaResPosic <- as.data.frame(matrix(nrow=",numfilas,",ncol=",
            numcolumnas,"))")
            justDoIt(instruccion)
            colnames(.TablaResPosic) <- x
          }
          if (selec4 > 0)
          {
            numfilas <- selec4
            numcolumnas <- length(x)
            instruccion <- paste(".TablaResForm <- as.data.frame(matrix(nrow=",numfilas,",ncol=",
            numcolumnas,"))")
            justDoIt(instruccion)
            colnames(.TablaResForm) <- x
          }
          titulo <- "Indicadores descriptivos para variables ordinales"
          HTML(as.title(titulo),file=.archivo)
        }
        j <- 0
        for (variable in x)
        {
          i <- 0
          j <- j + 1
          k <- 0
          m <- 0
          l <- 0
          if (medianaval == 1)
          {
            instruccion <- paste(".mediana <- median(as.numeric(",.BaseDatosActiva,"$",variable,"),na.rm=TRUE)",sep="")
            justDoIt(instruccion)
            if (echocodigo == 1) logger(instruccion)              
            doItAndPrint(paste(".mediana  # Mediana para", variable))
            if (echocodigo == 1) logger("remove(.mediana)")
            if (creahtml == 1)
            {
              i <- i+1
              if (j == 1) rownames(.TablaResTC)[i] <- "Mediana"
              .TablaResTC[i,j] <- .mediana
            }
            remove(.mediana, envir=.GlobalEnv)
          }
          if (modaval == 1)
          {     
            instruccion1 <- paste(".ni <- table(as.numeric(", .BaseDatosActiva, "$", variable, "))", sep="")
            justDoIt(instruccion1)
            if (echocodigo == 1)
            {
              logger(instruccion1)
            }                
            instruccion2 <- ".moda <- as.numeric(names(.ni)[which(.ni==max(.ni))])"              
            justDoIt(instruccion2)
            if (echocodigo == 1) logger(instruccion2)            
            doItAndPrint(paste(".moda  # Moda para", variable))
            if (echocodigo == 1)
            {
             logger("remove(.ni)")
             logger("remove(.moda)")
            }
            if (creahtml == 1)
            {
              i <- i+1
              if (j == 1) rownames(.TablaResTC)[i] <- "Moda"
              if (length(.moda) > 1)
              {
                .moda1 <- .moda[1]
  	            Message(message=gettextRcmdr(paste("Variable ",variable," tiene mas de una moda: ",
                "En documento HTML se muestra un solo valor.", sep="")),
            		type="warning")
                .TablaResTC[i,j] <- .moda1
	            }
              else
              .TablaResTC[i,j] <- .moda
            }
            remove(.ni, envir=.GlobalEnv)
            remove(.moda, envir=.GlobalEnv)
          }
          if (trimediaval == 1)
          {
            instruccion1 <- paste(".Finf <- fivenum(as.numeric(",.BaseDatosActiva,"$",variable,"))[2]",sep="")
            justDoIt(instruccion1)
            instruccion2 <- paste(".Md <- fivenum(as.numeric(",.BaseDatosActiva,"$",variable,"))[3]",sep="")
            justDoIt(instruccion2)
            instruccion3 <- paste(".Fsup <- fivenum(as.numeric(",.BaseDatosActiva,"$",variable,"))[4]",sep="")
            justDoIt(instruccion3)
            instruccion4 <- "(.trimedia <- .Finf+2*.Md+.Fsup)/4"
            justDoIt(instruccion4)
            if (echocodigo == 1) 
            {
              logger(instruccion1)
              logger(instruccion2)
              logger(instruccion3)
              logger(instruccion4)
            }
            doItAndPrint(paste(".trimedia  # Trimedia para", variable))
            if (echocodigo == 1)
            {
              logger("remove(.Finf)")              
              logger("remove(.Md)")              
              logger("remove(.Fsup)")              
              logger("remove(.trimedia)")
            }
            if (creahtml == 1)
            {
              i <- i+1
              if (j == 1) rownames(.TablaResTC)[i] <- "Trimedia"
              .TablaResTC[i,j] <- .trimedia
            }
            remove(.Finf, envir=.GlobalEnv)
            remove(.Md, envir=.GlobalEnv)
            remove(.Fsup, envir=.GlobalEnv)
            remove(.trimedia, envir=.GlobalEnv)
          }
          if (promcuarval == 1)
          {
            instruccion1 <- paste(".Q1 <- quantile(as.numeric(",.BaseDatosActiva,"$",variable,"),na.rm=TRUE)[2]",sep="")
            justDoIt(instruccion1)
            instruccion2 <- paste(".Q3 <- quantile(as.numeric(",.BaseDatosActiva,"$",variable,"),na.rm=TRUE)[4]",sep="")
            justDoIt(instruccion2)
            instruccion3 <- ".promcuartiles <- (.Q1+.Q3)/2"
            justDoIt(instruccion3)
            if (echocodigo == 1) 
            {
              logger(instruccion1)
              logger(instruccion2)
              logger(instruccion3)
            }
            justDoIt("names(.promcuartiles)<-NULL")
            doItAndPrint(paste(".promcuartiles  # Promedio de cuartiles para", variable))
            if (echocodigo == 1)
            {
              logger("remove(.Q1)")              
              logger("remove(.Q3)")              
              logger("remove(.promcuartiles)")              
            }
            if (creahtml == 1)
            {
              i <- i+1
              if (j == 1) rownames(.TablaResTC)[i] <- "Promedio Cuartiles"
              .TablaResTC[i,j] <- .promcuartiles
            }
            remove(.Q1, envir=.GlobalEnv)
            remove(.Q3, envir=.GlobalEnv)
            remove(.promcuartiles, envir=.GlobalEnv)
          }            
          if (midRval == 1)
          {
            instruccion1 <- paste(".min <- quantile(as.numeric(",.BaseDatosActiva,"$",variable,"),na.rm=TRUE)[1]",sep="")
            justDoIt(instruccion1)
            instruccion2 <- paste(".max <- quantile(as.numeric(",.BaseDatosActiva,"$",variable,"),na.rm=TRUE)[5]",sep="")
            justDoIt(instruccion2)
            instruccion3 <- ".midR <- (.min+.max)/2"
            justDoIt(instruccion3)
            if (echocodigo == 1) 
            {
              logger(instruccion1)
              logger(instruccion2)
              logger(instruccion3)
            }
            justDoIt("names(.midR)<-NULL")
            doItAndPrint(paste(".midR  # Rango medio para", variable))
            if (echocodigo == 1)
            {
              logger("remove(.min)")              
              logger("remove(.max)")              
              logger("remove(.midR)")              
            }
            if (creahtml == 1)
            {
              i <- i+1
              if (j == 1) rownames(.TablaResTC)[i] <- "Rango medio"
              .TablaResTC[i,j] <- .midR
            }
            remove(.min, envir=.GlobalEnv)
            remove(.max, envir=.GlobalEnv)
            remove(.midR, envir=.GlobalEnv)
          }
            
          if (medrecval == 1)
          {
            rec <- as.numeric(tclvalue(trimVariable))
            if ( rec < .0 || rec > .5 || !is.numeric(rec) )
            {
              rec <- 0.05
              Message(message=gettextRcmdr("Proporcion de recorte invalida se utilizara valor por defecto."),
                type="warning")              
            }
            instruccion <- paste(".media.rec <- mean(as.numeric(",.BaseDatosActiva,"$",variable,"),trim=",rec,",na.rm=TRUE)",sep="")
            justDoIt(instruccion)
            if (echocodigo == 1) logger(instruccion)             
            doItAndPrint(paste(".media.rec  # Media recortada para", variable))
            if (echocodigo == 1) logger("remove(.media.rec)")
            if (creahtml == 1)
            {
              i <- i+1
              if (j == 1) rownames(.TablaResTC)[i] <- paste("Media Recortada al ",
                          rec*100,"%",sep="")
              .TablaResTC[i,j] <- .media.rec
            }            
            remove(.media.rec, envir=.GlobalEnv)
          }          
          if (rangoval == 1)
          {
            instruccion1 <- paste(".min <- quantile(as.numeric(",.BaseDatosActiva,"$",variable,"),na.rm=TRUE)[1]",sep="")
            justDoIt(instruccion1)
            instruccion2 <- paste(".max <- quantile(as.numeric(",.BaseDatosActiva,"$",variable,"),na.rm=TRUE)[5]",sep="")
            justDoIt(instruccion2)
            instruccion3 <- ".rango <- .max-.min"
            justDoIt(instruccion3)
            if (echocodigo == 1) 
            {
              logger(instruccion1)
              logger(instruccion2)
              logger(instruccion3)
            }
            justDoIt("names(.rango)<-NULL")
            doItAndPrint(paste(".rango  # Amplitud para", variable))
            if (echocodigo == 1)
            {
              logger("remove(.min)")              
              logger("remove(.max)")              
              logger("remove(.rango)")              
            }
            if (creahtml == 1)
            {
              k <- k+1
              if (j == 1) rownames(.TablaResDisp)[k] <- "Amplitud"
              .TablaResDisp[k,j] <- .rango
            }
            remove(.min, envir=.GlobalEnv)
            remove(.max, envir=.GlobalEnv)
            remove(.rango, envir=.GlobalEnv)            
          }
          if (IQRval == 1)
          {
            instruccion1 <- paste(".Q1 <- quantile(as.numeric(",.BaseDatosActiva,"$",variable,"),na.rm=TRUE)[2]",sep="")
            justDoIt(instruccion1)
            instruccion2 <- paste(".Q3 <- quantile(as.numeric(",.BaseDatosActiva,"$",variable,"),na.rm=TRUE)[4]",sep="")
            justDoIt(instruccion2)
            instruccion3 <- ".IQR <- .Q3-.Q1"
            justDoIt(instruccion3)
            if (echocodigo == 1) 
            {
              logger(instruccion1)
              logger(instruccion2)
              logger(instruccion3)
            }
            justDoIt("names(.IQR)<-NULL")
            doItAndPrint(paste(".IQR  # Amplitud Intercuartil para", variable))
            if (echocodigo == 1)
            {
              logger("remove(.Q1)")              
              logger("remove(.Q3)")              
              logger("remove(.IQR)")              
            }
            if (creahtml == 1)
            {
              k <- k+1
              if (j == 1) rownames(.TablaResDisp)[k] <- "Amplitud Intercuartil"
              .TablaResDisp[k,j] <- .IQR
            }
            remove(.Q1, envir=.GlobalEnv)
            remove(.Q3, envir=.GlobalEnv)
            remove(.IQR, envir=.GlobalEnv)            
          }
          if (madval == 1)
          {
            instruccion1 <- paste(".mediana <- median(as.numeric(",.BaseDatosActiva,"$",variable,"),na.rm=TRUE)",sep="")
            justDoIt(instruccion1)
            instruccion2 <- paste(".mad <- median(abs(as.numeric(",.BaseDatosActiva,"$",variable,
                            ")-.mediana),na.rm=TRUE)",sep="")
            justDoIt(instruccion2)
            if (echocodigo == 1)
            {
              logger(instruccion1)
              logger(instruccion2)
            }
            doItAndPrint(paste(".mad  # Indice MAD para", variable))
            if (echocodigo == 1)
            {
              logger("remove(.mediana)")
              logger("remove(.mad)")
            }
            if (creahtml == 1)
            {
              k <- k+1
              if (j == 1) rownames(.TablaResDisp)[k] <- "MAD"
              .TablaResDisp[k,j] <- .mad
            }
            remove(.mediana, envir=.GlobalEnv)
            remove(.mad, envir=.GlobalEnv)            
          }
          if (CVRval == 1)
          {
            instruccion1 <- paste(".Finf <- fivenum(as.numeric(",.BaseDatosActiva,"$",variable,"))[2]",sep="")
            justDoIt(instruccion1)
            instruccion2 <- paste(".Fsup <- fivenum(as.numeric(",.BaseDatosActiva,"$",variable,"))[4]",sep="")
            justDoIt(instruccion2)
            instruccion3 <- ".CVR <- round((.Fsup-.Finf)/(.Finf+.Fsup), 2)"
            justDoIt(instruccion3)
            if (echocodigo == 1) 
            {
              logger(instruccion1)
              logger(instruccion2)
              logger(instruccion3)
            }
            doItAndPrint(paste(".CVR  # Coeficiente Variacion Robusto para", variable))
            if (echocodigo == 1)
            {
              logger("remove(.Finf)")                            
              logger("remove(.Fsup)")              
              logger("remove(.CVR)")
            }
            if (creahtml == 1)
            {
              k <- k+1
              if (j == 1) rownames(.TablaResDisp)[k] <- "Coef. Var. Robusto"
              .TablaResDisp[k,j] <- .CVR
            }
            remove(.Finf, envir=.GlobalEnv)
            remove(.Fsup, envir=.GlobalEnv)
            remove(.CVR, envir=.GlobalEnv)            
          }
          if (desvcuarval == 1)
          {
            instruccion1 <- paste(".Q1 <- quantile(as.numeric(",.BaseDatosActiva,"$",variable,"),na.rm=TRUE)[2]",sep="")
            justDoIt(instruccion1)
            instruccion2 <- paste(".Q3 <- quantile(as.numeric(",.BaseDatosActiva,"$",variable,"),na.rm=TRUE)[4]",sep="")
            justDoIt(instruccion2)
            instruccion3 <- ".DQ <- (.Q3-.Q1)/2"
            justDoIt(instruccion3)
            if (echocodigo == 1) 
            {
              logger(instruccion1)
              logger(instruccion2)
              logger(instruccion3)
            }
            justDoIt("names(.DQ)<-NULL")
            doItAndPrint(paste(".DQ  # Desviacion cuartil para", variable))
            if (echocodigo == 1)
            {
              logger("remove(.Q1)")              
              logger("remove(.Q3)")              
              logger("remove(.DQ)")              
            }
            if (creahtml == 1)
            {
              k <- k+1
              if (j == 1) rownames(.TablaResDisp)[k] <- "Desviacion cuartil"
              .TablaResDisp[k,j] <- .DQ
            }
            remove(.Q1, envir=.GlobalEnv)
            remove(.Q3, envir=.GlobalEnv)
            remove(.DQ, envir=.GlobalEnv)            
          }
          if (ACentval == 1)
          {
            propdat <- as.numeric(tclvalue(ACVariable))
            if ( propdat < .0 || propdat > 1. || !is.numeric(propdat) )
            {
              prop.dat <- 0.9
              Message(message=gettextRcmdr("Proporcion de datos invalida se utilizara valor por defecto."),
                type="warning")              
            }
            instruccion1 <- paste(".ACinf <- round(quantile(as.numeric(",.BaseDatosActiva,"$",variable,"),probs=",(1-propdat)/2,",na.rm=TRUE), 2)",sep="")
            justDoIt(instruccion1)
            instruccion2 <- paste(".ACsup <- round(quantile(as.numeric(",.BaseDatosActiva,"$",variable,"),probs=",1-(1-propdat)/2,",na.rm=TRUE), 2)",sep="")
            justDoIt(instruccion2)
            instruccion3 <- ".AC <- .ACsup-.ACinf"
            justDoIt(instruccion3)
            if (echocodigo == 1) 
            {
              logger(instruccion1)
              logger(instruccion2)
              logger(instruccion3)
            }
            justDoIt("names(.AC)<-NULL")
            doItAndPrint(paste(".AC  # Amplitud centilica con ",propdat*100,"% datos para ", variable,sep=""))
            if (echocodigo == 1)
            {
              logger("remove(.ACinf)")              
              logger("remove(.ACsup)")              
              logger("remove(.AC)")              
            }
            if (creahtml == 1)
            {
              k <- k+1
              if (j == 1) rownames(.TablaResDisp)[k] <- paste("Amplitud Centilica ",
                          propdat*100,"% datos",sep="")
              .TablaResDisp[k,j] <- .AC
            }
            remove(.ACinf, envir=.GlobalEnv)
            remove(.ACsup, envir=.GlobalEnv)
            remove(.AC, envir=.GlobalEnv)
          } 
          if (minval == 1)
          {
            instruccion <- paste(".min <-min(as.numeric(",.BaseDatosActiva,"$",variable,"),na.rm=TRUE)",sep="")
            justDoIt(instruccion)
            if (echocodigo == 1) logger(instruccion)             
            doItAndPrint(paste(".min  # Minimo valor para", variable))
            if (echocodigo == 1) logger("remove(.min)")
            if (creahtml == 1)
            {
              m <- m+1
              if (j == 1) rownames(.TablaResPosic)[m] <- "Minimo"
              .TablaResPosic[m,j] <- .min
            }
            remove(.min, envir=.GlobalEnv)            
          }
          if (maxval == 1)
          {
            instruccion <- paste(".max <- max(as.numeric(",.BaseDatosActiva,"$",variable,"),na.rm=TRUE)",sep="")
            justDoIt(instruccion)
            if (echocodigo == 1) logger(instruccion)              
            doItAndPrint(paste(".max  # Maximo valor para", variable))
            if (echocodigo == 1) logger("remove(.max)")
            if (creahtml == 1)
            {
              m <- m+1
              if (j == 1) rownames(.TablaResPosic)[m] <- "Maximo"
              .TablaResPosic[m,j] <- .max
            }
            remove(.max, envir=.GlobalEnv)            
          }
          if (Q1val == 1)
          {
            instruccion <- paste(".Q1 <- quantile(as.numeric(",.BaseDatosActiva,"$",variable,"),na.rm=TRUE)[2]",sep="")
            justDoIt(instruccion)
            if (echocodigo == 1) logger(instruccion)
            justDoIt("names(.Q1)<-NULL")
            doItAndPrint(paste(".Q1  # Primer cuartil para", variable))
            if (echocodigo == 1) logger("remove(.Q1)")
            if (creahtml == 1)
            {
              m <- m+1
              if (j == 1) rownames(.TablaResPosic)[m] <- "Primer Cuartil"
              .TablaResPosic[m,j] <- .Q1
            }
            remove(.Q1, envir=.GlobalEnv)           
          }  
          if (Q2val == 1)
          {
            instruccion <- paste(".Q2 <- quantile(as.numeric(",.BaseDatosActiva,"$",variable,"),na.rm=TRUE)[3]",sep="")
            justDoIt(instruccion)
            if (echocodigo == 1) logger(instruccion)
            justDoIt("names(.Q2)<-NULL")
            doItAndPrint(paste(".Q2  # Segundo cuartil para", variable))
            if (echocodigo == 1) logger("remove(.Q2)")
            if (creahtml == 1)
            {
              m <- m+1
              if (j == 1) rownames(.TablaResPosic)[m] <- "Segundo Cuartil"
              .TablaResPosic[m,j] <- .Q2
            }
            remove(.Q2, envir=.GlobalEnv)            
          }
          if (Q3val == 1)
          {
            instruccion <- paste(".Q3 <- quantile(as.numeric(",.BaseDatosActiva,"$",variable,"),na.rm=TRUE)[4]",sep="")
            justDoIt(instruccion)
            if (echocodigo == 1) logger(instruccion)
            justDoIt("names(.Q3)<-NULL")
            doItAndPrint(paste(".Q3  # Tercer cuartil para", variable))
            if (echocodigo == 1) logger("remove(.Q3)")
            if (creahtml == 1)
            {
              m <- m+1
              if (j == 1) rownames(.TablaResPosic)[m] <- "Tercer Cuartil"
              .TablaResPosic[m,j] <- .Q3
            }
            remove(.Q3, envir=.GlobalEnv)            
          }
          if (percentval == 1)
          {
             instruccion <- paste(".Pct <- quantile(as.numeric(",.BaseDatosActiva,"$",variable,"),probs=c(",pct,"),na.rm=TRUE)",sep="")
             justDoIt(instruccion)
             if (echocodigo == 1) logger(instruccion)             
             doItAndPrint(paste(".Pct  # Percentiles para", variable))
             if (echocodigo == 1) logger("remove(.PCt)")
            if (creahtml == 1)
            {
              m <- m+1
              pct1 <- as.numeric(unlist(strsplit(pct,",")))
              if (j == 1) rownames(.TablaResPosic)[m:(m+length(pct1)-1)] <- paste(
              "Percentil ",pct1*100,sep="")
              .TablaResPosic[m:(m+length(pct1)-1),j] <- .Pct
            }
             remove(.Pct, envir=.GlobalEnv)
          }
          if (H1val == 1)
          {
            instruccion1 <- paste(".Finf <- fivenum(as.numeric(",.BaseDatosActiva,"$",variable,"))[2]",sep="")
            justDoIt(instruccion1)
            instruccion2 <- paste(".Md <- fivenum(as.numeric(",.BaseDatosActiva,"$",variable,"))[3]",sep="")
            justDoIt(instruccion2)
            instruccion3 <- paste(".Fsup <- fivenum(as.numeric(",.BaseDatosActiva,"$",variable,"))[4]",sep="")
            justDoIt(instruccion3)
            instruccion4 <- ".H1 <- (.Finf+.Fsup-2*.Md)/(2*.Md)"
            justDoIt(instruccion4)
            if (echocodigo == 1) 
            {
              logger(instruccion1)
              logger(instruccion2)
              logger(instruccion3)
              logger(instruccion4)
            }
            doItAndPrint(paste(".H1  # Indice de Yule H1 para", variable))
            if (echocodigo == 1)
            {
              logger("remove(.Finf)")              
              logger("remove(.Md)")              
              logger("remove(.Fsup)")              
              logger("remove(.H1)")
            }
            if (creahtml == 1)
            {
              l <- l+1
              if (j == 1) rownames(.TablaResForm)[l] <- "Coef. Asim. H1"
              .TablaResForm[l,j] <- .H1
            }
            remove(.Finf, envir=.GlobalEnv)
            remove(.Md, envir=.GlobalEnv)
            remove(.Fsup, envir=.GlobalEnv)
            remove(.H1, envir=.GlobalEnv)            
          }
          if (H3val == 1)
          {
            instruccion1 <- paste(".AC90 <- quantile(as.numeric(",.BaseDatosActiva,"$",variable,"),probs=0.9,na.rm=TRUE)",sep="")
            justDoIt(instruccion1)
            instruccion2 <- paste(".AC10 <- quantile(as.numeric(",.BaseDatosActiva,"$",variable,"),probs=0.1,na.rm=TRUE)",sep="")
            justDoIt(instruccion2)
            instruccion3 <- paste(".Md <- fivenum(as.numeric(",.BaseDatosActiva,"$",variable,"))[3]",sep="")
            justDoIt(instruccion3)
            instruccion4 <- ".H3 <- (.AC90+.AC10-2*.Md)/(2*.Md)"
            justDoIt(instruccion4)
            if (echocodigo == 1) 
            {
              logger(instruccion1)
              logger(instruccion2)
              logger(instruccion3) 
              logger(instruccion4)
            }
            justDoIt("names(.H3)<-NULL")
            doItAndPrint(paste(".H3  # Indice de Kelly H3  para ", variable,sep=""))
            if (echocodigo == 1)
            {
              logger("remove(.AC90)")              
              logger("remove(.AC10)")              
              logger("remove(.Md)")
              logger("remove(.H3)")
            }
            if (creahtml == 1)
            {
              l <- l+1
              if (j == 1) rownames(.TablaResForm)[l] <- "Coef. Asim. H3"
              .TablaResForm[l,j] <- .H3
            }
            remove(.AC90, envir=.GlobalEnv)
            remove(.AC10, envir=.GlobalEnv)
            remove(.Md, envir=.GlobalEnv)
            remove(.H3, envir=.GlobalEnv)
          }
          if (K2val == 1)
          {
            instruccion1 <- paste(".AC90 <- quantile(as.numeric(",.BaseDatosActiva,"$",variable,"),probs=0.9,na.rm=TRUE)",sep="")
            justDoIt(instruccion1)
            instruccion2 <- paste(".AC10 <- quantile(as.numeric(",.BaseDatosActiva,"$",variable,"),probs=0.1,na.rm=TRUE)",sep="")
            justDoIt(instruccion2)
            instruccion3 <- paste(".Q1 <- quantile(as.numeric(",.BaseDatosActiva,"$",variable,"),na.rm=TRUE)[2]",sep="")
            justDoIt(instruccion3)
            instruccion4 <- paste(".Q3 <- quantile(as.numeric(",.BaseDatosActiva,"$",variable,"),na.rm=TRUE)[4]",sep="")
            justDoIt(instruccion4)
            instruccion5 <- ".K2 <- (.AC90-.AC10)/(1.9*(.Q3-.Q1))"
            justDoIt(instruccion5)
            if (echocodigo == 1) 
            {
              logger(instruccion1)
              logger(instruccion2)              
              logger(instruccion3)
              logger(instruccion4)
              logger(instruccion5)
            }
            justDoIt("names(.K2)<-NULL")
            doItAndPrint(paste(".K2  # Coef. Apunt. K2 para", variable))
            if (echocodigo == 1)
            {
              logger("remove(.AC90)")              
              logger("remove(.AC10)")
              logger("remove(.Q1)")              
              logger("remove(.Q3)")              
              logger("remove(.K2)")              
            }
            if (creahtml == 1)
            {
              l <- l+1
              if (j == 1) rownames(.TablaResForm)[l] <- "Coef. Apunt. K2"
              .TablaResForm[l,j] <- .K2
            }
            remove(.AC90, envir=.GlobalEnv)
            remove(.AC10, envir=.GlobalEnv)
            remove(.Q1, envir=.GlobalEnv)
            remove(.Q3, envir=.GlobalEnv)
            remove(.K2, envir=.GlobalEnv)            
          }
          if (K3val == 1)
          {
            instruccion1 <- paste(".Einf <- quantile(as.numeric(",.BaseDatosActiva,"$",variable,"),probs=0.125,na.rm=TRUE)",sep="")
            justDoIt(instruccion1)
            instruccion2 <- paste(".Esup <- quantile(as.numeric(",.BaseDatosActiva,"$",variable,"),probs=0.875,na.rm=TRUE)",sep="")
            justDoIt(instruccion2)
            instruccion3 <- paste(".Finf <- fivenum(as.numeric(",.BaseDatosActiva,"$",variable,"))[2]",sep="")
            justDoIt(instruccion3)
            instruccion4 <- paste(".Fsup <- fivenum(as.numeric(",.BaseDatosActiva,"$",variable,"))[4]",sep="")
            justDoIt(instruccion4)
            instruccion5 <- ".K3 <- (.Esup-.Einf)/(1.7*(.Fsup-.Finf))"
            justDoIt(instruccion5)
            if (echocodigo == 1) 
            {
              logger(instruccion1)
              logger(instruccion2)            
              logger(instruccion3)
              logger(instruccion4)
              logger(instruccion5)
            }
            justDoIt("names(.K3)<-NULL")
            doItAndPrint(paste(".K3  # Coef. Apunt. K3 para", variable))
            if (echocodigo == 1)
            {
              logger("remove(.Einf)")              
              logger("remove(.Esup)")
              logger("remove(.Finf)")              
              logger("remove(.Fsup)")              
              logger("remove(.K3)")              
            }
            if (creahtml == 1)
            {
              l <- l+1
              if (j == 1) rownames(.TablaResForm)[l] <- "Coef. Apunt. K3"
              .TablaResForm[l,j] <- .K3
            }
            remove(.Einf, envir=.GlobalEnv)
            remove(.Esup, envir=.GlobalEnv)
            remove(.Finf, envir=.GlobalEnv)
            remove(.Fsup, envir=.GlobalEnv)
            remove(.K3, envir=.GlobalEnv)            
          }
          closeDialog()
          tkfocus(CommanderWindow())
        }
        if (creahtml == 1)
        {
          if (selec > 0)
          {
            HTML("Indices descriptivos de Tendencia Central", file=.archivo)
            HTML(.TablaResTC, file=.archivo)
	    .TablaResTC <- round(.TablaResTC,3)
            remove(.TablaResTC, envir=.GlobalEnv)
          }
          if (selec2 > 0)
          {
            HTML("Indices descriptivos de Dispersion", file=.archivo)
	    .TablaResDisp <- round(.TablaResDisp,3)
            HTML(.TablaResDisp, file=.archivo)
            remove(.TablaResDisp, envir=.GlobalEnv)
          }
          if (selec3 > 0)
          {
            HTML("Indices descriptivos de Posicion", file=.archivo)
            HTML(.TablaResPosic, file=.archivo)
	    .TablaResPosic <- round(.TablaResPosic,3)
            remove(.TablaResPosic, envir=.GlobalEnv)
          }
          if (selec4 > 0)
          {
            HTML("Indices descriptivos de Forma", file=.archivo)
            .TablaResForm <- round(.TablaResForm,3)
            HTML(.TablaResForm, file=.archivo)
            remove(.TablaResForm, envir=.GlobalEnv)
          }
          HTMLhr(file = .archivo)
          }
    } 
    OKCancelHelp(helpSubject="RcmdrPlugin.EACSPIR")
    tkgrid(getFrame(listaVar), sticky="nw")
    tkgrid(labelRcmdr(tcFrame,
    text=gettextRcmdr("Indices de Tendencia Central"), 
    fg="blue"), columnspan=2, sticky="w")
    tkgrid(labelRcmdr(tcFrame, 
    text=gettextRcmdr("Mediana ")), 
    medianaCheckBox, labelRcmdr(tcFrame, 
    text=gettextRcmdr("Moda ")), 
    modaCheckBox, sticky="w")
    tkgrid(labelRcmdr(tcFrame, 
    text=gettextRcmdr("Promedio de cuartiles ")), 
    promcuarCheckBox,labelRcmdr(tcFrame, 
    text=gettextRcmdr("Trimedia ")), 
    trimediaCheckBox,sticky="w")
    tkgrid(labelRcmdr(trimFrame,
    text=gettextRcmdr("Proporcion datos recortados = ")),
    trimField, sticky="w")
    tkgrid(labelRcmdr(tcFrame, 
    text=gettextRcmdr("Rango medio ")), 
    midRCheckBox, labelRcmdr(tcFrame,
    text="Media Recortada"), medrecCheckBox, trimFrame, sticky="ew")    
    tkgrid(labelRcmdr(dispFrame,
    text=gettextRcmdr("Indices de Dispersion"), 
    fg="blue"), columnspan=2, sticky="w")
    tkgrid(labelRcmdr(dispFrame, 
    text=gettextRcmdr("Amplitud ")), 
    rangoCheckBox, labelRcmdr(dispFrame, 
    text=gettextRcmdr("Amplitud intercuartil (IQR) ")), 
    IQRCheckBox, sticky="w")                      
    tkgrid(labelRcmdr(dispFrame, 
    text=gettextRcmdr("Mediana Desviaciones Absolutas (MAD) ")), 
    madCheckBox, labelRcmdr(dispFrame, 
    text=gettextRcmdr("Coeficiente Variacion Robusto ")), 
    CVRCheckBox, sticky="w")
    tkgrid(labelRcmdr(ACFrame,
    text=gettextRcmdr("Proporcion datos utilizado = ")),
    ACField, sticky="w")
    tkgrid(labelRcmdr(dispFrame, 
    text=gettextRcmdr("Desviacion cuartil ")), 
    desvcuarCheckBox,labelRcmdr(dispFrame,
    text="Desviacion centilica"), ACentCheckBox, ACFrame, sticky="we")
    tkgrid(labelRcmdr(posicFrame,
    text=gettextRcmdr("Indices de Posicion"), 
    fg="blue"), columnspan=2, sticky="w")
    tkgrid(labelRcmdr(posicFrame, 
    text=gettextRcmdr("Primer cuartil ")), 
    Q1CheckBox,labelRcmdr(posicFrame, 
    text=gettextRcmdr("Minimo ")), 
    minCheckBox, sticky="w")
    tkgrid(labelRcmdr(posicFrame, 
    text=gettextRcmdr("Segundo cuartil ")), 
    Q2CheckBox, labelRcmdr(posicFrame, 
    text=gettextRcmdr("Maximo ")), 
    maxCheckBox, sticky="w")
    tkgrid(labelRcmdr(percentFrame,
    text=gettextRcmdr("Seleccione cuantilas = ")),
    percentField, sticky="w")
    tkgrid(labelRcmdr(posicFrame, 
    text=gettextRcmdr("Tercer cuartil ")), 
    Q3CheckBox,labelRcmdr(posicFrame,
    text="Cuantilas"), percentCheckBox, percentFrame, sticky="w")
    tkgrid(opcionesFrame, sticky="w")
    tkgrid(labelRcmdr(formaFrame,
    text=gettextRcmdr("Indices de Forma"), 
    fg="blue"), columnspan=2, sticky="w")
    tkgrid(labelRcmdr(formaFrame, 
    text=gettextRcmdr("Coef. Asimetria H1 ")), 
    H1CheckBox, labelRcmdr(formaFrame, 
    text=gettextRcmdr("Coef. Asimetria H3")), 
    H3CheckBox, sticky="w")
    tkgrid(labelRcmdr(formaFrame, 
    text=gettextRcmdr("Coef. Apuntamiento K2")), 
    K2CheckBox, labelRcmdr(formaFrame, 
    text=gettextRcmdr("Coef. Apuntamiento K3")), 
    K3CheckBox, sticky="w")              
    tkgrid(tcFrame, sticky="w")
    tkgrid(dispFrame, sticky="w")
    tkgrid(posicFrame, sticky="w")    
    tkgrid(formaFrame, sticky="w")
    tkgrid(labelRcmdr(opcionesFrame,
    text=gettextRcmdr("Opciones"), fg="blue"), sticky="w")
    tkgrid(labelRcmdr(opcionesFrame, 
    text=gettextRcmdr("Mostrar en pantalla el codigo de R ejecutado ")),
    echoCheckBox, sticky="w")
    tkgrid(labelRcmdr(opcionesFrame,
    text=gettextRcmdr("Generar informe de resultados ")),
    htmlCheckBox,sticky="w")
    tkgrid(labelRcmdr(opcionesFrame,
    text=gettextRcmdr("Calcular todos los indices ")),
    selectodasCheckBox,sticky="w")
    tkgrid(opcionesFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=15, columns=2)
    }

resumen.numericas <- function(){
  initializeDialog(title=gettextRcmdr("Indices para variables numericas"))
  listaVar <- variableListBox(top, Numeric(), selectmode="multiple",
                              title=gettextRcmdr("Variables (escoja una o mas)"))
  opcionesFrame <- tkframe(top)
  tcFrame <- tkframe(opcionesFrame,borderwidth=2, relief="groove")
  echocodigoVariable <- tclVar("0")
  echoCheckBox <- tkcheckbutton(opcionesFrame, variable=echocodigoVariable)
  creahtmlVariable <- tclVar("0")
  htmlCheckBox <- tkcheckbutton(opcionesFrame, variable=creahtmlVariable)
  selectodasVariable <- tclVar("0")
  selectodasCheckBox <- tkcheckbutton(opcionesFrame, variable=selectodasVariable)
  mediaVariable <- tclVar("0")
  mediaCheckBox <- tkcheckbutton(tcFrame, variable=mediaVariable)
  medianaVariable <- tclVar("0")
  medianaCheckBox <- tkcheckbutton(tcFrame, variable=medianaVariable)
  modaVariable <- tclVar("0")
  modaCheckBox <- tkcheckbutton(tcFrame, variable=modaVariable)
  mediageomVariable <- tclVar("0")
  mediageomCheckBox <- tkcheckbutton(tcFrame, variable=mediageomVariable)
  trimediaVariable <- tclVar("0")
  trimediaCheckBox <- tkcheckbutton(tcFrame, variable=trimediaVariable)
  promcuarVariable <- tclVar("0")
  promcuarCheckBox <- tkcheckbutton(tcFrame, variable=promcuarVariable)
  midRVariable <- tclVar("0")
  midRCheckBox <- tkcheckbutton(tcFrame, variable=midRVariable)
  medrecVariable <- tclVar("0")
  medrecCheckBox <- tkcheckbutton(tcFrame, variable=medrecVariable)    
  trimFrame <- tkframe(tcFrame)
  trimVariable <- tclVar(gettextRcmdr("0.05"))
  trimField <- ttkentry(trimFrame, width="8", textvariable=trimVariable)
  dispFrame <- tkframe(opcionesFrame,borderwidth=2, relief="groove")
  varianciaVariable <- tclVar("0")
  varianciaCheckBox <- tkcheckbutton(dispFrame, variable=varianciaVariable)
  sdVariable <- tclVar("0")
  sdCheckBox <- tkcheckbutton(dispFrame, variable=sdVariable)
  sdgeomVariable <- tclVar("0")
  sdgeomCheckBox <- tkcheckbutton(dispFrame, variable=sdgeomVariable)
  desvmedVariable <- tclVar("0")
  desvmedCheckBox <- tkcheckbutton(dispFrame, variable=desvmedVariable)
  CVVariable <- tclVar("0")
  CVCheckBox <- tkcheckbutton(dispFrame, variable=CVVariable)
  rangoVariable <- tclVar("0")
  rangoCheckBox <- tkcheckbutton(dispFrame, variable=rangoVariable)    
  IQRVariable <- tclVar("0")
  IQRCheckBox <- tkcheckbutton(dispFrame, variable=IQRVariable) 
  desvcuarVariable <- tclVar("0")
  desvcuarCheckBox <- tkcheckbutton(dispFrame, variable=desvcuarVariable)
  madVariable <- tclVar("0")
  madCheckBox <- tkcheckbutton(dispFrame, variable=madVariable)
  CVRVariable <- tclVar("0")
  CVRCheckBox <- tkcheckbutton(dispFrame, variable=CVRVariable)
  ACentVariable <- tclVar("0")
  ACentCheckBox <- tkcheckbutton(dispFrame, variable=ACentVariable)    
  ACFrame <- tkframe(dispFrame)
  ACVariable <- tclVar(gettextRcmdr("0.9"))
  ACField <- ttkentry(ACFrame, width="4", textvariable=ACVariable)
  posicFrame <- tkframe(opcionesFrame,borderwidth=2, relief="groove")
  minVariable <- tclVar("0")
  minCheckBox <- tkcheckbutton(posicFrame, variable=minVariable)
  maxVariable <- tclVar("0")
  maxCheckBox <- tkcheckbutton(posicFrame, variable=maxVariable)
  Q1Variable <- tclVar("0")
  Q1CheckBox <- tkcheckbutton(posicFrame, variable=Q1Variable)
  Q2Variable <- tclVar("0")
  Q2CheckBox <- tkcheckbutton(posicFrame, variable=Q2Variable)    
  Q3Variable <- tclVar("0")
  Q3CheckBox <- tkcheckbutton(posicFrame, variable=Q3Variable)
  percentVariable <- tclVar("0")
  percentCheckBox <- tkcheckbutton(posicFrame, variable=percentVariable)    
  percentFrame <- tkframe(posicFrame)
  percentilVariable <- tclVar("0, .25, .5, .75, 1")
  percentField <- ttkentry(percentFrame, width="15", textvariable=percentilVariable)
  formaFrame <- tkframe(opcionesFrame,borderwidth=2, relief="groove")
  H1Variable <- tclVar("0")
  H1CheckBox <- tkcheckbutton(formaFrame, variable=H1Variable)
  H3Variable <- tclVar("0")
  H3CheckBox <- tkcheckbutton(formaFrame, variable=H3Variable)
  K2Variable <- tclVar("0")
  K2CheckBox <- tkcheckbutton(formaFrame, variable=K2Variable)
  K3Variable <- tclVar("0")
  K3CheckBox <- tkcheckbutton(formaFrame, variable=K3Variable)
  beta1Variable <- tclVar("0")
  beta1CheckBox <- tkcheckbutton(formaFrame, variable=beta1Variable)
  gamma1Variable <- tclVar("0")
  gamma1CheckBox <- tkcheckbutton(formaFrame, variable=gamma1Variable)    
  beta2Variable <- tclVar("0")
  beta2CheckBox <- tkcheckbutton(formaFrame, variable=beta2Variable)
  gamma2Variable <- tclVar("0")
  gamma2CheckBox <- tkcheckbutton(formaFrame, variable=gamma2Variable)
  onOK <- function(){
    x <- getSelection(listaVar)
    if (length(x) == 0)
    {
      errorCondition(recall=resumen.numericas, message=gettextRcmdr("Debe escoger una variable."))
      return()
    }
    .BaseDatosActiva <- ActiveDataSet()
    echocodigo <- tclvalue(echocodigoVariable)
    creahtml <- tclvalue(creahtmlVariable)
    selectodas <- tclvalue(selectodasVariable)
    if (selectodas == 1)
    {
      mediaval = medianaval = modaval = mediageomval = trimediaval =
        promcuarval = midRval = medrecval = varianciaval = sdval = CVval =
        sdgeomval = desvmedval = rangoval = IQRval = desvcuarval =
        madval = CVRval = ACentval = minval = maxval = Q1val = Q2val =
        Q3val = percentval = H1val = H3val = K2val = K3val = beta1val =
        gamma1val = beta2val = gamma2val = TRUE
    }
    else
    {
      mediaval <- tclvalue(mediaVariable)
      medianaval <- tclvalue(medianaVariable)
      modaval <- tclvalue(modaVariable)
      mediageomval <- tclvalue(mediageomVariable)
      trimediaval <- tclvalue(trimediaVariable)
      promcuarval <- tclvalue(promcuarVariable)
      midRval <- tclvalue(midRVariable)
      medrecval <- tclvalue(medrecVariable)
      varianciaval <- tclvalue(varianciaVariable)
      sdval <- tclvalue(sdVariable)
      CVval <- tclvalue(CVVariable)
      sdgeomval <- tclvalue(sdgeomVariable)
      desvmedval <- tclvalue(desvmedVariable)
      rangoval <- tclvalue(rangoVariable)
      IQRval <- tclvalue(IQRVariable)
      desvcuarval <- tclvalue(desvcuarVariable)
      madval <- tclvalue(madVariable)
      CVRval <- tclvalue(CVRVariable)
      ACentval <- tclvalue(ACentVariable)  
      minval <- tclvalue(minVariable)
      maxval <- tclvalue(maxVariable)
      Q1val <- tclvalue(Q1Variable)
      Q2val <- tclvalue(Q2Variable)
      Q3val <- tclvalue(Q3Variable)
      percentval <- tclvalue(percentVariable)
      H1val <- tclvalue(H1Variable)
      H3val <- tclvalue(H3Variable)
      K2val <- tclvalue(K2Variable)
      K3val <- tclvalue(K3Variable)
      beta1val <- tclvalue(beta1Variable)        
      gamma1val <- tclvalue(gamma1Variable)        
      beta2val <- tclvalue(beta2Variable)        
      gamma2val <- tclvalue(gamma2Variable)
    }        
    selec <- as.numeric(mediaval) + as.numeric(medianaval) + 
      as.numeric(modaval) + as.numeric(mediageomval) + 
      as.numeric(trimediaval) + as.numeric(promcuarval) +
      as.numeric(midRval) + as.numeric(medrecval)
    selec2 <- as.numeric(varianciaval) + as.numeric(sdval) + 
      as.numeric(CVval) + as.numeric(sdgeomval) + 
      as.numeric(desvmedval) + as.numeric(rangoval) +
      as.numeric(IQRval) + as.numeric(desvcuarval) +        
      as.numeric(madval) + as.numeric(CVRval) +
      as.numeric(ACentval)
    selec3 <- as.numeric(minval) + as.numeric(maxval) + 
      as.numeric(Q1val) + as.numeric(Q2val) + 
      as.numeric(Q3val) + as.numeric(percentval)
    selec4 <- as.numeric(H1val) + as.numeric(H3val) + 
      as.numeric(K2val) + as.numeric(K3val) + 
      as.numeric(beta1val) + as.numeric(gamma1val) +
      as.numeric(beta2val) + as.numeric(gamma2val)
    seleccion <- selec + selec2 + selec3 + selec4
    if (seleccion == 0){
      errorCondition(recall=resumen.numericas, 
                     message=gettextRcmdr("Debe escoger algun indicador."))
      return()
    }
    if (percentval == 1)
    {
      pct <- c(gsub(" ", ",",gsub(", ", ",",tclvalue(percentilVariable))))
      pct1 <- as.numeric(unlist(strsplit(pct,",")))
      if ( is.na(pct1) || (sum(pct1<0.0)>0) || (sum(pct1>1.0)>0) || (sum(!is.numeric(pct1))>0) )
      {
        pct <- paste(seq(0.,1.,.25),collapse=",")
        Message(message=gettextRcmdr("Vector de percentiles invalido. Se utilizara vector por defecto."),
                type="warning")
      }
    }
    if (creahtml == 1)
    {
      require(R2HTML)
      if (!file.exists("Informe de Resultados.html"))
        .archivo <- HTMLInitFile(file.path(getwd()),
                                 "Informe de Resultados", BackGroundColor="#FFFFCC")
      else
        .archivo <- file.path(getwd(), "Informe de Resultados.html")
      if (selec > 0)
      {
        numfilas <- selec
        numcolumnas <- length(x)
        instruccion <- paste(".TablaResTC <- as.data.frame(matrix(nrow=",numfilas,",ncol=",
                             numcolumnas,"))")
        justDoIt(instruccion)
        colnames(.TablaResTC) <- x
      }
      if (selec2 > 0)
      {
        numfilas <- selec2
        numcolumnas <- length(x)
        instruccion <- paste(".TablaResDisp <- as.data.frame(matrix(nrow=",numfilas,",ncol=",
                             numcolumnas,"))")
        justDoIt(instruccion)
        colnames(.TablaResDisp) <- x
      }
      if (selec3 > 0)
      {
        if (percentval == 1)
        {
          numperc <-length(as.numeric(unlist(strsplit(pct,","))))
          numfilas <- selec3 + numperc - 1
        }
        else  numfilas <- selec3
        numcolumnas <- length(x)
        instruccion <- paste(".TablaResPosic <- as.data.frame(matrix(nrow=",numfilas,",ncol=",
                             numcolumnas,"))")
        justDoIt(instruccion)
        colnames(.TablaResPosic) <- x
      }
      if (selec4 > 0)
      {
        numfilas <- selec4
        numcolumnas <- length(x)
        instruccion <- paste(".TablaResForm <- as.data.frame(matrix(nrow=",numfilas,",ncol=",
                             numcolumnas,"))")
        justDoIt(instruccion)
        colnames(.TablaResForm) <- x
      }
      titulo <- "Indicadores descriptivos para variables numericas"
      HTML(as.title(titulo),file=.archivo)
    }
    j <- 0
    for (variable in x)
    {
      i <- 0
      j <- j + 1
      k <- 0
      m <- 0
      l <- 0
      if (mediaval == 1)
      {
        instruccion <- paste(".media <- mean(",.BaseDatosActiva,"$",variable,",na.rm=TRUE)",sep="")
        justDoIt(instruccion)
        if (echocodigo == 1) logger(instruccion)              
        doItAndPrint(paste(".media  # Media para", variable))
        if (echocodigo == 1) logger("remove(.media)")
        if (creahtml == 1)
        {
          i <- i+1
          if (j == 1) rownames(.TablaResTC)[i] <- "Media"
          .TablaResTC[i,j] <- .media
        }
        remove(.media, envir=.GlobalEnv)
      }
      if (medianaval == 1)
      {
        instruccion <- paste(".mediana <- median(",.BaseDatosActiva,"$",variable,",na.rm=TRUE)",sep="")
        justDoIt(instruccion)
        if (echocodigo == 1) logger(instruccion)             
        doItAndPrint(paste(".mediana  # Mediana para", variable))
        if (echocodigo == 1) logger("remove(.mediana)")
        if (creahtml == 1)
        {
          i <- i+1
          if (j == 1) rownames(.TablaResTC)[i] <- "Mediana"
          .TablaResTC[i,j] <- .mediana
        }
        remove(.mediana, envir=.GlobalEnv)
      }
      if (modaval == 1)
      {     
        instruccion1 <- paste(".ni <- table(", .BaseDatosActiva, "$", variable, ")", sep="")
        justDoIt(instruccion1)
        if (echocodigo == 1)
        {
          logger(instruccion1)
        }                
        instruccion2 <- ".moda <- as.numeric(names(.ni)[which(.ni==max(.ni))])"              
        justDoIt(instruccion2)
        if (echocodigo == 1) logger(instruccion2)              
        doItAndPrint(paste(".moda  # Moda para", variable))
        if (echocodigo == 1)
        {
          logger("remove(.ni)")
          logger("remove(.moda)")
        }
        if (creahtml == 1)
        {
          i <- i+1
          if (j == 1) rownames(.TablaResTC)[i] <- "Moda"
          if (length(.moda) > 1)
          {
            .moda1 <- .moda[1]
            Message(message=gettextRcmdr(paste("Variable ",variable," tiene mas de una moda: ",
                                               "En documento HTML se muestra un solo valor.", sep="")),
                    type="warning")
            .TablaResTC[i,j] <- .moda1
          }
          else
            .TablaResTC[i,j] <- .moda
        }
        remove(.ni, envir=.GlobalEnv)
        remove(.moda, envir=.GlobalEnv)
      }
      if (mediageomval == 1)
      {
        instruccion <- paste(".media.geom <- mean(log(",.BaseDatosActiva,"$",variable,"),na.rm=TRUE)",sep="")
        justDoIt(instruccion)
        if (echocodigo == 1) logger(instruccion)             
        doItAndPrint(paste(".media.geom  # Media geometrica para", variable))
        if (echocodigo == 1) logger("remove(.media.geom)")
        if (creahtml == 1)
        {
          i <- i+1
          if (j == 1) rownames(.TablaResTC)[i] <- "Media Geometrica"
          .TablaResTC[i,j] <- .media.geom
        }
        remove(.media.geom, envir=.GlobalEnv)
      }
      if (trimediaval == 1)
      {
        instruccion1 <- paste(".Finf <- fivenum(as.numeric(",.BaseDatosActiva,"$",variable,"))[2]",sep="")
        justDoIt(instruccion1)
        instruccion2 <- paste(".Md <- fivenum(as.numeric(",.BaseDatosActiva,"$",variable,"))[3]",sep="")
        justDoIt(instruccion2)
        instruccion3 <- paste(".Fsup <- fivenum(as.numeric(",.BaseDatosActiva,"$",variable,"))[4]",sep="")
        justDoIt(instruccion3)
        instruccion4 <- "(.trimedia <- .Finf+2*.Md+.Fsup)/4"
        justDoIt(instruccion4)
        if (echocodigo == 1) 
        {
          logger(instruccion1)
          logger(instruccion2)
          logger(instruccion3)
          logger(instruccion4)
        }
        doItAndPrint(paste(".trimedia  # Trimedia para", variable))
        if (echocodigo == 1)
        {
          logger("remove(.Finf)")              
          logger("remove(.Md)")              
          logger("remove(.Fsup)")              
          logger("remove(.trimedia)")
        }
        if (creahtml == 1)
        {
          i <- i+1
          if (j == 1) rownames(.TablaResTC)[i] <- "Trimedia"
          .TablaResTC[i,j] <- .trimedia
        }
        remove(.Finf, envir=.GlobalEnv)
        remove(.Md, envir=.GlobalEnv)
        remove(.Fsup, envir=.GlobalEnv)
        remove(.trimedia, envir=.GlobalEnv)
      }
      if (promcuarval == 1)
      {
        instruccion1 <- paste(".Q1 <- quantile(as.numeric(",.BaseDatosActiva,"$",variable,"),na.rm=TRUE)[2]",sep="")
        justDoIt(instruccion1)
        instruccion2 <- paste(".Q3 <- quantile(as.numeric(",.BaseDatosActiva,"$",variable,"),na.rm=TRUE)[4]",sep="")
        justDoIt(instruccion2)
        instruccion3 <- ".promcuartiles <- (.Q1+.Q3)/2"
        justDoIt(instruccion3)
        if (echocodigo == 1) 
        {
          logger(instruccion1)
          logger(instruccion2)
          logger(instruccion3)
        }
        justDoIt("names(.promcuartiles)<-NULL")
        doItAndPrint(paste(".promcuartiles  # Promedio de cuartiles para", variable))
        if (echocodigo == 1)
        {
          logger("remove(.Q1)")              
          logger("remove(.Q3)")              
          logger("remove(.promcuartiles)")              
        }
        if (creahtml == 1)
        {
          i <- i+1
          if (j == 1) rownames(.TablaResTC)[i] <- "Promedio Cuartiles"
          .TablaResTC[i,j] <- .promcuartiles
        }
        remove(.Q1, envir=.GlobalEnv)
        remove(.Q3, envir=.GlobalEnv)
        remove(.promcuartiles, envir=.GlobalEnv)
      }            
      if (midRval == 1)
      {
        instruccion1 <- paste(".min <- quantile(as.numeric(",.BaseDatosActiva,"$",variable,"),na.rm=TRUE)[1]",sep="")
        justDoIt(instruccion1)
        instruccion2 <- paste(".max <- quantile(as.numeric(",.BaseDatosActiva,"$",variable,"),na.rm=TRUE)[5]",sep="")
        justDoIt(instruccion2)
        instruccion3 <- ".midR <- (.min+.max)/2"
        justDoIt(instruccion3)
        if (echocodigo == 1) 
        {
          logger(instruccion1)
          logger(instruccion2)
          logger(instruccion3)
        }
        justDoIt("names(.midR)<-NULL")
        doItAndPrint(paste(".midR  # Rango medio para", variable))
        if (echocodigo == 1)
        {
          logger("remove(.min)")              
          logger("remove(.max)")              
          logger("remove(.midR)")              
        }
        if (creahtml == 1)
        {
          i <- i+1
          if (j == 1) rownames(.TablaResTC)[i] <- "Rango medio"
          .TablaResTC[i,j] <- .midR
        }
        remove(.min, envir=.GlobalEnv)
        remove(.max, envir=.GlobalEnv)
        remove(.midR, envir=.GlobalEnv)
      }
      
      if (medrecval == 1)
      {
        rec <- as.numeric(tclvalue(trimVariable))
        if ( rec < .0 || rec > .5 || !is.numeric(rec) )
        {
          rec <- 0.05
          Message(message=gettextRcmdr("Proporcion de recorte invalida se utilizara valor por defecto."),
                  type="warning")              
        }
        instruccion <- paste(".media.rec <- mean(as.numeric(",.BaseDatosActiva,"$",variable,"),trim=",rec,",na.rm=TRUE)",sep="")
        justDoIt(instruccion)
        if (echocodigo == 1) logger(instruccion)             
        doItAndPrint(paste(".media.rec  # Media recortada para", variable))
        if (echocodigo == 1) logger("remove(.media.rec)")
        if (creahtml == 1)
        {
          i <- i+1
          if (j == 1) rownames(.TablaResTC)[i] <- paste("Media Recortada al ",
                                                        rec*100,"%",sep="")
          .TablaResTC[i,j] <- .media.rec
        }            
        remove(.media.rec, envir=.GlobalEnv)
      }         
      if (sdval == 1)
      {
        instruccion <- paste(".dt <- sd(",.BaseDatosActiva,"$",variable,",na.rm=TRUE)",sep="")
        justDoIt(instruccion)
        if (echocodigo == 1) logger(instruccion)             
        doItAndPrint(paste(".dt  # Desviacion tipica para", variable))
        if (echocodigo == 1) logger("remove(.dt)")
        if (creahtml == 1)
        {
          k <- k+1
          if (j == 1) rownames(.TablaResDisp)[k] <- "Desviacion tipica"
          .TablaResDisp[k,j] <- .dt
        }
        remove(.dt, envir=.GlobalEnv)            
      }
      if (varianciaval == 1)
      {
        instruccion <- paste(".variancia <- var(",.BaseDatosActiva,"$",variable,",na.rm=TRUE)",sep="")
        justDoIt(instruccion)
        if (echocodigo == 1) logger(instruccion)              
        doItAndPrint(paste(".variancia  # Variancia para", variable))
        if (echocodigo == 1) logger("remove(.variancia)")
        if (creahtml == 1)
        {
          k <- k+1
          if (j == 1) rownames(.TablaResDisp)[k] <- "Variancia"
          .TablaResDisp[k,j] <- .variancia
        }
        remove(.variancia, envir=.GlobalEnv)            
      }
      if (sdgeomval == 1)
      {
        instruccion <- paste(".dt.geometrica <- sd(log(",.BaseDatosActiva,"$",variable,"),na.rm=TRUE)",sep="")
        justDoIt(instruccion)
        if (echocodigo == 1) logger(instruccion)             
        doItAndPrint(paste(".dt.geometrica  # Desv. tipica geometrica para", variable))
        if (echocodigo == 1) logger("remove(.dt.geometrica)")
        if (creahtml == 1)
        {
          k <- k+1
          if (j == 1) rownames(.TablaResDisp)[k] <- "Desv. tip. Geometrica"
          .TablaResDisp[k,j] <- .dt.geometrica
        }
        remove(.dt.geometrica, envir=.GlobalEnv)            
      }
      if (desvmedval == 1)
      {
        instruccion1 <- paste(".media <- mean(",.BaseDatosActiva,"$",variable,",na.rm=TRUE)",sep="")
        justDoIt(instruccion1)
        instruccion2 <- paste(".desv.media <- sum(abs(",.BaseDatosActiva,"$",variable,
                              "-.media),na.rm=TRUE)/length(na.omit(",.BaseDatosActiva,"$",variable,"))",sep="")
        justDoIt(instruccion2)
        if (echocodigo == 1)
        {
          logger(instruccion1)
          logger(instruccion2)
        }
        doItAndPrint(paste(".desv.media  # Desviacion media para", variable))
        if (echocodigo == 1)
        {
          logger("remove(.media)")
          logger("remove(.desv.media)")
        }
        if (creahtml == 1)
        {
          k <- k+1
          if (j == 1) rownames(.TablaResDisp)[k] <- "Desviacion media"
          .TablaResDisp[k,j] <- .desv.media
        }
        remove(.media, envir=.GlobalEnv)
        remove(.desv.media, envir=.GlobalEnv)           
      }
      if (CVval == 1)
      {
        instruccion1 <- paste(".media <- mean(",.BaseDatosActiva,"$",variable,",na.rm=TRUE)",sep="")
        justDoIt(instruccion1)
        instruccion2 <- paste(".dt <- sd(",.BaseDatosActiva,"$",variable,",na.rm=TRUE)",sep="")
        justDoIt(instruccion2)
        instruccion3 <- ".CV <- .dt/.media"
        justDoIt(instruccion3)         
        if (echocodigo == 1)
        {
          logger(instruccion1)
          logger(instruccion2)
          logger(instruccion3)
        }
        doItAndPrint(paste(".CV  # Coeficiente de Variacion para", variable))
        if (echocodigo == 1)
        {
          logger("remove(.media)")
          logger("remove(.dt)")
          logger("remove(.CV)")              
        }
        if (creahtml == 1)
        {
          k <- k+1
          if (j == 1) rownames(.TablaResDisp)[k] <- "Coef. Variacion"
          .TablaResDisp[k,j] <- .CV
        }
        remove(.media, envir=.GlobalEnv)
        remove(.dt, envir=.GlobalEnv) 
        remove(.CV, envir=.GlobalEnv)
      }
      if (rangoval == 1)
      {
        instruccion1 <- paste(".min <- quantile(as.numeric(",.BaseDatosActiva,"$",variable,"),na.rm=TRUE)[1]",sep="")
        justDoIt(instruccion1)
        instruccion2 <- paste(".max <- quantile(as.numeric(",.BaseDatosActiva,"$",variable,"),na.rm=TRUE)[5]",sep="")
        justDoIt(instruccion2)
        instruccion3 <- ".rango <- .max-.min"
        justDoIt(instruccion3)
        if (echocodigo == 1) 
        {
          logger(instruccion1)
          logger(instruccion2)
          logger(instruccion3)
        }
        justDoIt("names(.rango)<-NULL")
        doItAndPrint(paste(".rango  # Amplitud para", variable))
        if (echocodigo == 1)
        {
          logger("remove(.min)")              
          logger("remove(.max)")              
          logger("remove(.rango)")              
        }
        if (creahtml == 1)
        {
          k <- k+1
          if (j == 1) rownames(.TablaResDisp)[k] <- "Amplitud"
          .TablaResDisp[k,j] <- .rango
        }
        remove(.min, envir=.GlobalEnv)
        remove(.max, envir=.GlobalEnv)
        remove(.rango, envir=.GlobalEnv)            
      }
      if (IQRval == 1)
      {
        instruccion1 <- paste(".Q1 <- quantile(as.numeric(",.BaseDatosActiva,"$",variable,"),na.rm=TRUE)[2]",sep="")
        justDoIt(instruccion1)
        instruccion2 <- paste(".Q3 <- quantile(as.numeric(",.BaseDatosActiva,"$",variable,"),na.rm=TRUE)[4]",sep="")
        justDoIt(instruccion2)
        instruccion3 <- ".IQR <- .Q3-.Q1"
        justDoIt(instruccion3)
        if (echocodigo == 1) 
        {
          logger(instruccion1)
          logger(instruccion2)
          logger(instruccion3)
        }
        justDoIt("names(.IQR)<-NULL")
        doItAndPrint(paste(".IQR  # Amplitud Intercuartil para", variable))
        if (echocodigo == 1)
        {
          logger("remove(.Q1)")              
          logger("remove(.Q3)")              
          logger("remove(.IQR)")              
        }
        if (creahtml == 1)
        {
          k <- k+1
          if (j == 1) rownames(.TablaResDisp)[k] <- "Amplitud Intercuartil"
          .TablaResDisp[k,j] <- .IQR
        }
        remove(.Q1, envir=.GlobalEnv)
        remove(.Q3, envir=.GlobalEnv)
        remove(.IQR, envir=.GlobalEnv)            
      }
      if (madval == 1)
      {
        instruccion1 <- paste(".mediana <- median(as.numeric(",.BaseDatosActiva,"$",variable,"),na.rm=TRUE)",sep="")
        justDoIt(instruccion1)
        instruccion2 <- paste(".mad <- median(abs(as.numeric(",.BaseDatosActiva,"$",variable,
                              ")-.mediana),na.rm=TRUE)",sep="")
        justDoIt(instruccion2)
        if (echocodigo == 1)
        {
          logger(instruccion1)
          logger(instruccion2)
        }
        doItAndPrint(paste(".mad  # Indice MAD para", variable))
        if (echocodigo == 1)
        {
          logger("remove(.mediana)")
          logger("remove(.mad)")
        }
        if (creahtml == 1)
        {
          k <- k+1
          if (j == 1) rownames(.TablaResDisp)[k] <- "MAD"
          .TablaResDisp[k,j] <- .mad
        }
        remove(.mediana, envir=.GlobalEnv)
        remove(.mad, envir=.GlobalEnv)            
      }
      if (CVRval == 1)
      {
        instruccion1 <- paste(".Finf <- fivenum(as.numeric(",.BaseDatosActiva,"$",variable,"))[2]",sep="")
        justDoIt(instruccion1)
        instruccion2 <- paste(".Fsup <- fivenum(as.numeric(",.BaseDatosActiva,"$",variable,"))[4]",sep="")
        justDoIt(instruccion2)
        instruccion3 <- ".CVR <- round((.Fsup-.Finf)/(.Finf+.Fsup), 2)"
        justDoIt(instruccion3)
        if (echocodigo == 1) 
        {
          logger(instruccion1)
          logger(instruccion2)
          logger(instruccion3)
        }
        doItAndPrint(paste(".CVR  # Coeficiente Variacion Robusto para", variable))
        if (echocodigo == 1)
        {
          logger("remove(.Finf)")                            
          logger("remove(.Fsup)")              
          logger("remove(.CVR)")
        }
        if (creahtml == 1)
        {
          k <- k+1
          if (j == 1) rownames(.TablaResDisp)[k] <- "Coef. Var. Robusto"
          .TablaResDisp[k,j] <- .CVR
        }
        remove(.Finf, envir=.GlobalEnv)
        remove(.Fsup, envir=.GlobalEnv)
        remove(.CVR, envir=.GlobalEnv)            
      }
      if (desvcuarval == 1)
      {
        instruccion1 <- paste(".Q1 <- quantile(as.numeric(",.BaseDatosActiva,"$",variable,"),na.rm=TRUE)[2]",sep="")
        justDoIt(instruccion1)
        instruccion2 <- paste(".Q3 <- quantile(as.numeric(",.BaseDatosActiva,"$",variable,"),na.rm=TRUE)[4]",sep="")
        justDoIt(instruccion2)
        instruccion3 <- ".DQ <- (.Q3-.Q1)/2"
        justDoIt(instruccion3)
        if (echocodigo == 1) 
        {
          logger(instruccion1)
          logger(instruccion2)
          logger(instruccion3)
        }
        justDoIt("names(.DQ)<-NULL")
        doItAndPrint(paste(".DQ  # Desviacion cuartil para", variable))
        if (echocodigo == 1)
        {
          logger("remove(.Q1)")              
          logger("remove(.Q3)")              
          logger("remove(.DQ)")              
        }
        if (creahtml == 1)
        {
          k <- k+1
          if (j == 1) rownames(.TablaResDisp)[k] <- "Desviacion cuartil"
          .TablaResDisp[k,j] <- .DQ
        }
        remove(.Q1, envir=.GlobalEnv)
        remove(.Q3, envir=.GlobalEnv)
        remove(.DQ, envir=.GlobalEnv)            
      }
      if (ACentval == 1)
      {
        propdat <- as.numeric(tclvalue(ACVariable))
        if ( propdat < .0 || propdat > 1. || !is.numeric(propdat) )
        {
          prop.dat <- 0.9
          Message(message=gettextRcmdr("Proporcion de datos invalida se utilizara valor por defecto."),
                  type="warning")              
        }
        instruccion1 <- paste(".ACinf <- round(quantile(as.numeric(",.BaseDatosActiva,"$",variable,"),probs=",(1-propdat)/2,",na.rm=TRUE), 2)",sep="")
        justDoIt(instruccion1)
        instruccion2 <- paste(".ACsup <- round(quantile(as.numeric(",.BaseDatosActiva,"$",variable,"),probs=",1-(1-propdat)/2,",na.rm=TRUE), 2)",sep="")
        justDoIt(instruccion2)
        instruccion3 <- ".AC <- .ACsup-.ACinf"
        justDoIt(instruccion3)
        if (echocodigo == 1) 
        {
          logger(instruccion1)
          logger(instruccion2)
          logger(instruccion3)
        }
        justDoIt("names(.AC)<-NULL")
        doItAndPrint(paste(".AC  # Amplitud centilica con ",propdat*100,"% datos para ", variable,sep=""))
        if (echocodigo == 1)
        {
          logger("remove(.ACinf)")              
          logger("remove(.ACsup)")              
          logger("remove(.AC)")              
        }
        if (creahtml == 1)
        {
          k <- k+1
          if (j == 1) rownames(.TablaResDisp)[k] <- paste("Amplitud Centilica ",
                                                          propdat*100,"% datos",sep="")
          .TablaResDisp[k,j] <- .AC
        }
        remove(.ACinf, envir=.GlobalEnv)
        remove(.ACsup, envir=.GlobalEnv)
        remove(.AC, envir=.GlobalEnv)
      } 
      if (minval == 1)
      {
        instruccion <- paste(".min <-min(as.numeric(",.BaseDatosActiva,"$",variable,"),na.rm=TRUE)",sep="")
        justDoIt(instruccion)
        if (echocodigo == 1) logger(instruccion)             
        doItAndPrint(paste(".min  # Minimo valor para", variable))
        if (echocodigo == 1) logger("remove(.min)")
        if (creahtml == 1)
        {
          m <- m+1
          if (j == 1) rownames(.TablaResPosic)[m] <- "Minimo"
          .TablaResPosic[m,j] <- .min
        }
        remove(.min, envir=.GlobalEnv)            
      }
      if (maxval == 1)
      {
        instruccion <- paste(".max <- max(as.numeric(",.BaseDatosActiva,"$",variable,"),na.rm=TRUE)",sep="")
        justDoIt(instruccion)
        if (echocodigo == 1) logger(instruccion)              
        doItAndPrint(paste(".max  # Maximo valor para", variable))
        if (echocodigo == 1) logger("remove(.max)")
        if (creahtml == 1)
        {
          m <- m+1
          if (j == 1) rownames(.TablaResPosic)[m] <- "Maximo"
          .TablaResPosic[m,j] <- .max
        }
        remove(.max, envir=.GlobalEnv)            
      }
      if (Q1val == 1)
      {
        instruccion <- paste(".Q1 <- quantile(as.numeric(",.BaseDatosActiva,"$",variable,"),na.rm=TRUE)[2]",sep="")
        justDoIt(instruccion)
        if (echocodigo == 1) logger(instruccion)
        justDoIt("names(.Q1)<-NULL")
        doItAndPrint(paste(".Q1  # Primer cuartil para", variable))
        if (echocodigo == 1) logger("remove(.Q1)")
        if (creahtml == 1)
        {
          m <- m+1
          if (j == 1) rownames(.TablaResPosic)[m] <- "Primer Cuartil"
          .TablaResPosic[m,j] <- .Q1
        }
        remove(.Q1, envir=.GlobalEnv)           
      }  
      if (Q2val == 1)
      {
        instruccion <- paste(".Q2 <- quantile(as.numeric(",.BaseDatosActiva,"$",variable,"),na.rm=TRUE)[3]",sep="")
        justDoIt(instruccion)
        if (echocodigo == 1) logger(instruccion)
        justDoIt("names(.Q2)<-NULL")
        doItAndPrint(paste(".Q2  # Segundo cuartil para", variable))
        if (echocodigo == 1) logger("remove(.Q2)")
        if (creahtml == 1)
        {
          m <- m+1
          if (j == 1) rownames(.TablaResPosic)[m] <- "Segundo Cuartil"
          .TablaResPosic[m,j] <- .Q2
        }
        remove(.Q2, envir=.GlobalEnv)            
      }
      if (Q3val == 1)
      {
        instruccion <- paste(".Q3 <- quantile(as.numeric(",.BaseDatosActiva,"$",variable,"),na.rm=TRUE)[4]",sep="")
        justDoIt(instruccion)
        if (echocodigo == 1) logger(instruccion)
        justDoIt("names(.Q3)<-NULL")
        doItAndPrint(paste(".Q3  # Tercer cuartil para", variable))
        if (echocodigo == 1) logger("remove(.Q3)")
        if (creahtml == 1)
        {
          m <- m+1
          if (j == 1) rownames(.TablaResPosic)[m] <- "Tercer Cuartil"
          .TablaResPosic[m,j] <- .Q3
        }
        remove(.Q3, envir=.GlobalEnv)            
      }
      if (percentval == 1)
      {
        instruccion <- paste(".Pct <- quantile(as.numeric(",.BaseDatosActiva,"$",variable,"),probs=c(",pct,"),na.rm=TRUE)",sep="")
        justDoIt(instruccion)
        if (echocodigo == 1) logger(instruccion)             
        doItAndPrint(paste(".Pct  # Percentiles para", variable))
        if (echocodigo == 1) logger("remove(.PCt)")
        if (creahtml == 1)
        {
          m <- m+1
          pct1 <- as.numeric(unlist(strsplit(pct,",")))
          if (j == 1) rownames(.TablaResPosic)[m:(m+length(pct1)-1)] <- paste(
            "Percentil ",pct1*100,sep="")
          .TablaResPosic[m:(m+length(pct1)-1),j] <- .Pct
        }
        remove(.Pct, envir=.GlobalEnv)
      }
      if (H1val == 1)
      {
        instruccion1 <- paste(".Finf <- fivenum(as.numeric(",.BaseDatosActiva,"$",variable,"))[2]",sep="")
        justDoIt(instruccion1)
        instruccion2 <- paste(".Md <- fivenum(as.numeric(",.BaseDatosActiva,"$",variable,"))[3]",sep="")
        justDoIt(instruccion2)
        instruccion3 <- paste(".Fsup <- fivenum(as.numeric(",.BaseDatosActiva,"$",variable,"))[4]",sep="")
        justDoIt(instruccion3)
        instruccion4 <- ".H1 <- (.Finf+.Fsup-2*.Md)/(2*.Md)"
        justDoIt(instruccion4)
        if (echocodigo == 1) 
        {
          logger(instruccion1)
          logger(instruccion2)
          logger(instruccion3)
          logger(instruccion4)
        }
        doItAndPrint(paste(".H1  # Indice de Yule H1 para", variable))
        if (echocodigo == 1)
        {
          logger("remove(.Finf)")              
          logger("remove(.Md)")              
          logger("remove(.Fsup)")              
          logger("remove(.H1)")
        }
        if (creahtml == 1)
        {
          l <- l+1
          if (j == 1) rownames(.TablaResForm)[l] <- "Coef. Asim. H1"
          .TablaResForm[l,j] <- .H1
        }
        remove(.Finf, envir=.GlobalEnv)
        remove(.Md, envir=.GlobalEnv)
        remove(.Fsup, envir=.GlobalEnv)
        remove(.H1, envir=.GlobalEnv)            
      }
      if (H3val == 1)
      {
        instruccion1 <- paste(".AC90 <- quantile(as.numeric(",.BaseDatosActiva,"$",variable,"),probs=0.9,na.rm=TRUE)",sep="")
        justDoIt(instruccion1)
        instruccion2 <- paste(".AC10 <- quantile(as.numeric(",.BaseDatosActiva,"$",variable,"),probs=0.1,na.rm=TRUE)",sep="")
        justDoIt(instruccion2)
        instruccion3 <- paste(".Md <- fivenum(as.numeric(",.BaseDatosActiva,"$",variable,"))[3]",sep="")
        justDoIt(instruccion3)
        instruccion4 <- ".H3 <- (.AC90+.AC10-2*.Md)/(2*.Md)"
        justDoIt(instruccion4)
        if (echocodigo == 1) 
        {
          logger(instruccion1)
          logger(instruccion2)
          logger(instruccion3) 
          logger(instruccion4)
        }
        justDoIt("names(.H3)<-NULL")
        doItAndPrint(paste(".H3  # Indice de Kelly H3  para ", variable,sep=""))
        if (echocodigo == 1)
        {
          logger("remove(.AC90)")              
          logger("remove(.AC10)")              
          logger("remove(.Md)")
          logger("remove(.H3)")
        }
        if (creahtml == 1)
        {
          l <- l+1
          if (j == 1) rownames(.TablaResForm)[l] <- "Coef. Asim. H3"
          .TablaResForm[l,j] <- .H3
        }
        remove(.AC90, envir=.GlobalEnv)
        remove(.AC10, envir=.GlobalEnv)
        remove(.Md, envir=.GlobalEnv)
        remove(.H3, envir=.GlobalEnv)
      }
      if (beta1val == 1)
      {
        instruccion1 <- paste(".media <- mean(",.BaseDatosActiva,"$",variable,",na.rm=TRUE)",sep="")
        justDoIt(instruccion1)
        instruccion2 <- paste(".beta1 <- (sum((",.BaseDatosActiva,"$",variable,
                              "-.media)^3,na.rm=TRUE)/length(na.omit(",.BaseDatosActiva,"$",
                              variable,")))^2/", "(sum((",.BaseDatosActiva,"$",
                              variable, "-.media)^2,na.rm=TRUE)/length(na.omit(",.BaseDatosActiva,
                              "$",variable,")))^3",sep="")
        justDoIt(instruccion2)
        if (echocodigo == 1)
        {
          logger(instruccion1)
          logger(instruccion2)
        }
        doItAndPrint(paste(".beta1  # Coef. Asim. Pearson para", variable))
        if (echocodigo == 1)
        {
          logger("remove(.media)")
          logger("remove(.beta1)")
        }
        if (creahtml == 1)
        {
          l <- l+1
          if (j == 1) rownames(.TablaResForm)[l] <- "Coef. Asim. Pearson"
          .TablaResForm[l,j] <- .beta1
        }
        remove(.media, envir=.GlobalEnv)
        remove(.beta1, envir=.GlobalEnv)            
      }
      if (gamma1val == 1)
      {
        instruccion1 <- paste(".media <- mean(",.BaseDatosActiva,"$",variable,",na.rm=TRUE)",sep="")
        justDoIt(instruccion1)
        instruccion2 <- paste(".dt <- sd(",.BaseDatosActiva,"$",variable,",na.rm=TRUE)",sep="")
        justDoIt(instruccion2)
        instruccion3 <- paste(".n <- length(na.omit(",.BaseDatosActiva,"$",
                              variable,"))",sep="")
        justDoIt(instruccion3)            
        instruccion4 <- paste(".gamma1 <- .n*sum((",.BaseDatosActiva,"$",variable,
                              "-.media)^3,na.rm=TRUE)/((.n-1)*(.n-2))/(.dt^3)",sep="")            
        justDoIt(instruccion4)
        if (echocodigo == 1)
        {
          logger(instruccion1)
          logger(instruccion2)
          logger(instruccion3)
          logger(instruccion4)
        }
        doItAndPrint(paste(".gamma1  # Coef. Asim. Fisher para", variable))
        if (echocodigo == 1)
        {
          logger("remove(.media)")
          logger("remove(.dt)")
          logger("remove(.n)")
          logger("remove(.gamma1)")
        }
        if (creahtml == 1)
        {
          l <- l+1
          if (j == 1) rownames(.TablaResForm)[l] <- "Coef. Asim. Fisher"
          .TablaResForm[l,j] <- .gamma1
        }
        remove(.media, envir=.GlobalEnv)
        remove(.dt, envir=.GlobalEnv) 
        remove(.n, envir=.GlobalEnv)
        remove(.gamma1, envir=.GlobalEnv)
      }
      if (K2val == 1)
      {
        instruccion1 <- paste(".AC90 <- quantile(as.numeric(",.BaseDatosActiva,"$",variable,"),probs=0.9,na.rm=TRUE)",sep="")
        justDoIt(instruccion1)
        instruccion2 <- paste(".AC10 <- quantile(as.numeric(",.BaseDatosActiva,"$",variable,"),probs=0.1,na.rm=TRUE)",sep="")
        justDoIt(instruccion2)
        instruccion3 <- paste(".Q1 <- quantile(as.numeric(",.BaseDatosActiva,"$",variable,"),na.rm=TRUE)[2]",sep="")
        justDoIt(instruccion3)
        instruccion4 <- paste(".Q3 <- quantile(as.numeric(",.BaseDatosActiva,"$",variable,"),na.rm=TRUE)[4]",sep="")
        justDoIt(instruccion4)
        instruccion5 <- ".K2 <- (.AC90-.AC10)/(1.9*(.Q3-.Q1))"
        justDoIt(instruccion5)
        if (echocodigo == 1) 
        {
          logger(instruccion1)
          logger(instruccion2)              
          logger(instruccion3)
          logger(instruccion4)
          logger(instruccion5)
        }
        justDoIt("names(.K2)<-NULL")
        doItAndPrint(paste(".K2  # Coef. Apunt. K2 para", variable))
        if (echocodigo == 1)
        {
          logger("remove(.AC90)")              
          logger("remove(.AC10)")
          logger("remove(.Q1)")              
          logger("remove(.Q3)")              
          logger("remove(.K2)")              
        }
        if (creahtml == 1)
        {
          l <- l+1
          if (j == 1) rownames(.TablaResForm)[l] <- "Coef. Apunt. K2"
          .TablaResForm[l,j] <- .K2
        }
        remove(.AC90, envir=.GlobalEnv)
        remove(.AC10, envir=.GlobalEnv)
        remove(.Q1, envir=.GlobalEnv)
        remove(.Q3, envir=.GlobalEnv)
        remove(.K2, envir=.GlobalEnv)            
      }
      if (K3val == 1)
      {
        instruccion1 <- paste(".Einf <- quantile(as.numeric(",.BaseDatosActiva,"$",variable,"),probs=0.125,na.rm=TRUE)",sep="")
        justDoIt(instruccion1)
        instruccion2 <- paste(".Esup <- quantile(as.numeric(",.BaseDatosActiva,"$",variable,"),probs=0.875,na.rm=TRUE)",sep="")
        justDoIt(instruccion2)
        instruccion3 <- paste(".Finf <- fivenum(as.numeric(",.BaseDatosActiva,"$",variable,"))[2]",sep="")
        justDoIt(instruccion3)
        instruccion4 <- paste(".Fsup <- fivenum(as.numeric(",.BaseDatosActiva,"$",variable,"))[4]",sep="")
        justDoIt(instruccion4)
        instruccion5 <- ".K3 <- (.Esup-.Einf)/(1.7*(.Fsup-.Finf))"
        justDoIt(instruccion5)
        if (echocodigo == 1) 
        {
          logger(instruccion1)
          logger(instruccion2)            
          logger(instruccion3)
          logger(instruccion4)
          logger(instruccion5)
        }
        justDoIt("names(.K3)<-NULL")
        doItAndPrint(paste(".K3  # Coef. Apunt. K3 para", variable))
        if (echocodigo == 1)
        {
          logger("remove(.Einf)")              
          logger("remove(.Esup)")
          logger("remove(.Finf)")              
          logger("remove(.Fsup)")              
          logger("remove(.K3)")              
        }
        if (creahtml == 1)
        {
          l <- l+1
          if (j == 1) rownames(.TablaResForm)[l] <- "Coef. Apunt. K3"
          .TablaResForm[l,j] <- .K3
        }
        remove(.Einf, envir=.GlobalEnv)
        remove(.Esup, envir=.GlobalEnv)
        remove(.Finf, envir=.GlobalEnv)
        remove(.Fsup, envir=.GlobalEnv)
        remove(.K3, envir=.GlobalEnv)            
      }
      if (beta2val == 1)
      {
        instruccion1 <- paste(".media <- mean(",.BaseDatosActiva,"$",variable,",na.rm=TRUE)",sep="")
        justDoIt(instruccion1)
        instruccion2 <- paste(".beta2 <- sum((",.BaseDatosActiva,"$",variable,
                              "-.media)^4,na.rm=TRUE)/length(na.omit(",.BaseDatosActiva,"$",
                              variable,"))/", "(sum((",.BaseDatosActiva,"$",
                              variable, "-.media)^2,na.rm=TRUE)/length(na.omit(",.BaseDatosActiva,
                              "$",variable,")))^2",sep="")
        justDoIt(instruccion2)
        if (echocodigo == 1)
        {
          logger(instruccion1)
          logger(instruccion2)
        }
        doItAndPrint(paste(".beta2  # Coef. Apunt. Pearson para", variable))
        if (echocodigo == 1)
        {
          logger("remove(.media)")
          logger("remove(.beta2)")
        }
        if (creahtml == 1)
        {
          l <- l+1
          if (j == 1) rownames(.TablaResForm)[l] <- "Coef. Apunt. Pearson"
          .TablaResForm[l,j] <- .beta2
        }
        remove(.media, envir=.GlobalEnv)
        remove(.beta2, envir=.GlobalEnv)            
      }
      if (gamma2val == 1)
      {
        instruccion1 <- paste(".media <- mean(",.BaseDatosActiva,"$",variable,",na.rm=TRUE)",sep="")
        justDoIt(instruccion1)
        instruccion2 <- paste(".dt <- sd(",.BaseDatosActiva,"$",variable,",na.rm=TRUE)",sep="")
        justDoIt(instruccion2)
        instruccion3 <- paste(".n <- length(na.omit(",.BaseDatosActiva,"$",
                              variable,"))",sep="")
        justDoIt(instruccion3)           
        instruccion4 <- paste(".gamma2 <- (.n*(.n+1)*sum((",.BaseDatosActiva,"$",variable,
                              "-.media)^4,na.rm=TRUE)/((.n-1)*(.n-2)*(.n-3))-3*
                            sum((",.BaseDatosActiva,"$",variable,
                            "-.media)^2,na.rm=TRUE)^2/((.n-2)*(.n-3)))/(.dt^4)",sep="")            
        justDoIt(instruccion4)
        if (echocodigo == 1)
        {
          logger(instruccion1)
          logger(instruccion2)
          logger(instruccion3)
          logger(instruccion4)
        }
        doItAndPrint(paste(".gamma2  # Coef. Apunt. Fisher para", variable))
        if (echocodigo == 1)
        {
          logger("remove(.media)")
          logger("remove(.dt)")
          logger("remove(.n)")
          logger("remove(.gamma2)")
        }
        if (creahtml == 1)
        {
          l <- l+1
          if (j == 1) rownames(.TablaResForm)[l] <- "Coef. Apunt. Fisher"
          .TablaResForm[l,j] <- .gamma2
        }
        remove(.media, envir=.GlobalEnv)
        remove(.dt, envir=.GlobalEnv)
        remove(.n, envir=.GlobalEnv)
        remove(.gamma2, envir=.GlobalEnv)
      }
      closeDialog()
      tkfocus(CommanderWindow())
    }
    if (creahtml == 1)
    {
      if (selec > 0)
      {
        HTML("Indices descriptivos de Tendencia Central", file=.archivo)
        HTML(.TablaResTC, file=.archivo)
        .TablaResTC <- round(.TablaResTC,3)
        remove(.TablaResTC, envir=.GlobalEnv)
      }
      if (selec2 > 0)
      {
        HTML("Indices descriptivos de Dispersion", file=.archivo)
        .TablaResDisp <- round(.TablaResDisp,3)
        HTML(.TablaResDisp, file=.archivo)
        remove(.TablaResDisp, envir=.GlobalEnv)
      }
      if (selec3 > 0)
      {
        HTML("Indices descriptivos de Posicion", file=.archivo)
        HTML(.TablaResPosic, file=.archivo)
        .TablaResPosic <- round(.TablaResPosic,3)
        remove(.TablaResPosic, envir=.GlobalEnv)
      }
      if (selec4 > 0)
      {
        HTML("Indices descriptivos de Forma", file=.archivo)
        .TablaResForm <- round(.TablaResForm,3)
        HTML(.TablaResForm, file=.archivo)
        remove(.TablaResForm, envir=.GlobalEnv)
      }
      HTMLhr(file = .archivo)
    }
  } 
  OKCancelHelp(helpSubject="RcmdrPlugin.EACSPIR")
  tkgrid(getFrame(listaVar), sticky="nw")
  tkgrid(labelRcmdr(tcFrame,
  text=gettextRcmdr("Indices de Tendencia Central"), 
  fg="blue"), columnspan=1, sticky="w")
  tkgrid(labelRcmdr(tcFrame, 
  text=gettextRcmdr("Media ")), 
  mediaCheckBox, labelRcmdr(tcFrame, 
  text=gettextRcmdr("Mediana ")), 
  medianaCheckBox, labelRcmdr(tcFrame, 
  text=gettextRcmdr("Moda ")), 
  modaCheckBox, labelRcmdr(tcFrame, 
  text=gettextRcmdr("Media Geometrica ")), 
  mediageomCheckBox, sticky="w")
  tkgrid(labelRcmdr(trimFrame,
  text=gettextRcmdr("Proporcion datos recortados = ")),
  trimField, sticky="w")
  tkgrid(labelRcmdr(tcFrame, 
  text=gettextRcmdr("Trimedia ")), 
  trimediaCheckBox, labelRcmdr(tcFrame, 
  text=gettextRcmdr("Promedio de cuartiles ")), 
  promcuarCheckBox,labelRcmdr(tcFrame, 
  text=gettextRcmdr("Rango medio ")), 
  midRCheckBox,labelRcmdr(tcFrame,
  text="Media Recortada"), medrecCheckBox, trimFrame, sticky="ew")    
  tkgrid(labelRcmdr(dispFrame,
  text=gettextRcmdr("Indices de Dispersion"), 
  fg="blue"), columnspan=1, sticky="w")
  tkgrid(labelRcmdr(dispFrame, 
  text=gettextRcmdr("Desviacion Tipica ")), 
  sdCheckBox, labelRcmdr(dispFrame, 
  text=gettextRcmdr("Variancia ")), 
  varianciaCheckBox, labelRcmdr(dispFrame, 
  text=gettextRcmdr("Desviacion Geometrica ")), 
  sdgeomCheckBox, labelRcmdr(dispFrame, 
  text=gettextRcmdr("Desviacion media ")), 
  desvmedCheckBox, sticky="w")
  tkgrid(labelRcmdr(dispFrame, 
  text=gettextRcmdr("Coeficiente Variacion ")), 
  CVCheckBox,labelRcmdr(dispFrame, 
  text=gettextRcmdr("Amplitud ")), 
  rangoCheckBox, labelRcmdr(dispFrame, 
                                   text=gettextRcmdr("Amplitud intercuartil (IQR) ")), 
         IQRCheckBox, labelRcmdr(dispFrame, 
                                 text=gettextRcmdr("Mediana Desviaciones Absolutas (MAD) ")), 
         madCheckBox, sticky="w")                      
  tkgrid(labelRcmdr(ACFrame,
                    text=gettextRcmdr("Proporcion datos utilizado = ")),
         ACField, sticky="w")    
  tkgrid(labelRcmdr(dispFrame, 
                    text=gettextRcmdr("Coeficiente Variacion Robusto ")), 
         CVRCheckBox,labelRcmdr(dispFrame, 
                                text=gettextRcmdr("Desviacion cuartil ")), 
         desvcuarCheckBox,labelRcmdr(dispFrame,
                                     text="Desviacion centilica"), ACentCheckBox, ACFrame, sticky="we")
  tkgrid(labelRcmdr(posicFrame,
                    text=gettextRcmdr("Indices de Posicion"), 
                    fg="blue"), columnspan=2, sticky="w")
  tkgrid(labelRcmdr(posicFrame, 
                    text=gettextRcmdr("Primer cuartil ")), 
         Q1CheckBox,labelRcmdr(posicFrame, 
                               text=gettextRcmdr("Minimo ")), 
         minCheckBox, sticky="w")
  tkgrid(labelRcmdr(posicFrame, 
                    text=gettextRcmdr("Segundo cuartil ")), 
         Q2CheckBox, labelRcmdr(posicFrame, 
                                text=gettextRcmdr("Maximo ")), 
         maxCheckBox, sticky="w")
  tkgrid(labelRcmdr(percentFrame,
                    text=gettextRcmdr("Seleccione cuantilas = ")),
         percentField, sticky="w")
  tkgrid(labelRcmdr(posicFrame, 
                    text=gettextRcmdr("Tercer cuartil ")), 
         Q3CheckBox,labelRcmdr(posicFrame,
                               text="Cuantilas"), percentCheckBox, percentFrame, sticky="w")
  tkgrid(opcionesFrame, sticky="w")
  tkgrid(labelRcmdr(formaFrame,
                    text=gettextRcmdr("Indices de Forma"), 
                    fg="blue"), columnspan=2, sticky="w")
  tkgrid(labelRcmdr(formaFrame, 
                    text=gettextRcmdr("Coef. Asimetria H1 ")), 
         H1CheckBox, labelRcmdr(formaFrame, 
                                text=gettextRcmdr("Coef. Asimetria H3")), 
         H3CheckBox, labelRcmdr(formaFrame, 
                                text=gettextRcmdr("Coef. Asimetria Pearson")), 
         beta1CheckBox, labelRcmdr(formaFrame, 
                                   text=gettextRcmdr("Coef. Asimetria Fisher")), 
         gamma1CheckBox, sticky="w")
  tkgrid(labelRcmdr(formaFrame, 
                    text=gettextRcmdr("Coef. Apuntamiento K2")), 
         K2CheckBox, labelRcmdr(formaFrame, 
                                text=gettextRcmdr("Coef. Apuntamiento K3")), 
         K3CheckBox, labelRcmdr(formaFrame, 
                                text=gettextRcmdr("Coef. Apuntamiento Pearson")), 
         beta2CheckBox, labelRcmdr(formaFrame, 
                                   text=gettextRcmdr("Coef. Apuntamiento Fisher")), 
         gamma2CheckBox, sticky="w")              
  tkgrid(tcFrame, sticky="w")
  tkgrid(dispFrame, sticky="w")
  tkgrid(posicFrame, sticky="w")    
  tkgrid(formaFrame, sticky="w")
  tkgrid(labelRcmdr(opcionesFrame,
                    text=gettextRcmdr("Opciones"), fg="blue"), sticky="w")
  tkgrid(labelRcmdr(opcionesFrame, 
                    text=gettextRcmdr("Mostrar en pantalla el codigo de R ejecutado ")),
         echoCheckBox, sticky="w")
  tkgrid(labelRcmdr(opcionesFrame,
                    text=gettextRcmdr("Generar informe de resultados ")),
         htmlCheckBox,sticky="w")
  tkgrid(labelRcmdr(opcionesFrame,
                    text=gettextRcmdr("Calcular todos los indices ")),
         selectodasCheckBox,sticky="w")
  tkgrid(opcionesFrame, sticky="w")
  tkgrid(buttonsFrame, sticky="w")
  dialogSuffix(rows=15, columns=2)
}

# Funcion creada a partir de la funcion Hist de John Fox incluida en R-Commander #
histograma <- function(){
    initializeDialog(title=gettextRcmdr("Histograma"))
    selecVar <- variableListBox(top, Numeric(), title=gettextRcmdr("Variables (escoja una)"))
    opcionesFrame <- tkframe(top)
    radioButtons(name="escala", buttons=c("frequency","density"),
    labels=gettextRcmdr(c("Frecuencias Absolutas","Densidades")),
    title=gettextRcmdr("Escala eje ordenadas"))
    intervalosFrame <- tkframe(opcionesFrame)
    intervalosVariable <- tclVar(gettextRcmdr("<auto>"))
    intervalosField <- ttkentry(intervalosFrame, width="8", textvariable=intervalosVariable)
    normalsupVariable <- tclVar("0")
    normalsupCheckBox <- tkcheckbutton(opcionesFrame, variable=normalsupVariable)
    echocodigoVariable <- tclVar("0")
    echoCheckBox <- tkcheckbutton(opcionesFrame, variable=echocodigoVariable)
    creahtmlVariable <- tclVar("0")
    htmlCheckBox <- tkcheckbutton(opcionesFrame, variable=creahtmlVariable)
    onOK <- function(){
        .BaseDatosActiva <- ActiveDataSet()
        variable <- getSelection(selecVar)
        interv <- tclvalue(intervalosVariable)
        opts <- options(warn=-1)
        interv <- if (interv == gettextRcmdr("<auto>")) '"Sturges"'
        else as.numeric(interv)
        options(opts)
        escala <- tclvalue(escalaVariable)
        normalsup <- tclvalue(normalsupVariable)
        echocodigo <- tclvalue(echocodigoVariable)
        creahtml <- tclvalue(creahtmlVariable)
        if (length(variable) == 0){
            errorCondition(recall=histograma, message=gettextRcmdr("Debe escoger una variable."))
            return()
            }
        if (creahtml == 1)
        {
          require(R2HTML)
          if (!file.exists("Informe de Resultados.html"))
            .archivo <- HTMLInitFile(file.path(getwd()),
            "Informe de Resultados", BackGroundColor="#FFFFCC")
          else
            .archivo <- file.path(getwd(), "Informe de Resultados.html")
        }
        titulop <- paste("Histograma para ",variable, sep="")
        if (escala == "frequency")
        {
          tituloy <- "Frecuencias absolutas"
          frecuencia  <- TRUE
        }
        if (escala == "density")
        {
          tituloy <- "Densidades"
          frecuencia <- FALSE
        }
        titulox <- "Intervalos"
        instruccion <- paste(".h <- hist(", ActiveDataSet(), "$", variable, ",freq=",
            frecuencia,", breaks=", interv,",main='",titulop,"',ylab='",tituloy,
            "',xlab='",titulox,"',col='red')", sep="")
        justDoIt(instruccion)
        if (echocodigo == 1)
          logger(instruccion) 
        if (normalsup == 1)
        {
          instruccion2 <- paste(".xfit <- seq(min(", ActiveDataSet(), "$", variable,",na.rm=TRUE),max(",
                          ActiveDataSet(), "$", variable,",na.rm=TRUE),length=1000)",sep="")
          justDoIt(instruccion2)
          instruccion3 <- paste(".yfit <- dnorm(x=.xfit,mean=mean(",ActiveDataSet(), "$", variable,
                          ",na.rm=TRUE),sd=sd(",ActiveDataSet(), "$", variable,",na.rm=TRUE))",sep="")
          justDoIt(instruccion3)
          if (escala == "frequency")
          { 
            instruccion4 <- paste(".yfit <- .yfit*diff(.h$mids[1:2])*length(na.omit(",
                            ActiveDataSet(), "$", variable,"))",sep="")
            justDoIt(instruccion4)
          }
          instruccion5 <- "lines(.xfit,.yfit,col='blue',lwd=2)"
          instruccion6 <- "box()"
          if (echocodigo == 1)
          {
            logger(instruccion2) 
            logger(instruccion3)
            if (escala == "frequency")
              logger(instruccion4)
            doItAndPrint(instruccion5)
            doItAndPrint(instruccion6)
            logger("remove(.xfit)")
            logger("remove(.yfit)") 
            remove(.xfit, envir=.GlobalEnv)
            remove(.yfit, envir=.GlobalEnv)
          }
          else
          {
            justDoIt(instruccion5)
            justDoIt(instruccion6)
          }
        }
        if (creahtml == 1)
        {
          titulo <- paste("Histograma para variable ",variable,sep="")
          HTML(as.title(titulo),file=.archivo)
          nombre.archivo <- paste("HistogramaR",gsub(":","",substr(Sys.time(),12,19)),
          ".jpg",sep="")
          dev.print(jpeg, filename=paste(getwd(),"/",nombre.archivo,sep=""),
          width=500, height=500)
          HTMLInsertGraph(nombre.archivo,file=.archivo,append=TRUE)
          HTMLhr(file = .archivo)
        }
        if (echocodigo == 1) logger("remove(.h)")
        remove(.h, envir=.GlobalEnv)  
        closeDialog()        
        tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject="hist")
    tkgrid(getFrame(selecVar), sticky="nw")
    tkgrid(labelRcmdr(opcionesFrame,
    text=gettextRcmdr("Configuracion del histograma"), fg="blue"),
    columnspan=6, sticky="w")
    tkgrid(labelRcmdr(intervalosFrame,
    text=gettextRcmdr("Numero de intervalos: ")),
    intervalosField, sticky="w")
    tkgrid(intervalosFrame, sticky="w")
    tkgrid(escalaFrame, sticky="w")
    tkgrid(labelRcmdr(opcionesFrame,
    text=gettextRcmdr("Superponer curva normal ")),
    normalsupCheckBox,sticky="w")
    tkgrid(labelRcmdr(opcionesFrame,
    text=gettextRcmdr("Opciones"), fg="blue"), sticky="w")
    tkgrid(labelRcmdr(opcionesFrame, 
    text=gettextRcmdr("Mostrar en pantalla el codigo de R ejecutado ")), 
    echoCheckBox, sticky="w")
    tkgrid(labelRcmdr(opcionesFrame,
    text=gettextRcmdr("Generar informe de resultados ")),
    htmlCheckBox,sticky="w")
    tkgrid(opcionesFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")        
    dialogSuffix(rows=6, columns=1)        
}
           
diagrama.caja.ord <- function (){
    initializeDialog(title=gettextRcmdr("Diagrama de Caja"))
    selecVar <- variableListBox(top, Factors(), title=gettextRcmdr("Variables (escoja una)"))
    opcionesFrame <- tkframe(top)
    intervalosFrame <- tkframe(opcionesFrame)
    intervinfVariable <- tclVar(gettextRcmdr("1.5"))
    intervinfField <- ttkentry(intervalosFrame, width="6", textvariable=intervinfVariable)
    intervsupVariable <- tclVar(gettextRcmdr("3.0"))
    intervsupField <- ttkentry(intervalosFrame, width="6", textvariable=intervsupVariable)
    echocodigoVariable <- tclVar("0")
    echoCheckBox <- tkcheckbutton(opcionesFrame, variable=echocodigoVariable)
    creahtmlVariable <- tclVar("0")
    htmlCheckBox <- tkcheckbutton(opcionesFrame, variable=creahtmlVariable)
    identificaVariable <- tclVar("0")
    identifCheckBox <- tkcheckbutton(opcionesFrame, variable=identificaVariable)
    onOK <- function(){
        .BaseDatosActiva <- ActiveDataSet()
        variable <- getSelection(selecVar)
        intervinf <- as.numeric(tclvalue(intervinfVariable))
        intervsup <- as.numeric(tclvalue(intervsupVariable))
          if ( is.na(intervinf) || (intervinf<0) || (!is.numeric(intervinf)) ||
             is.na(intervsup) || (intervsup<0) || (!is.numeric(intervsup)) ||
             (intervsup < intervinf) )
          {
            intervinf <- 1.5
            intervsup <- 3.0
            Message(message=gettextRcmdr("Coeficientes inferior y superior no validos. Se utilizara valores por defecto."),
            type="warning")
          }        

        opts <- options(warn=-1)
        echocodigo <- tclvalue(echocodigoVariable)
        creahtml <- tclvalue(creahtmlVariable)
        identif <- tclvalue(identificaVariable)
        if (length(variable) == 0){
            errorCondition(recall=diagrama.caja.ord, message=gettextRcmdr("Debe escoger una variable."))
            return()
            }
        justDoIt(paste("cond <- !is.ordered(",paste(ActiveDataSet(),"$",variable,sep=""),")",sep=""))
        if (cond){
            errorCondition(recall=diagrama.caja.ord, message=gettextRcmdr(paste("Variable ",variable, " no es ordinal.",sep='')))
            return()
            }
        remove("cond", envir=.GlobalEnv)
        if (creahtml == 1)
        {
          require(R2HTML)
          if (!file.exists("Informe de Resultados.html"))
            .archivo <- HTMLInitFile(file.path(getwd()),
            "Informe de Resultados", BackGroundColor="#FFFFCC")
          else
            .archivo <- file.path(getwd(), "Informe de Resultados.html")
        }
        titulop <- paste("Diagrama de caja para ",variable, sep="")
        instruccion <- paste(".bxp1 <- boxplot.stats(as.numeric(", ActiveDataSet(), "$", variable,
                             "),coef=",intervinf,")",sep="")
        justDoIt(instruccion)
        if (echocodigo == 1)
          logger(instruccion)
        instruccion2 <- paste(".bxp2 <- boxplot.stats(as.numeric(", ActiveDataSet(), "$", variable,
                             "),coef=",intervsup,")",sep="")
        justDoIt(instruccion2)
        if (echocodigo == 1)
          logger(paste(".bxp2 <-", instruccion2))
        instruccion3 <- paste("boxplot(as.numeric(", ActiveDataSet(), "$", variable,
                             "),main='",titulop,"',col='red',outpch=NA)",sep="")
        if (echocodigo == 1) doItAndPrint(instruccion3)
        else justDoIt(instruccion3)
        instruccion4 <- ".selec <- .bxp1$out %in% .bxp2$out"
        justDoIt(instruccion4)
        if (echocodigo == 1)
          logger(instruccion4)
        instruccion5 <- ".anom <- .bxp1$out"
        justDoIt(instruccion5)
        if (echocodigo == 1)
          logger(instruccion5)
        instruccion6 <- ".anom[.selec] <- NA"
        if (echocodigo == 1) doItAndPrint(instruccion6)
        else justDoIt(instruccion6)
        instruccion7 <- "points(rep(1, length(.anom)), .anom, pch = 1, col = 'blue')"
        if (echocodigo == 1) doItAndPrint(instruccion7)
        else justDoIt(instruccion7)
        instruccion8 <- ".extrem <- .bxp2$out"
        justDoIt(instruccion8)
        if (echocodigo == 1)
          logger(instruccion8)
        instruccion9 <- "points(rep(1, length(.extrem)), .extrem, pch = 8, col = 'red')"
        if (echocodigo == 1) doItAndPrint(instruccion9)
        else justDoIt(instruccion9)
        if (identif == 1)
        {
         instruccion10 <- paste("identify(rep(1,length(", ActiveDataSet(), "$", variable,
                                ")),as.numeric(", ActiveDataSet(), "$", variable,
                                "),rownames(", ActiveDataSet(),"))",sep="")
         if (echocodigo == 1) doItAndPrint(instruccion10)
         else justDoIt(instruccion10)
        }
        if (echocodigo == 1) logger("remove(list=c('.bxp1','.bxp2','.selec','.anom','.extrem'))")
        remove(list=c(".bxp1",".bxp2",".selec",".anom",".extrem"),envir=.GlobalEnv)
        if (creahtml == 1)
        {
          titulo <- paste("Diagrama de caja para variable ",variable,sep="")
          HTML(as.title(titulo),file=.archivo)
          nombre.archivo <- paste("DiagramaCajaR",gsub(":","",substr(Sys.time(),12,19)),
          ".jpg",sep="")
          dev.print(jpeg, filename=paste(getwd(),"/",nombre.archivo,sep=""),
          width=500, height=500)
          HTMLInsertGraph(nombre.archivo,file=.archivo,append=TRUE)
          HTMLhr(file = .archivo)
        }
        closeDialog()        
        tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject="boxplot")
    tkgrid(getFrame(selecVar), sticky="nw")
    tkgrid(labelRcmdr(opcionesFrame,
    text=gettextRcmdr("Configuracion del grafico"), fg="blue"),
    columnspan=6, sticky="w")
    tkgrid(labelRcmdr(intervalosFrame,
    text=gettextRcmdr("Coeficiente Limite inferior: ")),
    intervinfField, labelRcmdr(intervalosFrame,
    text=gettextRcmdr("Coeficiente Limite superior: ")),
    intervsupField, sticky="w")
    tkgrid(intervalosFrame, sticky="w")
    tkgrid(labelRcmdr(opcionesFrame,
    text=gettextRcmdr("Opciones"), fg="blue"), sticky="w")
    tkgrid(labelRcmdr(opcionesFrame,
    text=gettextRcmdr("Identificar sujetos (por numero de fila ocupado) ")),
    identifCheckBox,sticky="w")
    tkgrid(labelRcmdr(opcionesFrame, 
    text=gettextRcmdr("Mostrar en pantalla el codigo de R ejecutado ")), 
    echoCheckBox, sticky="w")
    tkgrid(labelRcmdr(opcionesFrame,
    text=gettextRcmdr("Generar informe de resultados ")),
    htmlCheckBox,sticky="w")
    tkgrid(opcionesFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")        
    dialogSuffix(rows=6, columns=1)        
}        

diagrama.caja <- function (){
    initializeDialog(title=gettextRcmdr("Diagrama de Caja"))
    selecVar <- variableListBox(top, Numeric(), title=gettextRcmdr("Variables (escoja una)"))
    opcionesFrame <- tkframe(top)
    intervalosFrame <- tkframe(opcionesFrame)
    intervinfVariable <- tclVar(gettextRcmdr("1.5"))
    intervinfField <- ttkentry(intervalosFrame, width="6", textvariable=intervinfVariable)
    intervsupVariable <- tclVar(gettextRcmdr("3.0"))
    intervsupField <- ttkentry(intervalosFrame, width="6", textvariable=intervsupVariable)
    echocodigoVariable <- tclVar("0")
    echoCheckBox <- tkcheckbutton(opcionesFrame, variable=echocodigoVariable)
    creahtmlVariable <- tclVar("0")
    htmlCheckBox <- tkcheckbutton(opcionesFrame, variable=creahtmlVariable)
    identificaVariable <- tclVar("0")
    identifCheckBox <- tkcheckbutton(opcionesFrame, variable=identificaVariable)
    onOK <- function(){
        .BaseDatosActiva <- ActiveDataSet()
        variable <- getSelection(selecVar)
        intervinf <- as.numeric(tclvalue(intervinfVariable))
        intervsup <- as.numeric(tclvalue(intervsupVariable))
          if ( is.na(intervinf) || (intervinf<0) || (!is.numeric(intervinf)) ||
             is.na(intervsup) || (intervsup<0) || (!is.numeric(intervsup)) ||
             (intervsup < intervinf) )
          {
            intervinf <- 1.5
            intervsup <- 3.0
            Message(message=gettextRcmdr("Coeficientes inferior y superior no validos. Se utilizara valores por defecto."),
            type="warning")
          }        

        opts <- options(warn=-1)
        echocodigo <- tclvalue(echocodigoVariable)
        creahtml <- tclvalue(creahtmlVariable)
        identif <- tclvalue(identificaVariable)
        if (length(variable) == 0){
            errorCondition(recall=diagrama.caja, message=gettextRcmdr("Debe escoger una variable."))
            return()
            }
        if (creahtml == 1)
        {
          require(R2HTML)
          if (!file.exists("Informe de Resultados.html"))
            .archivo <- HTMLInitFile(file.path(getwd()),
            "Informe de Resultados", BackGroundColor="#FFFFCC")
          else
            .archivo <- file.path(getwd(), "Informe de Resultados.html")
        }
        titulop <- paste("Diagrama de caja para ",variable, sep="")
        instruccion <- paste(".bxp1 <- boxplot.stats(", ActiveDataSet(), "$", variable,
                             ",coef=",intervinf,")",sep="")
        justDoIt(instruccion)
        if (echocodigo == 1)
          logger(instruccion)
        instruccion2 <- paste(".bxp2 <- boxplot.stats(", ActiveDataSet(), "$", variable,
                             ",coef=",intervsup,")",sep="")
        justDoIt(instruccion2)
        if (echocodigo == 1)
          logger(instruccion2)
        instruccion3 <- paste("boxplot(", ActiveDataSet(), "$", variable,
                             ",main='",titulop,"',col='red',outpch=NA)",sep="")
        if (echocodigo == 1) doItAndPrint(instruccion3)
        else justDoIt(instruccion3)
        instruccion4 <- ".selec <- .bxp1$out %in% .bxp2$out"
        justDoIt(instruccion4)
        if (echocodigo == 1)
          logger(instruccion4)
        instruccion5 <- ".anom <- .bxp1$out"
        justDoIt(instruccion5)
        if (echocodigo == 1)
          logger(instruccion5)
        instruccion6 <- ".anom[.selec] <- NA"
        if (echocodigo == 1) doItAndPrint(instruccion6)
        else justDoIt(instruccion6)
        instruccion7 <- "points(rep(1, length(.anom)), .anom, pch = 1, col = 'blue')"
        if (echocodigo == 1) doItAndPrint(instruccion7)
        else justDoIt(instruccion7)
        instruccion8 <- ".extrem <- .bxp2$out"
        justDoIt(instruccion8)
        if (echocodigo == 1)
          logger(instruccion8)
        instruccion9 <- "points(rep(1, length(.extrem)), .extrem, pch = 8, col = 'red')"
        if (echocodigo == 1) doItAndPrint(instruccion9)
        else justDoIt(instruccion9)
        if (identif == 1)
        instruccion10 <- paste("identify(rep(1,length(", ActiveDataSet(), "$", variable,
                               ")),", ActiveDataSet(), "$", variable,
                               ",rownames(", ActiveDataSet(),"))",sep="")
        if (echocodigo == 1) doItAndPrint(instruccion10)
        else justDoIt(instruccion10)
        if (echocodigo == 1) logger("remove(list=c('.bxp1','.bxp2','.selec','.anom','.extrem'))")
        remove(list=c(".bxp1",".bxp2",".selec",".anom",".extrem"),envir=.GlobalEnv)
        if (creahtml == 1)
        {
          titulo <- paste("Diagrama de caja para variable ",variable,sep="")
          HTML(as.title(titulo),file=.archivo)
          nombre.archivo <- paste("DiagramaCajaR",gsub(":","",substr(Sys.time(),12,19)),
          ".jpg",sep="")
          dev.print(jpeg, filename=paste(getwd(),"/",nombre.archivo,sep=""),
          width=500, height=500)
          HTMLInsertGraph(nombre.archivo,file=.archivo,append=TRUE)
          HTMLhr(file = .archivo)
        }
        closeDialog()        
        tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject="boxplot")
    tkgrid(getFrame(selecVar), sticky="nw")
    tkgrid(labelRcmdr(opcionesFrame,
    text=gettextRcmdr("Configuracion del grafico"), fg="blue"),
    columnspan=6, sticky="w")
    tkgrid(labelRcmdr(intervalosFrame,
    text=gettextRcmdr("Coeficiente Limite inferior: ")),
    intervinfField, labelRcmdr(intervalosFrame,
    text=gettextRcmdr("Coeficiente Limite superior: ")),
    intervsupField, sticky="w")
    tkgrid(intervalosFrame, sticky="w")
    tkgrid(labelRcmdr(opcionesFrame,
    text=gettextRcmdr("Opciones"), fg="blue"), sticky="w")
    tkgrid(labelRcmdr(opcionesFrame,
    text=gettextRcmdr("Identificar sujetos (por numero de fila ocupado) ")),
    identifCheckBox,sticky="w")
    tkgrid(labelRcmdr(opcionesFrame, 
    text=gettextRcmdr("Mostrar en pantalla el codigo de R ejecutado ")), 
    echoCheckBox, sticky="w")
    tkgrid(labelRcmdr(opcionesFrame,
    text=gettextRcmdr("Generar informe de resultados ")),
    htmlCheckBox,sticky="w")
    tkgrid(opcionesFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")        
    dialogSuffix(rows=6, columns=1)        
}

bivariante.categoricas <- function(){
  require("abind")
  initializeDialog(title=gettextRcmdr("Descripcion tablas de contingencia"))
  opcionesFrame <- tkframe(top)
  variablesFrame <- tkframe(top)
  filaVar <- variableListBox(variablesFrame, Factors(), title=gettextRcmdr("Variable por filas (escoja una)"))
  columnaVar <- variableListBox(variablesFrame, Factors(), title=gettextRcmdr("Variable por columnas (escoja una)"))
  subsetBox()
  radioButtons(name="porcentajes", buttons=c("fila",
                                             "columna", "total", "ninguno"),values=c("fila", "columna",
                                                                                     "total", "ninguno"), initialValue="ninguno",
               labels=gettextRcmdr(c("Porcentajes respecto a marginales fila",
                                     "Porcentajes respecto a marginales columna","Porcentajes respecto al total",
                                     "Ningun porcentaje")),title=gettextRcmdr("Calcular Porcentajes"))
  echocodigoVariable <- tclVar("0")
  echoCheckBox <- tkcheckbutton(opcionesFrame, variable=echocodigoVariable)
  creahtmlVariable <- tclVar("0")
  htmlCheckBox <- tkcheckbutton(opcionesFrame, variable=creahtmlVariable)
  esperadasFrame <- tkframe(top)
  frecEspVariable <- tclVar("0")
  frecEspCheckBox <- tkcheckbutton(esperadasFrame, variable=frecEspVariable)
  descjiFrame <- tkframe(top, borderwidth=2, relief="groove")
  jicuadradoVariable <- tclVar("0")
  jicuadradoCheckBox <- tkcheckbutton(descjiFrame, variable=jicuadradoVariable)
  jiComponentesVariable <- tclVar("0")
  jiComponentesCheckBox <- tkcheckbutton(descjiFrame, variable=jiComponentesVariable)
  phiPearsonVariable <- tclVar("0")
  phiPearsonCheckBox <- tkcheckbutton(descjiFrame, variable=phiPearsonVariable)    
  contingPearsonVariable <- tclVar("0")
  contingPearsonCheckBox <- tkcheckbutton(descjiFrame, variable=contingPearsonVariable)
  sakodaVariable <- tclVar("0")
  sakodaCheckBox <- tkcheckbutton(descjiFrame, variable=sakodaVariable)
  chuprovVariable <- tclVar("0")
  chuprovCheckBox <- tkcheckbutton(descjiFrame, variable=chuprovVariable)
  VCramerVariable <- tclVar("0")
  VCramerCheckBox <- tkcheckbutton(descjiFrame, variable=VCramerVariable)
  yuleVariable <- tclVar("0")
  yuleCheckBox <- tkcheckbutton(descjiFrame, variable=yuleVariable)
  errorpredFrame <- tkframe(top, borderwidth=2, relief="groove")
  lambdaVariable <- tclVar("0")
  lambdaCheckBox <- tkcheckbutton(errorpredFrame, variable=lambdaVariable)
  tauVariable <- tclVar("0")
  tauCheckBox <- tkcheckbutton(errorpredFrame, variable=tauVariable)
  theilVariable <- tclVar("0")
  theilCheckBox <- tkcheckbutton(errorpredFrame, variable=theilVariable)
  onOK <- function(){
    fila <- getSelection(filaVar)
    columna <- getSelection(columnaVar)
    if (length(fila) == 0 || length(columna) == 0){
      errorCondition(recall=bivariante.categoricas, message=gettextRcmdr("Debe seleccionar dos variables."))
      return()
    }
    if (fila == columna) {
      errorCondition(recall=bivariante.categoricas, message=gettextRcmdr("Debe seleccionar dos variables distintas."))
      return()
    }
    porcentajes <- as.character(tclvalue(porcentajesVariable))
    esperadas <- tclvalue(frecEspVariable)
    jicuadrado <- tclvalue(jicuadradoVariable)
    jicomponentes <- tclvalue(jiComponentesVariable)
    phival <- tclvalue(phiPearsonVariable)
    contingval <- tclvalue(contingPearsonVariable)
    sakodaval <- tclvalue(sakodaVariable)
    chuprovval <- tclvalue(chuprovVariable)
    VCramerval <- tclvalue(VCramerVariable)
    yuleval <- tclvalue(yuleVariable)
    lambdaval <- tclvalue(lambdaVariable)
    tauval <- tclvalue(tauVariable)
    theilval <- tclvalue(theilVariable)
    subconjunto <- tclvalue(subsetVariable)
    subconjunto <- if (trim.blanks(subconjunto) == gettextRcmdr("<all valid cases>")) ""
    else paste(", subset=", subconjunto, sep="")
    echocodigo <- tclvalue(echocodigoVariable)
    selec <- as.numeric(jicuadrado) + as.numeric(phival) + as.numeric(contingval) + 
      as.numeric(sakodaval) + as.numeric(chuprovval) + as.numeric(VCramerval) +
      as.numeric(yuleval)*3
    selec2 <- as.numeric(lambdaval)*3 + as.numeric(tauval)*2 + as.numeric(theilval)*3
    creahtml <- tclvalue(creahtmlVariable)
    if (creahtml == 1)
    {
      require(R2HTML)
      if (!file.exists("Informe de Resultados.html"))
        .archivo <- HTMLInitFile(file.path(getwd()),
                                 "Informe de Resultados", BackGroundColor="#FFFFCC")
      else
        .archivo <- file.path(getwd(), "Informe de Resultados.html")
      if (selec > 0)
      {
        numfilas <- selec
        instruccion <- paste(".TablaCoefAsoc <- as.data.frame(matrix(nrow=",numfilas,",ncol=1))")
        justDoIt(instruccion)
        colnames(.TablaCoefAsoc) <- "Valores"
      }
      if (selec2 > 0)
      {
        numfilas <- selec2
        instruccion <- paste(".TablaErrorPred <- as.data.frame(matrix(nrow=",numfilas,",ncol=1))")
        justDoIt(instruccion)
        colnames(.TablaErrorPred) <- "Valores"
      }             
      titulo <- paste("Descripcion bivariante de datos categoricos: ",fila, 
                      " y ", columna, sep="")
      HTML(as.title(titulo),file=.archivo)
    }
    closeDialog()
    instruccion <- paste(".Tabla <- xtabs(~", fila, "+", columna, ", data=", ActiveDataSet(),
                         subconjunto, ")", sep="")
    justDoIt(instruccion)
    if (echocodigo == 1) logger(instruccion)
    doItAndPrint(paste(".Tabla  # Tabla de contingencia para ",
                       fila," y ",columna,sep=""))
    if (creahtml == 1)
    {
      subtitulo <- "Tabla de contingencia"      
      HTML(subtitulo,file=.archivo)
      HTML(.Tabla,file=.archivo)
    }
    if (porcentajes == "fila")
    {
      instruccion2 <- ".porcentajes <- rowPercents(.Tabla)"
      justDoIt(instruccion2)
      if (echocodigo == 1) logger(instruccion2)
      doItAndPrint(".porcentajes  # Porcentajes respecto a marginales fila ")
      if (echocodigo == 1) logger("remove(.porcentajes)")
      if (creahtml == 1)
      {
        subtitulo <- "Tabla porcentajes respecto a marginales fila"      
        HTML(subtitulo,file=.archivo)
        HTML(.porcentajes,file=.archivo)
      }
      remove(.porcentajes, envir=.GlobalEnv)
    }
    if (porcentajes == "columna")
    {
      instruccion2 <- ".porcentajes <- colPercents(.Tabla)"
      justDoIt(instruccion2)
      if (echocodigo == 1) logger(instruccion2)
      doItAndPrint(".porcentajes  # Porcentajes respecto a marginales columna ")
      if (echocodigo == 1) logger("remove(.porcentajes)")
      if (creahtml == 1)
      {
        subtitulo <- "Tabla porcentajes respecto a marginales columna"      
        HTML(subtitulo,file=.archivo)
        HTML(.porcentajes,file=.archivo)
      }
      remove(.porcentajes, envir=.GlobalEnv)
    }
    if (porcentajes == "total")
    {
      instruccion2 <- ".porcentajes <- totPercents(.Tabla)"
      justDoIt(instruccion2)
      if (echocodigo == 1) logger(instruccion2)
      doItAndPrint(".porcentajes  # Porcentajes respecto al total")
      if (echocodigo == 1) logger("remove(.porcentajes)")
      if (creahtml == 1)
      {
        subtitulo <- "Tabla porcentajes respecto al total"      
        HTML(subtitulo,file=.archivo)
        HTML(.porcentajes,file=.archivo)
      }
      remove(.porcentajes, envir=.GlobalEnv)
    }
    if (esperadas == 1)
    {
      instruccion4 <- ".esperadas <- chisq.test(.Tabla, correct=FALSE)$expected"
      justDoIt(instruccion4)
      if (echocodigo == 1) logger(instruccion4)
      doItAndPrint(".esperadas  # Frecuencias Esperadas")
      mensAviso <- NULL
      if (0 < (emq1 <- sum(.esperadas < 1))) mensAviso <- paste(emq1,
                                                                gettextRcmdr("frecuencias esperadas menores que 1"))
      if (0 < (emq5 <- sum(.esperadas < 5))) mensAviso <- paste(mensAviso, "\n", emq5,
                                                                gettextRcmdr(" frecuencias esperadas menores que 5"), sep="")
      if (!is.null(mensAviso)) Message(message=mensAviso,
                                       type="warning")
      if (echocodigo == 1) logger("remove(.esperadas)")
      if (creahtml == 1)
      {
        subtitulo <- "Frecuencias Esperadas"      
        HTML(subtitulo,file=.archivo)
        HTML(.esperadas,file=.archivo)
      }
      remove(.esperadas, envir=.GlobalEnv)
    }
    if ( jicomponentes == 1 ||phival == 1 || contingval == 1 ||
           sakodaval == 1 || chuprovval == 1 || VCramerval == 1 ) 
      jicuadrado <- TRUE
    i <- 0
    j <- 0
    if (jicuadrado == 1)
    {
      instruccion3 <- ".Jicuadrado <- chisq.test(.Tabla, correct=FALSE)$statistic"
      if (echocodigo == 1) logger(instruccion3)
      justDoIt(instruccion3)
      justDoIt("names(.Jicuadrado)<-NULL")
      doItAndPrint(".Jicuadrado # Estadistico Ji cuadrado de Pearson")
      if (creahtml == 1)
      {
        i <- i+1
        rownames(.TablaCoefAsoc)[i] <- "Ji Cuadrado"
        .TablaCoefAsoc[i,1] <- .Jicuadrado
      }
    }
    if (jicomponentes == 1)
    {
      instruccion5 <- ".Componentes <- round(chisq.test(.Tabla, correct=FALSE)$residuals^2,
      2)"
      if (echocodigo == 1) logger(instruccion5)
      justDoIt(instruccion5)
      doItAndPrint(".Componentes # Descomposicion del estadistico Ji cuadrado")
      if (echocodigo == 1) logger("remove(.Componentes)")
      if (creahtml == 1)
      {
        subtitulo <- "Descomposicion Ji Cuadrado de Pearson"      
        HTML(subtitulo,file=.archivo)
        HTML(.Componentes,file=.archivo)
      }
      remove(.Componentes, envir=.GlobalEnv)
    }
    if (phival == 1)
    {
      instruccion6 <- ".phi <- sqrt(.Jicuadrado/sum(.Tabla))"
      if (echocodigo == 1) logger(instruccion6)
      justDoIt(instruccion6)
      doItAndPrint(".phi # Coeficiente Phi de Pearson")
      if (echocodigo == 1) logger("remove(.phi)")
      if (creahtml == 1)
      {
        i <- i+1
        rownames(.TablaCoefAsoc)[i] <- "Phi Pearson"
        .TablaCoefAsoc[i,1] <- .phi
      }
      remove(.phi, envir=.GlobalEnv)
    }
    if (contingval == 1)
    {
      instruccion7 <- ".Coef.Contingencia <- sqrt(.Jicuadrado/(sum(.Tabla)+.Jicuadrado))"
      if (echocodigo == 1) logger(instruccion7)
      justDoIt(instruccion7)
      doItAndPrint(".Coef.Contingencia # Coeficiente Contingencia de Pearson")
      if (echocodigo == 1) logger("remove(.Coef.Contingencia)")
      if (creahtml == 1)
      {
        i <- i+1
        rownames(.TablaCoefAsoc)[i] <- "Coeficiente Contingencia"
        .TablaCoefAsoc[i,1] <- .Coef.Contingencia
      }
      remove(.Coef.Contingencia, envir=.GlobalEnv)
    }
    if (sakodaval == 1)
    {
      instruccion8 <- ".sakoda<- sqrt(min(dim(.Tabla))*.Jicuadrado/((min(dim(.Tabla))-1)*
      (sum(.Tabla)+.Jicuadrado)))"
      logger(instruccion8)
      justDoIt(instruccion8)
      doItAndPrint(".sakoda # Transformacion Sakoda ")
      if (echocodigo == 1) logger("remove(.sakoda)")
      if (creahtml == 1)
      {
        i <- i+1
        rownames(.TablaCoefAsoc)[i] <- "Transf. Sakoda"
        .TablaCoefAsoc[i,1] <- .sakoda
      }
      remove(.sakoda, envir=.GlobalEnv)
    }
    if (chuprovval == 1)
    {
      instruccion9 <- ".chuprov <- sqrt(.Jicuadrado/(sum(.Tabla)*(dim(.Tabla)[1]-1)*
      (dim(.Tabla)[2]-1)))"
      if (echocodigo == 1) logger(instruccion7)
      justDoIt(instruccion9)
      doItAndPrint(".chuprov # Coeficiente Contingencia Chuprov")
      if (echocodigo == 1) logger("remove(.chuprov)")
      if (creahtml == 1)
      {
        i <- i+1
        rownames(.TablaCoefAsoc)[i] <- "Coef. Contingencia Chuprov"
        .TablaCoefAsoc[i,1] <- .chuprov
      }
      remove(.chuprov, envir=.GlobalEnv)
    }
    if (VCramerval == 1)
    {
      instruccion10 <- ".VCramer <- sqrt(.Jicuadrado/((min(dim(.Tabla))-1)*sum(.Tabla)))"
      if (echocodigo == 1) logger(instruccion10)
      justDoIt(instruccion10)
      doItAndPrint(".VCramer # Coef. Contingencia Cramer ")
      if (echocodigo == 1) logger("remove(.VCramer)")
      if (creahtml == 1)
      {
        i <- i+1
        rownames(.TablaCoefAsoc)[i] <- "Coef. Contingencia Cramer"
        .TablaCoefAsoc[i,1] <- .VCramer
      }
      remove(.VCramer, envir=.GlobalEnv)
    }
    if (yuleval == 1)
    {
      if ( dim(.Tabla)[1] != 2 || dim(.Tabla)[2] != 2){
        Message(message=gettextRcmdr("La tabla de contingencia no es de tamano 2x2: \n Indices de Yule no se calcularan"),
                type="warning")
      }
      instruccion11 <- ".a <- .Tabla[1,1]"
      if (echocodigo == 1) logger(instruccion11)
      justDoIt(instruccion11)
      instruccion12 <- ".b <- .Tabla[1,2]"
      if (echocodigo == 1) logger(instruccion12)
      justDoIt(instruccion12)
      instruccion13 <- ".c <- .Tabla[2,1]"
      if (echocodigo == 1) logger(instruccion13)
      justDoIt(instruccion13)
      instruccion14 <- ".d <- .Tabla[2,2]"
      if (echocodigo == 1) logger(instruccion14)
      justDoIt(instruccion14)
      instruccion15 <- ".Q <- (.a*.d-.b*.c)/(.a*.d+.b*.c)"
      if (echocodigo == 1) logger(instruccion15)
      justDoIt(instruccion15)
      doItAndPrint(".Q # Coef. Q de Yule ")
      instruccion16 <- ".Y <- (sqrt(.a*.d)-sqrt(.b*.c))/(sqrt(.a*.d)+sqrt(.b*.c))"
      if (echocodigo == 1) logger(instruccion16)
      justDoIt(instruccion16)
      doItAndPrint(".Y # Coef. Y de Yule ")
      instruccion17 <- ".V <- (.a*.d-.b*.c)/((.a+.b)*(.a+.c)*(.b+.d)*(.c+.d))"
      if (echocodigo == 1) logger(instruccion17)
      justDoIt(instruccion17)
      doItAndPrint(".V # Coef. V de Yule ")
      if (echocodigo == 1) logger("remove(list=c('.a','.b','.c','.d','.Q','.Y','.V'))")
      if (creahtml == 1)
      {
        i <- i+1
        rownames(.TablaCoefAsoc)[i:(i+2)] <- paste("Coeficiente ",c("Q","Y","V")," de Yule",sep="")
        .TablaCoefAsoc[i:(i+2),1] <- c(.Q,.Y,.V)
      }
      remove(list=c('.a','.b','.c','.d','.Q','.Y','.V'), envir=.GlobalEnv)
    }
    if (lambdaval == 1)
    {
      instruccion18 <- "lambda.a.b <- (sum(apply(.Tabla,2,max)/sum(.Tabla)) - 
      max(rowSums(.Tabla))/sum(.Tabla))/(1 - 
      max(rowSums(.Tabla))/sum(.Tabla))"
      if (echocodigo == 1) logger(instruccion18)
      justDoIt(instruccion18)
      doItAndPrint(paste("lambda.a.b # Lambda de Goodman-Kruskal (",fila,
                         " dependiente)",sep=""))
      instruccion19 <- "lambda.b.a <- (sum(apply(.Tabla,1,max)/sum(.Tabla)) - 
      max(colSums(.Tabla))/sum(.Tabla))/(1 - 
      max(colSums(.Tabla))/sum(.Tabla))"
      if (echocodigo == 1) logger(instruccion19)
      justDoIt(instruccion19)
      doItAndPrint(paste("lambda.b.a # Lambda de Goodman-Kruskal (",columna,
                         " dependiente)",sep=""))
      instruccion20 <- ".lambda <- (lambda.a.b + lambda.b.a)/2"
      if (echocodigo == 1) logger(instruccion20)
      justDoIt(instruccion20)
      doItAndPrint(".lambda # Lambda de Goodman-Kruskal (simetrica) ")
      if (echocodigo == 1) logger("remove(list=c('lambda.a.b','lambda.b.a','.lambda'))")
      if (creahtml == 1)
      {
        j <- j+1
        rownames(.TablaErrorPred)[j:(j+2)] <- paste("Coeficiente Lambda ",c("A/B","B/A","Simetrico")," Goodman-Kruskal",sep="")
        .TablaErrorPred[j:(j+2),1] <- c(lambda.a.b,lambda.b.a,.lambda)
        j <- j+2
      }
      remove(list=c('lambda.a.b','lambda.b.a','.lambda'), envir=.GlobalEnv)
    }
    if (tauval == 1)
    {
      instruccion21 <- "tau.a.b <- (sum((.Tabla/sum(.Tabla))^2/matrix(colSums(.Tabla)[col(.Tabla)]
      /sum(.Tabla),nrow=nrow(.Tabla)))-sum((rowSums(.Tabla)/sum(.Tabla))^2))/
      (1-sum((rowSums(.Tabla)/sum(.Tabla))^2))"
      if (echocodigo == 1) logger(instruccion21)
      justDoIt(instruccion21)
      doItAndPrint(paste("tau.a.b # Tau de Goodman-Kruskal (",fila,
                         " dependiente)",sep=""))
      instruccion22 <- "tau.b.a <- (sum((.Tabla/sum(.Tabla))^2/matrix(rowSums(.Tabla)[row(.Tabla)]/
      sum(.Tabla),nrow=nrow(.Tabla)))-sum((colSums(.Tabla)/sum(.Tabla))^2))/
      (1-sum((colSums(.Tabla)/sum(.Tabla))^2))"
      if (echocodigo == 1) logger(instruccion22)
      justDoIt(instruccion22)
      doItAndPrint(paste("tau.b.a # Tau de Goodman-Kruskal (",columna,
                         " dependiente)",sep=""))
      if (echocodigo == 1) logger("remove(list=c('tau.a.b','tau.b.a'))")
      if (creahtml == 1)
      {
        j <- j+1
        rownames(.TablaErrorPred)[j:(j+1)] <- paste("Coeficiente Tau ",c("A/B","B/A")," Goodman-Kruskal",sep="")
        .TablaErrorPred[j:(j+1),1] <- c(tau.a.b,tau.b.a)
        j <- j+1
      }
      remove(list=c('tau.a.b','tau.b.a'), envir=.GlobalEnv)
    }
    if (theilval == 1)
    {
      instruccion23 <- "H.a.b <- -sum(.Tabla/sum(.Tabla)*log(.Tabla/sum(.Tabla)),na.rm=TRUE)"
      if (echocodigo == 1) logger(instruccion23)
      justDoIt(instruccion23)
      instruccion24 <- "H.a <- -sum(rowSums(.Tabla)/sum(.Tabla)*log(rowSums(.Tabla)/sum(.Tabla)),na.rm=TRUE)"
      if (echocodigo == 1) logger(instruccion24)
      justDoIt(instruccion24)
      instruccion25 <- "H.b <- -sum(colSums(.Tabla)/sum(.Tabla)*log(colSums(.Tabla)/sum(.Tabla)),na.rm=TRUE)"
      if (echocodigo == 1) logger(instruccion25)
      justDoIt(instruccion25)
      instruccion26 <- "theil.a.b <- (H.a + H.b - H.a.b)/H.a"
      if (echocodigo == 1) logger(instruccion26)
      justDoIt(instruccion26)
      doItAndPrint(paste("theil.a.b # Coef. de Theil ( ",fila,
                         " dependiente)",sep=""))
      instruccion27 <- "theil.b.a <- (H.a + H.b - H.a.b)/H.b"
      if (echocodigo == 1) logger(instruccion27)
      justDoIt(instruccion27)
      doItAndPrint(paste("theil.b.a # Coef. de Theil ( ",columna,
                         " dependiente)",sep=""))
      instruccion28 <- ".theil <- 2*(H.a + H.b - H.a.b)/(H.a+H.b)"
      if (echocodigo == 1) logger(instruccion28)
      justDoIt(instruccion28)
      doItAndPrint(".theil # Coef. de Theil (simetrico) ")
      if (echocodigo == 1) logger("remove(list=c('H.a','H.b','H.a.b',
                                  'theil.a.b','theil.b.a','.theil'))")
      if (creahtml == 1)
      {
        j <- j+1
        rownames(.TablaErrorPred)[j:(j+2)] <- paste("Coef. Incertidumbre de Theil ",c("A/B","B/A","Simetrico"),sep="")
        .TablaErrorPred[j:(j+2),1] <- c(theil.a.b,theil.b.a,.theil)
        j <- j+2
      }
      remove(list=c('H.a','H.b','H.a.b','theil.a.b','theil.b.a','.theil'),
             envir=.GlobalEnv)
    }
    if (echocodigo == 1)
    {
      logger("remove(.Tabla)")
      if (jicuadrado == 1) logger("remove(.Jicuadrado)")
    }
    remove(.Tabla, envir=.GlobalEnv)
    if (jicuadrado == 1) remove(.Jicuadrado, envir=.GlobalEnv)
    tkfocus(CommanderWindow())
    if (creahtml == 1)
    {
      if (selec > 0)
      {
        HTML("Coeficientes de Asociacion", file=.archivo)
        HTML(.TablaCoefAsoc, file=.archivo)
        .TablaCoefAsoc <- round(.TablaCoefAsoc,3)
        remove(.TablaCoefAsoc, envir=.GlobalEnv)
      }
      HTMLhr(file = .archivo)
    }
    if (creahtml == 1)
    {
      if (selec2 > 0)
      {
        HTML("Medidas del error de prediccion", file=.archivo)
        HTML(.TablaErrorPred, file=.archivo)
        .TablaErrorPred <- round(.TablaErrorPred,3)
        remove(.TablaErrorPred, envir=.GlobalEnv)
      }
      HTMLhr(file = .archivo)
    }
  }
  OKCancelHelp(helpSubject="xtabs")
  tkgrid(labelRcmdr(esperadasFrame,
                    text=gettextRcmdr("Frecuencias Esperadas"), fg="blue"), columnspan=6, sticky="w")
  tkgrid(labelRcmdr(esperadasFrame,text=gettextRcmdr("Calcular frecuencias esperadas ")), 
         frecEspCheckBox, sticky="w")
  tkgrid(labelRcmdr(descjiFrame,
                    text=gettextRcmdr("Coeficientes de Asociacion"), fg="blue"), columnspan=6, sticky="w")
  tkgrid(labelRcmdr(descjiFrame,text=gettextRcmdr("Ji Cuadrado ")), 
         jicuadradoCheckBox, labelRcmdr(descjiFrame, 
                                        text=gettextRcmdr("Descomposicion Ji cuadrado de Pearson ")), 
         jiComponentesCheckBox,sticky="w")
  tkgrid(labelRcmdr(descjiFrame,text=gettextRcmdr("Phi de Pearson ")), 
         phiPearsonCheckBox, labelRcmdr(descjiFrame,
                                        text=gettextRcmdr("Coef. Contingencia Pearson ")),contingPearsonCheckBox,
         sticky="w")
  tkgrid(labelRcmdr(descjiFrame,text=gettextRcmdr("Transformacion de Sakoda ")), 
         sakodaCheckBox, labelRcmdr(descjiFrame,
                                    text=gettextRcmdr("Coef. Contingencia Chuprov ")),chuprovCheckBox,
         sticky="w")
  tkgrid(labelRcmdr(descjiFrame,text=gettextRcmdr("Coef. Contingencia Cramer ")), 
         VCramerCheckBox, labelRcmdr(descjiFrame,
                                     text=gettextRcmdr("Coeficientes Yule (Tablas 2x2) ")),yuleCheckBox,
         sticky="w")
  tkgrid(labelRcmdr(errorpredFrame,
                    text=gettextRcmdr("Medidas Error de Prediccion"), fg="blue"), columnspan=6, sticky="w")
  tkgrid(labelRcmdr(errorpredFrame,text=gettextRcmdr("Lambda Goodman-Kruskal ")), 
         lambdaCheckBox, labelRcmdr(errorpredFrame,text=gettextRcmdr("Tau Goodman-Kruskal ")), 
         tauCheckBox, labelRcmdr(errorpredFrame,text=gettextRcmdr("Coef. Incertidumbre Theil ")), 
         theilCheckBox,sticky="w")
  tkgrid(labelRcmdr(opcionesFrame,
                    text=gettextRcmdr("Opciones"), fg="blue"), sticky="w")
  tkgrid(labelRcmdr(opcionesFrame, 
                    text=gettextRcmdr("Mostrar en pantalla el codigo de R ejecutado ")), 
         echoCheckBox, sticky="w")
  tkgrid(labelRcmdr(opcionesFrame,
                    text=gettextRcmdr("Generar informe de resultados ")),
         htmlCheckBox,sticky="w")
  tkgrid(getFrame(filaVar), labelRcmdr(variablesFrame, text="    "),
         getFrame(columnaVar), sticky="nw")
  tkgrid(variablesFrame, sticky="w")
  tkgrid(porcentajesFrame, sticky="w")
  tkgrid(esperadasFrame, sticky="w")
  tkgrid(descjiFrame, sticky="w")
  tkgrid(errorpredFrame, sticky="w")
  tkgrid(subsetFrame, sticky="w")
  tkgrid(opcionesFrame, sticky="w")
  tkgrid(buttonsFrame, sticky="w")
  dialogSuffix(rows=6, columns=1)
  }

barras.agrupadas <- function(){
  require("abind")
  initializeDialog(title=gettextRcmdr("Diagrama de barras agrupadas"))
  opcionesFrame <- tkframe(top)
  variablesFrame <- tkframe(top)
  filaVar <- variableListBox(variablesFrame, Factors(), title=gettextRcmdr("Variable por filas (escoja una)"))
  columnaVar <- variableListBox(variablesFrame, Factors(), title=gettextRcmdr("Variable por columnas (escoja una)"))
  radioButtons(top, name="tabla", buttons=c("niBoton", "fiBoton"), values=c("ni", "fi"),
               labels=gettextRcmdr(c("Frecuencias Absolutas", "Frecuencias Relativas")),
               title=gettextRcmdr("Tablas basadas en:"))
  echocodigoVariable <- tclVar("0")
  echoCheckBox <- tkcheckbutton(opcionesFrame, variable=echocodigoVariable)
  creahtmlVariable <- tclVar("0")
  htmlCheckBox <- tkcheckbutton(opcionesFrame, variable=creahtmlVariable)
  onOK <- function(){
    fila <- getSelection(filaVar)
    columna <- getSelection(columnaVar)
    if (length(fila) == 0 || length(columna) == 0){
      errorCondition(recall=barras.agrupadas, message=gettextRcmdr("Debe seleccionar dos variables."))
      return()
    }
    if (fila == columna) {
      errorCondition(recall=barras.agrupadas, message=gettextRcmdr("Debe seleccionar dos variables distintas."))
      return()
    }
    tabVariable <- as.character(tclvalue(tablaVariable))
    echocodigo <- tclvalue(echocodigoVariable)
    creahtml <- tclvalue(creahtmlVariable)
    if (creahtml == 1)
    {
      require(R2HTML)
      if (!file.exists("Informe de Resultados.html"))
        .archivo <- HTMLInitFile(file.path(getwd()),
                                 "Informe de Resultados", BackGroundColor="#FFFFCC")
      else
        .archivo <- file.path(getwd(), "Informe de Resultados.html")
      titulo <- paste("Diagrama de barras agrupadas para : ",fila, 
                      " y ", columna, sep="")
      HTML(as.title(titulo),file=.archivo)
    }
    closeDialog()
    instruccion <- paste(".Tabla <- xtabs(~", fila, "+", columna, ", data=", ActiveDataSet(),
                         ")", sep="")
    justDoIt(instruccion)
    if (echocodigo == 1) logger(instruccion)
    if (tabVariable == "fi")
    {  
      instruccion2 <- ".Tabla <- .Tabla/sum(.Tabla)"
      justDoIt(instruccion2)
      if (echocodigo == 1) logger(instruccion2)
    }
    titulo <- paste("Barras agrupadas para ",fila," y ",columna, sep="")
    tituloy <- if (tabVariable == "ni") "Frecuencias Absolutas"
    else "Frecuencias Relativas"
    instruccion3 <- paste("barplot(.Tabla,beside=TRUE,main='",titulo,
                          "',ylab='",tituloy,"',xlab='",columna,"',ylim=c(0,max(.Tabla)*1.05),",
                          "col=heat.colors(length(levels(",ActiveDataSet(),"$",fila,
                          "))),legend.text=TRUE,args.legend=list(x='topright',title='",fila,"'))",
                          sep="")
    instruccion4 <- "box()"
    justDoIt(instruccion3)
    justDoIt(instruccion4)
    if (echocodigo == 1)
    {
      logger(instruccion3)
      logger(instruccion4)
      logger("remove(.Tabla)")
    }
    remove(.Tabla, envir=.GlobalEnv)          
    if (creahtml == 1)
    {
      nombre.archivo <- paste("BarrasAgrupadasR",gsub(":","",substr(Sys.time(),12,19)),
                              ".jpg",sep="")
      dev.print(jpeg, filename=paste(getwd(),"/",nombre.archivo,sep=""),
                width=500, height=500)
      HTMLInsertGraph(nombre.archivo,file=.archivo,append=TRUE)
      HTMLhr(file = .archivo)
    }
    closeDialog()        
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="barplot")
  tkgrid(labelRcmdr(opcionesFrame,
                    text=gettextRcmdr("Opciones"), fg="blue"), sticky="w")
  tkgrid(labelRcmdr(opcionesFrame, 
                    text=gettextRcmdr("Mostrar en pantalla el codigo de R ejecutado ")), 
         echoCheckBox, sticky="w")
  tkgrid(labelRcmdr(opcionesFrame,
                    text=gettextRcmdr("Generar informe de resultados ")),
         htmlCheckBox,sticky="w")
  tkgrid(getFrame(filaVar), labelRcmdr(variablesFrame, text="    "),
         getFrame(columnaVar), sticky="nw")
  tkgrid(variablesFrame, sticky="w")
  tkgrid(tablaFrame, sticky="w")
  tkgrid(opcionesFrame, sticky="w")
  tkgrid(buttonsFrame, sticky="w")
  dialogSuffix(rows=6, columns=1)        
}

grafico.mosaico <- function(){
    require("abind")
    initializeDialog(title=gettextRcmdr("Grafico de Mosaico"))
    opcionesFrame <- tkframe(top)
    variablesFrame <- tkframe(top)
    filaVar <- variableListBox(variablesFrame, Factors(), title=gettextRcmdr("Variable por filas (escoja una)"))
    columnaVar <- variableListBox(variablesFrame, Factors(), title=gettextRcmdr("Variable por columnas (escoja una)"))
    echocodigoVariable <- tclVar("0")
    echoCheckBox <- tkcheckbutton(opcionesFrame, variable=echocodigoVariable)
    creahtmlVariable <- tclVar("0")
    htmlCheckBox <- tkcheckbutton(opcionesFrame, variable=creahtmlVariable)
    onOK <- function(){
        fila <- getSelection(filaVar)
        columna <- getSelection(columnaVar)
        if (length(fila) == 0 || length(columna) == 0){
            errorCondition(recall=grafico.mosaico, message=gettextRcmdr("Debe seleccionar dos variables."))
            return()
            }
        if (fila == columna) {
            errorCondition(recall=grafico.mosaico, message=gettextRcmdr("Debe seleccionar dos variables distintas."))
            return()
            }
        echocodigo <- tclvalue(echocodigoVariable)
        creahtml <- tclvalue(creahtmlVariable)
        if (creahtml == 1)
        {
         require(R2HTML)
         if (!file.exists("Informe de Resultados.html"))
           .archivo <- HTMLInitFile(file.path(getwd()),
                       "Informe de Resultados", BackGroundColor="#FFFFCC")
         else
           .archivo <- file.path(getwd(), "Informe de Resultados.html")
         titulo <- paste("Grafico de mosaico para : ",fila, 
                   " y ", columna, sep="")
         HTML(as.title(titulo),file=.archivo)
        }
        closeDialog()
        instruccion <- paste(".Tabla <- xtabs(~", fila, "+", columna, ", data=", ActiveDataSet(),
                       ")", sep="")
        justDoIt(instruccion)
        if (echocodigo == 1) logger(instruccion)
        titulo <- paste("Grafico de mosaico para ",fila," y ",columna, sep="")
        instruccion2 <- paste("mosaicplot(.Tabla,main='",titulo,"',
        col=dim(.Tabla))",sep="")
        instruccion3 <- "box()"
        justDoIt(instruccion2)
        justDoIt(instruccion3)
        if (echocodigo == 1)
        {
          logger(instruccion2)
          logger(instruccion3)
          logger("remove(.Tabla)")
        }
        remove(.Tabla, envir=.GlobalEnv)
        if (creahtml == 1)
        {
          nombre.archivo <- paste("GraficoMosaicoR",gsub(":","",substr(Sys.time(),12,19)),
          ".jpg",sep="")
          dev.print(jpeg, filename=paste(getwd(),"/",nombre.archivo,sep=""),
          width=500, height=500)
          HTMLInsertGraph(nombre.archivo,file=.archivo,append=TRUE)
          HTMLhr(file = .archivo)
        }
        closeDialog()        
        tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject="mosaicplot")
    tkgrid(labelRcmdr(opcionesFrame,
    text=gettextRcmdr("Opciones"), fg="blue"), sticky="w")
    tkgrid(labelRcmdr(opcionesFrame, 
    text=gettextRcmdr("Mostrar en pantalla el codigo de R ejecutado ")), 
    echoCheckBox, sticky="w")
    tkgrid(labelRcmdr(opcionesFrame,
    text=gettextRcmdr("Generar informe de resultados ")),
    htmlCheckBox,sticky="w")
    tkgrid(getFrame(filaVar), labelRcmdr(variablesFrame, text="    "),
    getFrame(columnaVar), sticky="nw")
    tkgrid(variablesFrame, sticky="w")
    tkgrid(opcionesFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=6, columns=1)        
}

bivariante.ordinales <- function(){
    require("abind")
    initializeDialog(title=gettextRcmdr("Descripcion bivariante datos ordinales"))
    opcionesFrame <- tkframe(top)
    variablesFrame <- tkframe(top)
    filaVar <- variableListBox(variablesFrame, c(Factors(),Numeric()), title=gettextRcmdr("Variable por filas (escoja una)"))
    columnaVar <- variableListBox(variablesFrame, c(Factors(),Numeric()), title=gettextRcmdr("Variable por columnas (escoja una)"))
    subsetBox()
    radioButtons(name="porcentajes", buttons=c("fila",
    "columna", "total", "ninguno"),values=c("fila", "columna",
    "total", "ninguno"), initialValue="ninguno",
    labels=gettextRcmdr(c("Porcentajes respecto a marginales fila",
    "Porcentajes respecto a marginales columna","Porcentajes respecto al total",
    "Ningun porcentaje")),title=gettextRcmdr("Calcular Porcentajes"))
    echocodigoVariable <- tclVar("0")
    echoCheckBox <- tkcheckbutton(opcionesFrame, variable=echocodigoVariable)
    creahtmlVariable <- tclVar("0")
    htmlCheckBox <- tkcheckbutton(opcionesFrame, variable=creahtmlVariable)
    asocFrame <- tkframe(top, borderwidth=2, relief="groove")
    gammaVariable <- tclVar("0")
    gammaCheckBox <- tkcheckbutton(asocFrame, variable=gammaVariable)
    tauVariable <- tclVar("0")
    tauCheckBox <- tkcheckbutton(asocFrame, variable=tauVariable)
    sommersVariable <- tclVar("0")
    sommersCheckBox <- tkcheckbutton(asocFrame, variable=sommersVariable)
    wilsonVariable <- tclVar("0")
    wilsonCheckBox <- tkcheckbutton(asocFrame, variable=wilsonVariable)
    onOK <- function(){
        fila <- getSelection(filaVar)
        columna <- getSelection(columnaVar)
        if (length(fila) == 0 || length(columna) == 0){
            errorCondition(recall=bivariante.ordinales, message=gettextRcmdr("Debe seleccionar dos variables."))
            return()
            }
        if (fila == columna) {
            errorCondition(recall=bivariante.ordinales, message=gettextRcmdr("Debe seleccionar dos variables distintas."))
            return()
            }
        justDoIt(paste("cond1 <- !is.ordered(",paste(ActiveDataSet(),"$",fila,sep=""),")",sep=""))
        justDoIt(paste("cond2 <- !is.numeric(",paste(ActiveDataSet(),"$",fila,sep=""),")",sep=""))
        justDoIt(paste("cond3 <- !is.ordered(",paste(ActiveDataSet(),"$",columna,sep=""),")",sep=""))
        justDoIt(paste("cond4 <- !is.numeric(",paste(ActiveDataSet(),"$",columna,sep=""),")",sep=""))        
        if (cond1 && cond2 || cond3 && cond4){
            errorCondition(recall=bivariante.ordinales, message=gettextRcmdr("Escoja variables ordinales"))
            return()
            }
        remove(list=c("cond1","cond2","cond3","cond4"), envir=.GlobalEnv)
        porcentajes <- as.character(tclvalue(porcentajesVariable))
        gammaval <- tclvalue(gammaVariable)
        tauval <- tclvalue(tauVariable)
        sommersval <- tclvalue(sommersVariable)
        wilsonval <- tclvalue(wilsonVariable)        
        subconjunto <- tclvalue(subsetVariable)
        subconjunto <- if (trim.blanks(subconjunto) == gettextRcmdr("<all valid cases>")) ""
            else paste(", subset=", subconjunto, sep="")
        echocodigo <- tclvalue(echocodigoVariable)
        selec <- as.numeric(gammaval) + as.numeric(tauval)*3 + as.numeric(sommersval)*3 +
                 as.numeric(wilsonval)
        creahtml <- tclvalue(creahtmlVariable)
        if (creahtml == 1)
        {
         require(R2HTML)
         if (!file.exists("Informe de Resultados.html"))
           .archivo <- HTMLInitFile(file.path(getwd()),
                       "Informe de Resultados", BackGroundColor="#FFFFCC")
         else
           .archivo <- file.path(getwd(), "Informe de Resultados.html")
         if (selec > 0)
         {
           numfilas <- selec
           instruccion <- paste(".TablaCoefAsoc <- as.data.frame(matrix(nrow=",numfilas,",ncol=1))")
           justDoIt(instruccion)
           colnames(.TablaCoefAsoc) <- "Valores"
         }
         titulo <- paste("Descripcion bivariante de datos ordinales: ",fila, 
                   " y ", columna, sep="")
         HTML(as.title(titulo),file=.archivo)
        }
        closeDialog()
        instruccion <- paste(".Tabla <- xtabs(~", fila, "+", columna, ", data=", ActiveDataSet(),
            subconjunto, ")", sep="")
        justDoIt(instruccion)
        if (echocodigo == 1) logger(instruccion)
        doItAndPrint(paste(".Tabla  # Tabla de contingencia para ",
                     fila," y ",columna,sep=""))
        if (creahtml == 1)
        {
          subtitulo <- "Tabla de contingencia"      
          HTML(subtitulo,file=.archivo)
          HTML(.Tabla,file=.archivo)
        }
        if (porcentajes == "fila")
        {
          instruccion2 <- ".porcentajes <- rowPercents(.Tabla)"
          justDoIt(instruccion2)
          if (echocodigo == 1) logger(instruccion2)
          doItAndPrint(".porcentajes  # Porcentajes respecto a marginales fila ")
          if (echocodigo == 1) logger("remove(.porcentajes)")
          if (creahtml == 1)
          {
             subtitulo <- "Tabla porcentajes respecto a marginales fila"      
             HTML(subtitulo,file=.archivo)
             HTML(.porcentajes,file=.archivo)
          }
          remove(.porcentajes, envir=.GlobalEnv)
        }
        if (porcentajes == "columna")
        {
          instruccion2 <- ".porcentajes <- colPercents(.Tabla)"
          justDoIt(instruccion2)
          if (echocodigo == 1) logger(instruccion2)
          doItAndPrint(".porcentajes  # Porcentajes respecto a marginales columna ")
          if (echocodigo == 1) logger("remove(.porcentajes)")
          if (creahtml == 1)
          {
             subtitulo <- "Tabla porcentajes respecto a marginales columna"      
             HTML(subtitulo,file=.archivo)
             HTML(.porcentajes,file=.archivo)
          }
          remove(.porcentajes, envir=.GlobalEnv)
        }
        if (porcentajes == "total")
        {
          instruccion2 <- "porcentajes <- totPercents(.Tabla)"
          justDoIt(instruccion2)
          if (echocodigo == 1) logger(instruccion2)
          doItAndPrint(".porcentajes  # Porcentajes respecto al total")
          if (echocodigo == 1) logger("remove(.porcentajes)")
          if (creahtml == 1)
          {
             subtitulo <- "Tabla porcentajes respecto al total"      
             HTML(subtitulo,file=.archivo)
             HTML(.porcentajes,file=.archivo)
          }
          remove(.porcentajes, envir=.GlobalEnv)
        }
        instruccion3 <- "filas <- row(.Tabla)"
        justDoIt(instruccion3)
        if (echocodigo == 1) logger(instruccion3)
        instruccion4 <- "columnas <- col(.Tabla)"
        justDoIt(instruccion4)
        if (echocodigo == 1) logger(instruccion4)
        instruccion5 <- "n <- sum(.Tabla)"
        justDoIt(instruccion5)
        if (echocodigo == 1) logger(instruccion5)
        instruccion6 <- "q <- min(dim(.Tabla))"
        justDoIt(instruccion6)
        if (echocodigo == 1) logger(instruccion6)
        instruccion7 <- "C <- sum(.Tabla * mapply(function(f, c){sum(.Tabla[(filas > f) &
        (columnas > c)])}, f = filas, c = columnas))"
        justDoIt(instruccion7)
        if (echocodigo == 1) logger(instruccion7)
        doItAndPrint("C  # Numero de pares concordantes")
        instruccion8 <- "D <- sum(.Tabla * mapply(function(f, c){sum(.Tabla[(filas > f) &
        (columnas < c)])}, f = filas, c = columnas))"
        justDoIt(instruccion8)
        if (echocodigo == 1) logger(instruccion8)
        doItAndPrint("D  # Numero de pares discordantes")
        instruccion9 <- "E.X <- (sum(apply(.Tabla,1,sum)^2)-n)/2"
        justDoIt(instruccion9)
        if (echocodigo == 1) logger(instruccion9)
        doItAndPrint(paste("E.X  # Numero de empates en ",fila,sep=""))
        instruccion10 <- "E.Y <- (sum(apply(.Tabla,2,sum)^2)-n)/2"
        justDoIt(instruccion10)
        if (echocodigo == 1) logger(instruccion10)
        doItAndPrint(paste("E.Y  # Numero de empates en ",columna,sep=""))
        instruccion11 <- "E.XY <- (sum(.Tabla^2)-n)/2"
        justDoIt(instruccion11)
        if (echocodigo == 1) logger(instruccion11)
        doItAndPrint(paste("E.XY  # Numero de empates en ",fila," y ",columna,sep=""))
        if (creahtml == 1)
        {
          tablares <- matrix(c(C,D,E.X,E.Y,E.XY),nrow=5,ncol=1)
          rownames(tablares) <- c("Pares concordantes","Pares discordantes",
                                paste("Empates.",fila),paste("Empates.",columna),
                                paste("Empates.",fila,".",columna))
          subtitulo <- "Descripcion Tabla de contingencia"      
          HTML(subtitulo,file=.archivo)
          HTML(tablares,file=.archivo)
        } 
        i <- 0
        if (gammaval == 1)
        {
          instruccion12 <- "gammaGK <- (C-D)/(C+D)"
          if (echocodigo == 1) logger(instruccion12)
          justDoIt(instruccion12)
          doItAndPrint("gammaGK # Gamma de Goodman-Kruskal ")
          if (echocodigo == 1) logger("remove(gammaGK)")
          if (creahtml == 1)
          {
            i <- i+1
            rownames(.TablaCoefAsoc)[i] <- "Coeficiente Gamma Goodman-Kruskal"
            .TablaCoefAsoc[i] <- gammaGK
          }
          remove(gammaGK, envir=.GlobalEnv)
        }
        if (tauval == 1)
        {
          instruccion13 <- "tau.a <- (C-D)/choose(n,2)"
          if (echocodigo == 1) logger(instruccion13)
          justDoIt(instruccion13)
          doItAndPrint("tau.a # Tau a de Kendall ")
          instruccion14 <- "tau.b <- (C-D)/sqrt((C+D+E.X-E.XY)*(C+D+E.Y-E.XY))"
          if (echocodigo == 1) logger(instruccion14)
          justDoIt(instruccion14)
          doItAndPrint("tau.b # Tau b de Kendall ")
          instruccion15 <- "tau.c <- 2*q*(C-D)/(n^2*(q-1))"
          if (echocodigo == 1) logger(instruccion15)
          justDoIt(instruccion15)
          doItAndPrint("tau.c # Tau c de Kendall ")
          if (echocodigo == 1) logger("remove(list=c('tau.a','tau.b','tau.c'))")
          if (creahtml == 1)
          {
            i <- i+1
            rownames(.TablaCoefAsoc)[i:(i+2)] <- paste("Coeficiente Tau ",c("a","b","c")," de Kendall",sep="")
            .TablaCoefAsoc[i:(i+2),1] <- c(tau.a,tau.b,tau.c)
            i <- i+2
          }
          remove(list=c('tau.a','tau.b','tau.c'), envir=.GlobalEnv)
        }
        if (sommersval == 1)
        {
          instruccion16 <- "sommers.x.y <- (C-D)/(C+D+E.X-E.XY)"
          if (echocodigo == 1) logger(instruccion16)
          justDoIt(instruccion16)
          doItAndPrint(paste("sommers.x.y # Coef. de Sommers ( ",fila,
                      " dependiente)",sep=""))
          instruccion17 <- "sommers.y.x <- (C-D)/(C+D+E.Y-E.XY)"
          if (echocodigo == 1) logger(instruccion17)
          justDoIt(instruccion17)
          doItAndPrint(paste("sommers.y.x # Coef. de Sommers ( ",columna,
                      " dependiente)",sep=""))          
          instruccion18 <- ".sommers <- (C-D)/(C+D+(E.X+E.Y)/2-E.XY)"
          if (echocodigo == 1) logger(instruccion18)
          justDoIt(instruccion18)
          doItAndPrint(".sommers # Coef. de Sommers (simetrico) ")
          if (echocodigo == 1) logger("remove(list=c('sommers.x.y','sommers.y.x',
                                      '.sommers'))")
          if (creahtml == 1)
          {
            i <- i+1
            rownames(.TablaCoefAsoc)[i:(i+2)] <- paste("Indice d ",c("X/Y","Y/X","Simetrico")," de Sommers",sep="")
            .TablaCoefAsoc[i:(i+2),1] <- c(sommers.x.y,sommers.y.x,.sommers)
            i <- i+2
          }
          remove(list=c('sommers.x.y','sommers.y.x','.sommers'),envir=.GlobalEnv)
        }
        if (wilsonval == 1)
        {
          instruccion19 <- ".wilson <- 2*(C-D)/(choose(n,2)-E.XY)"
          if (echocodigo == 1) logger(instruccion19)
          justDoIt(instruccion19)
          doItAndPrint(".wilson # e de Wilson ")
          if (echocodigo == 1) logger("remove(.wilson)")
          if (creahtml == 1)
          {
            i <- i+1
            rownames(.TablaCoefAsoc)[i] <- "Indice e de Wilson"
            .TablaCoefAsoc[i,1] <- .wilson
          }
          remove(.wilson, envir=.GlobalEnv)
        }
        if (echocodigo == 1)
        {
          logger("remove(list=c('.Tabla','filas','columnas','n','q','C',
                 'D','E.X','E.Y','E.XY'))")
        }
        remove(list=c('.Tabla','filas','columnas','n','q','C',
                 'D','E.X','E.Y','E.XY'), envir=.GlobalEnv)
        tkfocus(CommanderWindow())
        if (creahtml == 1)
        {
          if (selec > 0)
          {
            HTML("Coeficientes de Asociacion", file=.archivo)
            HTML(.TablaCoefAsoc, file=.archivo)
            .TablaCoefAsoc <- round(.TablaCoefAsoc,3)
            remove(.TablaCoefAsoc, envir=.GlobalEnv)
          }
          HTMLhr(file = .archivo)
        }
    }
    OKCancelHelp(helpSubject="xtabs")
    tkgrid(labelRcmdr(asocFrame,
    text=gettextRcmdr("Coeficientes de Asociacion"), fg="blue"), columnspan=6, sticky="w")
    tkgrid(labelRcmdr(asocFrame,text=gettextRcmdr("Coef. Gamma de Goodman-Kruskal ")), 
    gammaCheckBox, sticky="w")
    tkgrid(labelRcmdr(asocFrame,text=gettextRcmdr("Coefs. Tau de Kendall ")), 
    tauCheckBox, sticky="w")
    tkgrid(labelRcmdr(asocFrame,text=gettextRcmdr("Indices d de Sommers ")), 
    sommersCheckBox, sticky="w")
    tkgrid(labelRcmdr(asocFrame,text=gettextRcmdr("Indice e de Wilson ")), 
    wilsonCheckBox, sticky="w")
    tkgrid(labelRcmdr(opcionesFrame,
    text=gettextRcmdr("Opciones"), fg="blue"), sticky="w")
    tkgrid(labelRcmdr(opcionesFrame, 
    text=gettextRcmdr("Mostrar en pantalla el codigo de R ejecutado ")), 
    echoCheckBox, sticky="w")
    tkgrid(labelRcmdr(opcionesFrame,
    text=gettextRcmdr("Generar informe de resultados ")),
    htmlCheckBox,sticky="w")
    tkgrid(getFrame(filaVar), labelRcmdr(variablesFrame, text="    "),
    getFrame(columnaVar), sticky="nw")
    tkgrid(variablesFrame, sticky="w")
    tkgrid(porcentajesFrame, sticky="w")
    tkgrid(asocFrame, sticky="w")
    tkgrid(subsetFrame, sticky="w")
    tkgrid(opcionesFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=6, columns=1)
    }

dispersion.ordinales <- function(){
    require("reshape")
    initializeDialog(title=gettextRcmdr("Diagrama de puntos para datos ordinales"))
    opcionesFrame <- tkframe(top)
    variablesFrame <- tkframe(top)
    filaVar <- variableListBox(variablesFrame, Factors(), title=gettextRcmdr("Variable por filas (escoja una)"))
    columnaVar <- variableListBox(variablesFrame, Factors(), title=gettextRcmdr("Variable por columnas (escoja una)"))
    echocodigoVariable <- tclVar("0")
    echoCheckBox <- tkcheckbutton(opcionesFrame, variable=echocodigoVariable)
    creahtmlVariable <- tclVar("0")
    htmlCheckBox <- tkcheckbutton(opcionesFrame, variable=creahtmlVariable)
    onOK <- function(){
        fila <- getSelection(filaVar)
        columna <- getSelection(columnaVar)
        if (length(fila) == 0 || length(columna) == 0){
            errorCondition(recall=dispersion.ordinales, message=gettextRcmdr("Debe seleccionar dos variables."))
            return()
            }
        if (fila == columna) {
            errorCondition(recall=dispersion.ordinales, message=gettextRcmdr("Debe seleccionar dos variables distintas."))
            return()
            }
        justDoIt(paste("cond1 <- !is.ordered(",paste(ActiveDataSet(),"$",fila,sep=""),")",sep=""))
        justDoIt(paste("cond2 <- !is.ordered(",paste(ActiveDataSet(),"$",columna,sep=""),")",sep=""))
        if (cond1 || cond2){
            errorCondition(recall=dispersion.ordinales, message=gettextRcmdr("Escoja variables ordinales"))
            return()
            }
        remove(list=c("cond1","cond2"), envir=.GlobalEnv)
        echocodigo <- tclvalue(echocodigoVariable)
        creahtml <- tclvalue(creahtmlVariable)
        if (creahtml == 1)
        {
         require(R2HTML)
         if (!file.exists("Informe de Resultados.html"))
           .archivo <- HTMLInitFile(file.path(getwd()),
                       "Informe de Resultados", BackGroundColor="#FFFFCC")
         else
           .archivo <- file.path(getwd(), "Informe de Resultados.html")
         titulo <- paste("Diagrama de puntos para datos ordinales: ",fila, 
                   " y ", columna, sep="")
         HTML(as.title(titulo),file=.archivo)
        }
        instruccion <- "def.par <- par(no.readonly = TRUE)"
        justDoIt(instruccion)
        instruccion2 <- paste("sup <- max(c(table(",ActiveDataSet(),"$",fila,"),
        table(",ActiveDataSet(),"$",columna,")),na.rm=TRUE)",sep="")
        justDoIt(instruccion2)
        instruccion3 <- paste("rangox <- c(min(as.numeric(",ActiveDataSet(),"$",fila,"),na.rm=TRUE),
        max(as.numeric(",ActiveDataSet(),"$",fila,"),na.rm=TRUE))",sep="")
        justDoIt(instruccion3)
        instruccion4 <- paste("rangoy <- c(min(as.numeric(",ActiveDataSet(),"$",columna,"),na.rm=TRUE),
        max(as.numeric(",ActiveDataSet(),"$",columna,"),na.rm=TRUE))",sep="")
        justDoIt(instruccion4)
        instruccion5 <- "nf <- layout(matrix(c(2,0,1,3),2,2,byrow=TRUE), c(3,1), c(1,3), TRUE)"
        justDoIt(instruccion5)
        instruccion6 <- "par(mar=c(5,4,3,3))" 
        instruccion7 <-paste("plot(as.numeric(",ActiveDataSet(),"$",fila,"),
        as.numeric(",ActiveDataSet(),"$",columna,"),xlim=rangox,ylim=rangoy,
        xlab='",fila,"',ylab='",columna,"')",sep="") 
        instruccion8 <- paste(z <- "with(",ActiveDataSet(),",merge(data.frame(",fila,",",
        columna,"),melt(table(",fila,",",columna,")),sort =F)$value)",sep="")
        justDoIt(instruccion8)
        instruccion9 <- paste("z <- 1.5*z/length(",ActiveDataSet(),"$",fila,")",sep="")
        justDoIt(instruccion9)
        instruccion10 <- paste("symbols(na.omit(data.frame(",ActiveDataSet(),"$",fila,
        ",",ActiveDataSet(),"$",columna,")),circles=z,inches=F, bg='grey',fg=NA,add=T)",sep="")
        instruccion11 <- paste("ok <- is.finite(",ActiveDataSet(),"$",fila,") & is.finite(",
        ActiveDataSet(),"$",columna,")",sep="")
        justDoIt(instruccion11)
        if (any(ok)) 
        instruccion12<- paste("lines(stats::lowess(",ActiveDataSet(),"$",fila,"[ok],",
        ActiveDataSet(),"$",columna,"[ok], f = 2/3, iter = 3), col = 'red')",sep="")
        instruccion13 <- "box()"
        instruccion14 <- "par(mar=c(0,3,1,1))" 
        instruccion15 <- paste("barplot(table(",ActiveDataSet(),"$",fila,"),axes=FALSE,ylim=c(0, sup),space=0.1,col='grey')",sep="") 
        instruccion16 <- paste("title('Diagrama de puntos para ",fila," y ",columna,"')",sep="")
        instruccion17 <- "par(mar=c(3,0,1,1))" 
        instruccion18 <- paste("barplot(table(",ActiveDataSet(),"$",columna,"),axes=FALSE,xlim=c(0, sup), space=0.1, horiz=TRUE,col='grey')",sep="") 
        instruccion19 <- "par(def.par)"
        if (echocodigo == 1)
        {
          logger(instruccion) 
          logger(instruccion2)
          logger(instruccion3)
          logger(instruccion4)
          doItAndPrint(instruccion5)
          doItAndPrint(instruccion6)
          doItAndPrint(instruccion7)
          logger(instruccion8)
          logger(instruccion9)
          doItAndPrint(instruccion10)
          logger(paste("ok <-", instruccion11))
          doItAndPrint(instruccion12)
          doItAndPrint(instruccion13)
          doItAndPrint(instruccion14)
          doItAndPrint(instruccion15)
          doItAndPrint(instruccion16)
          doItAndPrint(instruccion17)
          doItAndPrint(instruccion18)
          doItAndPrint(instruccion19)
        }
        else
        {
          justDoIt(instruccion5)
          justDoIt(instruccion6)
          justDoIt(instruccion7)  
          justDoIt(instruccion10)
          justDoIt(instruccion12)
          justDoIt(instruccion13)
          justDoIt(instruccion14)
          justDoIt(instruccion15)
          justDoIt(instruccion16)
          justDoIt(instruccion17)
          justDoIt(instruccion18)
          justDoIt(instruccion19)
        }
        if (creahtml == 1)
        {
          nombre.archivo <- paste("DiagramaPuntosOrdinalesR",gsub(":","",substr(Sys.time(),12,19)),
          ".jpg",sep="")
          dev.print(jpeg, filename=paste(getwd(),"/",nombre.archivo,sep=""),
          width=500, height=500)
          HTMLInsertGraph(nombre.archivo,file=.archivo,append=TRUE)
          HTMLhr(file = .archivo)
        }
        if (echocodigo == 1) logger("remove(list=c('def.par','rangox','rangoy','nf','z','ok'))")
        remove(list=c('def.par','sup','rangox','rangoy','z','ok','nf'), envir=.GlobalEnv)
        closeDialog()
        tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject="plot")
    tkgrid(labelRcmdr(opcionesFrame,
    text=gettextRcmdr("Opciones"), fg="blue"), sticky="w")
    tkgrid(labelRcmdr(opcionesFrame, 
    text=gettextRcmdr("Mostrar en pantalla el codigo de R ejecutado ")), 
    echoCheckBox, sticky="w")
    tkgrid(labelRcmdr(opcionesFrame,
    text=gettextRcmdr("Generar informe de resultados ")),
    htmlCheckBox,sticky="w")
    tkgrid(getFrame(filaVar), labelRcmdr(variablesFrame, text="    "),
    getFrame(columnaVar), sticky="nw")
    tkgrid(variablesFrame, sticky="w")
    tkgrid(opcionesFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=6, columns=1)
    }

bivariante.numericas <- function(){
    initializeDialog(title=gettextRcmdr("Descripcion bivariante datos cuantitativos"))
    opcionesFrame <- tkframe(top)
    variablesFrame <- tkframe(top)
    variable1Var <- variableListBox(variablesFrame, Numeric(), title=gettextRcmdr("Variable x (escoja una)"))
    variable2Var <- variableListBox(variablesFrame, Numeric(), title=gettextRcmdr("Variable y (escoja una)"))
    subsetBox()
    echocodigoVariable <- tclVar("0")
    echoCheckBox <- tkcheckbutton(opcionesFrame, variable=echocodigoVariable)
    creahtmlVariable <- tclVar("0")
    htmlCheckBox <- tkcheckbutton(opcionesFrame, variable=creahtmlVariable)
    asocFrame <- tkframe(top, borderwidth=2, relief="groove")
    CovVariable <- tclVar("0")
    CovCheckBox <- tkcheckbutton(asocFrame, variable=CovVariable)
    pearsonVariable <- tclVar("0")
    pearsonCheckBox <- tkcheckbutton(asocFrame, variable=pearsonVariable)
    spearmanVariable <- tclVar("0")
    spearmanCheckBox <- tkcheckbutton(asocFrame, variable=spearmanVariable)
    kendallVariable <- tclVar("0")
    kendallCheckBox <- tkcheckbutton(asocFrame, variable=kendallVariable)
    determVariable <- tclVar("0")
    determCheckBox <- tkcheckbutton(asocFrame, variable=determVariable)
    onOK <- function(){
        var1 <- getSelection(variable1Var)
        var2 <- getSelection(variable2Var)
        if (length(var1) == 0 || length(var2) == 0){
            errorCondition(recall=bivariante.numericas, message=gettextRcmdr("Debe seleccionar dos variables."))
            return()
            }
        if (var1 == var2) {
            errorCondition(recall=bivariante.numericas, message=gettextRcmdr("Debe seleccionar dos variables distintas."))
            return()
            }
        Covval <- tclvalue(CovVariable)
        pearsonval <- tclvalue(pearsonVariable)
        spearmanval <- tclvalue(spearmanVariable)
        kendallval <- tclvalue(kendallVariable)   
        determval <- tclvalue(determVariable)
        subconjunto <- tclvalue(subsetVariable)
        subconjunto <- if (trim.blanks(subconjunto) == gettextRcmdr("<all valid cases>")) ""
            else paste(", subset=", subconjunto, sep="")
        .BaseDatosActiva <- paste("subset(",ActiveDataSet(),subconjunto,")",sep="")
        echocodigo <- tclvalue(echocodigoVariable)
        selec <- as.numeric(Covval) + as.numeric(pearsonval) + as.numeric(spearmanval) +
                 as.numeric(kendallval) + as.numeric(determval)*(as.numeric(pearsonval) + 
                 as.numeric(spearmanval) +as.numeric(kendallval))
        if (selec == 0){
          errorCondition(recall=bivariante.numericas, 
          message=gettextRcmdr("Debe escoger algun indicador."))
          return()
        }
        creahtml <- tclvalue(creahtmlVariable)
        if (creahtml == 1)
        {
         require(R2HTML)
         if (!file.exists("Informe de Resultados.html"))
           .archivo <- HTMLInitFile(file.path(getwd()),
                       "Informe de Resultados", BackGroundColor="#FFFFCC")
         else
           .archivo <- file.path(getwd(), "Informe de Resultados.html")
         if (selec > 0)
         {
           numfilas <- selec
           instruccion <- paste(".TablaCoefAsoc <- as.data.frame(matrix(nrow=",numfilas,",ncol=1))")
           justDoIt(instruccion)
           colnames(.TablaCoefAsoc) <- "Valores"
         }
         titulo <- paste("Descripcion bivariante de datos cuantitativos: ",var1, 
                   " y ", var2, sep="")
         HTML(as.title(titulo),file=.archivo)
        }
        closeDialog()
        i <- 0
        if (Covval == 1)
        {
          instruccion <- paste("covariancia <- cov(",.BaseDatosActiva,"$",var1,",",
                         .BaseDatosActiva,"$",var2,",use='na.or.complete')",sep="")
          if (echocodigo == 1) logger(instruccion)
          justDoIt(instruccion)
          doItAndPrint(paste("covariancia # Coeficiente de covariancia entre ",
                             var1," y ",var2,sep=""))
          if (echocodigo == 1) logger("remove(covariancia)")
          if (creahtml == 1)
          {
            i <- i+1
            rownames(.TablaCoefAsoc)[i] <- "Covariancia"
            .TablaCoefAsoc[i,1] <- covariancia
          }
          remove(covariancia, envir=.GlobalEnv)
        }
        if (pearsonval == 1)
        {
          instruccion2 <- paste("correlacion <- cor(",.BaseDatosActiva,"$",var1,",",
                         .BaseDatosActiva,"$",var2,",method='pearson',use='na.or.complete')",sep="")
          if (echocodigo == 1) logger(instruccion2)
          justDoIt(instruccion2)
          doItAndPrint(paste("correlacion # Correlacion de Pearson entre ",
                             var1," y ",var2,sep=""))
          if (echocodigo == 1) logger("remove(correlacion)")
          if (creahtml == 1)
          {
            i <- i+1
            rownames(.TablaCoefAsoc)[i] <- "Correlacion de Pearson"
            .TablaCoefAsoc[i,1] <- correlacion
          }
          if (determval == 1)
          {
            instruccion <- "R2pearson <- correlacion^2"
            justDoIt(instruccion)
            if (echocodigo == 1) logger(instruccion)
            doItAndPrint(paste("R2pearson # Coef. determinacion (Pearson) entre ",
            var1," y ",var2,sep=""))
            if (creahtml == 1)
            {
              i <- i+1
              rownames(.TablaCoefAsoc)[i] <- "R cuadrado Pearson"
              .TablaCoefAsoc[i,1] <- R2pearson
            }
            if (echocodigo == 1) logger("remove(R2pearson)")
            remove(R2pearson, envir=.GlobalEnv)
          }
          remove(correlacion, envir=.GlobalEnv)
        }
        if (spearmanval == 1)
        {
          instruccion3 <- paste("correlacion <- cor(",.BaseDatosActiva,"$",var1,",",
                         .BaseDatosActiva,"$",var2,",method='spearman',use='na.or.complete')",sep="")
          if (echocodigo == 1) logger(instruccion3)
          justDoIt(instruccion3)
          doItAndPrint(paste("correlacion # Correlacion de Spearman entre ",
                             var1," y ",var2,sep=""))
          if (echocodigo == 1) logger("remove(correlacion)")
          if (creahtml == 1)
          {
            i <- i+1
            rownames(.TablaCoefAsoc)[i] <- "Correlacion de Spearman"
            .TablaCoefAsoc[i,1] <- correlacion
          }
          if (determval == 1)
          {
            instruccion2 <- "R2spearman <- correlacion^2"
            justDoIt(instruccion2)
            if (echocodigo == 1) logger(instruccion2)
            doItAndPrint(paste("R2spearman # Coef. determinacion (Spearman) entre ",
            var1," y ",var2,sep=""))
            if (creahtml == 1)
            {
              i <- i+1
              rownames(.TablaCoefAsoc)[i] <- "R cuadrado Spearman"
              .TablaCoefAsoc[i,1] <- R2spearman
            }
            if (echocodigo == 1) logger("remove(R2spearman)")
            remove(R2spearman, envir=.GlobalEnv)
          }
          remove(correlacion, envir=.GlobalEnv)
        }
        if (kendallval == 1)
        {
          instruccion4 <- paste("correlacion <- cor(",.BaseDatosActiva,"$",var1,",",
                         .BaseDatosActiva,"$",var2,",method='kendall',use='na.or.complete')",sep="")
          if (echocodigo == 1) logger(instruccion4)
          justDoIt(instruccion4)
          doItAndPrint(paste("correlacion # Correlacion de Kendall entre ",
                             var1," y ",var2,sep=""))
          if (echocodigo == 1) logger("remove(correlacion)")
          if (creahtml == 1)
          {
            i <- i+1
            rownames(.TablaCoefAsoc)[i] <- "Correlacion de Kendall"
            .TablaCoefAsoc[i,1] <- correlacion
          }
          if (determval == 1)
          {
            instruccion3 <- "R2kendall <- correlacion^2"
            justDoIt(instruccion3)
            if (echocodigo == 1) logger(instruccion3)
            doItAndPrint(paste("R2kendall # Coef. determinacion (Kendall) entre ",
            var1," y ",var2,sep=""))
            if (creahtml == 1)
            {
              i <- i+1
              rownames(.TablaCoefAsoc)[i] <- "R cuadrado Kendall"
              .TablaCoefAsoc[i,1] <- R2kendall
            }
            if (echocodigo == 1) logger("remove(R2kendall)")
            remove(R2kendall, envir=.GlobalEnv)
          }
          remove(correlacion, envir=.GlobalEnv)
        }
        tkfocus(CommanderWindow())
        if (creahtml == 1)
        {
            HTML("Coeficientes de Asociacion", file=.archivo)
            HTML(.TablaCoefAsoc, file=.archivo)
            .TablaCoefAsoc <- round(.TablaCoefAsoc,3)
            remove(.TablaCoefAsoc, envir=.GlobalEnv)
            HTMLhr(file = .archivo)
        }
    }
    OKCancelHelp(helpSubject="cov")
    tkgrid(labelRcmdr(asocFrame,
    text=gettextRcmdr("Coeficientes de Asociacion"), fg="blue"), columnspan=6, sticky="w")
    tkgrid(labelRcmdr(asocFrame,text=gettextRcmdr("Coeficiente de covariancia ")), 
    CovCheckBox, sticky="w")
    tkgrid(labelRcmdr(asocFrame,text=gettextRcmdr("Coeficiente de correlacion de Pearson ")), 
    pearsonCheckBox, sticky="w")
    tkgrid(labelRcmdr(asocFrame,text=gettextRcmdr("Coeficiente de correlacion de Spearman ")), 
    spearmanCheckBox, sticky="w")
    tkgrid(labelRcmdr(asocFrame,text=gettextRcmdr("Coeficiente de correlacion de Kendall ")), 
    kendallCheckBox, sticky="w")
    tkgrid(labelRcmdr(asocFrame,text=gettextRcmdr("Coeficiente de determinacion ")), 
    determCheckBox, sticky="w")
    tkgrid(labelRcmdr(opcionesFrame,
    text=gettextRcmdr("Opciones"), fg="blue"), sticky="w")
    tkgrid(labelRcmdr(opcionesFrame, 
    text=gettextRcmdr("Mostrar en pantalla el codigo de R ejecutado ")), 
    echoCheckBox, sticky="w")
    tkgrid(labelRcmdr(opcionesFrame,
    text=gettextRcmdr("Generar informe de resultados ")),
    htmlCheckBox,sticky="w")
    tkgrid(getFrame(variable1Var), labelRcmdr(variablesFrame, text="    "),
    getFrame(variable2Var), sticky="nw")
    tkgrid(variablesFrame, sticky="w")
    tkgrid(asocFrame, sticky="w")
    tkgrid(subsetFrame, sticky="w")
    tkgrid(opcionesFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=6, columns=1)
    }

dispersion.numericas <- function(){
    initializeDialog(title=gettextRcmdr("Diagrama de dispersion para variables cuantitativas"))
    opcionesFrame <- tkframe(top)
    variablesFrame <- tkframe(top)
    variable1Var <- variableListBox(variablesFrame, Numeric(), title=gettextRcmdr("Variable x (escoja una)"))
    variable2Var <- variableListBox(variablesFrame, Numeric(), title=gettextRcmdr("Variable y (escoja una)"))
    subsetBox()
    echocodigoVariable <- tclVar("0")
    echoCheckBox <- tkcheckbutton(opcionesFrame, variable=echocodigoVariable)
    creahtmlVariable <- tclVar("0")
    htmlCheckBox <- tkcheckbutton(opcionesFrame, variable=creahtmlVariable)
    onOK <- function(){
        var1 <- getSelection(variable1Var)
        var2 <- getSelection(variable2Var)
        if (length(var1) == 0 || length(var2) == 0){
            errorCondition(recall=dispersion.numericas, message=gettextRcmdr("Debe seleccionar dos variables."))
            return()
            }
        if (var1 == var2) {
            errorCondition(recall=dispersion.numericas, message=gettextRcmdr("Debe seleccionar dos variables distintas."))
            return()
            }
        echocodigo <- tclvalue(echocodigoVariable)
        creahtml <- tclvalue(creahtmlVariable)
        if (creahtml == 1)
        {
         require(R2HTML)
         if (!file.exists("Informe de Resultados.html"))
           .archivo <- HTMLInitFile(file.path(getwd()),
                       "Informe de Resultados", BackGroundColor="#FFFFCC")
         else
           .archivo <- file.path(getwd(), "Informe de Resultados.html")
         titulo <- paste("Diagrama de puntos para datos cuantitativos: ",var1, 
                   " y ", var2, sep="")
         HTML(as.title(titulo),file=.archivo)
        }
        subconjunto <- tclvalue(subsetVariable)
        subconjunto <- if (trim.blanks(subconjunto) == gettextRcmdr("<all valid cases>")) ""
            else paste(", subset=", subconjunto, sep="")
        .BaseDatosActiva <- paste("subset(",ActiveDataSet(),subconjunto,")",sep="")
        instruccion <- "def.par <- par(no.readonly = TRUE)"
        justDoIt(instruccion)
        instruccion2 <- paste("xhist <- hist(",.BaseDatosActiva,"$",var1,",plot=FALSE)",sep="")
        justDoIt(instruccion2)
        instruccion3 <- paste("yhist <- hist(",.BaseDatosActiva,"$",var2,",plot=FALSE)",sep="")
        justDoIt(instruccion3)
        instruccion4 <- "sup <- max(c(xhist$counts,yhist$counts))"
        justDoIt(instruccion4)      
        instruccion5 <- paste("rangox <- c(min(",.BaseDatosActiva,"$",var1,",na.rm=TRUE),
        max(",.BaseDatosActiva,"$",var1,",na.rm=TRUE))",sep="")
        justDoIt(instruccion5)
        instruccion6 <- paste("rangoy <- c(min(",.BaseDatosActiva,"$",var2,",na.rm=TRUE),
        max(",.BaseDatosActiva,"$",var2,",na.rm=TRUE))",sep="")
        justDoIt(instruccion6)
        instruccion7 <- "nf <- layout(matrix(c(2,0,1,3),2,2,byrow=TRUE), c(3,1), c(1,3), TRUE)"
        justDoIt(instruccion7)
        instruccion8 <- "par(mar=c(5,4,3,3))" 
        instruccion9 <-paste("plot(",.BaseDatosActiva,"$",var1,",",.BaseDatosActiva,
        "$",var2,",xlim=rangox,ylim=rangoy,xlab='",var1,"',ylab='",var2,"')",sep="") 
        instruccion10 <- "box()"
        instruccion11 <- "par(mar=c(0,3,1,1))" 
        instruccion12 <- "barplot(xhist$counts,axes=FALSE,ylim=c(0, sup),space=0,col='grey')" 
        instruccion13 <- paste("title('Diagrama de dispersion para ",var1," y ",var2,"')",sep="")
        instruccion14 <- "par(mar=c(3,0,1,1))" 
        instruccion15 <- "barplot(yhist$counts,axes=FALSE,xlim=c(0, sup),space=0,
        col='grey',horiz=TRUE)" 
        instruccion16 <- "par(def.par)"
        if (echocodigo == 1)
        {
          logger(instruccion)
          logger(instruccion2)
          logger(instruccion3)
          logger(instruccion4)
          logger(instruccion5)
          logger(instruccion6)
          doItAndPrint(instruccion7)
          doItAndPrint(instruccion8)
          doItAndPrint(instruccion9)
          doItAndPrint(instruccion10)
          doItAndPrint(instruccion11)
          doItAndPrint(instruccion12)
          doItAndPrint(instruccion13)
          doItAndPrint(instruccion14)
          doItAndPrint(instruccion15)
          doItAndPrint(instruccion16)
        }
        else
        {
          justDoIt(instruccion7)  
          justDoIt(instruccion8)
          justDoIt(instruccion9)
          justDoIt(instruccion10)
          justDoIt(instruccion11)
          justDoIt(instruccion12)
          justDoIt(instruccion13)
          justDoIt(instruccion14)
          justDoIt(instruccion15)
          justDoIt(instruccion16)
        }
        if (creahtml == 1)
        {
          nombre.archivo <- paste("DiagramaDispersionR",gsub(":","",substr(Sys.time(),12,19)),
          ".jpg",sep="")
          dev.print(jpeg, filename=paste(getwd(),"/",nombre.archivo,sep=""),
          width=500, height=500)
          HTMLInsertGraph(nombre.archivo,file=.archivo,append=TRUE)
          HTMLhr(file = .archivo)
        }
        if (echocodigo == 1) logger("remove(list=c('def.par','rangox','rangoy','nf',
        'xhist','yhist'))")
        remove(list=c('def.par','sup','rangox','rangoy','xhist','yhist','nf'), envir=.GlobalEnv)
        closeDialog()
        tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject="plot")
    tkgrid(labelRcmdr(opcionesFrame,
    text=gettextRcmdr("Opciones"), fg="blue"), sticky="w")
    tkgrid(labelRcmdr(opcionesFrame, 
    text=gettextRcmdr("Mostrar en pantalla el codigo de R ejecutado ")), 
    echoCheckBox, sticky="w")
    tkgrid(labelRcmdr(opcionesFrame,
    text=gettextRcmdr("Generar informe de resultados ")),
    htmlCheckBox,sticky="w")
    tkgrid(getFrame(variable1Var), labelRcmdr(variablesFrame, text="    "),
    getFrame(variable2Var), sticky="nw")
    tkgrid(variablesFrame, sticky="w")
    tkgrid(subsetFrame, sticky="w")
    tkgrid(opcionesFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=6, columns=1)
    }

# Alguna funciones necesarias para llevar a cabo el test de Yuen-Welch, extraidas del paquete WRS #

winvar <- function(x,tr=.2,na.rm=FALSE){
    if(na.rm)x<-x[!is.na(x)]
    y<-sort(x)
    n<-length(x)
    ibot<-floor(tr*n)+1
    itop<-length(x)-ibot+1
    xbot<-y[ibot]
    xtop<-y[itop]
    y<-ifelse(y<=xbot,xbot,y)
    y<-ifelse(y>=xtop,xtop,y)
    winvar<-var(y)
    winvar
}

yuen.test <- function(x,y,tr=.2,alpha=.05){
    if(tr==.5)stop("No se puede utilizar tr=0.5")
    if(tr>.25)print("Aviso: con tr>.25 el control sobre el error tipo I puede ser insuficiente")
    x<-x[!is.na(x)]
    y<-y[!is.na(y)]
    h1<-length(x)-2*floor(tr*length(x))
    h2<-length(y)-2*floor(tr*length(y))
    q1<-(length(x)-1)*winvar(x,tr)/(h1*(h1-1))
    q2<-(length(y)-1)*winvar(y,tr)/(h2*(h2-1))
    df<-(q1+q2)^2/((q1^2/(h1-1))+(q2^2/(h2-1)))
    crit<-qt(1-alpha/2,df)
    dif<-mean(x,tr)-mean(y,tr)
    low<-dif-crit*sqrt(q1+q2)
    up<-dif+crit*sqrt(q1+q2)
    test<-abs(dif/sqrt(q1+q2))
    pval<-2*(1-pt(test,df))
    res <- list(ci=c(low,up),p.value=pval,dif=dif,se=sqrt(q1+q2),
           teststat=test,crit=crit,gl=df,ic=1-alpha,rec=tr)
    class(res)<-"yuen"
    res
}

print.yuen <- function (x,digits = max(4,getOption("digits") - 4),...)
{
    cat("\n")
    cat("\t",'Prueba t de Yuen-Welch',"\n")
    cat("\n")
    cat(paste("T de Yuen = ",round(x$teststat,4),", gl = ",round(x$gl,2),", valor p = ",
    round(x$p.value,4),sep=""),"\n")
    cat("Hipotesis Alternativa: Diferencia entre medias recortadas distinta de 0","\n")
    cat(paste("Intervalo de confianza ",x$ic*100,"%: ",sep=""),"\n")
    cat("\t",round(x$ci,4),"\n")
    cat(paste("Estimacion puntual de la diferencia de medias recortadas al ",x$rec*100,"%: ",sep=""),"\n")
    cat("\t",round(x$dif,4),"\n")  
    cat("\n\n")
    invisible(x)  
}

yuenWelch.tTest <- function(){
    initializeDialog(title=gettextRcmdr("Prueba t de Yuen-Welch"))
    opcionesFrame <- tkframe(top)
    variablesFrame <- tkframe(top)
    grupoVar <- variableListBox(variablesFrame, TwoLevelFactors(), title=gettextRcmdr("Grupos (elija uno)"))
    variableVar <- variableListBox(variablesFrame, Numeric(), title=gettextRcmdr("Variable de respuesta (elija una)"))
    trimFrame <- tkframe(top)
    trimVariable <- tclVar(gettextRcmdr("0.2"))
    trimField <- ttkentry(trimFrame, width="8", textvariable=trimVariable)
    confianzaFrame <- tkframe(top)
    nivelconf <- tclVar(".95")
    confianzaField <- ttkentry(confianzaFrame, width="6", textvariable=nivelconf)
    echocodigoVariable <- tclVar("0")
    echoCheckBox <- tkcheckbutton(opcionesFrame, variable=echocodigoVariable)
    creahtmlVariable <- tclVar("0")
    htmlCheckBox <- tkcheckbutton(opcionesFrame, variable=creahtmlVariable)
    onOK <- function(){
        grupo <- getSelection(grupoVar)
        if (length(grupo) == 0) {
            errorCondition(recall=yuenWelch.tTest, message=gettextRcmdr("Debe seleccionar una variable de agrupacion."))
            return()
            }
        varresp <- getSelection(variableVar)
        if (length(varresp) == 0) {
            errorCondition(recall=yuenWelch.tTest, message=gettextRcmdr("Debe seleccionar una variable de respuesta."))
            return()
            }
        echocodigo <- tclvalue(echocodigoVariable)
        creahtml <- tclvalue(creahtmlVariable)
        rec <- as.numeric(tclvalue(trimVariable))
        if (rec > 1 | rec < 0)
        {
          rec <- 0.2
          Message(message=gettextRcmdr("Proporcion de recorte invalida, se utilizara valor por defecto."),
          type="warning")              
        }
        ic <- as.numeric(tclvalue(nivelconf))
        if ( ic < .0 || ic > 1. || !is.numeric(ic) )
        {
          ic <- 0.95
          Message(message=gettextRcmdr("Nivel de confianza invalido, se utilizara valor por defecto."),
          type="warning")              
        }
        if (creahtml == 1)
        {
         require(R2HTML)
         if (!file.exists("Informe de Resultados.html"))
           .archivo <- HTMLInitFile(file.path(getwd()),
                       "Informe de Resultados", BackGroundColor="#FFFFCC")
         else
           .archivo <- file.path(getwd(), "Informe de Resultados.html")
         titulo <- paste("Prueba t de Yuen-Welch: ",varresp, 
                   " por ", grupo, sep="")
         HTML(as.title(titulo),file=.archivo)
        }
        nivel <- tclvalue(nivelconf)
        closeDialog()
        instruccion1 <- paste("x <-", ActiveDataSet(),"$",varresp,"[",ActiveDataSet(),"$",grupo,
                        "==levels(",ActiveDataSet(),"$",grupo,")[1]]",sep="")
        justDoIt(instruccion1)
        instruccion2 <- paste("y <- ", ActiveDataSet(),"$",varresp,"[",ActiveDataSet(),"$",grupo,
                        "==levels(",ActiveDataSet(),"$",grupo,")[2]]",sep="")
        justDoIt(instruccion2)
        instruccion3 <- paste("ywttest <- yuen.test(x,y,tr=",rec,",alpha=1-",ic,")",sep="")
        justDoIt(instruccion3)   
        if (echocodigo == 1)
        {
          logger(paste("x <-", instruccion1))
          logger(paste("y <-", instruccion2))
          logger(paste("ywttest <-", instruccion3))
        }
        doItAndPrint(paste("ywttest # Prueba t de Yuen-Welch para ", varresp," segun ",grupo,sep=""))
        if (creahtml == 1)
        {

          HTML('Prueba t de Yuen-Welch', file=.archivo)
          HTML(paste("datos: ",ActiveDataSet(),"$",varresp," por ",ActiveDataSet(),"$",grupo,sep=""), file=.archivo)
          HTML(paste("T de Yuen = ",round(ywttest[[5]],4),", gl = ",round(ywttest[[7]],2),", valor p = ",
          round(ywttest[[2]],4),sep=""), file=.archivo)
          HTML("Hipotesis Alternativa: Diferencia entre medias recortadas distinta de 0", file=.archivo)
          HTML(paste("Intervalo de confianza ",ic*100,"%: ",sep=""), file=.archivo)
          HTML(round(ywttest[[1]],4), file=.archivo)
          HTML(paste("Estimacion puntual de la diferencia de medias recortadas al ",rec*100,"%: ",sep=""), file=.archivo)
          HTML(round(ywttest[[3]],4), file=.archivo)
          HTMLhr(file = .archivo)
        }
        if (echocodigo == 1) logger("remove(list=c('x','y','ywttest'))")
        remove(list=c('x','y','ywttest'), envir=.GlobalEnv)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="t.test")
    tkgrid(labelRcmdr(opcionesFrame,
    text=gettextRcmdr("Opciones"), fg="blue"), sticky="w")
    tkgrid(labelRcmdr(opcionesFrame, 
    text=gettextRcmdr("Mostrar en pantalla el codigo de R ejecutado ")), 
    echoCheckBox, sticky="w")
    tkgrid(labelRcmdr(opcionesFrame,
    text=gettextRcmdr("Generar informe de resultados ")),
    htmlCheckBox,sticky="w")
    tkgrid(getFrame(grupoVar), labelRcmdr(variablesFrame, text="    "),
    getFrame(variableVar), sticky="nw")
    tkgrid(variablesFrame, sticky="w")
    groupsLabel(groupsBox=grupoVar, columnspan=2)
    tkgrid(labelRcmdr(trimFrame,
    text=gettextRcmdr("Proporcion datos recortados = ")),
    trimField, sticky="w")
    tkgrid(labelRcmdr(confianzaFrame,
    text=gettextRcmdr("Nivel de confianza = ")),
    confianzaField, sticky="w")
    tkgrid(trimFrame, sticky="w")
    tkgrid(confianzaFrame, sticky="w")
    tkgrid(opcionesFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=8, columns=1)
    }

# Funcion adaptada de twoSampleWilcoxonTest de J. Fox #

U.Mann.Whitney <- function(){
    initializeDialog(title=gettextRcmdr("Prueba U de Mann-Whitney"))
    opcionesFrame <- tkframe(top,width=90)
    variablesFrame <- tkframe(top,width=60)
    grupoVar <- variableListBox(variablesFrame, TwoLevelFactors(), title=gettextRcmdr("Grupos (elija uno)"))
    variableVar <- variableListBox(variablesFrame, Numeric(), title=gettextRcmdr("Variable de respuesta (elija una)"))
    radioButtons(opcionesFrame,name="alternativa", buttons=c("twosided", "less", "greater"), values=c("two.sided", "less", "greater"),
        labels=gettextRcmdr(c("Bilateral", "Diferencia < 0", "Diferencia > 0")), title=gettextRcmdr("Hipotesis alternativa"))
    radioButtons(opcionesFrame,name="prueba", buttons=c("default", "exact", "normal", "correct"), 
        labels=gettextRcmdr(c("Por defecto", "Exacta", "Aproximacion normal", "Aprox. normal con\ncorrecc. continuidad")), 
        title=gettextRcmdr("Tipo de Prueba"))
    confianzaFrame <- tkframe(opcionesFrame,width=15)
    nivelconf <- tclVar(".95")
    confianzaField <- ttkentry(confianzaFrame, width="6", textvariable=nivelconf)
    echocodigoVariable <- tclVar("0")
    echoCheckBox <- tkcheckbutton(opcionesFrame, variable=echocodigoVariable)
    creahtmlVariable <- tclVar("0")
    htmlCheckBox <- tkcheckbutton(opcionesFrame, variable=creahtmlVariable)
    onOK <- function(){
        grupo <- getSelection(grupoVar)
        if (length(grupo) == 0) {
            errorCondition(recall=U.Mann.Whitney, message=gettextRcmdr("Debe seleccionar una variable de agrupacion."))
            return()
            }
        varresp <- getSelection(variableVar)
        if (length(varresp) == 0) {
            errorCondition(recall=U.Mann.Whitney, message=gettextRcmdr("Debe seleccionar una variable de respuesta."))
            return()
            }
        echocodigo <- tclvalue(echocodigoVariable)
        creahtml <- tclvalue(creahtmlVariable)
        ic <- as.numeric(tclvalue(nivelconf))
        if ( ic < .0 || ic > 1. || !is.numeric(ic) )
        {
          ic <- 0.95
          Message(message=gettextRcmdr("Nivel de confianza invalido, se utilizara valor por defecto."),
          type="warning")              
        }
        if (creahtml == 1)
        {
         require(R2HTML)
         if (!file.exists("Informe de Resultados.html"))
           .archivo <- HTMLInitFile(file.path(getwd()),
                       "Informe de Resultados", BackGroundColor="#FFFFCC")
         else
           .archivo <- file.path(getwd(), "Informe de Resultados.html")
         titulo <- paste("Prueba U de Mann-Whitney: ",varresp, 
                   " por ", grupo, sep="")
         HTML(as.title(titulo),file=.archivo)
        }
        nivel <- tclvalue(nivelconf)
        alternativa <- as.character(tclvalue(alternativaVariable))
        prueba <- as.character(tclvalue(pruebaVariable))
        closeDialog()
        .baseDatosActiva <- ActiveDataSet()
        instruccion <- paste("meds <- tapply(",.baseDatosActiva,"$", varresp,",",.baseDatosActiva,"$",grupo,
        ", median, na.rm=TRUE)", sep="")
        justDoIt(instruccion)        
        if (echocodigo==1) logger(instruccion)
        doItAndPrint(paste("meds # Medianas para ", varresp," segun ",grupo,sep=""))        
        instruccion2 <- paste("g1 <- with(",.baseDatosActiva,",",varresp,"[as.numeric(factor(",grupo,"))==1])",sep="")
        justDoIt(instruccion2)
        if (echocodigo==1) logger(instruccion2)
        instruccion3 <- paste("g2 <- with(",.baseDatosActiva,",",varresp,"[as.numeric(factor(",grupo,"))==2])",sep="")
        justDoIt(instruccion3)
        if (echocodigo==1) logger(instruccion3)
        instruccion4 <- "rangos <- rank(c(na.omit(g1),na.omit(g2)))"
        justDoIt(instruccion4)
        if (echocodigo==1) logger(instruccion4)
        instruccion5 <- "R1 <- sum(rangos[1:length(g1)])"
        justDoIt(instruccion5)
        if (echocodigo==1) logger(instruccion5)
        instruccion6 <- "R2 <- sum(rangos[(length(na.omit(g1))+1):length(rangos)])"
        justDoIt(instruccion6)
        if (echocodigo==1) logger(instruccion6)
        instruccion7 <-paste("Rangos <- matrix(round(c(R1/length(na.omit(g1)),R1,R2/length(na.omit(g2)),R2),2),nrow=2,byrow=T,
        dimnames=list(levels(",.baseDatosActiva,"$",grupo,"),c('Rango promedio','Suma Rangos')))",sep="")
        justDoIt(instruccion7)
        if (echocodigo==1) logger(instruccion7)        
        doItAndPrint(paste("Rangos # Resumen descriptivo para ", varresp," segun ",grupo,sep=""))
        if (prueba == "default"){
            instruccion8 <- paste("UMW <- wilcox.test(", varresp, " ~ ", grupo, ', alternative="', 
            alternativa, '",conf.int=TRUE,conf.level=',ic,",data=", .baseDatosActiva, ")", sep="")
        justDoIt(instruccion8)        
        if (echocodigo==1) logger(instruccion2)
        doItAndPrint(paste("UMW # Prueba U de Mann-Whitney para ", varresp," segun ",grupo,sep=""))
            }
        else {
          instruccion8 <- paste("UMW <- wilcox.test(", varresp, " ~ ", grupo, ", alternative='", 
            alternativa, "', exact=", prueba=="exact", 
            ", correct=", prueba=="correct",", conf.int=TRUE, conf.level=",ic,", data=",
            .baseDatosActiva, ")", sep="")
        justDoIt(instruccion8)        
        if (echocodigo==1) logger(instruccion3)
        doItAndPrint(paste("UMW # Prueba U de Mann-Whitney para ", varresp," segun ",grupo,sep=""))
        }
        if (creahtml == 1)
        {
          HTML(paste('Medianas para ',varresp," segun ",grupo,sep=""), file=.archivo)
          .medstabla <- as.data.frame(matrix(meds,2,1,dimnames=list(names(meds),"Mediana")))
          HTML(.medstabla,file=.archivo)
          HTML('Resumen Descriptivo',file=.archivo)
          HTML(as.data.frame(Rangos),file=.archivo)
          HTML(paste('Prueba U de Mann-Whitney',if(prueba=='exact')" (exacta)",
          if(prueba=='correct')" (Normal con correccion por continuidad)",
          if(prueba=='normal')" (Normal)",sep=""),file=.archivo)
          HTML(paste("datos: ",ActiveDataSet(),"$",varresp," segun ",ActiveDataSet(),"$",grupo,sep=""), file=.archivo)
          if (UMW[[3]] > 0.0001) pval<-paste(" = ",round(UMW[[3]],4),sep="") else pval<- " < 0.0001"
          HTML(paste("U = ",round(UMW[[1]],4),", valor p",pval,sep=""), file=.archivo)
          HTML("Hipotesis Alternativa: Diferencia entre distribuciones distinta de 0", file=.archivo)
          HTML(paste("Intervalo de confianza ",ic*100,"%: ",sep=""), file=.archivo)
          HTML(round(UMW[[8]],4), file=.archivo)
          HTML(paste("Estimacion puntual de la diferencia de medianas: ",sep=""), file=.archivo)
          meddif <- UMW[[9]]
	        names(meddif) <- NULL
          HTML(round(meddif,4), file=.archivo)
          HTMLhr(file = .archivo)
        }
        if (echocodigo == 1) logger("remove(list=c('meds','g1', 'g2', 'rangos','R1', 'R2', 'Rangos','UMW'))")
        remove(list=c('meds', 'g1', 'g2', 'rangos','R1', 'R2', 'Rangos','UMW'), envir=.GlobalEnv)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="wilcox.test")
    tkgrid(getFrame(grupoVar), labelRcmdr(variablesFrame, text="    "),
    getFrame(variableVar), sticky="w")
    tkgrid(variablesFrame, sticky="nw")
    groupsLabel(groupsBox=grupoVar)
    tkgrid(labelRcmdr(confianzaFrame,
    text=gettextRcmdr("Nivel de confianza"), fg="blue"))
    tkgrid(confianzaField,sticky="w")
    tkgrid(alternativaFrame,confianzaFrame, sticky="nw")
    tkgrid(pruebaFrame, sticky="nw")
    tkgrid(labelRcmdr(opcionesFrame,
    text=gettextRcmdr("Opciones"), fg="blue"), sticky="nw")
    tkgrid(labelRcmdr(opcionesFrame, 
    text=gettextRcmdr("Mostrar en pantalla el codigo de R ejecutado ")), 
    echoCheckBox, sticky="nw")
    tkgrid(labelRcmdr(opcionesFrame,
    text=gettextRcmdr("Generar informe de resultados ")),
    htmlCheckBox,sticky="nw")
    tkgrid(opcionesFrame, sticky="nw")
    tkgrid(buttonsFrame, sticky="nw")
    dialogSuffix(rows=4, columns=1)
    }

pruebaT.Wilcoxon <- function(){
    initializeDialog(title=gettextRcmdr("Prueba T de Wilcoxon"))
    opcionesFrame <- tkframe(top,width=90)
    variablesFrame <- tkframe(top,width=60)
    variable1Var <- variableListBox(variablesFrame, Numeric(), title=gettextRcmdr("Variable 1 (escoja una)"))
    variable2Var <- variableListBox(variablesFrame, Numeric(), title=gettextRcmdr("Variable 2 (escoja una)"))
    radioButtons(opcionesFrame,name="alternativa", buttons=c("twosided", "less", "greater"), values=c("two.sided", "less", "greater"),
        labels=gettextRcmdr(c("Bilateral", "Diferencia < 0", "Diferencia > 0")), title=gettextRcmdr("Hipotesis alternativa"))
    radioButtons(opcionesFrame,name="prueba", buttons=c("default", "exact", "normal", "correct"), 
        labels=gettextRcmdr(c("Por defecto", "Exacta", "Aproximacion normal", "Aprox. normal con\ncorrecc. continuidad")), 
        title=gettextRcmdr("Tipo de Prueba"))
    confianzaFrame <- tkframe(opcionesFrame,width=15)
    nivelconf <- tclVar(".95")
    confianzaField <- ttkentry(confianzaFrame, width="6", textvariable=nivelconf)
    echocodigoVariable <- tclVar("0")
    echoCheckBox <- tkcheckbutton(opcionesFrame, variable=echocodigoVariable)
    creahtmlVariable <- tclVar("0")
    htmlCheckBox <- tkcheckbutton(opcionesFrame, variable=creahtmlVariable)
    onOK <- function(){
        var1 <- getSelection(variable1Var)
        var2 <- getSelection(variable2Var)
        if (length(var1) == 0 || length(var2) == 0){
            errorCondition(recall=pruebaT.Wilcoxon, message=gettextRcmdr("Debe seleccionar dos variables."))
            return()
            }
        if (var1 == var2) {
            errorCondition(recall=pruebaT.Wilcoxon, message=gettextRcmdr("Debe seleccionar dos variables distintas."))
            return()
            }
        echocodigo <- tclvalue(echocodigoVariable)
        creahtml <- tclvalue(creahtmlVariable)
        if (creahtml == 1)
        {
         require(R2HTML)
         if (!file.exists("Informe de Resultados.html"))
           .archivo <- HTMLInitFile(file.path(getwd()),
                       "Informe de Resultados", BackGroundColor="#FFFFCC")
         else
           .archivo <- file.path(getwd(), "Informe de Resultados.html")
         titulo <- paste("Prueba de Wilcoxon (muestras relacionadas) para: ",var1, 
                   " y ", var2, sep="")
         HTML(as.title(titulo),file=.archivo)
        }
        nivel <- tclvalue(nivelconf)
        alternativa <- as.character(tclvalue(alternativaVariable))
        prueba <- as.character(tclvalue(pruebaVariable))
        ic <- as.numeric(tclvalue(nivelconf))
        if ( ic < .0 || ic > 1. || !is.numeric(ic) )
        {
          ic <- 0.95
          Message(message=gettextRcmdr("Nivel de confianza invalido, se utilizara valor por defecto."),
          type="warning")              
        }
        closeDialog()
        .baseDatosActiva <- ActiveDataSet()
        instruccion <- paste("meddif <- median(", .baseDatosActiva, "$", var1, " - ", .baseDatosActiva, "$", var2, 
            ", na.rm=TRUE)", sep="")
        justDoIt(instruccion)        
        if (echocodigo==1) logger(instruccion)
        doItAndPrint(paste("meddif # Diferencia de medianas entre ", var1," y ",var2,sep=""))
        instruccion2 <- paste("m1 <- ",.baseDatosActiva,"$",var1,sep="")
        justDoIt(instruccion2)
        if (echocodigo==1) logger(instruccion2)
        instruccion3 <- paste("m2 <- ",.baseDatosActiva,"$",var2,sep="")
        justDoIt(instruccion3)
        if (echocodigo==1) logger(instruccion3)
        instruccion4 <- "difs <- na.omit((m1-m2)[(m1-m2)!=0])"
        justDoIt(instruccion4)
        if (echocodigo==1) logger(instruccion4)
        instruccion5 <- "Rpos <- sum(rank(abs(difs))[difs>0])"
        instruccion6 <- "difpos <- sum(difs>0)"
        justDoIt(instruccion5)
        justDoIt(instruccion6)        
        if (echocodigo==1) logger(instruccion5)
        if (echocodigo==1) logger(instruccion6)        
        instruccion7 <- "Rneg <- sum(rank(abs(difs))[difs<0])"
        instruccion8 <- "difneg <- sum(difs<0)"
        justDoIt(instruccion7)
        justDoIt(instruccion8) 
        if (echocodigo==1) logger(instruccion7)
        if (echocodigo==1) logger(instruccion8)
        instruccion9 <- "empates <- sum((m1-m2)==0)"
        justDoIt(instruccion9) 
        if (echocodigo==1) logger(instruccion9)
        instruccion10 <-"Rangos <- matrix(round(c(difpos,if (difpos !=0) Rpos/difpos else 0,
        Rpos,difneg,if (difneg !=0) Rneg/difneg else 0,Rneg,empates,0,0),2),nrow=3,byrow=T,
        dimnames=list(c('Rangos positivos','Rangos negativos','Empates'),c('Frecuencia','Rango promedio','Suma Rangos')))"
        justDoIt(instruccion10)
        if (echocodigo==1) logger(instruccion10)        
        doItAndPrint(paste("Rangos # Resumen descriptivo para ", var1," - ",var2,sep=""))
        if (prueba == "default"){
             instruccion11 <- paste("TWilcoxon <- wilcox.test(", .baseDatosActiva, "$", var1, ", ", 
                .baseDatosActiva, "$", var2,", alternative='", alternativa,
                "',conf.int=TRUE, conf.level=",ic,", paired=TRUE)", sep="")
        justDoIt(instruccion11)        
        if (echocodigo==1) logger(instruccion11)
        doItAndPrint(paste("TWilcoxon # Prueba T de Wilcoxon para ", var1," y ", var2,sep=""))
            }
        else if (prueba == "exact"){
            instruccion11 <- paste("TWilcoxon <- wilcox.test(", .baseDatosActiva, "$", var1, ", ", 
            .baseDatosActiva, "$", var2,", alternative='", alternativa,
            "',conf.int=TRUE, conf.level=",ic,", exact=TRUE,paired=TRUE)", sep="")
        justDoIt(instruccion11)        
        if (echocodigo==1) logger(instruccion11)
        doItAndPrint(paste("TWilcoxon # Prueba T de Wilcoxon para ", var1," y ", var2,sep=""))
        }
        else {
            instruccion11 <- paste("TWilcoxon <- wilcox.test(", .baseDatosActiva, "$", var1, ", ", 
            .baseDatosActiva, "$", var2,", alternative='", alternativa, 
            "', correct=", prueba=="correct",",conf.int=TRUE, conf.level=",ic,
            ", exact=FALSE, paired=TRUE)", sep="")
        justDoIt(instruccion11)        
        if (echocodigo==1) logger(instruccion11)
        doItAndPrint(paste("TWilcoxon # Prueba T de Wilcoxon para ", var1," y ", var2,sep=""))
        }
        if (creahtml == 1)
        {
          HTML(paste('Diferencia de medianas entre ',var1," y ",var2," : ",meddif,sep=""), file=.archivo)
          HTML(paste('Resumen Descriptivo para ', var1, ' - ', var2,sep=""),file=.archivo)
          HTML(as.data.frame(Rangos),file=.archivo)
          HTML(paste('Prueba T de Wilcoxon',if(prueba=='exact')" (exacta)",
          if(prueba=='correct')" (Normal con correccion por continuidad)",
          if(prueba=='normal')" (Normal)",sep=""),file=.archivo)
          HTML(paste("datos: ",ActiveDataSet(),"$",var1," y ",ActiveDataSet(),"$",var2,sep=""), file=.archivo)
          if (TWilcoxon[[3]] > 0.0001) pval<-paste(" = ",round(TWilcoxon[[3]],4),sep="") else pval<- " < 0.0001"
          HTML(paste("V = ",round(TWilcoxon[[1]],4),", valor p",pval,sep=""), file=.archivo)
          HTML("Hipotesis Alternativa: Diferencia entre distribuciones distinta de 0", file=.archivo)
          HTML(paste("Intervalo de confianza ",ic*100,"%: ",sep=""), file=.archivo)
          HTML(round(TWilcoxon[[8]],4), file=.archivo)
          HTML(paste("Estimacion puntual de la (pseudo)mediana de las diferencias: ",sep=""), file=.archivo)
          meddifest <- TWilcoxon[[9]]
          names(meddifest) <- NULL
          HTML(round(meddifest,4), file=.archivo)
          HTMLhr(file = .archivo)
        }
        if (echocodigo == 1) logger("remove(list=c('meddif','difs','Rpos','difpos',
        'Rneg','difneg','empates','Rangos','TWilcoxon'))")
        remove(list=c('meddif','difs','Rpos','difpos',
        'Rneg','difneg','empates','Rangos','TWilcoxon'), envir=.GlobalEnv)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="wilcox.test")
    tkgrid(getFrame(variable1Var), labelRcmdr(variablesFrame, text="    "),
    getFrame(variable2Var), sticky="w")
    tkgrid(variablesFrame, sticky="nw")
    tkgrid(labelRcmdr(confianzaFrame,
    text=gettextRcmdr("Nivel de confianza"), fg="blue"))
    tkgrid(confianzaField,sticky="w")
    tkgrid(alternativaFrame,confianzaFrame, sticky="nw")
    tkgrid(pruebaFrame, sticky="nw")
    tkgrid(labelRcmdr(opcionesFrame,
    text=gettextRcmdr("Opciones"), fg="blue"), sticky="nw")
    tkgrid(labelRcmdr(opcionesFrame, 
    text=gettextRcmdr("Mostrar en pantalla el codigo de R ejecutado ")), 
    echoCheckBox, sticky="nw")
    tkgrid(labelRcmdr(opcionesFrame,
    text=gettextRcmdr("Generar informe de resultados ")),
    htmlCheckBox,sticky="nw")
    tkgrid(opcionesFrame, sticky="nw")
    tkgrid(buttonsFrame, sticky="nw")
    dialogSuffix(rows=4, columns=1)
}

prueba.conformidad.forma <- function(){
    initializeDialog(title=gettextRcmdr("Prueba conformidad parametro de forma"))
    opcionesFrame <- tkframe(top,width=90)
    variablesFrame <- tkframe(top,width=60)
    variableVar <- variableListBox(variablesFrame, Numeric(), title=gettextRcmdr("Variables (escoja una)"))
    echocodigoVariable <- tclVar("0")
    echoCheckBox <- tkcheckbutton(opcionesFrame, variable=echocodigoVariable)
    creahtmlVariable <- tclVar("0")
    htmlCheckBox <- tkcheckbutton(opcionesFrame, variable=creahtmlVariable)
    onOK <- function(){
        var <- getSelection(variableVar)
        if (length(var) == 0) {
            errorCondition(recall=prueba.conformidad.forma, message=gettextRcmdr("Debe seleccionar una variable."))
            return()
            }
        echocodigo <- tclvalue(echocodigoVariable)
        creahtml <- tclvalue(creahtmlVariable)
        if (creahtml == 1)
        {
         require(R2HTML)
         if (!file.exists("Informe de Resultados.html"))
           .archivo <- HTMLInitFile(file.path(getwd()),
                       "Informe de Resultados", BackGroundColor="#FFFFCC")
         else
           .archivo <- file.path(getwd(), "Informe de Resultados.html")
         titulo <- paste("Prueba de conformidad respecto al parametro de forma para: ",var,
                         sep="")
         HTML(as.title(titulo),file=.archivo)
        }
        closeDialog()
        .baseDatosActiva <- ActiveDataSet()
        instruccion <- paste("n <- length(na.omit(",.baseDatosActiva,"$",var,"))",sep="")
        justDoIt(instruccion)
        if (echocodigo==1) logger(instruccion)
        instruccion2 <- paste("sesgo <- (n*sum((",.baseDatosActiva,"$",var,"-mean(",.baseDatosActiva,"$",var,
        ",na.rm=TRUE))^3,na.rm=TRUE))/((n-1)*(n-2))/(sd(",.baseDatosActiva,"$",var,",na.rm=TRUE)^3)",sep="")
        justDoIt(instruccion2)
        if (echocodigo==1) logger(instruccion2)
        instruccion3 <- "error.simetria <- sqrt((6*n*(n-1))/((n-2)*(n+1)*(n+3)))"
        justDoIt(instruccion3)
        if (echocodigo==1) logger(instruccion3)
        instruccion4 <- "sim.estandar <- sesgo/error.simetria"
        justDoIt(instruccion4)
        if (echocodigo==1) logger(instruccion4)
        if (sign(sim.estandar) > 0) instruccion5 <- "valor.p <- 2*pnorm(sim.estandar,lower.tail=FALSE)"
        else instruccion5 <- "valor.p <- 2*pnorm(sim.estandar)"
        justDoIt(instruccion5)
        if (echocodigo==1) logger(instruccion5)
        doItAndPrint(paste("sesgo # Coeficiente de simetria para ", var,sep=""))
        doItAndPrint(paste("error.simetria # Error tipico del coeficiente de simetria para ", var,sep=""))
        doItAndPrint(paste("sim.estandar # Coeficiente de simetria estandarizado para ", var,sep=""))
        doItAndPrint(paste("valor.p # Significacion estadistica (bilateral) asociada para ", var,sep=""))        
        if (creahtml == 1)
        {
          HTML('Prueba de Conformidad para el parametro de forma',file=.archivo)
          HTML(paste("datos: ",ActiveDataSet(),"$",var,sep=""), file=.archivo)
          if (valor.p > 0.0001) pval<-paste(" = ",round(valor.p,4),sep="") else pval<- " < 0.0001"
          HTML(paste("Coeficiente de simetria = ",round(sesgo,4),", Error tipico = ",round(error.simetria,4),
          sep=""),file=.archivo)
          HTML(paste("Coeficiente de simetria estandarizado = ",round(sim.estandar,4),", valor p",pval,sep=""),
          file=.archivo)
          HTML("Hipotesis Alternativa: Parametro beta distinto de 0", file=.archivo)
          HTMLhr(file = .archivo)
        }
        if (echocodigo == 1) logger("remove(list=c('n','sesgo','error.simetria','sim.estandar',
        'valor.p'))")
        remove(list=c('n','sesgo','error.simetria','sim.estandar','valor.p'), envir=.GlobalEnv)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="RcmdrPlugin.EACSPIR")
    tkgrid(getFrame(variableVar), labelRcmdr(variablesFrame, text="    "),sticky="w")
    tkgrid(variablesFrame, sticky="nw")
    tkgrid(labelRcmdr(opcionesFrame,
    text=gettextRcmdr("Opciones"), fg="blue"), sticky="nw")
    tkgrid(labelRcmdr(opcionesFrame, 
    text=gettextRcmdr("Mostrar en pantalla el codigo de R ejecutado ")), 
    echoCheckBox, sticky="nw")
    tkgrid(labelRcmdr(opcionesFrame,
    text=gettextRcmdr("Generar informe de resultados ")),
    htmlCheckBox,sticky="nw")
    tkgrid(opcionesFrame, sticky="nw")
    tkgrid(buttonsFrame, sticky="nw")
    dialogSuffix(rows=4, columns=1)
}

prueba.conformidad.apuntamiento <- function(){
    initializeDialog(title=gettextRcmdr("Prueba conformidad parametro de apuntamiento"))
    opcionesFrame <- tkframe(top,width=90)
    variablesFrame <- tkframe(top,width=60)
    variableVar <- variableListBox(variablesFrame, Numeric(), title=gettextRcmdr("Variables (escoja una)"))
    echocodigoVariable <- tclVar("0")
    echoCheckBox <- tkcheckbutton(opcionesFrame, variable=echocodigoVariable)
    creahtmlVariable <- tclVar("0")
    htmlCheckBox <- tkcheckbutton(opcionesFrame, variable=creahtmlVariable)
    onOK <- function(){
        var <- getSelection(variableVar)
        if (length(var) == 0) {
            errorCondition(recall=prueba.conformidad.apuntamiento, message=gettextRcmdr("Debe seleccionar una variable."))
            return()
            }
        echocodigo <- tclvalue(echocodigoVariable)
        creahtml <- tclvalue(creahtmlVariable)
        if (creahtml == 1)
        {
         require(R2HTML)
         if (!file.exists("Informe de Resultados.html"))
           .archivo <- HTMLInitFile(file.path(getwd()),
                       "Informe de Resultados", BackGroundColor="#FFFFCC")
         else
           .archivo <- file.path(getwd(), "Informe de Resultados.html")
         titulo <- paste("Prueba de conformidad respecto al parametro de apuntamiento para: ",var,
                         sep="")
         HTML(as.title(titulo),file=.archivo)
        }
        closeDialog()
        .baseDatosActiva <- ActiveDataSet()
        instruccion <- paste("n <- length(na.omit(",.baseDatosActiva,"$",var,"))",sep="")
        justDoIt(instruccion)
        if (echocodigo==1) logger(instruccion)
        instruccion2 <- paste("curtosis <-c(n*(n+1)*sum((",.baseDatosActiva,"$",var,"-mean(",.baseDatosActiva,"$",var,
        ",na.rm=TRUE))^4,na.rm=TRUE)/((n-1)*(n-2)*(n-3))-3*sum((",.baseDatosActiva,"$",var,"-mean(",.baseDatosActiva,"$",var,
        ",na.rm=TRUE))^2,na.rm=TRUE)^2/((n-2)*(n-3)))/(sd(",.baseDatosActiva,"$",var,",na.rm=TRUE)^4)",sep="")
        justDoIt(instruccion2)
        if (echocodigo==1) logger(instruccion2)
        instruccion3 <- "error.curtosis <- sqrt((24*n*(n-1)^2)/((n-3)*(n-2)*(n+3)*(n+5)))"
        justDoIt(instruccion3)
        if (echocodigo==1) logger(instruccion3)
        instruccion4 <- "curtosis.estandar <- curtosis/error.curtosis"
        justDoIt(instruccion4)
        if (echocodigo==1) logger(instruccion4)
        if (sign(curtosis.estandar) > 0) instruccion5 <- "valor.p <- 2*pnorm(curtosis.estandar,lower.tail=FALSE)"
        else instruccion5 <- "valor.p <- 2*pnorm(curtosis.estandar)"
        justDoIt(instruccion5)
        if (echocodigo==1) logger(instruccion5)
        doItAndPrint(paste("curtosis # Coeficiente de apuntamiento para ", var,sep=""))
        doItAndPrint(paste("error.curtosis # Error tipico del coeficiente de apuntamiento para ", var,sep=""))
        doItAndPrint(paste("curtosis.estandar # Coeficiente de apuntamiento estandarizado para ", var,sep=""))
        doItAndPrint(paste("valor.p # Significacion estadistica (bilateral) asociada para ", var,sep=""))        
        if (creahtml == 1)
        {
          HTML('Prueba de Conformidad para el parametro de apuntamiento',file=.archivo)
          HTML(paste("datos: ",ActiveDataSet(),"$",var,sep=""), file=.archivo)
          if (valor.p > 0.0001) pval<-paste(" = ",round(valor.p,4),sep="") else pval<- " < 0.0001"
          HTML(paste("Coeficiente de apuntamiento = ",round(curtosis,4),", Error tipico = ",round(error.curtosis,4),
          sep=""),file=.archivo)
          HTML(paste("Coeficiente de apuntamiento estandarizado = ",round(curtosis.estandar,4),", valor p",pval,sep=""),
          file=.archivo)
          HTML("Hipotesis Alternativa: Parametro gamma distinto de 0", file=.archivo)
          HTMLhr(file = .archivo)
        }
        if (echocodigo == 1) logger("remove(list=c('n','curtosis','error.curtosis','curtosis.estandar','valor.p'))")
        remove(list=c('n','curtosis','error.curtosis','curtosis.estandar','valor.p'), envir=.GlobalEnv)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="RcmdrPlugin.EACSPIR")
    tkgrid(getFrame(variableVar), labelRcmdr(variablesFrame, text="    "),sticky="w")
    tkgrid(variablesFrame, sticky="nw")
    tkgrid(labelRcmdr(opcionesFrame,
    text=gettextRcmdr("Opciones"), fg="blue"), sticky="nw")
    tkgrid(labelRcmdr(opcionesFrame, 
    text=gettextRcmdr("Mostrar en pantalla el codigo de R ejecutado ")), 
    echoCheckBox, sticky="nw")
    tkgrid(labelRcmdr(opcionesFrame,
    text=gettextRcmdr("Generar informe de resultados ")),
    htmlCheckBox,sticky="nw")
    tkgrid(opcionesFrame, sticky="nw")
    tkgrid(buttonsFrame, sticky="nw")
    dialogSuffix(rows=4, columns=1)
}

tam.muestra <-function(conf,e,est=c("proporcion","media"),inf=TRUE,N=NULL,pi=0.5,sigma=NULL){
    if ((conf >= 1) || (conf <= 0)) stop("Especifique el valor para el nivel de confianza");
    alfa <- 1-conf
    if (!is.numeric(e)) stop("Especifique el valor de precision");
    if ( (inf==FALSE) && !is.numeric(N) ) stop("Especifique el tamano de la poblacion")
    est <- match.arg(est)
    if (est == "proporcion"){
      if ((pi > 1) || (pi < 0)) stop("Especifique el valor para la proporcion poblacional");
      if (inf == TRUE) n <- qnorm(alfa/2)^2*pi*(1-pi)/e^2
      else n <- qnorm(alfa/2)^2*pi*(1-pi)*N/(e^2*(N-1)+qnorm(alfa/2)^2*pi*(1-pi))
      res <- list(parametro=est,prop=pi,precision=e,confianza=conf,muestra=n)
      class(res) <- "tmuestra"
    }
    if (est == "media"){
      if (!is.numeric(sigma)) stop("Especifique el valor para la desviacion tipica poblacional");
      if (inf == TRUE) n <- qnorm(alfa/2)^2*sigma^2/e^2
      else n <- qnorm(alfa/2)^2*sigma^2*N/(e^2*(N-1)+qnorm(alfa/2)^2*sigma^2)
      res <- list(parametro=est,desv=sigma,precision=e,confianza=conf,muestra=n)
      class(res) <- "tmuestra"
    }
    res
}

print.tmuestra <- function (x,digits = max(4,getOption("digits") - 4),...)
{
    if (x$parametro == 'proporcion')
    {
      cat("\n")
      cat("Parametro a estimar: ",x$parametro, " = ", x$prop, "\n")
      cat("Precision: ", x$precision, "\n")
      cat("Nivel de confianza: ", x$confianza, "\n")
      cat("Tamano de la muestra requerido: ", x$muestra, "observaciones", "\n")
    }  
    if (x$parametro == 'media')
    {
      cat("\n")      
      cat("Parametro a estimar: ",x$parametro, "\n")
      cat("Sigma = ", x$desv, "\n")
      cat("Precision: ", x$precision, "\n")
      cat("Nivel de confianza: ", x$confianza, "\n")
      cat("Tamano de la muestra requerido: ", x$muestra, "observaciones", "\n")
    }    
    cat("\n")
    invisible(x)  
}

determ.tam.proporcion <- function(){
    initializeDialog(title=gettextRcmdr("Determinacion tamano muestra: proporciones"))
    opcionesFrame <- tkframe(top,width=90)
    propFrame <- tkframe(top)
    propVariable <- tclVar(gettextRcmdr("0.5"))
    propField <- ttkentry(propFrame, width="8", textvariable=propVariable) 
    pobNFrame <- tkframe(top)
    pobNVariable <- tclVar(gettextRcmdr("<auto>"))
    pobNField <- ttkentry(pobNFrame, width="8", textvariable=pobNVariable)
    precisionFrame <- tkframe(top)
    precisionVariable <- tclVar(gettextRcmdr("0.05"))
    precisionField <- ttkentry(precisionFrame, width="8", textvariable=precisionVariable)
    confianzaFrame <- tkframe(top)
    nivelconf <- tclVar(".95")
    confianzaField <- ttkentry(confianzaFrame, width="6", textvariable=nivelconf)
    echocodigoVariable <- tclVar("0")
    echoCheckBox <- tkcheckbutton(opcionesFrame, variable=echocodigoVariable)
    creahtmlVariable <- tclVar("0")
    htmlCheckBox <- tkcheckbutton(opcionesFrame, variable=creahtmlVariable)
    onOK <- function(){
        proporcion <- as.numeric(tclvalue(propVariable))
        pobN <- tclvalue(pobNVariable)
        precision <- as.numeric(tclvalue(precisionVariable))
        echocodigo <- tclvalue(echocodigoVariable)
        confianza <- as.numeric(tclvalue(nivelconf))
        creahtml <- tclvalue(creahtmlVariable)
        if ((proporcion > 1) || (proporcion < 0)) {
            errorCondition(recall=determ.tam.proporcion,
            message=gettextRcmdr("Especifique un valor del parametro proporcion."))
            return()
            }
        if (!is.numeric(precision)) {
            precision <- 0.05
            Message(message=gettextRcmdr("Valor de precision invalido, se utilizara valor por defecto."))
            return()
            }
        if (pobN == gettextRcmdr("<auto>")) N <- NULL
        else { 
    N <- as.numeric(pobN)
          if ( !is.numeric(N)) {
              N <- NULL
              Message(message=gettextRcmdr("Tamano de la poblacion invalido, se utilizara valor por defecto."))
              return()
              }
	}
        if ( confianza < .0 || confianza > 1. || !is.numeric(confianza) )
        {
          confianza <- 0.95
          Message(message=gettextRcmdr("Nivel de confianza invalido, se utilizara valor por defecto."),
          type="warning")              
        }
        if (creahtml == 1)
        {
         require(R2HTML)
         if (!file.exists("Informe de Resultados.html"))
           .archivo <- HTMLInitFile(file.path(getwd()),
                       "Informe de Resultados", BackGroundColor="#FFFFCC")
         else
           .archivo <- file.path(getwd(), "Informe de Resultados.html")
         titulo <- "Determinacion del tamano de la muestra: proporciones"
         HTML(as.title(titulo),file=.archivo)
        }
        closeDialog()
        if (is.numeric(N))
        instruccion <- paste("tam.muestra(conf=",confianza,",e=",precision,",est='proporcion'",
        ",inf=FALSE,N=",N,",pi=",proporcion,")",sep="")        
        else
        instruccion <- paste("dtm <- tam.muestra(conf=",confianza,",e=",precision,",est='proporcion'",
        ",inf=TRUE,N=NULL,pi=",proporcion,")",sep="")                
        justDoIt(instruccion)
        if (echocodigo==1) logger(instruccion)
        doItAndPrint("dtm # Determinacion tamano de la muestra: proporcion ")        
        if (creahtml == 1)
        {
          HTML('Determinacion del tamano de la muestra: Caso proporcion',file=.archivo)
          HTML(dtm,file= .archivo)
          HTMLhr(file = .archivo)
        }
        if (echocodigo == 1) logger("remove(list=c('dtm'))")
        remove(list=c('dtm'), envir=.GlobalEnv)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="RcmdrPlugin.EACSPIR")
    tkgrid(labelRcmdr(opcionesFrame,
    text=gettextRcmdr("Opciones"), fg="blue"), sticky="nw")
    tkgrid(labelRcmdr(opcionesFrame, 
    text=gettextRcmdr("Mostrar en pantalla el codigo de R ejecutado ")), 
    echoCheckBox, sticky="nw")
    tkgrid(labelRcmdr(opcionesFrame,
    text=gettextRcmdr("Generar informe de resultados ")),
    htmlCheckBox,sticky="nw")
    tkgrid(labelRcmdr(propFrame,
    text=gettextRcmdr("Parametro proporcion = ")),
    propField, sticky="w")
    tkgrid(labelRcmdr(pobNFrame,
    text=gettextRcmdr("Tamano poblacion = ")),
    pobNField, sticky="w")
    tkgrid(labelRcmdr(precisionFrame,
    text=gettextRcmdr("Valor de precision = ")),
    precisionField, sticky="w")
    tkgrid(labelRcmdr(confianzaFrame,
    text=gettextRcmdr("Nivel de confianza = ")),
    confianzaField, sticky="w")
    tkgrid(propFrame, sticky="w")
    tkgrid(pobNFrame, sticky="w")
    tkgrid(precisionFrame, sticky="w")
    tkgrid(confianzaFrame, sticky="w")        
    tkgrid(opcionesFrame, sticky="nw")
    tkgrid(buttonsFrame, sticky="nw")
    dialogSuffix(rows=4, columns=1)  
}

determ.tam.media <- function(){
    initializeDialog(title=gettextRcmdr("Determinacion tamano muestra: medias"))
    opcionesFrame <- tkframe(top,width=90)
    sigmaFrame <- tkframe(top)
    sigmaVariable <- tclVar(gettextRcmdr("1.0"))
    sigmaField <- ttkentry(sigmaFrame, width="8", textvariable=sigmaVariable) 
    pobNFrame <- tkframe(top)
    pobNVariable <- tclVar(gettextRcmdr("<auto>"))
    pobNField <- ttkentry(pobNFrame, width="8", textvariable=pobNVariable)
    precisionFrame <- tkframe(top)
    precisionVariable <- tclVar(gettextRcmdr("0.10"))
    precisionField <- ttkentry(precisionFrame, width="8", textvariable=precisionVariable)
    confianzaFrame <- tkframe(top)
    nivelconf <- tclVar(".95")
    confianzaField <- ttkentry(confianzaFrame, width="6", textvariable=nivelconf)
    echocodigoVariable <- tclVar("0")
    echoCheckBox <- tkcheckbutton(opcionesFrame, variable=echocodigoVariable)
    creahtmlVariable <- tclVar("0")
    htmlCheckBox <- tkcheckbutton(opcionesFrame, variable=creahtmlVariable)
    onOK <- function(){
        sigma <- as.numeric(tclvalue(sigmaVariable))
        pobN <- tclvalue(pobNVariable)
        precision <- as.numeric(tclvalue(precisionVariable))
        echocodigo <- tclvalue(echocodigoVariable)
        confianza <- as.numeric(tclvalue(nivelconf))
        creahtml <- tclvalue(creahtmlVariable)
        if (!is.numeric(sigma)) {
            errorCondition(recall=determ.tam.media,
            message=gettextRcmdr("Especifique un valor del parametro sigma."))
            return()
            }
        if (!is.numeric(precision)) {
            precision <- 0.05
            Message(message=gettextRcmdr("Valor de precision invalido, se utilizara valor por defecto."))
            return()
            }
        if (pobN == gettextRcmdr("<auto>")) N <- NULL
        else { 
          N <- as.numeric(pobN)
          if ( !is.numeric(N)) {
              N <- NULL
              Message(message=gettextRcmdr("Tamano de la poblacion invalido, se utilizara valor por defecto."))
              return()
              }
        }
        if ( confianza < .0 || confianza > 1. || !is.numeric(confianza) )
        {
          confianza <- 0.95
          Message(message=gettextRcmdr("Nivel de confianza invalido, se utilizara valor por defecto."),
          type="warning")              
        }
        if (creahtml == 1)
        {
         require(R2HTML)
         if (!file.exists("Informe de Resultados.html"))
           .archivo <- HTMLInitFile(file.path(getwd()),
                       "Informe de Resultados", BackGroundColor="#FFFFCC")
         else
           .archivo <- file.path(getwd(), "Informe de Resultados.html")
         titulo <- "Determinacion del tamano de la muestra: medias"
         HTML(as.title(titulo),file=.archivo)
        }
        closeDialog()
        if (is.numeric(N))
        instruccion <- paste("dtm <- tam.muestra(conf=",confianza,",e=",precision,",est='media'",
        ",inf=FALSE,N=",N,",sigma=",sigma,")",sep="")        
        else
        instruccion <- paste("dtm <- tam.muestra(conf=",confianza,",e=",precision,",est='media'",
        ",inf=TRUE,N=NULL,sigma=",sigma,")",sep="")                
        justDoIt(instruccion)
        if (echocodigo==1) logger(instruccion)
        doItAndPrint("dtm # Determinacion tamano de la muestra: media ")        
        if (creahtml == 1)
        {
          HTML('Determinacion del tamano de la muestra: Caso media',file=.archivo)
          HTML(dtm,file= .archivo)
          HTMLhr(file = .archivo)
        }
        if (echocodigo == 1) logger("remove(list=c('dtm'))")
        remove(list=c('dtm'), envir=.GlobalEnv)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="RcmdrPlugin.EACSPIR")
    tkgrid(labelRcmdr(opcionesFrame,
    text=gettextRcmdr("Opciones"), fg="blue"), sticky="nw")
    tkgrid(labelRcmdr(opcionesFrame, 
    text=gettextRcmdr("Mostrar en pantalla el codigo de R ejecutado ")), 
    echoCheckBox, sticky="nw")
    tkgrid(labelRcmdr(opcionesFrame,
    text=gettextRcmdr("Generar informe de resultados ")),
    htmlCheckBox,sticky="nw")
    tkgrid(labelRcmdr(sigmaFrame,
    text=gettextRcmdr("Parametro sigma = ")),
    sigmaField, sticky="w")
    tkgrid(labelRcmdr(pobNFrame,
    text=gettextRcmdr("Tamano poblacion = ")),
    pobNField, sticky="w")
    tkgrid(labelRcmdr(precisionFrame,
    text=gettextRcmdr("Valor de precision = ")),
    precisionField, sticky="w")
    tkgrid(labelRcmdr(confianzaFrame,
    text=gettextRcmdr("Nivel de confianza = ")),
    confianzaField, sticky="w")
    tkgrid(sigmaFrame, sticky="w")
    tkgrid(pobNFrame, sticky="w")
    tkgrid(precisionFrame, sticky="w")
    tkgrid(confianzaFrame, sticky="w")        
    tkgrid(opcionesFrame, sticky="nw")
    tkgrid(buttonsFrame, sticky="nw")
    dialogSuffix(rows=4, columns=1)  
}

pruebas.normalidad <- function(){
    require(nortest)
    initializeDialog(title=gettextRcmdr("Pruebas de Ajuste Distribucion Normal"))
    opcionesFrame <- tkframe(top,width=90)
    variablesFrame <- tkframe(top,width=60)
    pruebasFrame <- tkframe(top,width=60)    
    variableVar <- variableListBox(variablesFrame, Numeric(), title=gettextRcmdr("Variables (escoja una)"))
    pearsonVariable <- tclVar("0")
    pearsonCheckBox <- tkcheckbutton(pruebasFrame, variable=pearsonVariable)
    ksVariable <- tclVar("0")
    ksCheckBox <- tkcheckbutton(pruebasFrame, variable=ksVariable)
    adVariable <- tclVar("0")
    adCheckBox <- tkcheckbutton(pruebasFrame, variable=adVariable)
    shapiroVariable <- tclVar("0")
    shapiroCheckBox <- tkcheckbutton(pruebasFrame, variable=shapiroVariable)
    graficosVariable <- tclVar("0")
    graficosCheckBox <- tkcheckbutton(pruebasFrame, variable=graficosVariable)    
    echocodigoVariable <- tclVar("0")
    echoCheckBox <- tkcheckbutton(opcionesFrame, variable=echocodigoVariable)
    creahtmlVariable <- tclVar("0")
    htmlCheckBox <- tkcheckbutton(opcionesFrame, variable=creahtmlVariable)
    groupsBox(recall=pruebas.normalidad, label=gettextRcmdr("Pruebas segun:"),
              initialLabel=gettextRcmdr("Pruebas segun grupos"))    
    onOK <- function(){
        var <- getSelection(variableVar)
        if (length(var) == 0) {
            errorCondition(recall=pruebas.normalidad, message=gettextRcmdr("Debe seleccionar una variable."))
            return()
            }
        echocodigo <- tclvalue(echocodigoVariable)
        creahtml <- tclvalue(creahtmlVariable)
        pearsonval <- tclvalue(pearsonVariable)
        adval <- tclvalue(adVariable)
        ksval <-tclvalue(ksVariable)
        shapiroval <- tclvalue(shapiroVariable)
        grafval <- tclvalue(graficosVariable)
        seleccion <- as.numeric(pearsonval) + as.numeric(adval) + 
        as.numeric(ksval) + as.numeric(shapiroval) + as.numeric(grafval)
        if (seleccion == 0){
          errorCondition(recall=pruebas.normalidad, 
          message=gettextRcmdr("Debe escoger algun indicador."))
          return()
        }        
        if (creahtml == 1)
        {
         require(R2HTML)
         if (!file.exists("Informe de Resultados.html"))
           .archivo <- HTMLInitFile(file.path(getwd()),
                       "Informe de Resultados", BackGroundColor="#FFFFCC")
         else
           .archivo <- file.path(getwd(), "Informe de Resultados.html")
         titulo <- paste("Pruebas de ajuste a la distribucion normal para: ",var,
                         sep="")
         HTML(as.title(titulo),file=.archivo)
        }
        closeDialog()
        .baseDatosActiva <- ActiveDataSet()           
          if (.groups != FALSE) {
            grupos <- paste(.baseDatosActiva, "$", .groups, sep="")
          }        
          if (pearsonval == 1)
          { 
            if (.groups != FALSE)
            instruccion <- paste(".prueba.pearson <- by(",.baseDatosActiva,"$",var,",",grupos,",pearson.test)",sep="")
            else
              instruccion <- paste(".prueba.pearson <- pearson.test(",.baseDatosActiva,"$",var,")",sep="")
            if (echocodigo == 1)
            {
              logger(instruccion)
            }       
            justDoIt(instruccion)
            if (.groups != FALSE){                   
                for (i in 1:dim(.prueba.pearson))
                 .prueba.pearson[[i]]$data.name <- paste(.baseDatosActiva,"$",var,sep="")
            }
            doItAndPrint(paste(".prueba.pearson  # Prueba Ji-Cuadrado de Pearson para ", var))
            if (creahtml == 1)
            { 
              if (.groups == FALSE){
                HTML('Prueba de ajuste a la normal: Ji-Cuadrado de Pearson ',file=.archivo)
                HTML(paste("datos: ",ActiveDataSet(),"$",var,sep=""), file=.archivo)
                if (.prueba.pearson[[2]] > 0.0001) pval<-paste(" = ",round(.prueba.pearson[[2]],4),sep="") else pval<- " < 0.0001"
                HTML(paste("Ji Cuadrado de Pearson = ",round(.prueba.pearson[[1]],4),", valor p",pval,sep=""),
                file=.archivo)
                HTMLhr(file = .archivo) 
              }
              else {
                HTML('Prueba de ajuste a la normal: Ji-Cuadrado de Pearson ',file=.archivo)
                for (i in 1:dim(.prueba.pearson)){
                  HTML(paste("datos: ",ActiveDataSet(),"$",var," y ",grupos," = ",names(.prueba.pearson)[[i]],sep=""), file=.archivo)
                  if (.prueba.pearson[[i]]$p.value > 0.0001) pval<-paste(" = ",round(.prueba.pearson[[i]]$p.value,4),sep="") 
                  else pval<- " < 0.0001"
                HTML(paste("Ji Cuadrado de Pearson = ",round(.prueba.pearson[[i]]$statistic,4),", valor p",pval,sep=""),
                file=.archivo)
                HTML("----------------------------------------------------------------------------------",file=.archivo) 
              }
              HTMLhr(file = .archivo)        
              }
            }  
            if (echocodigo == 1) logger("remove(list=c('.prueba.pearson'))")            
            remove(list=c('.prueba.pearson'), envir=.GlobalEnv)
          }
          if (ksval == 1)
          { 
            if (.groups == FALSE)
            instruccion <-paste(".prueba.KS <- ks.test(",.baseDatosActiva,"$",var,",'pnorm',mean(",
            .baseDatosActiva,"$",var,"),sd(",.baseDatosActiva,"$",var,"))",sep="")
            else
              instruccion <- paste(".prueba.KS <- by(",.baseDatosActiva,"$",var,","
              ,grupos,",function(x)ks.test(x,'pnorm',mean(x),sd(x)))"
              ,sep="")
            if (echocodigo == 1)
            {
              logger(instruccion)
            }    
            opts <- options(warn=-1)
            justDoIt(instruccion)
            if (.groups != FALSE){                   
                for (i in 1:dim(.prueba.KS))
                 .prueba.KS[[i]]$data.name <- paste(.baseDatosActiva,"$",var,sep="")
            }
            doItAndPrint(paste(".prueba.KS  # Prueba de Kolmogorov-Smirnov para ", var))
            if (creahtml == 1)
            { 
              if (.groups == FALSE){
                HTML('Prueba de ajuste a la normal: Kolmogorov-Smirnov ',file=.archivo)
                HTML(paste("datos: ",ActiveDataSet(),"$",var,sep=""), file=.archivo)
                if (.prueba.KS[[2]] > 0.0001) pval<-paste(" = ",round(.prueba.KS[[2]],4),sep="") else pval<- " < 0.0001"
                HTML(paste("D = ",round(.prueba.KS[[1]],4),", valor p",pval,sep=""),
                file=.archivo)
                HTMLhr(file = .archivo) 
              }
              else {
                HTML('Prueba de ajuste a la normal: Kolmogorov-Smirnov ',file=.archivo)
                for (i in 1:dim(.prueba.KS)){
                  HTML(paste("datos: ",ActiveDataSet(),"$",var," y ",grupos," = ",names(.prueba.KS)[[i]],sep=""), file=.archivo)
                  if (.prueba.KS[[i]]$p.value > 0.0001) pval<-paste(" = ",round(.prueba.KS[[i]]$p.value,4),sep="") 
                  else pval<- " < 0.0001"
                HTML(paste("D = ",round(.prueba.KS[[i]]$statistic,4),", valor p",pval,sep=""),
                file=.archivo)
                HTML("----------------------------------------------------------------------------------",file=.archivo) 
              }
              HTMLhr(file = .archivo)        
              }
            }  
            if (echocodigo == 1) logger("remove(list=c('.prueba.KS'))")            
            remove(list=c('.prueba.KS'), envir=.GlobalEnv)
          }
          if (adval == 1)
          { 
            if (.groups != FALSE)
            instruccion <- paste(".prueba.AD <- by(",.baseDatosActiva,"$",var,",",grupos,",ad.test)",sep="")
            else
              instruccion <- paste(".prueba.AD <- ad.test(",.baseDatosActiva,"$",var,")",sep="")
            if (echocodigo == 1)
            {
              logger(instruccion)
            }       
            justDoIt(instruccion)
            if (.groups != FALSE){                   
                for (i in 1:dim(.prueba.AD))
                 .prueba.AD[[i]]$data.name <- paste(.baseDatosActiva,"$",var,sep="")
            }
            doItAndPrint(paste(".prueba.AD  # Prueba de Anderson-Darling para ", var))
            if (creahtml == 1)
            { 
              if (.groups == FALSE){
                HTML('Prueba de ajuste a la normal: Anderson-Darling ',file=.archivo)
                HTML(paste("datos: ",ActiveDataSet(),"$",var,sep=""), file=.archivo)
                if (.prueba.AD[[2]] > 0.0001) pval<-paste(" = ",round(.prueba.AD[[2]],4),sep="") else pval<- " < 0.0001"
                HTML(paste("A = ",round(.prueba.AD[[1]],4),", valor p",pval,sep=""),
                file=.archivo)
                HTMLhr(file = .archivo) 
              }
              else {
                HTML('Prueba de ajuste a la normal: Anderson-Darling ',file=.archivo)
                for (i in 1:dim(.prueba.AD)){
                  HTML(paste("datos: ",ActiveDataSet(),"$",var," y ",grupos," = ",names(.prueba.AD)[[i]],sep=""), file=.archivo)
                  if (.prueba.AD[[i]]$p.value > 0.0001) pval<-paste(" = ",round(.prueba.AD[[i]]$p.value,4),sep="") 
                  else pval<- " < 0.0001"
                HTML(paste("Ji Cuadrado de Pearson = ",round(.prueba.AD[[i]]$statistic,4),", valor p",pval,sep=""),
                file=.archivo)
                HTML("----------------------------------------------------------------------------------",file=.archivo) 
              }
              HTMLhr(file = .archivo)        
              }
            }  
            if (echocodigo == 1) logger("remove(list=c('.prueba.AD'))")            
            remove(list=c('.prueba.AD'), envir=.GlobalEnv)
          }
          if (shapiroval == 1)
          { 
            if (.groups != FALSE)
            instruccion <- paste(".prueba.shapiro <- by(",.baseDatosActiva,"$",var,",",grupos,",shapiro.test)",sep="")
            else
              instruccion <- paste(".prueba.shapiro <- shapiro.test(",.baseDatosActiva,"$",var,")",sep="")
            if (echocodigo == 1)
            {
              logger(instruccion)
            }       
            justDoIt(instruccion)
            if (.groups != FALSE){                   
                for (i in 1:dim(.prueba.shapiro))
                 .prueba.shapiro[[i]]$data.name <- paste(.baseDatosActiva,"$",var,sep="")
            }
            doItAndPrint(paste(".prueba.shapiro  # Prueba de Shapiro-Wilk para ", var))
            if (creahtml == 1)
            { 
              if (.groups == FALSE){
                HTML('Prueba de ajuste a la normal: Shapiro-Wilk ',file=.archivo)
                HTML(paste("datos: ",ActiveDataSet(),"$",var,sep=""), file=.archivo)
                if (.prueba.shapiro[[2]] > 0.0001) pval<-paste(" = ",round(.prueba.shapiro[[2]],4),sep="") else pval<- " < 0.0001"
                HTML(paste("W = ",round(.prueba.shapiro[[1]],4),", valor p",pval,sep=""),
                file=.archivo)
                HTMLhr(file = .archivo) 
              }
              else {
                HTML('Prueba de ajuste a la normal: Shapiro-Wilk ',file=.archivo)
                for (i in 1:dim(.prueba.shapiro)){
                  HTML(paste("datos: ",ActiveDataSet(),"$",var," y ",grupos," = ",names(.prueba.pearson)[[i]],sep=""), file=.archivo)
                  if (.prueba.shapiro[[i]]$p.value > 0.0001) pval<-paste(" = ",round(.prueba.shapiro[[i]]$p.value,4),sep="") 
                  else pval<- " < 0.0001"
                HTML(paste("W = ",round(.prueba.shapiro[[i]]$statistic,4),", valor p",pval,sep=""),
                file=.archivo)
                HTML("----------------------------------------------------------------------------------",file=.archivo) 
              }
              HTMLhr(file = .archivo)        
              }
            }  
            if (echocodigo == 1) logger("remove(list=c('.prueba.shapiro'))")            
            remove(list=c('.prueba.shapiro'), envir=.GlobalEnv)
          }        
          if (.groups != FALSE & grafval ==1){
          grafval <- 0.95
          Message(message=gettextRcmdr("Analisis segun variable de agrupacion: No se realizaran graficos agrupados."),
          type="warning")            
          }
          if (grafval == 1)
          {
            instruccion <- "par(mfrow=c(2,2))"
            instruccion2 <- paste("plot(density(",.baseDatosActiva,"$",var,"),
            main='",paste('Densidad suavizada para ',var,sep=""),"',ylab='Densidades')",sep="")
            instruccion3 <-"box()"
            justDoIt(paste("x <- ",.baseDatosActiva,"$",var,sep=""))
            instruccion4 <- paste("h <- hist(",.baseDatosActiva,"$",var,",freq=TRUE)",sep="")
            justDoIt(instruccion4)
            instruccion5 <-paste("plot(h,col='red', xlab='Intervalos', ylab='Frecuencias',main='",
            paste('Histograma para ',var,sep=""),"')",sep="")
            instruccion6 <- "xfit<-seq(min(x),max(x),length=1000)"
            instruccion7 <- "yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))"
            instruccion8 <- "yfit<-yfit*diff(h$mids[1:2])*length(x)"
            instruccion9 <- "lines(xfit, yfit, col='blue', lwd=2)"
            instruccion10 <-"box()"
            instruccion11 <- "z <- as.numeric(scale(x))"
            justDoIt(instruccion11)
            instruccion12 <- paste("qqnorm(z,xlab='Cuantilas teoricas',ylab='Cuantilas empiricas',
            main='",paste('Grafico QQ para ',var,sep=""),"')",sep="")
            instruccion13 <- "abline(0,1)"
            instruccion14 <- "box()"
            instruccion15 <-paste("plot(sort(z),pnorm(sort(z)),type='l',col='red',
            main='",paste("Grafico de cuantilas para ",var,sep=""),"',xlab='Puntuaciones Z',
            ylab='Probabilidad Acumulada')",sep="")
            instruccion16 <- "plot(ecdf(z),add=TRUE)"
            instruccion17 <- "box()"
          if (echocodigo == 1)
          {
            doItAndPrint(instruccion)
            doItAndPrint(instruccion2)
            doItAndPrint(instruccion3)
            logger(paste("x <-", paste(.baseDatosActiva,"$",var,sep="")))
            logger(instruccion4) 
            doItAndPrint(instruccion5)          
            doItAndPrint(instruccion6)
            doItAndPrint(instruccion7)
            doItAndPrint(instruccion8)
            doItAndPrint(instruccion9)
            doItAndPrint(instruccion10)         
            logger(instruccion11)
            doItAndPrint(instruccion12)
            doItAndPrint(instruccion13)
            doItAndPrint(instruccion14)
            doItAndPrint(instruccion15)
            doItAndPrint(instruccion16)
            doItAndPrint(instruccion17)
            logger("remove(list=c('x','h','z'))")            
            remove(list=c('x','h','z'), envir=.GlobalEnv)
          }
          else
          {
            justDoIt(instruccion)
            justDoIt(instruccion2)
            justDoIt(instruccion3)
            justDoIt(instruccion5)
            justDoIt(instruccion6)
            justDoIt(instruccion7)
            justDoIt(instruccion8)
            justDoIt(instruccion9)
            justDoIt(instruccion10)
            justDoIt(instruccion12)
            justDoIt(instruccion13)
            justDoIt(instruccion14)
            justDoIt(instruccion15)
            justDoIt(instruccion16)            
            justDoIt(instruccion17)
            remove(list=c('x','h','z'), envir=.GlobalEnv)
          }
          if (creahtml == 1)
          {
            titulo <- paste("Graficas para variable ",var,sep="")
            HTML(as.title(titulo),file=.archivo)
            nombre.archivo <- paste("GraficasNormR",gsub(":","",substr(Sys.time(),12,19)),
            ".jpg",sep="")
            dev.print(jpeg, filename=paste(getwd(),"/",nombre.archivo,sep=""),
            width=500, height=500)
            HTMLInsertGraph(nombre.archivo,file=.archivo,append=TRUE)
            HTMLhr(file = .archivo)
          }             
          }
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="RcmdrPlugin.EACSPIR")
    tkgrid(getFrame(variableVar), labelRcmdr(variablesFrame, text="    "),sticky="w")
    tkgrid(variablesFrame, sticky="nw")
    tkgrid(pruebasFrame, sticky="nw")
    tkgrid(labelRcmdr(pruebasFrame, 
    text=gettextRcmdr("Prueba Ji-Cuadrado de Pearson ")), 
    pearsonCheckBox, sticky="nw")
    tkgrid(labelRcmdr(pruebasFrame, 
    text=gettextRcmdr("Prueba de Kolmogorov-Smirnov ")), 
    ksCheckBox, sticky="nw")
    tkgrid(labelRcmdr(pruebasFrame, 
    text=gettextRcmdr("Prueba Anderson-Darling ")), 
    adCheckBox, sticky="nw")
    tkgrid(labelRcmdr(pruebasFrame, 
    text=gettextRcmdr("Prueba Shapiro-Wilk ")), 
    shapiroCheckBox, sticky="nw")
    tkgrid(labelRcmdr(pruebasFrame, 
    text=gettextRcmdr("Representaciones graficas ")), 
    graficosCheckBox, sticky="nw")
    tkgrid(labelRcmdr(opcionesFrame,
    text=gettextRcmdr("Opciones"), fg="blue"), sticky="nw")
    tkgrid(labelRcmdr(opcionesFrame, 
    text=gettextRcmdr("Mostrar en pantalla el codigo de R ejecutado ")), 
    echoCheckBox, sticky="nw")
    tkgrid(labelRcmdr(opcionesFrame,
    text=gettextRcmdr("Generar informe de resultados ")),
    htmlCheckBox,sticky="nw")
    tkgrid(opcionesFrame, sticky="nw")
    tkgrid(groupsFrame, sticky="nw")    
    tkgrid(buttonsFrame, sticky="nw")
    dialogSuffix(rows=4, columns=1)
}

avar.MR<- function(){
  require(ez)
  require(reshape)
  initializeDialog(title=gettextRcmdr("ANOVA Medidas Repetidas"))
  opcionesFrame <- tkframe(top,width=90)
  variablesFrame <- tkframe(top,width=60)
  SCuadradosFrame <- tkframe(top,width=60)
  pruebasFrame <- tkframe(top,width=60)
  variableVar <- variableListBox(variablesFrame, Numeric(), selectmode="multiple", title=gettextRcmdr("Medidas repetidas (escoja dos o mas)"))
  IDVar <- variableListBox(variablesFrame, selectmode="multiple", title=gettextRcmdr("Variable de identificacion (Opcional)"))
  factor.intra <- tclVar(gettextRcmdr("f.intrasujeto"))
  factorField <- ttkentry(variablesFrame, width="14", textvariable=factor.intra)
  med.rep <- tclVar(gettextRcmdr("medida.rep"))
  repField <- ttkentry(variablesFrame, width="14", textvariable=med.rep)
  radioButtons(top, name="SCuadrados", buttons=c("SC1Boton", "SC2Boton","SC3Boton"),
               values=c(1,2,3),labels=gettextRcmdr(c("Tipo I", "Tipo II", "Tipo III")),
               title=gettextRcmdr("Suma de Cuadrados:"),initialValue=..values[3])
  descripVariable <- tclVar("0")
  descripCheckBox <- tkcheckbutton(pruebasFrame, variable=descripVariable)
  graficosVariable <- tclVar("0")
  graficosCheckBox <- tkcheckbutton(pruebasFrame, variable=graficosVariable)    
  echocodigoVariable <- tclVar("0")
  echoCheckBox <- tkcheckbutton(opcionesFrame, variable=echocodigoVariable)
  creahtmlVariable <- tclVar("0")
  htmlCheckBox <- tkcheckbutton(opcionesFrame, variable=creahtmlVariable)
  onOK <- function(){
    var <- getSelection(variableVar)
    if (length(var) < 2) {
      errorCondition(recall=avar.MR, message=gettextRcmdr("Debe seleccionar 2 variables como minimo."))
      return()
    }
    idvar <-getSelection(IDVar)
    if (length(idvar)==0)
      var <- paste('"', var, '"', sep="")
    else
    {
      var <- paste('"', c(idvar,var), '"', sep="")          
    }       
    echocodigo <- tclvalue(echocodigoVariable)
    creahtml <- tclvalue(creahtmlVariable)
    descripval <- tclvalue(descripVariable)
    grafval <- tclvalue(graficosVariable)
    intra <- tclvalue(factor.intra)
    mr <- tclvalue(med.rep)
    sctype <- as.numeric(tclvalue(SCuadradosVariable))
    if (creahtml == 1)
    {
      require(R2HTML)
      if (!file.exists("Informe de Resultados.html"))
        .archivo <- HTMLInitFile(file.path(getwd()),
                                 "Informe de Resultados", BackGroundColor="#FFFFCC")
      else
        .archivo <- file.path(getwd(), "Informe de Resultados.html")
      titulo <- "ANOVA de Medidas Repetidas"
      HTML(as.title(titulo),file=.archivo)
    }
    closeDialog()
    if (length(idvar) == 0){
      instruccion <- paste(".baseDatosActiva <- na.omit(",ActiveDataSet(),"[,c(", paste(var, collapse=","),")])",sep="")
      justDoIt(instruccion)
      instruccion2 <- "ObsNumero <- as.factor(1:nrow(.baseDatosActiva))"
      justDoIt(instruccion2)
      instruccion3 <- ".baseDatosActiva <- cbind(.baseDatosActiva,ObsNumero)"
      justDoIt(instruccion3)
      instruccion4 <- paste(".baseDatosActiva <- melt.data.frame(.baseDatosActiva, id.vars=c('ObsNumero'), variable_name='",
                            intra,"')",sep='')
      justDoIt(instruccion4)
      idvar <- "ObsNumero"
      justDoIt(paste("colnames(.baseDatosActiva)[3] <- '",mr,"'",sep=''))
      if (echocodigo == 1)
      {
        logger(instruccion)
        logger(instruccion2)
        logger(instruccion3)
        logger(instruccion4)            
      }          
    }
    else{
      instruccion <- paste(".baseDatosActiva <- na.omit(",ActiveDataSet(),"[,c(", paste(var, collapse=","),")])",sep="")
      justDoIt(instruccion)
      instruccion2 <- paste(".baseDatosActiva <- melt.data.frame(.baseDatosActiva, id.vars='",idvar,"',variable_name='",
                            intra,"')",sep='')
      justDoIt(instruccion2)
      justDoIt(paste("colnames(.baseDatosActiva)[3] <- '",mr,"'",sep=''))
      if (echocodigo == 1)
      {
        logger(instruccion)
        logger(instruccion2)            
      }          
    }
    
    instruccion <- paste("datos.ANOVAMR <- ezANOVA(.baseDatosActiva,dv=.(",noquote(mr),"),wid=.(",noquote(idvar),"),
                         within=.(",noquote(intra),"),type=",sctype,",detailed=TRUE)",sep='')
    justDoIt(instruccion)
    instruccion2 <- "print(datos.ANOVAMR)"
    if (echocodigo == 1)
    {
      logger(instruccion)
      doItAndPrint(instruccion2)
    }
    else
    {
      doItAndPrint(instruccion2)
    }        
    
    if (descripval == 1)
    {
      instruccion <- paste("descriptivo.ANOVAMR <- ezStats(.baseDatosActiva,dv=.(",noquote(mr),"),
                           wid=.(",noquote(idvar),"),within=.(",noquote(intra),"))",sep="")
      justDoIt(instruccion)
      instruccion2 <- "print(descriptivo.ANOVAMR)"
      if (echocodigo == 1)
      {
        logger(instruccion)
        doItAndPrint(instruccion2)
      }
      else
      {
        doItAndPrint(instruccion2)
      }          
    }
    
    if (grafval ==1)
    {
      instruccion <- paste("grafico.Medias <- ezPlot(.baseDatosActiva,dv=.(",noquote(mr),"),
                           wid=.(",noquote(idvar),"),within=.(",noquote(intra),"),
                           x = .(",noquote(intra),"), do_lines = FALSE,x_lab = '",intra,
                           "',y_lab = '",mr,"')",sep="")
      justDoIt(instruccion)
      instruccion2 <- "print(grafico.Medias)"
      if (echocodigo == 1)
      {
        logger(instruccion)
        doItAndPrint(instruccion2)
        logger("remove(list=c('datos.ANOVAMR','ObsNumero','descriptivo.ANOVAMR','grafico.Medias'))")            
        remove(list=c('.baseDatosActiva','datos.ANOVAMR','ObsNumero','descriptivo.ANOVAMR','grafico.Medias'), envir=.GlobalEnv)
      }
      else
      {
        doItAndPrint(instruccion2)
        remove(list=c('.baseDatosActiva','datos.ANOVAMR','descriptivo.ANOVAMR',
                      'grafico.Medias'), envir=.GlobalEnv)
      }          
    }
    if (creahtml == 1)
    {
      HTML("Resumen ANOVA",file=.archivo)
      HTML(as.data.frame(datos.ANOVAMR[[1]]),file=.archivo)
      HTML("Prueba Esfericidad de Mauchly",file=.archivo)
      HTML(as.data.frame(datos.ANOVAMR[[2]]),file=.archivo)
      HTML("Correcciones Esfericidad",file=.archivo)
      HTML(as.data.frame(datos.ANOVAMR[[3]]),file=.archivo)
      if (descripval == 1)
      {
        HTML("Descripcion ANOVA",file=.archivo)
        HTML(as.data.frame(descriptivo.ANOVAMR),file=.archivo)
      }
      if (grafval == 1)
      {
        HTML("Grafica de medias ",file=.archivo)
        nombre.archivo <- paste("GraficaMediasANOVAMR",gsub(":","",substr(Sys.time(),12,19)),
                                ".jpg",sep="")
        dev.print(jpeg, filename=paste(getwd(),"/",nombre.archivo,sep=""),
                  width=500, height=500)
        HTMLInsertGraph(nombre.archivo,file=.archivo,append=TRUE)
        HTMLhr(file = .archivo)
      }
      if (grafval != 1) HTMLhr(file = .archivo)          
    }         
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="RcmdrPlugin.EACSPIR")
  tkgrid(getFrame(variableVar), labelRcmdr(variablesFrame, text="    "),
         getFrame(IDVar), sticky="nw")
  tkgrid(labelRcmdr(variablesFrame, 
                    text=gettextRcmdr("Nombre del factor Intrasujetos ")), 
         factorField,sticky="nw")
  tkgrid(labelRcmdr(variablesFrame,     
                    text=gettextRcmdr("Nombre de la variable cuantitativa ")),
         repField,sticky="nw")
  tkgrid(variablesFrame, sticky="nw") 
  tkgrid(SCuadradosFrame, sticky="nw")  
  tkgrid(pruebasFrame, sticky="nw")
  tkgrid(labelRcmdr(pruebasFrame,
                    text=gettextRcmdr("Pruebas Descriptivas"), fg="blue"), sticky="nw")
  tkgrid(labelRcmdr(pruebasFrame, 
                    text=gettextRcmdr("Mostrar descriptivos del ANOVA ")), 
         descripCheckBox, sticky="nw")
  tkgrid(labelRcmdr(pruebasFrame, 
                    text=gettextRcmdr("Mostrar grafico de medias ")), 
         graficosCheckBox, sticky="nw")
  tkgrid(labelRcmdr(opcionesFrame,
                    text=gettextRcmdr("Opciones"), fg="blue"), sticky="nw")
  tkgrid(labelRcmdr(opcionesFrame, 
                    text=gettextRcmdr("Mostrar en pantalla el codigo de R ejecutado ")), 
         echoCheckBox, sticky="nw")
  tkgrid(labelRcmdr(opcionesFrame,
                    text=gettextRcmdr("Generar informe de resultados ")),
         htmlCheckBox,sticky="nw")
  tkgrid(opcionesFrame, sticky="nw")
  tkgrid(buttonsFrame, sticky="nw")
  dialogSuffix(rows=4, columns=1)
}

ayuda.RcmdrPlugin.EACSPIR <- function(){
    doItAndPrint("help(\"RcmdrPlugin.EACSPIR\")")
    invisible(NULL)
}
