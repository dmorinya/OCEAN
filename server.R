vectorformato<-c('application/vnd.ms-excel',
                 'application/msexcel',
                 'application/x-msexcel',
                 'application/x-ms-excel',
                 'application/x-excel',
                 'application/x-dos_ms_excel',
                 'application/xls',
                 'application/x-xls',
                 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
                 '.xls',
                 '.xlsx')

shinyServer(function(input, output,session) {
  output$validaPlot <- renderPlot({
    res_file()
  }) 
  
  output$video <- renderUI({
    tags$video(src = "video_OCEAN-v2.mp4", type = "video/mp4", autoplay = TRUE, controls = TRUE)
  })
  
  output$warning = renderUI({
    shinyalert(
      title = "Warning",
      text = "If a needed input file is not provided, the corresponding example file will be used for the analysis. In particular, the transition probabilities matrix might not be calibrated to your specific settings.",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = TRUE,
      type = "warning",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#AEDEF4",
      timer = 10000,
      imageUrl = "",
      animation = TRUE
    )
  })
  
  output$warning2 = renderUI({
    shinyalert(
      title = "Warning",
      text = "If a needed input file is not provided, the corresponding example file will be used for the analysis. In particular, the transition probabilities matrix might not be calibrated to your specific settings.",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = TRUE,
      type = "warning",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#AEDEF4",
      timer = 10000,
      imageUrl = "",
      animation = TRUE
    )
  })
  
  output$HelpBox = renderUI({
    if ((input$reset_calibration %% 2)==0){
      HTML("<h3>Calibration help</h3>", "<p>The calibration panel needs the following inputs:</p> 
           <ul>", "<li>", "Number of simulations: Number of simulated cohorts.", "</li>",
           "<li>", "Number of simulations to keep: Number of simulated cohorts that will be kept 
                                      and used in the cost-effectiveness analyses. Among all simulated cohorts, the best ones (the matrices 
                                      producing the outcomes that minimize the differences with respect to target HPV infection prevalence 
                                      and CC incidence).", "</li>", 
           "<li> Percentage of change: Maximum allowed difference between two equivalent transition probabilities 
                                            from two matrices.</li>
                                      <li> Mortality: Checkbox. If mortality data is available and to be used in the calibration process 
                                           it should be ticked.</li>
                                      <li> Transition probabilities matrix: Starting values for the transition probabilities matrix. 
                                           A general transition probability matrix will be used if no user-specific matrix is provided.</li>
                                      <li> Incidence file: Registered cervical cancer incidence for the specific setting, without any medical 
                                           intervention. It is uploaded in an Excel file and will be used as a target in the calibration process.</li>
                                      <li> Prevalence file: Registered HPV16/18 infection prevalence for the specific setting. 
                                           It is uploaded in an Excel file and will be used as a target in the calibration process.</li>
                                      <li> Mortality file: Registered mortality due to cervical cancer for the specific setting. 
                                           It is uploaded in an Excel file and will be used as a target in the calibration process if mortality is checked.</li>",
           "</ul> <p>An Excel file including all calibrated matrices can be downloaded and used in the cost-effectiveness part.</p>")
    } else {
      return()
    }
  })
  
  output$HelpBox2 = renderUI({
    if ((input$reset_cea %% 2)==0){
      HTML("<h3>Cost-effectiveness help</h3>", "<p>The cost-effectiveness panel needs the following inputs:</p>
           <ul> <li> Number of different scenarios: The number of scenarios to be analysed in the current session. Note that each scenario may include none (no intervention or natural history), one or more prevention strategies (screening alone, vaccination alone or vaccination followed by screening).</li>
            <li> Matrix of transition probabilities between different health states without medical intervention: Transition probability matrix obtained in the previous panel.</li>
            <li> Medical costs and utility coefficients: An Excel file containing the treatment direct medical and non-medical costs, indirect costs and utility coefficients. An example can be downloaded from the tool.</li>
            <li> Discount rate: The undiscounted outcomes are given by default, and additionally a discount rate can be applied to health and costs outcomes by selecting the desired discount rate on this input.</li>
            <li> Uncertainty level: It is known that the results of cost-effectiveness analyses are affected by a certain degree of uncertainty at different levels. That can be reflected in the OCEAN tool by using more than one transition probabilities matrices to feed the Markov model. If a file with several sheets is used and the uncertainty level is set to 0, an averaged matrix will be used and only point estimates will be reported. If the uncertainty level is set to a value α between 0 and 50, all the matrices are used and the outcomes obtained from each one are recorded. Then, the mean and percentiles α/2 and 1-α/2 for each outcome are calculated and reported.</li>
           </ul>
           <p>The considered prevention strategies that can be selected for each scenario (alone or combined) include:</p>
          <ul> 
           <li> Natural history: No prevention strategy is considered in this scenario, which reproduces the natural history of HPV infection and cervical cancer. If this option is checked, the other options disappear.</li>
           <li> Screening: Screening scenarios may differ by screening test (cytology, HPV DNA test or visual inspection), frequency (every 1-10 years), targeted ages, and switch age from cytology to HPV testing. The screening coverage, positive predictive value, sensitivity and costs are read from the Excel file loaded as “Scenario specific values”. The structure of this file can be seen by downloading the example file from the tool. Screening may be organized (all women are screened with the selected frequency) or opportunistic (the screening period is variable). To set an opportunistic screening scenario, an additional Excel file specifying the proportion of women screened each period is needed.</li>
           <li> Vaccination: We assume that preadolescent girls are successfully vaccinated at the age of 11 years with one, two or three doses of the vaccine against HPV types 16 and 18. Efficacy and coverage are set by the user and vaccination costs are read from the Excel file loaded as “Scenario specific values”. The structure of this file can be seen by downloading the example file from the tool. Currently, only bivalent vaccine is considered but it is planned that quadrivalent and nonavalent vaccines will be available soon as well.</li> </ul>
      <p>An Excel file with all generated results can be downloaded, including information about the inputs used to generate those particular results.</p>")
    } else {
      return()
    }
  })
  
res_file <- reactive({
  input$b_calibration
    withBusyIndicatorServer("b_calibration",{
      isolate({
        inFile1 <- input$prob
        inFile2 <- input$load_m_inc
        inFile3 <- input$load_m_prev
        mort    <- NULL
        if (input$mort_check==TRUE) inFile4 <- input$mort
        if(is.null(input$prob)){
          direccion<-paste0(getwd(),"/data examples/Calibration/")
          probs<-read.xlsx(paste0(direccion,"/Transition Probabilities.xls"),1)
          validate(
            need(all(round(rowSums(probs[, 2:13]))==1), "Some rows in the transition probabilities matrix are not equal to 1.")
          )
        }else{
          probs <- read.xlsx(inFile1$datapath,1)
          validate(
            need(all(round(rowSums(probs[, 2:13]))==1), "Some rows in the transition probabilities matrix are not equal to 1.")
          )
        }
        if(is.null(input$load_m_inc) & is.null(input$load_m_prev) & input$b_calibration==0) return(NULL)
        if(is.null(input$load_m_inc)){
          direccion<-paste0(getwd(),"/data examples/Calibration/")
          incid<-read.xlsx(paste0(direccion,"/CC Observed Incidence.xls"), 1)[, 1:2]
        }else{
          incid  <- read.xlsx(inFile2$datapath,1)[, 1:2]
        }
        if(is.null(input$load_m_prev)){
          direccion<-paste0(getwd(),"/data examples/Calibration/")
          preval<-read.xlsx(paste0(direccion,"/HPV Observed Prevalence.xls"), 1)[, 1:2]
        }else{
          preval <- read.xlsx(inFile3$datapath,1)
        }
        if(input$mort_check==TRUE & is.null(input$mort)){
          direccion<-paste0(getwd(),"/data examples/Calibration/")
          mort<-read.xlsx(paste0(direccion,"/CC Observed Mortality.xls"), 1)[, 1:2]
        }
        if(input$mort_check==TRUE & !is.null(input$mort)){
          mort <- read.xls(inFile4$datapath)[, 1:2]
        }
        # if (!is.null(input$load_m_inc)) incid  <- read.xlsx(inFile2$datapath,1)
        # if (!is.null(input$load_m_prev)) preval <- read.xlsx(inFile3$datapath,1)
        # if ( !is.null(input$load_m_inc) & !is.null(input$load_m_prev))
        # if (!is.null(input$mort) & input$mort_check==TRUE) mort <- read.xls(inFile4$datapath)
        res_num <- NHvalida(probs[, 1:13], preval, incid, mort, input$N, input$keep, input$p.change)
    })
  })
})  
  
  output$b_d_matrix_calib <- downloadHandler(
    filename = function() { paste("calibrated_matrix", "xls", sep='.') },
    content = function(file) {
      fname<-paste0(file,".xls")
      res_num2 <- res_file()
      withBusyIndicatorServer("b_d_matrix_calib",{
      wb <- xlsx::createWorkbook(type="xls")
      st.col <- rep(c("Well","HR.HPV","CIN.1","CIN.2","CIN.3","FIGO.I","FIGO.II",
                      "FIGO.III","FIGO.IV","ICC.Survival","ICC.death","Death.from.other.causes"), 15)
      for (i in 1:length(res_num2)) {
        assign(paste0('Sheet.', i, sep = ''), as.data.frame(res_num2[i],optional=FALSE,fix.empty.names=FALSE,stringsAsFactors=FALSE))
        sname <- paste("Sheet_", i, sep='')
        sheet <- xlsx::createSheet(wb, sheetName = sname)
        df.final <- as.data.frame(cbind(res_num2[[i]], st.col))
        colnames(df.final) <- c("Age.group","Well","HR.HPV","CIN.1","CIN.2","CIN.3","FIGO.I","FIGO.II",
                                "FIGO.III","FIGO.IV","ICC.Survival","ICC.death",              
                                "Death.from.other.causes","States")
        addDataFrame(df.final, sheet=sheet,startColumn = 1,row.names = FALSE, col.names=TRUE)
      }
      xlsx::saveWorkbook(wb, file)
      })
    },
    contentType = "application/xls" 
  )
  
  #Panel dinámico
  output$panel<-renderUI({
    if(is.na(input$numb_scen)||input$numb_scen<1)return()
    lapply(1:input$numb_scen, function(i){
      tagList(
        h3(strong(paste0("Scenario ", i))),
        # SCREENING OPTIONS
        checkboxInput(paste0("natural_history",i,sep=""),"Natural History"),
        conditionalPanel(condition = paste0("input.",paste0("natural_history",i,sep=""),"==false"),
          checkboxInput(paste0("screen_check",i,sep=""), "Screening "),
          conditionalPanel(condition = paste0("input.",paste0("screen_check",i,sep=""),"==true"),
              radioButtons(paste0("stype",i,sep=""), "Choose screening type ",c("Organized screening" = 1,"Opportunistic screening" = 2)),
              sliderInput(paste0("screeningPeriod",i,sep=""), "Screening Period:",min = 1, max = 10,value = 3),
              conditionalPanel(condition = paste0("input.",paste0("stype",i,sep=""),"==2"),
                fluidRow(column(10,offset = 0,fileInput(paste0("Opscreen",i,sep=""), "Load opportunistic screening file ",accept = vectorformato)),
                    column(2,offset = 0,style='margin-top:25px',downloadButton(paste0("b_Opscreen",i,sep=""),label="",icon=icon("cloud-download",lib="glyphicon")))
                )              
              ),
              checkboxInput(paste0("dnascreen",i,sep=""), "HPV-DNA Screening ", value = FALSE),
              conditionalPanel(condition = paste0("input.",paste0("dnascreen",i,sep=""),"==true"),
              sliderInput(paste0("dnaScage",i,sep=""), "Select Age Group Range:",min = 0, max = 10000,value = c(5,9995))
               ),
              #VISUAL EXPLORATION (VIA) OPTIONS
              checkboxInput(paste0("via_check",i,sep=""),"VIA"),
              conditionalPanel(condition=paste0("input.",paste0("via_check",i,sep=""),"==true"),
                               sliderInput(paste0('vperiod',i,sep=""), 'VIA Period', min = 0, max = 20, value = 5, step = 1)
              )
          ),
          # VAC_CHECK OPTIONS
          checkboxInput(paste0("vac_check",i,sep=""), "Vaccination"),
          conditionalPanel(condition = paste0("input.",paste0("vac_check",i,sep=""),"==true"),
            textInput(paste0('efficacy',i,sep=""), 'Efficacy', value=0.7),      
            #selectInput(paste0("vac_type",i,sep=""), "Choose a vaccine type:",choices = c("Bivalent", "Quadrivalent", "Nonavalent")),
            #conditionalPanel(condition = paste0("input.",paste0("vac_type",i,sep=""),"=='Bivalent'"),
                selectInput(paste0("vac_dose.B",i,sep=""), "Number of doses:", c("1", "2", "3"), selected = "3"),           
                textInput(paste0('vaccov.B',i,sep=""), 'Vaccination coverage (from 0 to 1)', value=0.7)  
            # conditionalPanel(condition = paste0("input.",paste0("vac_type",i,sep=""),"=='Quadrivalent'"),
            #     selectInput(paste0("vac_dose.T",i,sep=""), "Choose the number of dosis:", c("1", "2", "3"), selected = "3"),              
            #     textInput(paste0('vaccov.T',i,sep=""), 'Quadrivalent vaccine coverage (from 0 to 1)', value=0.7)),  
            # conditionalPanel(condition = paste0("input.",paste0("vac_type",i,sep=""),"=='Nonavalent'"),
            #     selectInput(paste0("vac_dose.N",i,sep=""), "Choose the number of dosis:", c("1", "2", "3"), selected = "3"),              
            #     textInput(paste0('vaccov.N',i,sep=""), 'Nonavalent vaccine coverage (from 0 to 1)', value=0.7))               
          ),
          conditionalPanel(condition = paste0("input.",paste0("via_check",i,sep=""),"==true||input.",paste0("vac_check",i,sep=""),"==true||input.",paste0("screen_check",i,sep=""),"==true"),
                           fluidRow(column(10,offset = 0,fileInput(paste0("scenario_cost_and_probs",i,sep=""),"Scenario specific values",accept = vectorformato)),
                                    column(2,offset = 0,style='margin-top:25px',downloadButton(paste0("b_Scenario_costs_and_probs",i,sep=""),label="",icon=icon("cloud-download",lib="glyphicon")))
                           ) 
          )
        )
      )
    })
  })
  
  output$Scenario_table<-renderUI({
    if(is.na(input$numb_scen)||input$numb_scen<1)return()
    lapply(1:input$numb_scen, function(i){
      tagList(
        h2(paste0("Scenario ", i)),
        hr(style="border-color: black;"),
        tableOutput(paste0("Scenario", i, sep="")),
        br())
    })  
  })
  
  observeEvent(!is.null(input[[paste0("dnaScage",1,sep="")]]),{
    if(input$numb_scen>=1){
      ifelse(as.integer(input[[paste0("dnaScage",1,sep="")]][2])>9000,TRUE,return())
        partir<-read_main_file_for_slider()
        valor_inf<- as.integer(partir[[1]][1])
        valor_sup<-as.integer(partir[[length(partir)]][2])
        incremento<-round((valor_sup-valor_inf)/length(partir))
        
        lapply(1:input$numb_scen,function(i){
          updateSliderInput(session, paste0("dnaScage",i,sep=""),
                            label = paste("Select Age Group Range:"),max=valor_sup,min=valor_inf,
                            value = c(valor_inf,valor_sup),step = 5)
        })
      }
  })
  
  Scenario <- reactive({
    input$b_cost_effect
    
    isolate({
      evaluar<-ifelse(input$uncertainty_level>0,TRUE,FALSE)
      
      if(file.exists("tables_matrix.RData")){
        unlink("tables_matrix.RData")
      }
      if(file.exists("mean_sheet.RData")){
        unlink("mean_sheet.RData")
      }
      if(file.exists("scenarios_list.RData")){
        unlink("scenarios_list.RData")
      }
      if(evaluar==FALSE){
        scenarios_list<- lapply(1:input$numb_scen,function(i){
          if(is.null(input$probs1)){
            direccion<-paste0(getwd(),"/data examples/Cost-Effectiveness/calibrated_matrix.xls")
          }else{
            direccion<-input$probs1$datapath
          }
          if(input$b_calibration!=0)
          {
            probs1 <- res_file()[[1]]
          }else{
            probs1<-probs1_function(direccion)
          }
          cost_effectiveness_engine(probs1[, 1:13],i,as.integer(input$discount_ratio))
        })
        lapply(1:length(scenarios_list),function(numb_scenario){
          #matriz_temp<-format(matrix(scenarios_list[[numb_scenario]],dimnames = list(rownames(scenarios_list[[numb_scenario]]),c("Undiscounted",paste0(" Discounted ",as.integer(input$discount_ratio),"%"))),ncol=2))
          matriz_temp<-format(matrix(scenarios_list[[numb_scenario]],dimnames = list(rownames(scenarios_list[[numb_scenario]]),c(paste0(" Discounted ",as.integer(input$discount_ratio),"%"))),ncol=1))
          #matriz_numeric<-matrix(data=mapply(matriz_temp,FUN=as.numeric),ncol=2,nrow=11)
          matriz_numeric<-matrix(data=mapply(matriz_temp,FUN=as.numeric),ncol=1,nrow=11)
          #matriz_save<-matrix(data=format(as.double(matriz_numeric),big.mark = ",",scientific = FALSE),dimnames =list(rownames(scenarios_list[[numb_scenario]]),c(  "Undiscounted Mean" ,paste0("Discounted ",as.integer(input$discount_ratio),"% Mean"))),ncol = 2,nrow=11)
          matriz_save<-matrix(data=format(as.double(matriz_numeric),big.mark = ",",scientific = FALSE),dimnames =list(rownames(scenarios_list[[numb_scenario]]),c(paste0("Discounted ",as.integer(input$discount_ratio),"% Mean"))),ncol = 1,nrow=11)
          
          if(file.exists("tables_matrix.RData")){
            load("tables_matrix.RData")
            tables_matrix<-cbind(tables_matrix,matriz_save)
            save(tables_matrix,file="tables_matrix.RData")
          }else{
            tables_matrix<-matriz_save
            save(tables_matrix,file="tables_matrix.RData")
          }
          #matrix_decimals<-matrix(data=format(as.double(matriz_numeric[2:nrow(matriz_numeric),]),big.mark = ",",digits=2,nsmall=2,scientific = FALSE),ncol = 2,nrow=10)
          matrix_decimals<-matrix(data=format(as.double(matriz_numeric[2:nrow(matriz_numeric),]),big.mark = ",",digits=2,nsmall=2,scientific = FALSE),ncol = 1,nrow=10)
          full_matrix<-rbind(matriz_numeric[1,],matrix_decimals)
          #matriz_imp<-matrix(data=as.character(full_matrix),ncol=2,nrow=11)
          matriz_imp<-matrix(data=as.character(full_matrix),ncol=1,nrow=11)
          rownames(matriz_imp)<-rownames(tables_matrix)
          #colnames(matriz_imp)<-c("Undiscounted",paste0("Discounted ",as.integer(input$discount_ratio),"%"))
          colnames(matriz_imp)<-c(paste0("Discounted ",as.integer(input$discount_ratio),"%"))
          
          output[[paste0("Scenario",as.integer(numb_scenario),sep="")]]<-renderTable(matriz_imp, rownames = TRUE,align = 'r')
        })
      }else{
        if(is.null(input$probs1)){
          direccion<-paste0(getwd(),"/data examples/Cost-Effectiveness/calibrated_matrix.xls")
          
        }else{
          direccion<-input$probs1$datapath
        }
        wk<-loadWorkbook(direccion)
        if(input$b_calibration!=0)
        {
          sheets <- length(res_file()) 
         }else{
          sheets<-length(getSheets(wk))
        }
        scenarios_list<-lapply(1:input$numb_scen,function(i){
            lapply(1:sheets, function(x){
            if (input$b_calibration!=0)
            {
              probs1 <- res_file()[[x]]
            }else{
              probs1<-read.xlsx(direccion,x)
            }
            cost_effectiveness_engine(probs1[, 1:13],i,as.integer(input$discount_ratio))
          })
        })
        lapply(1:length(scenarios_list),function(numb_scenario){
          #matriz_temp<-format(matrix(cbind(do.call(pmean,scenarios_list[[numb_scenario]])[,1],pquantile.fromList(scenarios_list[[numb_scenario]],as.double(input$uncertainty_level)/200)[,1],pquantile.fromList(scenarios_list[[numb_scenario]],1-as.double(input$uncertainty_level)/200)[,1],do.call(pmean,scenarios_list[[numb_scenario]])[,2],pquantile.fromList(scenarios_list[[numb_scenario]],as.double(input$uncertainty_level)/200)[,2],pquantile.fromList(scenarios_list[[numb_scenario]],1-as.double(input$uncertainty_level)/200)[,2]),dimnames =list(rownames(scenarios_list[[1]][[1]]),c(  "Undiscounted Mean",paste0("Undiscounted P",as.double(input$uncertainty_level)/2) ,paste0("Undiscounted P",100-as.double(input$uncertainty_level)/2) ,paste0("Discounted ",as.integer(input$discount_ratio)," Mean"),paste0("Discounted ",as.integer(input$discount_ratio)," P",as.double(input$uncertainty_level)/2) ,paste0("Discounted ",as.integer(input$discount_ratio)," P",100-as.double(input$uncertainty_level)/2))),ncol=6))
          matriz_temp<-format(matrix(cbind(do.call(pmean,scenarios_list[[numb_scenario]])[,1],pquantile.fromList(scenarios_list[[numb_scenario]],as.double(input$uncertainty_level)/200)[,1],pquantile.fromList(scenarios_list[[numb_scenario]],1-as.double(input$uncertainty_level)/200)[,1]),dimnames =list(rownames(scenarios_list[[1]][[1]]),c(paste0("Discounted ",as.integer(input$discount_ratio)," Mean"),paste0("Discounted ",as.integer(input$discount_ratio)," P",as.double(input$uncertainty_level)/2) ,paste0("Discounted ",as.integer(input$discount_ratio)," P",100-as.double(input$uncertainty_level)/2))),ncol=3))
          #matriz_numeric<-matrix(data=mapply(matriz_temp,FUN=as.numeric),ncol=6,nrow=11)
          matriz_numeric<-matrix(data=mapply(matriz_temp,FUN=as.numeric),ncol=3,nrow=11)
          #matriz_save<-matrix(data=format(as.double(matriz_numeric),big.mark = ",",scientific = FALSE),dimnames =list(rownames(scenarios_list[[1]][[1]]),c("Undiscounted Mean",paste0("Undiscounted P",as.double(input$uncertainty_level)/2) ,paste0("Undiscounted P",100-as.double(input$uncertainty_level)/2) ,paste0("Discounted ",as.integer(input$discount_ratio),"% Mean"),paste0("Discounted ",as.integer(input$discount_ratio),"% P",as.double(input$uncertainty_level)/2) ,paste0("Discounted ",as.integer(input$discount_ratio),"% P",100-as.double(input$uncertainty_level)/2))),ncol = 6,nrow=11)
          matriz_save<-matrix(data=format(as.double(matriz_numeric),big.mark = ",",scientific = FALSE),dimnames =list(rownames(scenarios_list[[1]][[1]]),c(paste0("Discounted ",as.integer(input$discount_ratio),"% Mean"),paste0("Discounted ",as.integer(input$discount_ratio),"% P",as.double(input$uncertainty_level)/2) ,paste0("Discounted ",as.integer(input$discount_ratio),"% P",100-as.double(input$uncertainty_level)/2))),ncol = 3,nrow=11)
          
          if(file.exists("tables_matrix.RData")){
            load("tables_matrix.RData")
            tables_matrix<-cbind(tables_matrix,matriz_save)
            save(tables_matrix,file="tables_matrix.RData")
          }else{
            tables_matrix<-matriz_save
            save(tables_matrix,file="tables_matrix.RData")
          }
          
          #matrix_decimals<-matrix(data=format(as.double(matriz_numeric[2:nrow(matriz_numeric),]),big.mark = ",",digits=2,nsmall=2,scientific = FALSE),ncol = 6,nrow=10)
          matrix_decimals<-matrix(data=format(as.double(matriz_numeric[2:nrow(matriz_numeric),]),big.mark = ",",digits=2,nsmall=2,scientific = FALSE),ncol = 3,nrow=10)
          full_matrix<-rbind(matriz_numeric[1,],matrix_decimals)
          #text_matrix<-matrix(data=as.character(full_matrix),ncol=6,nrow=11)
          text_matrix<-matrix(data=as.character(full_matrix),ncol=3,nrow=11)
          #matriz_imp<-matrix(data=cbind(paste0(text_matrix[,1]," (",text_matrix[,2]," - ",text_matrix[,3], ")"), paste0(text_matrix[,4]," (",text_matrix[,5]," - ",text_matrix[,6], ")")),ncol=2,nrow=11)
          matriz_imp<-matrix(data=cbind(paste0(text_matrix[,1]," (",text_matrix[,2]," - ",text_matrix[,3], ")")),ncol=1,nrow=11)
          matriz_imp<-rbind(c(paste0("Mean (P",as.double(input$uncertainty_level)/2," - P",100-as.double(input$uncertainty_level)/2,")")),matriz_imp)
          rownames(matriz_imp)<-c(" ",rownames(tables_matrix))
          #colnames(matriz_imp)<-c("Undiscounted",paste0("Discounted ",as.integer(input$discount_ratio),"%"))
          colnames(matriz_imp)<-c(paste0("Discounted ",as.integer(input$discount_ratio),"%"))
          
          output[[paste0("Scenario",as.integer(numb_scenario),sep="")]]<-renderTable(matriz_imp, rownames = TRUE,align ='r')
        })
      }
      if(file.exists("parameters_list.RData")){
        unlink("parameters_list.RData")
      }
      lapply(1:input$numb_scen,function(i){
        ndosis<-1
        coverage<-0.7
        #if(input[[paste0("vac_type",i,sep="")]]=='Bivalent'){
          ndosis<-as.integer(input[[paste0("vac_dose.B",i,sep="")]])
          coverage<-as.double(input[[paste0('vaccov.B',i,sep="")]])
        #}
        # if(input[[paste0("vac_type",i,sep="")]]=='Quadrivalent'){
        #   ndosis<-as.integer(input[[paste0("vac_dose.T",i,sep="")]])
        #   coverage<-as.double(input[[paste0('vaccov.T',i,sep="")]])
        # }
        # if(input[[paste0("vac_type",i,sep="")]]=='Nonavalent'){
        #   ndosis<-as.integer(input[[paste0("vac_dose.N",i,sep="")]])
        #   coverage<-as.double(input[[paste0('vaccov.N',i,sep="")]])
        # }
        parameters_value_storing(i,input[[paste0("natural_history",i,sep="")]],input[[paste0("screen_check",i,sep="")]],as.integer(input[[paste0("stype",i,sep="")]]),as.integer(input[[paste0("screeningPeriod",i,sep="")]]),input[[paste0("dnascreen",i,sep="")]],input[[paste0("dnaScage",i,sep="")]][1],input[[paste0("dnaScage",i,sep="")]][2],input[[paste0("via_check",i,sep="")]],as.numeric(input[[paste0("vperiod",i,sep="")]]),input[[paste0("vac_check",i,sep="")]],as.double(input[[paste0('efficacy',i,sep="")]]),"Bivalent",ndosis,coverage,as.integer(input$discount_ratio),as.integer(input$uncertainty_level))
      })
    })
  })
  observeEvent(input$numb_scen,{
    shinyjs::hide("Scenario_table")
  })
  
  
  observeEvent(input$b_cost_effect,{
    withBusyIndicatorServer("b_cost_effect",{
      shinyjs::hide("Scenario_table")
      Scenario()
      shinyjs::show("Scenario_table")
    })
    if(file.exists("tables_matrix.RData")){
      load("tables_matrix.RData")
    }
  })
  
  output$b_down_cost_effect_results <- downloadHandler(
    filename = function() { paste("cost_effectiveness", "xls", sep='.') },
    content = function(file) {
      fname <- paste(file,"xls",sep=".")
      input$b_down_cost_effect_results
      withBusyIndicatorServer("b_down_cost_effect_results",{
      wb <- xlsx::createWorkbook(type="xls")
      if(file.exists("parameters_list.RData")){
        load("parameters_list.RData")
      }
      if(file.exists("tables_matrix.RData")){
        load("tables_matrix.RData")
      }
      evaluar<-ifelse(input$uncertainty_level>0,TRUE,FALSE)
      if(evaluar==FALSE){
        #multip<-2
        multip <- 1
      }else{
        #multip<-6
        multip <- 3
      }
      
      lim<-max(as.integer(input$numb_scen))*multip
      inicio<-seq(1, lim, by = multip)
      fin<-seq(multip, lim, by = multip)
      lapply(1:input$numb_scen,function(x){
        param_resultados<-strrep(paste0("Parameter",seq(1,length(parameters_list[[x]]))),rep(1,length(parameters_list[[x]])))
        if (fin[x]>inicio[x])
        {
          nombres_resultados<-array(rownames(tables_matrix[,inicio[x]:fin[x]]))
        }else{
          nombres_resultados<-array(names(tables_matrix[, 1]))
        }
        nombres<-c(nombres_resultados,param_resultados)
        list_combine<-rbindlist( list(as.data.frame(tables_matrix[,inicio[x]:fin[x]]),as.data.frame(parameters_list[[x]])),fill=TRUE)
        data_frame<-data.frame(list_combine,row.names=nombres)
          sheet=createSheet(wb,paste0("Scenario ",x))
          addDataFrame(data_frame, sheet = sheet,startColumn = 1,row.names = TRUE)
      })
      xlsx::saveWorkbook(wb,file)
      if(file.exists("tables_matrix.RData")){
        unlink("tables_matrix.RData")
      }
      })
    },
    contentType = "application/xls" 
  ) 
  
  output$text1 <- renderText({ 
    paste("Number of scenarios selected:", input$numb_scen)})
  
  colnames_tables<-function(i){
    cols_names_primary<- c("Disc.ratio = 0 Scenario ", "Disc.ratio = 3 Scenario ")
    max_num<-max(as.integer(input$numb_scen))
    if(i==max_num){
      for(x in 1:max_num){
        if(x==1){
          names_cols<-c(paste0(cols_names_primary,x))
        }else{
          names_cols <-c(names_cols,paste0(cols_names_primary,x))
        }
        if(x==max_num){
          return(names_cols)
        }
      }
    }
  }
  
  cost_effectiveness_engine<-function(probs1, i, discount=3){
    withProgress(message="", value=0, {
      incProgress(i/as.numeric(input$numb_scen), detail = paste("Scenario: ", i))
    scenario_params<-data.frame()
    if(is.null(input$gen_costs_matrix)){
      direccion<-read.xlsx(paste0(getwd(),"/data examples/Cost-Effectiveness/General_costs.xlsx"), 1)
    }else{
      Loadcost <- input$gen_costs_matrix
    }
    if (!is.null(input$gen_costs_matrix)){
      costCoefs.md  <- read.xls(Loadcost$datapath, header = TRUE)[1:12, 1:5]
      validate(
        need(dim(costCoefs.md)[2]==5, "Wrong dimensions in the costs and utilities file")
      )
      validate(
        need(dim(costCoefs.md)[1]==12, "Wrong dimensions in the costs and utilities file2")
      )
      medi <- as.numeric(unlist(na.omit(subset(costCoefs.md,select=2))))
      nomedi <- as.numeric(unlist(na.omit(subset(costCoefs.md,select=3))))
      indi <- as.numeric(unlist(na.omit(subset(costCoefs.md,select=4))))
      utility <- as.numeric(unlist(na.omit(subset(costCoefs.md,select=5))))
      validate(
        need(all(utility>=0), "Some utilities are lower than 0!"),
        need(all(utility<=1), "Some utilities are greater than 1!")
      )
    }
    
    if (is.null(input$gen_costs_matrix)){
      costCoefs.md  <- direccion
      validate(
        need(dim(costCoefs.md)==5, "Wrong dimensions in the costs and utilities file")
      )
      medi <- as.numeric(unlist(na.omit(subset(costCoefs.md,select=2))))
      nomedi <- as.numeric(unlist(na.omit(subset(costCoefs.md,select=3))))
      indi <- as.numeric(unlist(na.omit(subset(costCoefs.md,select=4))))
      utility <- as.numeric(unlist(na.omit(subset(costCoefs.md,select=5))))
      validate(
        need(all(utility>=0), "Some utilities are lower than 0!"),
        need(all(utility<=1), "Some utilities are greater than 1!")
      )
    }
    
    LoadScenarioparams<-input[[paste0("scenario_cost_and_probs",i,sep="")]]
    if(!is.null(input[[paste0("scenario_cost_and_probs",i,sep="")]])){scenario_params<-read.xls(LoadScenarioparams$datapath,header=TRUE)}
    if(is.null(input[[paste0("scenario_cost_and_probs",i,sep="")]])){scenario_params<-read.xls(paste0(getwd(), "/data examples/Cost-Effectiveness/scenario1.xlsx"))}
    
    if(ncol(probs1)-1 != length(medi)) return("Number of rows in Direct Medical Costs must be the same as number of health states in Matrix of transition probabilities without medical intervention")
    if(ncol(probs1)-1 != length(nomedi)) return("Number of rows in DirectNon-medical Costs must be the same as number of health states in Matrix of transition probabilities without medical intervention")
    if(ncol(probs1)-1 != length(indi)) return("Number of rows in Indirect Medical Costs must be the same as number of health states in Matrix of transition probabilities without medical intervention")
    if(ncol(probs1)-1 != length(utility)) return("Number of rows in Utility Coefs must be the same as number of health states in Matrix of transition probabilities without medical intervention")
    
    figoSymProb <- c(0.11, 0.23, 0.66, 0.9)
    
    if(input[[paste0("natural_history",i,sep="")]]==FALSE){
      if(input[[paste0("screen_check",i,sep="")]]==TRUE){
        screening <- TRUE
        screeningPeriod <- as.numeric(input[[paste0("screeningPeriod",i,sep="")]])
        if (input[[paste0("stype",i,sep="")]]==2) 
        {ScreenType <- as.numeric(2) 
        LoadOpScreen <- input[[paste0("Opscreen",i,sep="")]]
        OpScreen <- read.xls(LoadOpScreen$datapath, header = TRUE)}
        
        if (input[[paste0("stype",i,sep="")]]==1) {ScreenType <- as.numeric(1)} 
        screenCoverage <- as.numeric(unlist(na.omit(subset(scenario_params,select=2))))
        screenSensi <- as.numeric(unlist(na.omit(subset(scenario_params,select=3))))
        scr_prob <- as.numeric(unlist(na.omit(subset(scenario_params,select=4))))
        screenPrice.md <- as.numeric(unlist(na.omit(subset(scenario_params,select=10))))
        screenPrice.nmd <- as.numeric(unlist(na.omit(subset(scenario_params,select=11))))
        screenPrice.i <- as.numeric(unlist(na.omit(subset(scenario_params,select=12))))
        
        if(nlevels(probs1[,1]) != length(screenCoverage))return("Length of Screening Coverage vector must be the same as number of groups of age in Matrix of transition probabilities without medical intervention ")
        if(ncol(probs1)-1 != length(screenSensi))return("Length of Screening Sensitivity vector must be the same as number of health states in Matrix of transition probabilities without medical intervention")
        if(ncol(probs1)-1 != length(scr_prob))return("Length of Screening Probabilities vector must be the same as number of health states in Matrix of transition probabilities without medical intervention")
        
        if (input[[paste0("dnascreen",i,sep="")]] == TRUE) {
          dnaScreening <- TRUE
          dnaScSensi <- as.numeric(unlist(na.omit(subset(scenario_params,select=5))))
          papScSensi <- as.numeric(unlist(na.omit(subset(scenario_params,select=6))))
          
          
            partir<-read_main_file_for_slider()
            g<-rep(seq_along(partir),sapply(partir, length))
            age_inf<-as.integer(sapply(as.character(input[[paste0("dnaScage",i,sep="")]][1]), function(x) g[which(unlist(partir) %in% x)]))
            age_sup<-as.integer(sapply(as.character(input[[paste0("dnaScage",i,sep="")]][2]), function(x) g[which(unlist(partir) %in% x)]))
            dnaScAgeGroups <- c(age_inf:age_sup)
         
          dnaScCost.md <- as.numeric(unlist(na.omit(subset(scenario_params,select=13))))
          dnaScCost.nmd <- as.numeric(unlist(na.omit(subset(scenario_params,select=14))))
          dnaScCost.i <- as.numeric(unlist(na.omit(subset(scenario_params,select=15))))
          
          if(ncol(probs1)-1 != length(dnaScSensi)) return("Number of rows in DNA Screening Sensitivity must be the same as number of health states in Matrix of transition probabilities without medical intervention")
          if(ncol(probs1)-1 != length(papScSensi)) return("Number of rows in papScSensi must be the same as number of health states in Matrix of transition probabilities without medical intervention")
          # if(nlevels(probs1[,1]) != length(dnaScAgeGroups))return("Length of Prob of participating in the screening depending on age vector must be the same as number of groups of age in Matrix of transition probabilities without medical intervention ")
          
        }
        if (input[[paste0("dnascreen",i,sep="")]]== FALSE){
          dnaScreening <- FALSE
          dnaScSensi <- NULL
          dnaScCost.md <- 0
          dnaScCost.nmd <- 0
          dnaScCost.i <- 0
          papScSensi <- NULL
          dnaScAgeGroups <- NULL}}
      
      if (input[[paste0("screen_check",i,sep="")]]==FALSE){
        screening <- FALSE
        ScreenType <- 0 
        screenPrice.md <- 0
        screenPrice.nmd <- 0
        screenPrice.i <- 0
        screenCoverage <- NULL 
        screenSensi <- NULL
        scr_prob <- NULL
        dnaScreening <- FALSE
        dnaScSensi <- NULL
        dnaScCost.md <- 0
        dnaScCost.nmd <- 0
        dnaScCost.i <- 0
        papScSensi <- NULL
        dnaScAgeGroups <- NULL
        screeningPeriod<-3
        via <- FALSE}
      
      if(input[[paste0("vac_check",i,sep="")]]==TRUE){
        #if(input[[paste0("vac_type",i,sep="")]]=='Bivalent'){
          probs2_temp <- probs1
          load("probs3.RData")
          for (rr in seq(1, dim(probs1)[1], 12))
          {
            probs2_temp[rr, 3] <- (1-as.double(input[[paste0('efficacy',i,sep="")]])) * probs2_temp[rr,3]
            row_sum<-rowSums(probs2_temp[rr , c(2, 4:ncol(probs2_temp))])
            probs2_temp[rr, c(2, 4:ncol(probs2_temp))] <- probs2_temp[rr, c(2, 4:ncol(probs2_temp))]*(1-probs2_temp[rr,3])/
              row_sum
          }
          probs2 <- probs2_temp
          probs3 <- probs3
          ndoses <- as.integer(input[[paste0('vac_dose.B',i,sep="")]])
          vaccinePrice.md <- as.numeric(unlist(na.omit(subset(scenario_params,select=19))))
          vaccinePrice.nmd <- as.numeric(unlist(na.omit(subset(scenario_params,select=20))))
          vaccinePrice.i <-as.numeric(unlist(na.omit(subset(scenario_params,select=21))))
          vaccineCova <- as.double(input[[paste0('vaccov.B',i,sep="")]])
          
        #}
        # if(input[[paste0("vac_type",i,sep="")]]=='Quadrivalent'){
        #   probs2_temp <- probs1
        #   probs2_temp[,3]<-(1- as.double(input[[paste0('efficacy',i,sep="")]])) * probs2_temp[,3]
        #   row_sum<-rowSums(probs2_temp[,2:ncol(probs2_temp)])
        #   probs2<-probs2_temp[,2:ncol(probs2_temp)]/row_sum
        #   probs2<-cbind(probs2_temp[,1],probs2)
        #   
        #   probs3 <- probs1
        #   ndoses <- as.integer(input[[paste0('vac_dose.T',i,sep="")]])
        #   vaccinePrice.md <- as.numeric(unlist(na.omit(subset(scenario_params,select="quadri_medical_cost"))))
        #   vaccinePrice.nmd <- as.numeric(unlist(na.omit(subset(scenario_params,select="quadri_non_medical_cost"))))
        #   vaccinePrice.i <- as.numeric(unlist(na.omit(subset(scenario_params,select="quadri_indirect_cost"))))
        #   vaccineCova <- as.double(input[[paste0('vaccov.T',i,sep="")]])
        # }
        # if(input[[paste0("vac_type",i,sep="")]]=='Nonavalent'){
        #   probs2_temp <- probs1
        #   probs2_temp[,3]<-(1- as.double(input[[paste0('efficacy',i,sep="")]])) * probs2_temp[,3]
        #   row_sum<-rowSums(probs2_temp[,2:ncol(probs2_temp)])
        #   probs2<-probs2_temp[,2:ncol(probs2_temp)]/row_sum
        #   probs2<-cbind(probs2_temp[,1],probs2)
        #   
        #   probs3 <- probs1
        #   ndoses <- as.integer(input[[paste0('vac_dose.N',i,sep="")]])
        #   vaccinePrice.md <- as.numeric(unlist(na.omit(subset(scenario_params,select="nona_medical_cost"))))
        #   vaccinePrice.nmd <- as.numeric(unlist(na.omit(subset(scenario_params,select="nona_non_medical_cost"))))
        #   vaccinePrice.i <- as.numeric(unlist(na.omit(subset(scenario_params,select="nona_indirect_cost"))))
        #   vaccineCova <- as.double(input[[paste0('vaccov.N',i,sep="")]])
        # }
      }
      if(input[[paste0("vac_check",i,sep="")]]==FALSE){
        probs2 <- NULL
        probs3 <- NULL
        ndoses <- 0
        vaccinePrice.md <- 0
        vaccinePrice.nmd <- 0
        vaccinePrice.i <- 0
        vaccineCova <- NULL}
      
      if(input[[paste0("via_check",i,sep="")]]==TRUE){
        via <- TRUE
        screening <- FALSE
        ScreenType <- 0
        screenPeriod <-0 
        screenCoverage <- NULL
        scr_prob <- NULL
        viaCoverage <- as.numeric(unlist(na.omit(subset(scenario_params,select=7))))
        viaSensi <- as.numeric(unlist(na.omit(subset(scenario_params,select=8))))
        viaProbs <- as.numeric(unlist(na.omit(subset(scenario_params,select=9))))
        viaPeriod <- as.numeric(input[[paste0("vperiod",i,sep="")]])
        via.md <- as.numeric(unlist(na.omit(subset(scenario_params,select=16))))
        via.nmd <- as.numeric(unlist(na.omit(subset(scenario_params,select=17))))
        via.i <- as.numeric(unlist(na.omit(subset(scenario_params,select=18))))
        
        if(nlevels(probs1[,1]) != length(viaCoverage))return("Length of VIA Coverage vector must be the same as number of groups of age in Matrix of transition probabilities without medical intervention ")
        if(ncol(probs1)-1 != length(viaSensi))return("Length of VIA Sensitivity vector must be the same as number of health states in Matrix of transition probabilities without medical intervention")
        if(ncol(probs1)-1 != length(viaProbs))return("Length of VIA Probs vector must be the same as number of health states in Matrix of transition probabilities without medical intervention ")
      }
      if(input[[paste0("via_check",i,sep="")]]==FALSE){
        via <- FALSE
        viaCoverage <- NULL
        viaSensi <- NULL
        viaProbs <- NULL
        viaPeriod <- 0
        via.md <- 0
        via.nmd <- 0
        via.i <- 0}
      
    }else{
      
      dnaScreening <- FALSE
      dnaScSensi <- NULL
      dnaScCost.md <- 0
      dnaScCost.nmd <- 0
      dnaScCost.i <- 0
      papScSensi <- NULL
      dnaScAgeGroups <- NULL
      
      screening <- FALSE
      ScreenType <- 0 
      screenPrice.md <- 0
      screenPrice.nmd <- 0
      screenPrice.i <- 0
      screenCoverage <- NULL 
      screenSensi <- NULL
      scr_prob <- NULL
      dnaScreening <- FALSE
      dnaScSensi <- NULL
      dnaScCost.md <- 0
      dnaScCost.nmd <- 0
      dnaScCost.i <- 0
      papScSensi <- NULL
      dnaScAgeGroups <- NULL
      screeningPeriod<-3
      
      probs2 <- NULL
      probs3 <- NULL
      ndoses <- 0
      vaccinePrice.md <- 0
      vaccinePrice.nmd <- 0
      vaccinePrice.i <- 0
      vaccineCova <- NULL
      
      via <- FALSE
      viaCoverage <- NULL
      viaSensi <- NULL
      viaProbs <- NULL
      viaPeriod <- 0
      via.md <- 0
      via.nmd <- 0
      via.i <- 0
    }
    esc <- simCohort(probs1 = probs1[, 1:13], probs2= probs2[, 1:13], probs3 = probs3, stopYear = 85, stepTime = 1,
                     M = 1e5, Nsim = 5,
                     screening = screening, screenCoverage = screenCoverage, screenSensi = screenSensi, screenProbs = scr_prob,
                     figoSymProb = figoSymProb, vaccinePrice.md = vaccinePrice.md, vaccinePrice.nmd = vaccinePrice.nmd, vaccinePrice.i = vaccinePrice.i, 
                     screenPrice.md = screenPrice.md, screenPrice.nmd = screenPrice.nmd, screenPrice.i = screenPrice.i, 
                     vaccineCov=vaccineCova, costCoefs.md = medi, costCoefs.nmd = nomedi,
                     costCoefs.i = indi, utilityCoefs = utility, screenPeriod = screeningPeriod, seed = 1234, dnaScreening = dnaScreening,  
                     dnaScSensi = dnaScSensi, dnaScCost.md = dnaScCost.md, dnaScCost.nmd = dnaScCost.nmd, dnaScCost.i = dnaScCost.i, papScSensi = papScSensi,
                     dnaScAgeGroups = dnaScAgeGroups, 
                     ScreenType = ScreenType, OpScreen = OpScreen, ndoses=ndoses, 
                     via = via, via.Sensi = viaSensi, viaCoverage = viaCoverage, viaProbs = viaProbs, viaPeriod = viaPeriod, via.md=via.md, via.nmd=via.nmd, via.i=via.i)
    
    res1 <- simCohort(probs1 = probs1[, 1:13], probs2= probs2[, 1:13], probs3 = probs3, stopYear = 85, stepTime = 1,
                      M = 1e5, Nsim = 5, 
                      screening = screening, screenCoverage = screenCoverage, screenSensi = screenSensi, screenProbs = scr_prob,
                      figoSymProb = figoSymProb, vaccinePrice.md = vaccinePrice.md, vaccinePrice.nmd = vaccinePrice.nmd, vaccinePrice.i = vaccinePrice.i, 
                      screenPrice.md = screenPrice.md, screenPrice.nmd = screenPrice.nmd, screenPrice.i = screenPrice.i, 
                      vaccineCov=vaccineCova, costCoefs.md = medi, costCoefs.nmd = nomedi,
                      costCoefs.i = indi, utilityCoefs = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1), screenPeriod = screeningPeriod, seed = 1234, dnaScreening = dnaScreening,  
                      dnaScSensi = dnaScSensi, dnaScCost.md = dnaScCost.md, dnaScCost.nmd = dnaScCost.nmd, dnaScCost.i = dnaScCost.i, papScSensi = papScSensi,
                      dnaScAgeGroups = dnaScAgeGroups, 
                      ScreenType = ScreenType, OpScreen = OpScreen, ndoses=ndoses, 
                      via = via, via.Sensi = viaSensi, viaCoverage = viaCoverage, viaProbs = viaProbs, viaPeriod = viaPeriod, via.md=via.md, via.nmd=via.nmd, via.i=via.i)
    
    res <- mean(apply(attr(res1, "utility"), 1, sum))/attr(res1, "Call")$M
    
    disc.ratio0 <- c(lemodif(esc, res),
                     qalysstrategy(esc),
                     qalysperson(esc),
                     medstrat(esc),
                     medpers(esc),
                     nomedstrat(esc),
                     nomedpers(esc),
                     indstrat(esc),
                     indpers(esc),
                     totalstrat(esc),
                     totalpers(esc))
    
    disc.ratio3 <- c(lemodif(esc, res, discount),
                     qalysstrategy(esc,discount),
                     qalysperson(esc,discount),
                     medstrat(esc,discount),
                     medpers(esc,discount),
                     nomedstrat(esc,discount),
                     nomedpers(esc,discount),
                     indstrat(esc,discount),
                     indpers(esc,discount),
                     totalstrat(esc,discount),
                     totalpers(esc,discount))
    
    names <- c("Life expectancy","Total QALYs per strategy", "Total QALYs per person", "Total medical cost per strategy", "Total medical cost per person",
               "Total non medical direct cost per strategy", "Total non medical direct cost per person", "Total indirect cost per strategy",
               "Total indirect cost per person", "Total cost per strategy", "Total cost per person")
    
    result <- matrix(cbind(as.numeric(disc.ratio0), as.numeric(disc.ratio3)),dimnames = list(names,c( "Disc.ratio = 0", paste0("Disc.ratio = ",discount))), ncol=2)
    result <- matrix(result[, 2], dimnames = list(names,c(paste0("Disc.ratio = ",discount))), ncol=1)
    row.names(result) <- names
    return(result)
    })
  }
  
  read_main_file_for_slider<-function(){
    if(is.null(input$probs1)){
      probs1<-read.xlsx(paste0(getwd(),"/data examples/Cost-Effectiveness/calibrated_matrix.xls"),1)[, 1:13]
    }else{
      probs1 <- read.xlsx(input$probs1$datapath,sheetIndex = 1)[, 1:13]
    }
    texti<-as.character(unique(probs1[,1]))
    partir<-strsplit(texti,"-")
    return(partir)
  }

  output$b_trans_prob <- downloadHandler(
    filename = function() { paste("Transition Probabilities", "xls", sep='.') },
    content = function(file) {
      fname <- paste(file,"xls",sep=".")
        wb <- xlsx::createWorkbook(type="xls")
        nsheets <- length(excel_sheets(paste0(getwd(),"/data examples/Calibration/Transition Probabilities.xls")))
        for (i in 1:nsheets)
        {
          assign(paste0('Sheet.', i, sep = ''), as.data.frame(read.xlsx(paste0(getwd(),"/data examples/Calibration/Transition Probabilities.xls"), i),
                                                            optional=FALSE,fix.empty.names=FALSE,stringsAsFactors=FALSE))
          sname <- paste("Sheet_", i, sep='')
          sheet <- xlsx::createSheet(wb, sheetName = sname)
          addDataFrame(read.xlsx(paste0(getwd(),"/data examples/Calibration/Transition Probabilities.xls"), i), sheet=sheet,startColumn = 1,row.names = FALSE)
        }
        xlsx::saveWorkbook(wb,file)
    },
    contentType = "application/xlsx" 
  ) 
  output$b_inc <- downloadHandler(
    filename = function() { paste("CC Observed Incidence", "xls", sep='.') },
    content = function(file) {
      fname <- paste(file,"xls",sep=".")
      wb <- xlsx::createWorkbook(type="xls")
      assign(paste0('Sheet.', 1, sep = ''), as.data.frame(read.xlsx(paste0(getwd(),"/data examples/Calibration/CC Observed Incidence.xls"),1),
                                                          optional=FALSE,fix.empty.names=FALSE,stringsAsFactors=FALSE))
      sname <- paste("Sheet_", 1, sep='')
      sheet <- xlsx::createSheet(wb, sheetName = sname)
      addDataFrame(read.xlsx(paste0(getwd(),"/data examples/Calibration/CC Observed Incidence.xls"),1), sheet=sheet,startColumn = 1,row.names = FALSE)
      xlsx::saveWorkbook(wb,file)
    },
    contentType = "application/xlsx" 
  ) 
  output$b_prev <- downloadHandler(
    filename = function() { paste("HPV Observed Prevalence", "xls", sep='.') },
    content = function(file) {
      fname <- paste(file,"xls",sep=".")
      wb <- xlsx::createWorkbook(type="xls")
      assign(paste0('Sheet.', 1, sep = ''), as.data.frame(read.xlsx(paste0(getwd(),"/data examples/Calibration/HPV Observed Prevalence.xls"),1),optional=FALSE,fix.empty.names=FALSE,stringsAsFactors=FALSE))
      sname <- paste("Sheet_", 1, sep='')
      sheet <- xlsx::createSheet(wb, sheetName = sname)
      addDataFrame(read.xlsx(paste0(getwd(),"/data examples/Calibration/HPV Observed Prevalence.xls"),1), sheet=sheet,startColumn = 1,row.names = FALSE)
      xlsx::saveWorkbook(wb,file)
    },
    contentType = "application/xlsx" 
  ) 
  output$b_mort <- downloadHandler(
    filename = function() { paste("CC Observed Mortality", "xls", sep='.') },
    content = function(file) {
      fname <- paste(file,"xls",sep=".")
      wb <- xlsx::createWorkbook(type="xls")
      assign(paste0('Sheet.', 1, sep = ''), as.data.frame(read.xlsx(paste0(getwd(),"/data examples/Calibration/CC Observed Mortality.xls"),1),optional=FALSE,fix.empty.names=FALSE,stringsAsFactors=FALSE))
      sname <- paste("Sheet_", 1, sep='')
      sheet <- xlsx::createSheet(wb, sheetName = sname)
      addDataFrame(read.xlsx(paste0(getwd(),"/data examples/Calibration/CC Observed Mortality.xls"),1), sheet=sheet,startColumn = 1,row.names = FALSE)
      xlsx::saveWorkbook(wb,file)
    },
    contentType = "application/xlsx" 
  ) 
  
  output$b_gen_costs_matrix <- downloadHandler(
    filename = function() { paste("General_costs", "xls", sep='.') },
    content = function(file) {
      fname <- paste(file,"xls",sep=".")
      wb <- xlsx::createWorkbook(type="xls")
      assign(paste0('Sheet.', 1, sep = ''), as.data.frame(read.xlsx(paste0(getwd(),"/data examples/Cost-Effectiveness/General_costs.xlsx"),1),optional=FALSE,fix.empty.names=FALSE,stringsAsFactors=FALSE))
      sname <- paste("Sheet_", 1, sep='')
      sheet <- xlsx::createSheet(wb, sheetName = sname)
      addDataFrame(read.xlsx(paste0(getwd(),"/data examples/Cost-Effectiveness/General_costs.xlsx"),1), sheet=sheet,startColumn = 1,row.names = FALSE)
      xlsx::saveWorkbook(wb,file)
    },
    contentType = "application/xls" 
  )
  observe({
    isolate({
      lapply(1:input$numb_scen, function(i){
        output[[paste0("b_Opscreen",as.integer(i),sep="")]]<-downloadHandler(
          filename = function() { paste("op screening", "xls", sep='.') },
          content = function(file) {
            fname <- paste(file,"xls",sep=".")
            wb <- xlsx::createWorkbook(type="xls")
            assign(paste0('Sheet.', i, sep = ''), as.data.frame(read.xlsx(paste0(getwd(),"/data examples/Cost-Effectiveness/op_screening.xlsx"),1),optional=FALSE,fix.empty.names=FALSE,stringsAsFactors=FALSE))
            sname <- paste("Sheet_", i, sep='')
            sheet <- xlsx::createSheet(wb, sheetName = sname)
            addDataFrame(read.xlsx(paste0(getwd(),"/data examples/Cost-Effectiveness/op_screening.xlsx"),1), sheet=sheet,startColumn = 1,startRow = 1,row.names = FALSE)
            xlsx::saveWorkbook(wb,file)
          },
          contentType = "application/xls" 
        )
        output[[paste0("b_Scenario_costs_and_probs",as.integer(i),sep="")]]<-downloadHandler(
          filename = function() { paste("Scenario Specific Parameters", "xls", sep='.') },
          content = function(file) {
            fname <- paste(file,"xls",sep=".")
            wb <- xlsx::createWorkbook(type="xls")
            assign(paste0('Sheet.', i, sep = ''), as.data.frame(read.xlsx(paste0(getwd(),"/data examples/Cost-Effectiveness/scenario1.xlsx"),1),optional=FALSE,fix.empty.names=FALSE,stringsAsFactors=FALSE))
            sname <- paste("Sheet_", i, sep='')
            sheet <- xlsx::createSheet(wb, sheetName = sname)
            addDataFrame(read.xlsx(paste0(getwd(),"/data examples/Cost-Effectiveness/scenario1.xlsx"),1), sheet=sheet,startColumn = 1,row.names = FALSE)
            xlsx::saveWorkbook(wb,file)
          },
          contentType = "application/xls" 
        )
      })
    })
  })
  parameters_value_storing<-function(i=1,natural_history=FALSE,screening=FALSE,stype=1,sPeriod=3,dnaScreen=FALSE,dnaScagemin=5,dnaScagemax=30,via_check=FALSE,via_period=5,vaccination=FALSE,efficacy=0.7,vac_type="Bivalent",ndosis=3,cov=0.828,discount=3,uncertainty=0){
    matrix_parameters<-matrix(byrow=TRUE,ncol=1)
    if(file.exists("parameters_list.RData")){
      load("parameters_list.RData")
    }else{
      parameters_list<-list()
    }
    if(natural_history==FALSE){
      if(screening==TRUE){
        matrix_parameters<-"Screening"
        if(stype==1){
          temp_matrix<-matrix(data="Organized Screening",byrow = TRUE,ncol=1)
          matrix_parameters<-rbind(matrix_parameters,temp_matrix)
        }else{
          temp_matrix<-matrix(data="Organized Screening",byrow = TRUE,ncol=1)
          matrix_parameters<-rbind(matrix_parameters,"Opportunistic Screening")
        }
        temp_matrix<-matrix(data=paste0("Screening Period: ",as.character(sPeriod)),byrow = TRUE,ncol=1)
        matrix_parameters<-rbind(matrix_parameters,temp_matrix)
        if(dnaScreen==TRUE){
          temp_matrix<-matrix(data=c("DNA-HPV Screening",paste0("DNA-HPV Screening Min Age: ",as.character(dnaScagemin)),paste0("DNA-HPV Screening Max Age: " ,as.character(dnaScagemax))),byrow = TRUE,ncol=1)
          matrix_parameters<-rbind(matrix_parameters,temp_matrix)
        }
        if(via_check==TRUE){
          temp_matrix<-matrix(data=c("VIA",paste0("VIA Period: ", as.character(via_period))),byrow = TRUE,ncol=1)
          matrix_parameters<-rbind(matrix_parameters,temp_matrix)
        }
      }
      if(vaccination==TRUE){
        temp_matrix<-matrix(data=c("Vaccination",paste0("Efficacy: " ,as.character(efficacy)),paste0("Vaccine Type: " ,as.character(vac_type)),paste0("N Doses: ",as.character(ndosis)),paste0("Coverage: ",as.character(cov))),byrow = TRUE,ncol=1)
        matrix_parameters<-rbind(matrix_parameters,temp_matrix)
      }
    }
    temp_matrix<-matrix(data=c(paste0("Discount: ",as.character(discount)),paste0("Uncertainty: ",as.character(uncertainty))),byrow = TRUE,ncol=1)
    matrix_parameters<-rbind(matrix_parameters,temp_matrix)
    matrix_parameters<-na.omit(matrix_parameters)
    rownames(matrix_parameters)<-c()
    parameters_list[[paste0("Scenario",i,sep="")]]<-matrix_parameters
    save(parameters_list,file="parameters_list.RData")
  }
})
