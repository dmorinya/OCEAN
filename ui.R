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
source("helpers/helpers.R")
shinyUI(fluidPage(shinyjs::useShinyjs(), shinyalert::useShinyalert(),
                  tagList(
                    # link js
                    tags$head(tags$link(includeScript("link.js"))),
                    tags$head(tags$style("a{cursor:pointer;}"))),
   navbarPage(title="OCEAN",
    tabPanel("Welcome and Overview", icon=icon("home",lib="glyphicon"),
             h3("Welcome"),
             p("Welcome to the Online Cost-Effectiveness ANalysis (OCEAN) tool, featuring an easy-to-use web interface 
               providing health professionals, researchers and decision makers involved in cervical cancer prevention 
               programmes with a useful and quick tool to conduct complex cost-effectiveness analyses, which are becoming 
               an essential tool in the last years as an approach for supporting decision-making that involve important 
               trade-offs. The tool is based in a discrete-time, stochastic Markov chain model programmed in R and the 
               web interface is constructed over the shiny package. The users can run sophisticated cost-effectiveness 
               evaluation of cervical cancer prevention strategies without deep knowledge of the underlying mathematical 
               model or any software, obtaining the most relevant costs and health outcomes in a convenient way."),
             p("A video tutorial illustrating the usage of the tool can be found on the ", 
               HTML("<a onclick=","customHref('video')" ,">",
                    "video tutorial","</a>"), " tab and on ",
               HTML("<a href=","https://youtu.be/pIH6ke5VRD0" ,">",
                    "YouTube","</a>.")),
             h3("Overview of the underlying model"),
             p("The underlying model is a discrete-time, stochastic Markov chain model that simulates the natural history 
               of HPV infection and cervical cancer. The basic model consists of 12 mutually exclusive and collectively 
               exhaustive health states (Figure 1) [healthy, HPV infection, CIN1–3 lesions, International Federation of 
               Gynecology and Obstetrics (FIGO) cervical cancer stages, cancer survival, cervical cancer death, and death from other causes]. 
               Death states (both from cervical cancer and other causes) reflect country-specific female mortality stratified by age. 
               This closed model follows a single cohort of 11-year-old girls until they reach the age of 85 years or death 
               using equal 1-year increments, where every woman has her own probability of progressing, regressing, or
               remaining at the same health state."), 
            p("All women start model simulations as healthy and can move to the HPV-infected state by acquiring the infection 
               with certain probability. If a woman shows clearance of the infection, she will regress to the healthy state 
               and then, reinfection is possible. If the infection persists, the woman will move into the CIN1 state and may 
               then progress to CIN2 and later to CIN3 and cancer, or can regress and show clearance of the infection. 
               Once in the cancer state, a woman may not regress to other health states, and instead progresses through the 
               four stages of cancer according to the FIGO classification. A woman may die from cervical cancer if she belongs 
               to the cancer stages or may die at any time from other noncervical cancer cause. Nonetheless, every woman has a 
               certain probability of developing symptoms and receiving treatment. After treatment, a woman can return to the 
               healthy state – if she belonged to one of the CIN2–3 states – or go to the cancer survival state – if she 
               belonged to one of the FIGO states."),
             p(style="text-align:center;", img(src="model.png", alt="Model schema", align="middle", class="center", height = 400, width = 700)),
             HTML("<div class='caption', style='text-align:center;'>Figure 1: Diagram of the Markov model that reproduces the natural history of cervical cancer.</div>"),
             h3("Calibration"),
             p("The goal of the ", 
               HTML("<a onclick=","customHref('calib')" ,">",
                    "calibration part","</a>"), " is to provide the user with a reliable transition 
               probabilities matrix adjusted to the specific setting to feed the Markov model that will be used 
               in the cost-effectiveness analyses. In a Markov model, the transition probabilities matrix contains the probabilities
               of moving from one state to another (or remaining on a state) after a step. Notice that each row in the
               transition probabilities matrix has to sum 1. In our case, this matrix is age-specific,
               so it has 12 columns (health states) and 180 rows (12 health states x 15 age groups). As these probabilities are
               generally obtained from a literature review and affected by some degree of uncertainty, several matrices can be used
               (uploaded in several sheets on the same file) to obtain the cost-effectiveness analysis results accounting for this 
               uncertainty. For instance, if 5 matrices are kept in the calibration tab and some degree of uncertainty is desired 
               ", HTML("(<em>Uncertainty Level</em>"), " input in the Cost-effectiveness tab is not zero) these 5 matrices are used and the
               cost-effectiveness outputs are computed for each, and then the percentiles corresponding to the selected uncertainty
               level are obtained and displayed for each output."),
             h3("Cost-effectiveness analysis"),
             p("The goal of the ", 
               HTML("<a onclick=","customHref('cea')" ,">",
                    "cost-effectiveness tab","</a>"), " is to provide a user-friendly framework to evaluate the cost-effectiveness
               of different cervical cancer prevention strategies once a calibrated transition probabilities matrix is obtained
               for the user specific data. A default transition probabilities matrix can also be used if no specific data is 
               available.")#,
            #h3("Contact"),
            #p("Contact the authors to get advice: "),
            #p("David Moriña, Barcelona Graduate School of Mathematics (BGSMath), Departament de Matemàtiques, Universitat Autònoma de Barcelona",
            #HTML("(<a href='mailto:david.morina@uab.cat'>david.morina@uab.cat</a>)")), HTML("<br>"),
            #p("Mireia Diaz, Unit of Infections and Cancer (UNIC-I&I), Cancer Epidemiology Research programme (CERP), Institut Català d’Oncologia (ICO) - IDIBELL, L'Hospitalet de Llobregat, Barcelona, Spain and Centro de Investigación Biomédica en Red (CIBERONC), Madrid, Spain",
            #HTML("(<a href='mailto:mireia@iconcologia.net'>mireia@iconcologia.net</a>)")),
            #h3("Acknowledgements"),
            #p("The research leading to these results has received funding from RecerCaixa (2015ACUP00129). This work was partially supported by grants from the Instituto de Salud Carlos III-ISCIII (Spanish Government), CIBERESP (CB06/02/0073) and CIBERONC (CB16/12/00401) co-funded by “FEDER” Funds/European Regional Development Fund (ERDF)-A Way to Build Europe [PI11/02090, PIE16/00049, PI16/01254]; and with the support of the Secretariat for Universities and Research of the Department of Business and Knowledge of the Government of Catalonia. Grants to support the activities of research groups (SGR 2017-2019). Grant number 2017SGR1718 and 2017SGR1085. We thank CERCA Programme/Generalitat de Catalunya for institutional support."),
            #img(src="LogoICO.jpg", align="left", height = 88.2, width = 1026.9)
             ),
     tabPanel("Calibration", value="calib", icon=icon("screenshot",lib="glyphicon"),
       sidebarPanel( 
       # AQUI ES ON HE AFEGIT EL DESPLEGABLE AMB "SELECT INPUT"
          #selectInput("cancertype", "Select the type of cancer", c("Cervical Cancer", "Colon Cancer", "Uterus Cancer"), selected = "Cervical Cancer"),
         #selectInput("cancertype", "Select the type of cancer", c("Cervical Cancer"), selected = "Cervical Cancer"),
       # NOMBRE DE SIMULACIONS = N            
          sliderInput("N", "Number of simulations :", min = 2, max = 500, value = 10),
       # NOMBRE DE SIMULACIONS A GUARDAR = keep
          sliderInput("keep", "Number of simulations to keep :", min = 2, max = 500, value = 5, step = 1),
       # PERCENTATGE DE CANVI = p.change
          sliderInput("p.change", "Percentage of change (in %) :", min = 1, max = 100, value = 10, step = 1),
       # MORTALITAT = mort_check
          checkboxInput("mort_check", "Mortality"),
       # OPTIMITZACIO = opt
       #checkboxInput("opt", "Optimization"),
       # MATRIU DE TRANSICIO = prob                              
        fluidRow(column(10,offset = 0,fileInput('prob', 'Load transition probabilities file',accept = vectorformato)),
                 column(2,offset = 0,style='margin-top:25px',downloadButton("b_trans_prob",label="",icon=icon("cloud-download",lib="glyphicon")))
        ),  
       # MATRIU D'INDCIDENCIA = load_m_inc   
       fluidRow(column(10,offset = 0,fileInput('load_m_inc', 'Load incidence file (x 100,000 women)',accept = vectorformato)),
                column(2,offset = 0,style='margin-top:25px',downloadButton("b_inc",label="",icon=icon("cloud-download",lib="glyphicon")))
       ),  
       
       # MATRIU DE PREVALENCIA = load2                              
          
       fluidRow(column(10,offset = 0,fileInput('load_m_prev', 'Load prevalence file',accept = vectorformato)),
                column(2,offset = 0,style='margin-top:25px',downloadButton("b_prev",label="",icon=icon("cloud-download",lib="glyphicon")))
       ),  
       # MATRIU DE MORTALITAT = mort // INPUT = mort_check==true                              
          conditionalPanel(condition="input.mort_check==true",
                fluidRow(column(10,offset = 0,fileInput('mort', 'Load mortality file (x 100,000 women)',accept = vectorformato)),
                         column(2,offset = 0,style='margin-top:25px',downloadButton("b_mort",label="",icon=icon("cloud-download",lib="glyphicon")))
                )
          ),
       # BOTO GO! = b_calibration                               
      withBusyIndicatorUI(actionButton("b_calibration", "Go!", icon=icon("hand-right", lib="glyphicon"))),
       # BOTO DOWNLOAD = DownloadButton                             
      conditionalPanel(condition="input.b_calibration!=0", withBusyIndicatorUI(downloadButton("b_d_matrix_calib", "Download calibrated matrix", icon=icon("download-alt", lib="glyphicon")))),
      actionButton("reset_calibration", "Show/Hide help", icon=icon("info-sign", lib="glyphicon"))#,
      # tags$div(tags$p("")),
      # tags$div(class="header", checked=NA,
      #         tags$p("Contact the authors to get advice:"),
      #        tags$a(href="mailto:david.morina@uab.cat", "david.morina@uab.cat"),
      #        tags$a(href="mailto:mireia@iconcologia.net", "mireia@iconcologia.net"))),
       ),
      mainPanel(conditionalPanel(condition="0==0", uiOutput("warning"))),
      mainPanel(conditionalPanel(condition="input.b_calibration==0 & input.reset_calibration==0", HTML("<h3>Calibration help</h3>", "<p>The calibration panel needs the following inputs:</p>"),
                                 HTML("<ul>", "<li>", "Number of simulations: Number of simulated cohorts.", "</li>",
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
                                      "</ul>"),
                                 p("An Excel file including all calibrated matrices can be downloaded and used in the cost-effectiveness part.")),
                conditionalPanel(condition="input.b_calibration!=0 | (input.b_calibration==0 & input.reset_calibration!=0)", uiOutput("HelpBox")),
        tags$style(type="text/css",  ".shiny-output-error {visibility: hidden;}", ".shiny-output-error:before {visibility: hidden;}",".shiny-progress .progress {height:7px;}"),
                            conditionalPanel(condition="input.b_calibration!=0", plotOutput('validaPlot'))#,
                            #img(src="LogoICO.jpg", align="left", height = 88.2, width = 1026.9)
      )
     ),
     ######################## COST-EFFECTIVENESS ##########################
     tabPanel("Cost-effectiveness", value="cea", icon=icon("stats",lib="glyphicon"),
        sidebarPanel(
          # NUMBER OF SCENARIOS = nscenarios
          textInput('numb_scen', 'Number of different scenarios', value=1),
          # MATRIX OF TRANSITION = probs1 
          conditionalPanel(condition="input.b_calibration==0",
                           fileInput('probs1', 'Matrix of transition probabilities between different health states without medical intervention',accept = vectorformato)),
          fixedRow(column(10,offset = 0,fileInput('gen_costs_matrix', 'Medical Costs and Utility coefficients',accept = vectorformato)),
                   column(2,offset = 0,style='margin-top:25px',downloadButton("b_gen_costs_matrix",label="",icon=icon("cloud-download",lib="glyphicon")))
          ),
          
          uiOutput("panel"),
                                      
          # BOTO GO 2= GoButton1
          textInput('discount_ratio', 'Discount Rate', value=3),
         # checkboxInput("uncertainty","Include uncertainty interval around the cost-effectiveness outcomes?",value=FALSE),
          sliderInput("uncertainty_level", "Uncertainty Level", min = 0, max = 50, value = 0,step = 5),
          withBusyIndicatorUI(actionButton("b_cost_effect", "Go!", icon=icon("hand-right", lib="glyphicon"))),
          # BOTO DOWNLOAD 2= DownloadButton                             
          conditionalPanel(condition="input.b_cost_effect!=0", withBusyIndicatorUI(downloadButton("b_down_cost_effect_results", "Download CEA results", icon=icon("download-alt", lib="glyphicon")))),
         actionButton("reset_cea", "Show/Hide help", icon=icon("info-sign", lib="glyphicon"))#,
         # tags$div(tags$p("")),
         # tags$div(class="header", checked=NA,
         #        tags$p("Contact the authors to get advice:"),
         #        tags$a(href="mailto:david.morina@uab.cat", "david.morina@uab.cat"), HTML("<br>"),
         #        tags$a(href="mailto:mireia@iconcologia.net", "mireia@iconcologia.net"))), 
        ), 
        mainPanel(conditionalPanel(condition="0==0", uiOutput("warning2"))),
        mainPanel(
             #verbatimTextOutput("text1"), ### Number of scenarios considered
           conditionalPanel(condition="input.b_cost_effect==0 & input.reset_cea==0", HTML("<h3>Cost-effectiveness help</h3>", "<p>The cost-effectiveness panel needs the following inputs:</p>"),
                            HTML("<ul>", "<li>", "Number of different scenarios: The number of scenarios to be analysed in the current session. Note that each scenario may include none (no intervention or natural history), one or more prevention strategies (screening alone, vaccination alone or vaccination followed by screening).", "</li>",
                                 "<li>", "Matrix of transition probabilities between different health states without medical intervention: Transition probability matrix obtained in the previous panel.", "</li>", 
                                 "<li> Medical costs and utility coefficients: An Excel file containing the treatment direct medical and non-medical costs, indirect costs and utility coefficients. An example can be downloaded from the tool.</li>
                                 <li> Discount rate: The undiscounted outcomes are given by default, and additionally a discount rate can be applied to health and costs outcomes by selecting the desired discount rate on this input.</li>
                                 <li> Uncertainty level: It is known that the results of cost-effectiveness analyses are affected by a certain degree of uncertainty at different levels. That can be reflected in the OCEAN tool by using more than one transition probabilities matrices to feed the Markov model. If a file with several sheets is used and the uncertainty level is set to 0, an averaged matrix will be used and only point estimates will be reported. If the uncertainty level is set to a value α between 0 and 50, all the matrices are used and the outcomes obtained from each one are recorded. Then, the mean and percentiles α/2 and 1-α/2 for each outcome are calculated and reported.</li>",
                                 "</ul>"),
                            p("The considered prevention strategies that can be selected for each scenario (alone or combined) include:"),
                            HTML("<ul>", 
                                 "<li> Natural history: No prevention strategy is considered in this scenario, which reproduces the natural history of HPV infection and cervical cancer. If this option is checked, the other options disappear.</li>
                                  <li> Screening: Screening scenarios may differ by screening test (cytology, HPV DNA test or visual inspection), frequency (every 1-10 years), targeted ages, and switch age from cytology to HPV testing. The screening coverage, positive predictive value, sensitivity and costs are read from the Excel file loaded as “Scenario specific values”. The structure of this file can be seen by downloading the example file from the tool. Screening may be organized (all women are screened with the selected frequency) or opportunistic (the screening period is variable). To set an opportunistic screening scenario, an additional Excel file specifying the proportion of women screened each period is needed.</li>
                                  <li> Vaccination: We assume that preadolescent girls are successfully vaccinated at the age of 11 years with one, two or three doses of the vaccine against HPV types 16 and 18. Efficacy and coverage are set by the user and vaccination costs are read from the Excel file loaded as “Scenario specific values”. The structure of this file can be seen by downloading the example file from the tool. Currently, only bivalent vaccine is considered but it is planned that quadrivalent and nonavalent vaccines will be available soon as well.</li> </ul>
"),
                            p("An Excel file with all generated results can be downloaded, including information about the inputs used to generate those particular results.")),
           conditionalPanel(condition="input.b_cost_effect!=0 | (input.b_cost_effect==0 & input.reset_cea!=0)", uiOutput("HelpBox2")),
                conditionalPanel(condition="input.b_cost_effect!=0", uiOutput("Scenario_table"))#,
             #img(src="LogoICO.jpg", align="left", height = 88.2, width = 1026.9)
         )
      ),
     tabPanel("Video tutorial", value="video", icon=icon("film",lib="glyphicon"),
              mainPanel(uiOutput("video"),
                        p("Notice that the file referred as 'scenario screening.xls' is available as 'Scenario Specific Parameters.xls' and includes the input parameters
                          used in an analysis.")#,
                                 # img(src="LogoICO.jpg", align="left", height = 88.2, width = 1026.9), tags$br(),
                                 # tags$br(), tags$br(), tags$br()),
                                 # tags$div(tags$br(), class="header", checked=NA, align="left",
                                 #        tags$p("Contact the authors to get advice:"),
                                 #        tags$a(href="mailto:david.morina@uab.cat", "david.morina@uab.cat"), HTML("<br>"),
                                 #        tags$a(href="mailto:mireia@iconcologia.net", "mireia@iconcologia.net"))
              )
     )
    )
  )
)

