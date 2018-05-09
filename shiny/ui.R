
shinyUI(navbarPage(
  "Puolueluokitin (Kunnallisvaalit 2017)",
  tabPanel("Luokitin",
           fluidPage(
             titlePanel("Puolueluokitin Ylen vaalikonevastausten perusteella"),
             sidebarLayout(
               mainPanel(
                 wellPanel(
                   "Valitse jonkin ehdokkaan vastaukset
                   klikkaamalla listaa. Kuvaaja näyttää arvioidun todennäköisyyden olla puolueiden ehdokas annetuilla vastauksilla.
                   Voit muokata vastauksia. Listassa ehdokkaan todellinen puolue. Mukana on Ylen vaalikonedatasta 
                   17842 vastaajaa."
                 ),
                 fluidRow(
                   plotOutput("plot")
                   ),
                 fluidRow(
                   hr(),
                   "Klikkaa ehdokasta",
                   br(),
                   br(),
                   DT::dataTableOutput("table", width = "99%", height = "auto")
                 )
                 ),
               sidebarPanel(
                 titlePanel("Kysymykset ja vastaukset"),
                 radioButtons(
                   "q1",
                   question[1],
                   selected = ex[1],
                   inline = TRUE,
                   width = NULL,
                   choiceNames = X.choice.labels,
                   choiceValues = X.choice.values
                 ),
                 radioButtons(
                   "q2",
                   question[2],
                   selected = ex[2],
                   inline = TRUE,
                   width = NULL,
                   choiceNames = X.choice.labels,
                   choiceValues = X.choice.values
                 ),
                 radioButtons(
                   "q3",
                   question[3],
                   selected = ex[3],
                   inline = TRUE,
                   width = NULL,
                   choiceNames = X.choice.labels,
                   choiceValues = X.choice.values
                 ),
                 radioButtons(
                   "q4",
                   question[4],
                   selected = ex[4],
                   inline = TRUE,
                   width = NULL,
                   choiceNames = X.choice.labels,
                   choiceValues = X.choice.values
                 ),
                 radioButtons(
                   "q5",
                   question[5],
                   selected = ex[5],
                   inline = TRUE,
                   width = NULL,
                   choiceNames = X.choice.labels,
                   choiceValues = X.choice.values
                 ),
                 radioButtons(
                   "q6",
                   question[6],
                   selected = ex[6],
                   inline = TRUE,
                   width = NULL,
                   choiceNames = X.choice.labels,
                   choiceValues = X.choice.values
                 ),
                 radioButtons(
                   "q7",
                   question[7],
                   selected = ex[7],
                   inline = TRUE,
                   width = NULL,
                   choiceNames = X.choice.labels,
                   choiceValues = X.choice.values
                 ),
                 radioButtons(
                   "q8",
                   question[8],
                   selected = ex[8],
                   inline = TRUE,
                   width = NULL,
                   choiceNames = X.choice.labels,
                   choiceValues = X.choice.values
                 ),
                 radioButtons(
                   "q9",
                   question[9],
                   selected = ex[9],
                   inline = TRUE,
                   width = NULL,
                   choiceNames = X.choice.labels,
                   choiceValues = X.choice.values
                 ),
                 radioButtons(
                   "q10",
                   question[10],
                   selected = ex[10],
                   inline = TRUE,
                   width = NULL,
                   choiceNames = X.choice.labels,
                   choiceValues = X.choice.values
                 ),
                 radioButtons(
                   "q11",
                   question[11],
                   selected = ex[11],
                   inline = TRUE,
                   width = NULL,
                   choiceNames = X.choice.labels,
                   choiceValues = X.choice.values
                 ),
                 radioButtons(
                   "q12",
                   question[12],
                   selected = ex[12],
                   inline = TRUE,
                   width = NULL,
                   choiceNames = X.choice.labels,
                   choiceValues = X.choice.values
                 ),
                 radioButtons(
                   "q13",
                   question[13],
                   selected = ex[13],
                   inline = TRUE,
                   width = NULL,
                   choiceNames = X.choice.labels,
                   choiceValues = X.choice.values
                 ),
                 radioButtons(
                   "q14",
                   question[14],
                   selected = ex[14],
                   inline = TRUE,
                   width = NULL,
                   choiceNames = X.choice.labels,
                   choiceValues = X.choice.values
                 ),
                 radioButtons(
                   "q15",
                   question[15],
                   selected = ex[15],
                   inline = TRUE,
                   width = NULL,
                   choiceNames = X.choice.labels,
                   choiceValues = X.choice.values
                 ),
                 radioButtons(
                   "q16",
                   question[16],
                   selected = ex[16],
                   inline = TRUE,
                   width = NULL,
                   choiceNames = X.choice.labels,
                   choiceValues = X.choice.values
                 ),
                 radioButtons(
                   "q17",
                   question[17],
                   selected = ex[17],
                   inline = TRUE,
                   width = NULL,
                   choiceNames = X.choice.labels,
                   choiceValues = X.choice.values
                 ),
                 radioButtons(
                   "q18",
                   question[18],
                   selected = ex[18],
                   inline = TRUE,
                   width = NULL,
                   choiceNames = X.choice.labels,
                   choiceValues = X.choice.values
                 ),
                 radioButtons(
                   "q19",
                   question[19],
                   selected = ex[19],
                   inline = TRUE,
                   width = NULL,
                   choiceNames = X.choice.labels,
                   choiceValues = X.choice.values
                 ),
                 radioButtons(
                   "q20",
                   question[20],
                   selected = ex[20],
                   inline = TRUE,
                   width = NULL,
                   choiceNames = X.choice.labels,
                   choiceValues = X.choice.values
                 ),
                 radioButtons(
                   "q21",
                   question[21],
                   selected = ex[21],
                   inline = TRUE,
                   width = NULL,
                   choiceNames = X.choice.labels,
                   choiceValues = X.choice.values
                 ),
                 radioButtons(
                   "q22",
                   question[22],
                   selected = ex[22],
                   inline = TRUE,
                   width = NULL,
                   choiceNames = X.choice.labels,
                   choiceValues = X.choice.values
                 ),
                 radioButtons(
                   "q23",
                   question[23],
                   selected = ex[23],
                   inline = TRUE,
                   width = NULL,
                   choiceNames = X.choice.labels,
                   choiceValues = X.choice.values
                 ),
                 radioButtons(
                   "q24",
                   question[24],
                   selected = ex[24],
                   inline = TRUE,
                   width = NULL,
                   choiceNames = X.choice.labels,
                   choiceValues = X.choice.values
                 ),
                 radioButtons(
                   "q25",
                   question[25],
                   selected = ex[25],
                   inline = TRUE,
                   width = NULL,
                   choiceNames = X.choice.labels,
                   choiceValues = X.choice.values
                 ),
                 radioButtons(
                   "q26",
                   question[26],
                   selected = ex[26],
                   inline = TRUE,
                   width = NULL,
                   choiceNames = X.choice.labels,
                   choiceValues = X.choice.values
                 ),
                 radioButtons(
                   "q27",
                   question[27],
                   selected = ex[27],
                   inline = TRUE,
                   width = NULL,
                   choiceNames = X.choice.labels,
                   choiceValues = X.choice.values
                 ),
                 radioButtons(
                   "q28",
                   question[28],
                   selected = ex[28],
                   inline = TRUE,
                   width = NULL,
                   choiceNames = X.choice.labels,
                   choiceValues = X.choice.values
                 ),
                 radioButtons(
                   "q29",
                   question[29],
                   selected = ex[29],
                   inline = TRUE,
                   width = NULL,
                   choiceNames = X.choice.labels,
                   choiceValues = X.choice.values
                 ),
                 radioButtons(
                   "q30",
                   question[30],
                   selected = ex[30],
                   inline = TRUE,
                   width = NULL,
                   choiceNames = X.choice.labels,
                   choiceValues = X.choice.values
                 ),
                 radioButtons(
                   "q31",
                   question[31],
                   selected = ex[31],
                   inline = TRUE,
                   width = NULL,
                   choiceNames = X.choice.labels,
                   choiceValues = X.choice.values
                 ),
                 radioButtons(
                   "q32",
                   question[32],
                   selected = ex[32],
                   inline = TRUE,
                   width = NULL,
                   choiceNames = X.choice.labels,
                   choiceValues = X.choice.values
                 ),
                 radioButtons(
                   "q33",
                   question[33],
                   selected = ex[33],
                   inline = TRUE,
                   width = NULL,
                   choiceNames = X.choice.labels,
                   choiceValues = X.choice.values
                 ),
                 radioButtons(
                   "z1",
                   "Tärkein lautakunta",
                   selected = z1,
                   inline = FALSE,
                   width = NULL,
                   choiceNames = c(
                     "ohita",
                     "Elinkeino",
                     "Kasvatus ja koulutus",
                     "Kulttuuri, vapaa-aika ja nuoriso",
                     "Sosiaali- ja terveyspalvelut",
                     "Ympäristö, maankäyttö ja rakentaminen"
                   ),
                   choiceValues = c("ohi", "eli", "kas", "kul", "sos", "ymp")
                 )
               )
               )
  )),
  tabPanel(
    "Luokittelumalli",
    fluidPage(
      titlePanel("Mallin kertoimet"),
      wellPanel(
        "Malli on regularisoitu logistinen multinomiaalimalli (R:n glmnet, regularisointi elasticnet).
        Likert-asteikkovastaukset on koodattu lineaarisina: -1, -1/2, 0, 1/2, 1
        Lautakuntasuosikki on one-hot-koodatu ja yksi vaihtoehto jätetty pois.
        Muutuja 'xtrem' on ++ ja -- vastausten osuus (0...1).
        Taulukossa mallin kertoimet.", br(), br(), "Järjestää kysymykset aakkosjärjestykseen tai jonkin puolueen kertoimien 
        itseisarvojen mukaiseen järjestykseen. Tämä kertoo ko. muuttujan tärkeydestä puolueen erottamiseksi, 
        mutta vain suuntaa antavasti, koska vastaustausmuuttujien varianssi on erilainen.", br(), br(),
               selectInput(
                 "puolueet",
                 label = "Kertoimien järjestys",
                 choices = c("Aakkosjärjestys", puolueet),
                 width="200px")
      ),
      fluidRow(plotOutput("coef.m1", height = "600px", width = "90%"))
      )),
  tabPanel("Luokittelutulos",
           fluidPage(
             titlePanel("Mallin laatu"), br(),
             textOutput("quality.m1"), br(),
             wellPanel("Matriisin sarake kuvaa prosenttiosuutta, jonka malli sijoittaa
                       oikeasta puolueesta ennustettuun puolueeseen. Sarake: TODELLINEN PUOLUE, rivi: ennustettu puolue. 
                       Lävistäjällä on siis kunkin luokan 'recall'. 
                       Tulokset on laskettu 5-taitteisesta ristiinvalidaatioajosta."),
             hr(), 
             fluidRow(plotOutput("confusion.m1"))
             )
           )
  #,
  #tabPanel("Kunnan koko",
  #         fluidPage(
  #           titlePanel("Kunnan koon vaikutus vastauksiin"),
  #           wellPanel("Kuvaajia kunnan koon (väkiluvun 2017/2 10-kantainen logaritmi) ja vastausen riippuvuudesta.", br(),br(),
  #             "Eri kokoisissa kunnissa on suheessa eri määrä eri puolueiden ehdokkaita, 
  #           mikä vaikuttaa tuloksiin kun kaikki ehdokkaat yhdistetään. Kannattaa siis tarkasella myös alla olevaa puoluekohtaista kuvaajaa. 
  #             Kuvaaja: gam, kuutiollinen splini, neljä solmukohtaa. Likert-asteikko on koodattu lineaarinsena (-1,-1/2,0,1/2,1) 
  #             asteikkona, vastaukset on kuvattu pisteenä, jotka on pystysunnassa hieman satunnaisesti (jitter) 
  #             jotteivät ne täysin peitäisi toisiaan", br(),
  #             selectInput(
  #             "kysymykset", width="800px",
  #             label = "Kysymys",
  #             choices = colnames(C1$X)[!grepl("^X.Lautakunta",colnames(C1$X))])),
  #           h3("Kaikki puolueet yhdessä"),
  #           fluidRow(plotOutput("kysymykset.kaikki",width = "70%",height="600px")),
  #           h3("Puolueittain"),
  #           fluidRow(plotOutput("kysymykset.puolue",width = "70%",height="600px")))
  #           )
  )
)



