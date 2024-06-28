#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(markdown)
library(bslib)
library(thematic)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggwordcloud)
library(stringr)
library(scales)
thematic::thematic_shiny(font = "auto")

survey = read.csv('Cometeer Survey - Sheet1.csv')
coffee_cleaning = read.csv('coffee_cleaning.csv')
coffee_cleaning$'no detail' = 1
master_word_cloud = read.csv('master_word_cloud.csv')


results = coffee_cleaning %>% 
  pivot_longer(cols = c(coffee_a_acidity, coffee_a_bitterness, coffee_a_personal_preference,
                        coffee_b_acidity, coffee_b_bitterness, coffee_b_personal_preference,
                        coffee_c_acidity, coffee_c_bitterness, coffee_c_personal_preference,
                        coffee_d_acidity, coffee_d_bitterness, coffee_d_personal_preference),
               values_to = 'score',
               names_to = 'metric') %>%
  mutate(type = case_when(
    str_detect(metric, "coffee_a") ~ "A",
    str_detect(metric, "coffee_b") ~ "B",
    str_detect(metric, "coffee_c") ~ "C",
    str_detect(metric, "coffee_d") ~ "D",
  ))


# Define UI for application that draws a histogram
ui <- navbarPage(
  theme = bs_theme(
    bg = '#F6F6E9',
    #bg = "#EDE7DD",
    fg = "#DC661F",
    primary = "#1D7151",
    secondary = '#ABFF78',
    heading_font = font_google("Allan"),
    #heading_font = font_google("Lilita One"),
    #heading_font = font_google("Tex Gyre Bonum"),
    base_font = font_google("Urbanist"),
    code_font = font_google("Urbanist")
  ),
  "",
  

# SPLASH PAGE
tabPanel("The Great Coffee Tasting ",
         fluidPage(
           style = "padding-bottom: 20px;padding-left: 20px;",
           #HTML("<h1><div style='font-size: 64px;'>Cometeer's<br></div><div style='font-size: 140px;'>The Great <br>American <br>Coffee Taste Test (results).
                       #</div><div style='font-size: 96px;'><i>RESULTS!<br></i></div></h1>"),
           
           fluidRow(column(5,
                           HTML("<h1><div style='font-size: 64px;'>Cometeer's<br></div><div style='font-size: 130px;'>The Great <br>American <br>Coffee Taste Test *results*
                       </div></h1>"),
           ),
                    #column(8,
                  #HTML("<h1><i><div style='font-size:64px;'>Come on in, the coffee's great!</div></i></h1><br>"),
           #),
                    column(5,
                           HTML("<br><br>"),
                  includeMarkdown("The_txt.txt"), 
                  HTML('<iframe width="400" height="200" src="https://www.youtube.com/embed/U489K2t_Tgc?si=plwUlQnFfnoOGXZc" 
                                  title="YouTube video player" frameborder="0" allow="accelerometer; 
                                  autoplay; clipboard-write; encrypted-media; gyroscope; 
                                  picture-in-picture; web-share" referrerpolicy="strict-origin-when-cross-origin" 
                                  allowfullscreen></iframe>'),
                  includeMarkdown("Second paragraph.txt"),
                  offset = 1
           ),
           
           )
         )
),

#SURVEY RESULTS
tabPanel("Survey Questions",
         fluidPage(
           style = "padding-bottom: 20px;padding-left: 50px;",
           HTML("<h1><div style='font-size: 32px;'>Survey Questions
                       </div></h1>"),
           
           fluidRow(column(5,
                           tableOutput("survey")
                           
           ),
           
           )
         )
),

#PARTICIPANTS TAB
tabPanel("Participants",
         fluidPage(
           fluidRow(style = "padding-bottom: 20px;padding-left: 20px;",
                    column(4, varSelectInput(
                      "pivotX", "Characteristic",
                      coffee_cleaning[c('age', 'gender', 'ethnicity_race',
                                        'where_drink', 'brew', 'purchase',
                                        'cups', 'favorite', 'coffee_black',
                                        'total_spend', 'most_paid', 'style',
                                        'most_willing', 'value_cafe',
                                        'spent_equipment', 'value_equipment',
                                        'education_level', 'wfh'
                                        
                                        )],
                      selected = ""
                    ),
                    ),
                    column(4, varSelectInput(
                      "pivotZ", "Detail",
                      coffee_cleaning[c('no detail', 
                                        'age', 'gender', 'ethnicity_race',
                                        'cups', 'favorite', 'coffee_black',
                                        'total_spend', 'most_paid', 'style',
                                        'most_willing', 'value_cafe',
                                        'spent_equipment', 'value_equipment',
                                        'education_level', 'wfh')],
                      selected = ""
                    ), 
                    ),
                    selectInput(
                      "pivot_view",
                      "View values as:",
                      c('percentages' = "stack",
                        'relative percentages' = 'fill'
                        )
                    )
           ),
           fluidRow(
             plotOutput('pivot_plot', height = "500px")
             
           )
         )
),

#COFFEE SCORES
tabPanel(
  "Individual Coffee Scores",
  fluidPage(
    style = "padding-bottom: 20px;padding-left: 20px;",
    
    #HTML("<h1><div style='font-size: 24px;'>Coffee Scores</div></h1>"),
    "The Contestants were asked to rate each of the coffees on acidity, bitterness,
                     and overall preference on a scale of 1 to 5. They were also able to insert tasting notes.",
    HTML("<br><br>"),
    fluidRow(
      column(2,),
      
      column(2,
             selectInput(
               "type",
               "Choose a Coffee:",
               c(
                 "A" = "A",
                 "B" = "B",
                 "C" = "C",
                 "D" = "D"
               ),
             )),
      
      column(5,
             tableOutput("summary_metrics"),),
      column(
        6,
        HTML("<div style='font-size: 18px;'>Tasting Scores</div>"),
        plotOutput("results_plotA1")
        
      ),
      
      column(
        6,
        HTML("<div style='font-size: 18px;'>Tasting Notes Word Cloud</div>"),
        plotOutput("word_cloud",
                   width = "100%", height = "100%")
      ),
      
      column(9,)
    )
  )
),


#COFFEE COMPARISONS
tabPanel("Coffee Comparisons",
         fluidPage(
           fluidRow(
             style = "padding-bottom: 20px;padding-left: 20px;",
             column(2,),
             column(
               3,
               varSelectInput("results_viewer", "Evaluate Results by:",
                              coffee_cleaning[c('age', 'gender', 'ethnicity_race',
                                                'cups', 'favorite', 'coffee_black',
                                                'total_spend', 'most_paid', 'style',
                                                'most_willing', 'value_cafe', 'roast_level',
                                                'spent_equipment', 'value_equipment',
                                                'education_level', 'wfh')],)
             ),
             column(3,
                    selectInput(
                      "position",
                      "View bars: (for last three tabs)",
                      c('stacked' = 'stack',
                        'side by side' = 'dodge',
                        'relative percentage' = 'fill')
                    )),
             # Output: Tabset w/ plot, summary, and table ----
             tabsetPanel(
               type = "tabs",
               tabPanel(
                 "Unaided Scores: A, B, C, D",
                 HTML(
                   "<br><i>Q: On a scale of 1 to 5, how do you rate this coffee in terms of personal preference?</i><br><br>"
                 ),
                 tableOutput("summary_means"),
                 plotOutput("preference_scores")
               ),
               tabPanel(
                 "A vs. B, vs. C",
                 HTML("<br><i>Q: Which do you prefer, A, B or C?</i><br><br>"),
                 plotOutput("abc")
               ),
               tabPanel(
                 "A vs. D",
                 HTML("<br><i>Q: Which do you prefer, A, or D?</i><br><br>"),
                 plotOutput("ad")
               ),
               tabPanel(
                 "Preferred Overall",
                 HTML("<br><i>Q: Which do you prefer overall?</i><br><br>"),
                 plotOutput('prefer_overall')
               )
             )
             
           )
           
         )),

#SEGMENT BUILDER
tabPanel("Segment Builder",
         fluidPage(
           fluidRow(
             style = "padding-bottom: 20px;padding-left: 20px;",
             column(4, varSelectInput(
               'Segment_1', 'Columns',
               coffee_cleaning[c(
                 'age',
                 'cups',
                 'where_drink',
                 'brew',
                 'purchase',
                 'favorite',
                 'coffee_black',
                 'gender',
                 'ethnicity_race',
                 'most_paid',
                 'style',
                 'total_spend',
                 'strength',
                 'roast_level'
               )],
             )),
             column(4, varSelectInput('Segment_2', 'Rows',
                                      coffee_cleaning[c(
                                        'age',
                                        'cups',
                                        'favorite',
                                        'coffee_black',
                                        'gender',
                                        'ethnicity_race',
                                        'most_paid',
                                        'style',
                                        'total_spend',
                                        'strength',
                                        'roast_level'
                                      )])),
             column(
               4,
               varSelectInput('Coffee_Type', 'Coffee Type',
                              coffee_cleaning[c(
                                'coffee_a_personal_preference',
                                'coffee_b_personal_preference',
                                'coffee_c_personal_preference',
                                'coffee_d_personal_preference'
                              )])
             ),
             
           ),
           fluidRow(plotOutput('segment_builder', height = "500px"))
         )),


#navbarMenu(
  #"More",
  #tabPanel("Table of Results",
           #DT::dataTableOutput("table")),
  tabPanel("About",
           fluidRow(
             column(6,
                    includeMarkdown("About.txt")),
             column(
               3,
               #img(src = "Subject.png", alt = "A description of the image", width = "600px")
               
               
               img(class = "img-polaroid",
                   
                   
                   src = "Subject.png",
                   width = "400px"),
               
             )
           )
)

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$simple_plot <- renderPlot({
    coffee_cleaning %>%
      ggplot(aes(!!input$simpleX, y = ..count.. / sum(..count..))) +
      geom_bar(aes(fill = !!input$simpleZ)) +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(y = 'Percent Respondents', x = NULL) +
      guides(x = guide_axis(n.dodge = 1, angle = 320)) +
      geom_text(
        aes(label = ..count..),
        stat = "count",
        vjust = -.5,
        colour = "black"
      ) +
      annotate(
        "text",
        x = Inf,
        y = Inf,
        label = paste("Total sample size:", sum(!is.na(
          coffee_cleaning %>% select(!!input$simpleX)
        ))),
        hjust = 1.1,
        vjust = 2,
        size = 6,
        color = "black"
      ) +
      theme(
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = 'bold'),
        axis.title.y = element_text(size = 14, face = 'bold')
      )
    
    
  })
  
  
  output$pivot_plot <- renderPlot({
    
    coffee_cleaning %>%
      separate(
        !!input$pivotX,
        sep = ',',
        fill = 'right',
        into = c('x1', 'x2', 'x3', 'x4', 'x5', 'x6', 'x7', 'x8', 'x9', 'x10'),
        remove = TRUE
      ) %>%
      pivot_longer(cols = x1:x10,
                   names_to = 'list',
                   values_to = 'x') %>%
      filter(!is.na(x)) %>%
      mutate(x = str_trim(x)) %>%
      ggplot(aes(x, y = ..count.. / (sum(
        !is.na(coffee_cleaning %>% select(!!input$pivotX))
      )))) +
      geom_bar(aes(fill = !!input$pivotZ), position = input$pivot_view) +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(y = 'Percent Respondents', x = NULL) +
      guides(x = guide_axis(n.dodge = 1, angle = 320)) +
      geom_text(
        aes(label = percent(..count../sum(!is.na(
            coffee_cleaning %>% select(!!input$pivotX)
          )), accuracy = 1)
        ),
        stat = "count",
        vjust = -.5,
        colour = "black"
      ) +
      annotate(
        "text",
        x = Inf,
        y = Inf,
        #label = paste("Total sample size:", sum(!is.na(
         # coffee_cleaning %>% select(!!input$pivotX)
        #))),
        label = paste("Total sample size:", sum(!is.na(
          coffee_cleaning %>% select(!!input$pivotX)
        ))),
        hjust = 1.1,
        vjust = 2,
        size = 6,
        color = "black"
      ) +
      theme(
        axis.text.x = element_text(size = 14,face = 'bold'),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16, face = 'bold'),
        axis.title.y = element_text(size = 16, face = 'bold'),
        legend.text = element_text(face = "bold")
      )
    
    
  })
  
  
  output$results_plotA <- renderPlot({
    results %>%
      filter(type == 'A') %>%
      ggplot() +
      geom_density(aes(score, color = metric),
                   binwidth = 1,
                   adjust = 4) +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(y = 'Percent Respondents',
           title = 'Tasting Scores') +
      #guides(x = guide_axis(n.dodge=1, angle = 320)) +
      annotate(
        "text",
        x = Inf,
        y = Inf,
        label = paste(
          "Total sample size:",
          #sum(!is.na(coffee_dropped$Age))),
          nrow(
            results %>%
              filter((type == 'A') &
                       (str_detect(
                         metric, "acidity"
                       ))) %>%
              group_by(submission_id) %>%
              summarise(n())
          )
        ),
        hjust = 1.1,
        vjust = 2,
        size = 4,
        color = "black"
      ) +
      theme(
        axis.text.x = element_text(size = 14,face = 'bold'),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16, face = 'bold'),
        axis.title.y = element_text(size = 16, face = 'bold'),
        legend.text = element_text(face = "bold"),
        legend.position = "bottom")
 
  })
  
  output$results_plotA1 <- renderPlot({
    results %>%
      filter(type == input$type) %>%
      ggplot() +
      geom_density(aes(score, color = metric),
                   binwidth = 1,
                   adjust = 4) +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(y = 'Percent Respondents',
           title = NULL) +
      #guides(x = guide_axis(n.dodge=1, angle = 320)) +
      annotate(
        "text",
        x = Inf,
        y = Inf,
        label = paste(
          "Total sample size:",
          #sum(!is.na(coffee_dropped$Age))),
          nrow(
            results %>%
              filter((type == 'A') &
                       (str_detect(
                         metric, "acidity"
                       ))) %>%
              group_by(submission_id) %>%
              summarise(n())
          )
        ),
        hjust = 1.1,
        vjust = 2,
        size = 4,
        color = "black"
      ) +
      theme(
        axis.text.x = element_text(size = 14,face = 'bold'),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16, face = 'bold'),
        axis.title.y = element_text(size = 16, face = 'bold'),
        legend.text = element_text(face = "bold"),
        legend.position = "bottom")
    
    
  })
  
  output$word_cloud <- renderPlot({
    master_word_cloud %>%
      filter(Type == input$type) %>%
      ggplot(aes(
        label = words,
        size = count,
        color = count
      )) +
      geom_text_wordcloud_area(rm_outside = TRUE, eccentricity = 1) +
      scale_size_area(max_size = 50, trans = power_trans(1 / .7)) +
      theme_minimal() +
      scale_color_gradient(low = "darkred", high = "red") +
      theme(
        axis.text.x = element_text(size = 14,face = 'bold'),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16, face = 'bold'),
        axis.title.y = element_text(size = 16, face = 'bold'),
        legend.text = element_text(face = "bold"),
        )
    
  })
  
  output$preference_scores <- renderPlot({
    coffee_cleaning %>%
      pivot_longer(
        cols = ends_with('preference'),
        values_to = 'Scores',
        names_to = 'Coffee_choice'
      ) %>%
      ggplot(aes(x = Coffee_choice)) +
      stat_summary(
        aes(y = Scores, fill = !!input$results_viewer),
        position = 'dodge',
        na.rm = TRUE,
        fun = "mean",
        geom = "bar"
      ) +
      labs(y = 'Average Score (1-5)', x = NULL) +
      scale_x_discrete(
        labels = c(
          "coffee_a_personal_preference" = "Coffee A",
          "coffee_b_personal_preference" = "Coffee B",
          "coffee_c_personal_preference" = "Coffee C",
          "coffee_d_personal_preference" = "Coffee D"
        )
      ) +
      theme(
        axis.text.x = element_text(size = 14,face = 'bold'),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16, face = 'bold'),
        axis.title.y = element_text(size = 16, face = 'bold'),
        legend.text = element_text(face = "bold")
        )
    
  })
  
  
  output$abc <- renderPlot({
    coffee_cleaning %>%
      ggplot(aes(prefer_abc, y = ..count.. / sum(..count..))) +
      geom_bar(aes(fill = !!input$results_viewer), position = input$position) +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(y = 'Percent Respondents', x = NULL) +
      geom_text(
        aes(label = percent(..count../3798, accuracy = 1)),
        stat = "count",
        vjust = -.5,
        colour = "black"
      ) +
      annotate(
        "text",
        x = Inf,
        y = Inf,
        label = paste("Total sample size:", 3798),
        hjust = 1.1,
        vjust = 2,
        size = 6,
        color = "black"
      ) +
      theme(
        axis.text.x = element_text(size = 14,face = 'bold'),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16, face = 'bold'),
        axis.title.y = element_text(size = 16, face = 'bold'),
        legend.text = element_text(face = "bold"),
  )
  })
  
  output$ad <- renderPlot({
    coffee_cleaning %>%
      ggplot(aes(prefer_ad, y = ..count.. / sum(..count..))) +
      geom_bar(aes(fill = !!input$results_viewer), position = input$position) +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(y = 'Percent Respondents', x = NULL) +
      geom_text(
        aes(label = percent(..count../3798, accuracy = 1)),
        stat = "count",
        vjust = -.5,
        colour = "black"
      ) +
      annotate(
        "text",
        x = Inf,
        y = Inf,
        label = paste("Total sample size:", 3798),
        hjust = 1.1,
        vjust = 2,
        size = 6,
        color = "black"
      ) +
      theme(
        axis.text.x = element_text(size = 14,face = 'bold'),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16, face = 'bold'),
        axis.title.y = element_text(size = 16, face = 'bold'),
        legend.text = element_text(face = "bold"),
      )
  })
  
  output$prefer_overall <- renderPlot({
    coffee_cleaning %>%
      ggplot(aes(prefer_overall, y = ..count.. / sum(..count..))) +
      geom_bar(aes(fill = !!input$results_viewer), position = input$position) +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(y = 'Percent Respondents', x = NULL) +
      geom_text(
        aes(label = percent(..count../3798, accuracy = 1)),
        stat = "count",
        vjust = -.5,
        colour = "black"
      ) +
      annotate(
        "text",
        x = Inf,
        y = Inf,
        label = paste("Total sample size:", 3798),
        hjust = 1.1,
        vjust = 2,
        size = 6,
        color = "black"
      ) +
      theme(
        axis.text.x = element_text(size = 14,face = 'bold'),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16, face = 'bold'),
        axis.title.y = element_text(size = 16, face = 'bold'),
        legend.text = element_text(face = "bold"),
      )
  })
  
  output$segment_builder <- renderPlot({
    #coffee_cleaning %>%
    #filter((!is.na(age)) & (!is.na(favorite))) %>%
    # ggplot(aes(!!input$Segment_1, !!input$Segment_2)) +
    #stat_summary_2d(aes(z = !!input$Coffee_Type), fun = 'mean', bins = 30) +
    #geom_count() +
    #guides(x = guide_axis(n.dodge=1, angle = 320)) +
    #scale_fill_gradient(name = "Score(1-5)", limits = c(1, 5)) +
    #scale_fill_viridis_c() +
    #theme(axis.text.x = element_text(size = 12),
    #     axis.text.y = element_text(size = 12),
    #    axis.title.x = element_text(size = 14, face = 'bold'),
    #   axis.title.y = element_text(size = 14, face = 'bold')) +
    #theme(legend.position = "bottom")
    
    coffee_cleaning %>%
      separate(
        !!input$Segment_1,
        sep = ',',
        fill = 'right',
        into = c('x1', 'x2', 'x3', 'x4', 'x5', 'x6', 'x7', 'x8', 'x9', 'x10'),
        remove = TRUE
      ) %>%
      pivot_longer(cols = x1:x10,
                   names_to = 'list',
                   values_to = 'x') %>%
      filter(!is.na(x)) %>%
      mutate(x = str_trim(x)) %>%
      ggplot(aes(x,!!input$Segment_2)) +
      stat_summary_2d(aes(z = !!input$Coffee_Type),
                      fun = 'mean',
                      bins = 30) +
      geom_count() +
      scale_fill_viridis_c() +
      guides(x = guide_axis(n.dodge = 1, angle = 320)) +
      theme(
        axis.text.x = element_text(size = 14,face = 'bold'),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16, face = 'bold'),
        axis.title.y = element_text(size = 16, face = 'bold'),
        legend.text = element_text(face = "bold"),
      ) +
      theme(legend.position = "bottom")
    
  })
  output$summary_means <- renderTable({
    coffee_cleaning %>%
      select(ends_with('preference')) %>%
      summarise(
        Avg_A = mean(coffee_a_personal_preference, na.rm = TRUE),
        Avg_B = mean(coffee_b_personal_preference, na.rm = TRUE),
        Avg_C = mean(coffee_c_personal_preference, na.rm = TRUE),
        Avg_D = mean(coffee_d_personal_preference, na.rm = TRUE)
      )
    
  })
  
  output$summary_metrics <- renderTable({
    as.data.frame(summary(coffee_cleaning %>%
                            select(contains(
                              paste0('_', tolower(input$type), '_')
                            )))) %>%
      select(Var2, Freq) %>%
      rename(Characteristic = Var2) %>%
      filter(str_detect(Freq, 'Mean'))
  })
  
  output$survey <- renderTable({
    survey
  })
  
  output$table <- DT::renderDataTable({
    coffee_cleaning
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
