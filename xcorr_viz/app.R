require('shiny')
require('parallel')
require('ggplot2')
require('dplyr')
require('MASS')
require('stringi')
require('glue')

final_sim_data<-read.csv('simulated_corrd_data.csv')

ui <- fluidPage(
  
  fluidRow(

    column(width = 4,
           align = 'center',
           h4('Population Correlation Coefficient',
              align = 'center'),
           hr(),
           sliderInput(inputId = 'true_r_size',
                       label = NULL,
                       min = -.95,
                       max = .95,
                       step = .05,
                       value = 0),
           p('This value reflects your expectations of the true linear relationship between X and Y in the 
             population from which your sample comes.')
           
           ),
    column(width = 4,
           align = 'center',
           h4('Percent of Population to Sample',
              align = 'center'),
           hr(),
           sliderInput(inputId = 'sample_size',
                       label = NULL,
                       min = 01,
                       max = 100,
                       step = 1,
                       value = 15),
           p('This number represents the percentage of the population you plan to sample in your survey.
             Samples which capture a larger proportion of the population generally provide more accurate estimations of effects
             at the population level.')
           ),
    column(width = 4,
           align = 'center',
           h4('Number of Samples to Draw',
              align = 'center'),
           hr(),
           sliderInput(inputId = 'n_samples',
                       label = NULL,
                       min = 100,
                       max = 1000,
                       step = 50,
                       value = 500),
           p('This value determines the number of random samples drawn from the population to produce the chart below. The more 
             samples that are drawn, the more stable and accurate the estimates.')
    )
  ),
  
  fluidRow(
    column(width = 4,
           align = 'center',
           hr()
    ),
    column(width = 4,
           align = 'center',
           hr()
    ),
    column(width = 4,
           align = 'center',
           hr()
    )
  ),

  fluidRow(
    column(width = 3,
           align = 'center',
           selectInput(inputId = 'x_treat',
                       label = 'X Treatment',
                       choices = list('Continuous',
                                      'Descrete',
                                      'Top-Box',
                                      'Top2-Box',
                                      'Median Split')
                       ),
           selectInput(inputId =  'y_treat',
                       label = 'Y Treatment',
                       choices = list('Continuous',
                                      'Descrete',
                                      'Top-Box',
                                      'Top2-Box',
                                      'Median Split')
                       ),
           actionButton(inputId = 'update',
                        label = 'Plot Results'),
           
           h4('Note:',
              align = 'left'),
           p('This app relies on calculation of the Pearson correlation coefficient,
             which may not be appropriate for all user-defined x/y pairs. Specifically, this type of bivariate estimate is
             not appropriate when both variables are dichotomous. When one variable is dichotomous, the Pearson correlation is 
             equivalent to the point-biserial correlation and therefore an appropriate estimate.',
             align = 'left'),
           
           h4('Background:',
              align = 'left'),
           p('Commonly, constructs assumed to exists on a continous dimension (e.g., attitudes) are measured using descretely-scaled
             survey instruments (e.g., Likert scales). As shown in these simulations, this results in a slight bias to our estimates.
             However, when additional transformations are applied to these descrete scales which restrict the expression of variance to 
             a binary form (e.g., median splits), estimations become unrepresentative of their true nature in the population. The purpose
             of this application is to demonstrate the effect of common variable transformations on correlation coefficient estimates.',
             align = 'left')
           
           ),
    column(width = 8,
           align = 'center',
           br(),
           plotOutput(outputId = 'coef_plot')
           )
  )
)
     
server <- function(input, output) {
  
  data_input<-eventReactive(input$update, {

    simd_pop<-dplyr::filter(final_sim_data,
                            true_r == input$true_r_size)
        
    base_data<-plyr::rdply(input$n_samples,
                           sample_frac(simd_pop,
                                       size = input$sample_size/100)
                           ) %>%
      group_by(.n) %>%
                  mutate(
                    x_descrete = as.numeric(cut(x, 7)),
                    y_descrete = as.numeric(cut(y, 7))
                  ) %>%
      ungroup()
    
    
    if(input$x_treat == 'Continuous'){
      udef_data<-group_by(base_data,
                          .n) %>%
        mutate(user_x = x) %>%
        ungroup()
      } else if(input$x_treat == 'Descrete'){
      udef_data<-group_by(base_data,
                          .n) %>%
        mutate(user_x = x_descrete) %>%
        ungroup()
      } else if(input$x_treat == 'Top-Box'){
        udef_data<-group_by(base_data,
                            .n) %>%
        mutate(user_x = ifelse(x_descrete == 7, 1, 0)) %>%
          ungroup()
      } else if(input$x_treat == 'Top2-Box'){
        udef_data<-group_by(base_data,
                            .n) %>%
        mutate(user_x = ifelse(x_descrete >= 6, 1, 0)) %>%
          ungroup()
      } else if(input$x_treat == 'Median Split'){
        udef_data<-group_by(base_data,
                            .n) %>%
        mutate(user_x = ifelse(x >= median(x), 1 ,0)) %>%
          ungroup()
      }
    
    if(input$y_treat == 'Continuous'){
      udef_data<-group_by(udef_data,
                          .n) %>%
        mutate(user_y = y) %>%
        ungroup()
    } else if(input$y_treat == 'Descrete'){
      udef_data<-group_by(udef_data,
                          .n) %>%
        mutate(user_y = y_descrete) %>%
        ungroup()
    } else if(input$y_treat == 'Top-Box'){
      udef_data<-group_by(udef_data,
                          .n) %>%
      mutate(user_y = ifelse(y_descrete == 7, 1, 0)) %>%
        ungroup()
    } else if(input$y_treat == 'Top2-Box'){
      udef_data<-group_by(udef_data,
                          .n) %>%
      mutate(user_y = ifelse(y_descrete >= 6, 1, 0)) %>%
        ungroup()
    } else if(input$y_treat == 'Median Split'){
      udef_data<-group_by(udef_data,
                          .n) %>%
      mutate(user_y = ifelse(y >= median(y), 1, 0)) %>%
        ungroup()
    }
    
    group_by(udef_data,
             .n) %>%
      summarize('Continuous' = cor(x,y),
                'Descrete' = cor(x_descrete, y_descrete),
                'User Defined' = cor(user_x, user_y)
      ) %>%
      tidyr::gather(key = type,
                    value = corr,
                    -.n
      )
    
    
  })
   
  
  output$coef_plot <- renderPlot({
    
    ggplot(data_input(), 
           aes(x = as.factor(0), #doesn't actually matter 
               y = corr,
               fill = factor(type,
                             labels = c('X and Y Continuous',
                                        'X and Y Descretized',
                                        stringr::str_c('X ', paste(input$x_treat), ' and Y ', paste(input$y_treat))
                                        )
                             )
               )) +
      geom_boxplot() +
      ggtitle("Correlation Coefficient Estimates by Variable Treatment") +
      labs(x = "",
           y = "Estimated Sample Correlation Coefficient",
           fill = "Variable Treatment") +
      theme(plot.title = element_text(hjust = .5),
            plot.subtitle = element_text(hjust = .5),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank())
  },
  res = 100
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)

