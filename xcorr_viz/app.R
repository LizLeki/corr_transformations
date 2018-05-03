library('shiny')
library('parallel')
library('ggplot2')
library('dplyr')
library('MASS')
library('stringi')
library('glue')
library('plotly')
library('shinycssloaders')


final_sim_data<-read.csv('simulated_corrd_data.csv')

quantiles_95 <- function(x) {
  r <- quantile(x, probs=c(0.05, 0.25, 0.5, 0.75, 0.95))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

ui <- fluidPage(

  title = 'Transformations & Correlations',
  
  fluidRow(
    column(width = 3,
           hr()
    ),
    column(width = 4,
           hr()
    ),
    column(width = 3,
           hr()
           )
  ),
  
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
             samples that are drawn, the more stable and accurate the estimates. However, this adds computational time as well.')
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
           checkboxInput(inputId = 'show_points',
                         label = 'Plot All Estimates as Points'),
           
           actionButton(inputId = 'update',
                        label = 'Plot Results'),
           
           h5(strong('Note:'),
              align = 'left'),
           p('In this version of a box and whiskers plot, boxes represents the 50% of values contained in the middle of the distribution,
             while the whiskers depict 95% confidence intervals. As such, non-overlapping whiskers can be used to determine whether 
             significant differences are derived from variable treatment.')
           
           
           ),
    column(width = 8,
           align = 'center',
           tabsetPanel(type = 'tabs',
                       tabPanel('Plot',
                                br(),
                                plotlyOutput(outputId = 'coef_plot') %>%
                                  withSpinner(type = 4)
                                ),
                       tabPanel('Background',
                                br(),
                                p('Commonly, constructs assumed to exist on a continous dimension (e.g., attitudes) are measured 
                                  using descretely-scaled survey instruments (e.g., Likert scales). As shown in these simulations, 
                                  this results in a slight bias to our estimates. Importantly, though, this bias is minor and well 
                                  outside the bounds of even liberal standards of statistical significance.',
                                  align = 'left'),
                                p('However, when additional transformations are applied to these descrete scales which restrict
                                  the expression of variance to a binary form (e.g., median splits), estimations become 
                                  unrepresentative of their true nature in the population. This seems especially true for strong bivariate relationships.
                                  The purpose of this application is to demonstrate the effect of these binary variable transformations on correlation
                                  coefficient estimates.',
                                  align = 'left'),
                                p('This app relies on calculation of the Pearson correlation coefficient, which may not be 
                                  appropriate for all user-defined x/y pairs. Specifically, this type of bivariate estimate is not 
                                  appropriate when both variables are dichotomous. When one variable is dichotomous, the Pearson 
                                  correlation is equivalent to the point-biserial correlation and therefore an appropriate estimate.',
                                  align = 'left')
                                )
                       )
           
           )
  ),
  fluidRow(
    column(width = 12,
           hr())
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
      ) %>%
      mutate(type = factor(type,
                           labels = c('X and Y Continuous',
                                      'X and Y Descretized',
                                      'User Defined')
                           )
      )
    
  })
   
  output$coef_plot<-renderPlotly({
    
    ggthemr::ggthemr('flat')
    
    p<-ggplot(data_input(), 
           aes(x = type, 
               y = corr,
               fill = type
               )) +
      stat_summary(fun.data = quantiles_95, geom = 'boxplot') +
      ggtitle("Correlation Coefficient Estimates by Variable Treatment") +
      labs(x = "",
           y = "Estimated Sample Correlation Coefficient",
           fill = "") +
      ylim(-1,1) +
      theme(plot.title = element_text(hjust = .2),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank()) 
    
   if(input$show_points){
     
     p<-ggplotly(p+geom_jitter()) %>%
       config(displayModeBar = FALSE)
   
     } else { 
       p<-ggplotly(p) %>%
     config(displayModeBar = FALSE)
       }
    
    
    
  }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
