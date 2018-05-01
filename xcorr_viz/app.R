library(shiny)
library(parallel)
library(ggplot2)
library(dplyr)

final_sim_data<-read.csv('simulated_corrd_data.csv')

ui <- fillPage(
  padding = c(10,20),
  
  fluidRow(

    column(width = 4,
           h4('Population Correlation Coefficient',
              align = 'center'),
           hr()
           ),
    column(width = 4,
           h4('Percent of Population to Sample',
              align = 'center'),
           hr()
           ),
    column(width = 4,
           h4('Number of Samples to Draw',
              align = 'center'),
           hr()
    )
  ),
  
  fluidRow(
      
    column(width = 4,
           align = 'center',
           sliderInput(inputId = 'true_r_size',
                       label = NULL,
                       min = -.95,
                       max = .95,
                       step = .05,
                       value = 0)
    ),
      
      column(width = 4,
             align = 'center',
             sliderInput(inputId = 'sample_size',
                         label = NULL,
                         min = 01,
                         max = 100,
                         step = 1,
                         value = 15)
      ),
    
    column(width = 4,
           align = 'center',
           sliderInput(inputId = 'n_samples',
                       label = NULL,
                       min = 100,
                       max = 1000,
                       step = 50,
                       value = 500)
           )
  ),
  
  fluidRow(
    
    column(width = 4,
           p('This value reflects your expectations of the true linear relationship between X and Y in the 
             population from which your sample comes.')
    ),
    column(width = 4,
           p('This number represents the percentage of the population you plan to sample in your survey.
             Samples which capture a larger proportion of the population generally provide more accurate estimations of effects
             at the population level.')
    ),
    column(width = 4,
           p('This value determines the number of random samples drawn from the population to produce the chart below. The more 
             samples that are drawn, the more stable and accurate the estimates.')
    )
  ),
  
  fluidRow(
    column(width = 12,
           align = 'center',
           hr(),
           br(),
           actionButton(inputId = 'update',
                        label = 'Update Results'))
  ),
  
  fluidRow(
    column(width = 12,
           align = 'center',
           br(),
           br(),
           plotOutput(outputId = 'coef_plot',
                      width = '85%')
    )
  )
)
     
server <- function(input, output) {
  
  data_input<-eventReactive(input$update, {

    simd_pop<-dplyr::filter(final_sim_data,
                            true_r == input$true_r_size)
        
    plyr::rdply(input$n_samples,
                sample_frac(simd_pop,
                            size = input$sample_size/100) %>%
                  mutate(
                    x_descrete = as.numeric(cut(x, 7)),
                    y_descrete = as.numeric(cut(y, 7)),
                    x_topbox = ifelse(x_descrete == 7, 1, 0),
                    y_topbox = ifelse(y_descrete == 7, 1, 0),
                    x_top2box = ifelse(x_descrete >= 6, 1, 0),
                    y_top2box = ifelse(y_descrete >= 6, 1, 0),
                    x_medsplit = ifelse(x >= median(x), 1 ,0),
                    y_medsplit = ifelse(y >= median(y), 1, 0)
                  )
    ) %>%
      group_by(.n) %>%
      summarize(continuous_corr = cor(x,y),
                descrete_corr = cor(x_descrete, y_descrete),
                topbox_corr = cor(x_topbox, y_topbox),
                top2box_corr = cor(x_top2box, y_top2box),
                medsplit_corr = cor(x_medsplit, y_medsplit)
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
               fill = factor(type, labels = c("Continuous", "Discrete", "Median Split",
                                              'Top2 Box', 'Top Box')))
    ) +
      geom_boxplot() +
      ggtitle("Correlation Coefficient Estimates by Variable Treatment") +
      labs(x = "",
           y = "Estimated Sample Correlation Coefficient",
           fill = "Variable Treatment") +
      theme(plot.title = element_text(hjust = .5),
            plot.subtitle = element_text(hjust = .5),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank())
  }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)

