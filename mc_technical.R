#-----------------------------------
# Morning Consult Analysis
# Coder: Salomon Villatoro
# Date: 06.1.2021
#-----------------------------------

  #--------------------
  # Load packages
  #--------------------
  library(tidyverse)
  library(janitor)
  library(stringr)


  #--------------------
  # Reading in the code
  #--------------------
  indFav <- read_csv("C:/Users/svillatoro/Desktop/morningconsult/part 1a) Individual Favorability Data.csv") %>%
    filter(!is.na(demAgeFull))
  
  #-----------------------------------
  # Making Codebooks
  #-----------------------------------
  codebook <- indFav[1,] %>% t() %>% as_tibble(rownames = NA) %>% 
    filter(str_detect(V1, "(.*take...-)")) %>% 
    mutate(V1 = str_remove(V1, "(.*take...-)")) %>% 
    rownames_to_column(var = "individual")
  
  #making factors for individuals to show up in the order
  #they appeared on the survey
  #UPDATE: not needed
  #fLevels <- rev(codebook[[2]])
  
  pId <- tibble(demPidNoLn = as.character(c(1:4)), 
                party = c("Republican", "Democrat", "Independent", "Something Else"))
  
  likert <- tibble(value = as.character(c(1:6)),
                   score = c("Very Favorable",
                   "Somewhat Favorable",
                   "Somwhat Unfavorable",
                   "Very Unfavorable",
                   "Heard Of, No Opinion", 
                   "Never Heard Of"))
  
  #-------------------
  # Cleaning data using
  # tidy techniques
  #-------------------
  indFav <- indFav %>%
    slice(-1) %>% 
    select(demPidNoLn,indPresApp_ID3_2:indPresApp_indPresApp_25) %>% 
    left_join(pId, by = "demPidNoLn")
  
  #calculating number of democrats and republicans
  parties <- indFav %>% count(demPidNoLn)
  
  dem <- parties %>% filter(demPidNoLn == 2) %>% pull(n)
  rep <- parties %>% filter(demPidNoLn == 1) %>% pull(n)
  # cleaning data
  
  # raw indivudual tidy data
  favorability <- indFav %>% 
    select(-1) %>% 
    pivot_longer(!party, names_to = "individual", values_to = "score_raw")
 
   #cleaned individual tidy data
  netFav <- favorability %>% 
    mutate(score = ifelse(score_raw == "1" | score_raw == "2", "favorable", 
                          ifelse(score_raw == "3" | score_raw == "4", "unfavorable","never_heard_no_opinion"))) %>% 
    left_join(codebook, by = "individual") %>% 
    select(party,V1,score) %>% 
    rename("individual" = "V1")
  
  #-------------------------------
  # Summarising favorability count
  #-------------------------------
  sumFav <- netFav %>% 
    group_by(individual, score) %>% 
    summarise(count = n(),
              total = nrow(indFav)) %>% 
    #filter(score != "N/A") %>% 
    mutate(percent = count/total) %>% 
    select(individual,score,percent) %>% 
    pivot_wider(names_from = score, 
                values_from = percent) %>% 
    mutate(net_favorability = favorable - unfavorable) %>% 
    mutate(ratio_favorability = favorable/unfavorable) %>% 
    rename("total_favorability" = "favorable") %>% 
    rename("total_unfavorability" = "unfavorable") %>% 
    arrange(desc(total_favorability))
  #---------------------------------
  # Saving results as a table CSV
  #---------------------------------
  write_csv(
    sumFav,
    "summary.csv"
  )
  
  #--------------------------------------
  #creating 2 tables to make effective viz
  # chartFav1 -> total favorability
  # chartFav2 -> total favorability 
  #               by political party
  #--------------------------------------
  chartFav1 <- netFav %>% 
    group_by(individual, score) %>% 
    summarise(count = n(),
              total = nrow(indFav)) %>% 
    mutate(percent = count/total) %>% 
    select(individual,score,percent)
  
  chartFav2 <- netFav %>% 
    group_by(individual, party, score) %>% 
    summarise(count = n()) %>% 
    filter(party == "Democrat" | party == "Republican") %>% 
    mutate(percent = ifelse(party == "Democrat", count/dem, count/rep)) %>% 
    select(individual,party,score,percent)
  
  #--------------------------------
  # Total favorability plot
  #-------------------------------
  chartFav1 <- chartFav1 %>% 
    mutate(individual = factor(individual,
                               levels = c("Hope Hicks",
                                          "Gary Cohn",
                                          "Steve Bannon",
                                          "Mitch McConnell",
                                          "Jared Kushner",
                                          "Jeff Sessions",
                                          "Charles Schumer",
                                          "Kellyanne Conway",
                                          "Republicans in Congress",
                                          "Paul Ryan",
                                          "Robert Mueller",
                                          "Nancy Pelosi",
                                          "Democrats in Congress",
                                          "Ivanka Trump",
                                          "Donald Trump",
                                          "Mike Pence",
                                          "Melania Trump"
                                          )))
  
  chartFav2 <- chartFav2 %>% 
    mutate(individual = factor(individual,
                               levels = c("Hope Hicks",
                                          "Gary Cohn",
                                          "Steve Bannon",
                                          "Mitch McConnell",
                                          "Jared Kushner",
                                          "Jeff Sessions",
                                          "Charles Schumer",
                                          "Kellyanne Conway",
                                          "Republicans in Congress",
                                          "Paul Ryan",
                                          "Robert Mueller",
                                          "Nancy Pelosi",
                                          "Democrats in Congress",
                                          "Ivanka Trump",
                                          "Donald Trump",
                                          "Mike Pence",
                                          "Melania Trump"
                               )))
  favPlot1 <- ggplot(chartFav1, 
                    mapping = aes(y = individual, 
                                  x = percent,
                                  fill = fct_rev(score))) +
    geom_bar(stat = "identity",
             position = "fill",
             width = 0.75)+
    geom_text(aes(label = paste0(round(100*percent,0), "%"), 
                  x = percent, 
                  y = individual),
              size = 4,
              position = position_fill(vjust = 0.6),
              color = "#FFFFFF",
              fontface = "bold")+
    scale_y_discrete(labels = label_wrap_gen(20)) +
    scale_x_continuous(labels = NULL) +
    scale_fill_manual(values = c("#e83237","#a9a9a9","#066b66"),
                      labels = c("Unfavorable", "No Opinion/\nNever Heard Of", "Favorable"))+
    ylab(NULL) +
    xlab(NULL) + 
    labs(subtitle = "Adults were asked if they had a favorable or unfavorable \nview of key national figures in the United States:")+
    ggtitle("Individual Favorability of National \nFigures In the United States:") +
    guides(fill = guide_legend(reverse = TRUE))+
    coord_cartesian(clip = "off") +
    theme(plot.background = element_rect(fill = NA),
          legend.position = "top",
          legend.title = element_blank(),
          panel.background = element_rect(fill = NA),
          aspect.ratio = 1.25,
          plot.title = element_text(family = "serif",
                                    hjust = 0.5,
                                    face = "bold",
                                    size = 16),
          plot.subtitle = element_text(hjust = 0.5,
                                       face = "bold",
                                       size = 11,
                                       color = "#7f7f7f"),
          axis.ticks = element_blank(),
          axis.text.y = element_text(face = "bold",
                                     size = 10,
                                     color = "#7f7f7f"))
  
  favPlot1
  ggsave("favPlot1.png",
         width = 6,
         height = 12,
         units = "in")
  #--------------------------------
  # Total favorability amongst 
  # either Democrats or Republicans
  #--------------------------------
  favPlot2 <- ggplot(chartFav2) +
    geom_bar(aes(y = party, 
                 x = percent,
                 fill = fct_rev(score)),
             stat = "identity",
             position = "fill") +
    facet_grid(rows = vars(fct_rev(individual)),
               switch ="y",
               labeller =(groupwrap = label_wrap_gen(20))) +
    geom_text(aes(label = paste0(round(100*percent,0), "%"), 
                  x = percent, 
                  y = fct_rev(party)),
              size = 4,
              position = position_fill(vjust = 0.5),
              color = "#FFFFFF",
              fontface = "bold")+
    scale_y_discrete(position = "right") +
    scale_x_continuous(labels = NULL) +
    scale_fill_manual(values = c("#e83237","#a9a9a9","#066b66"),
                      labels = c("Unfavorable", "No Opinion/\nNever Heard Of", "Favorable"))+
    ylab(NULL) +
    xlab(NULL) + 
    labs(subtitle = "Adults who identified as Democrats or Republicans were asked if they had \nfavorable or unfavorable views of key national figures in the United States:")+
    ggtitle("Individual Favorability of National Figures \nIn the United States by Respondent Party") +
    guides(fill = guide_legend(reverse = TRUE))+
    coord_cartesian(clip = "off") +
    theme(strip.text.y = element_text(size = 12, 
                                      color = "#7f7f7f", 
                                      face = "bold"),
          strip.text.y.left = element_text(angle = 0),
          strip.background = element_rect(fill = NA),
          strip.placement = 'outside',
          plot.background = element_rect(fill = NA),
          legend.position = "top",
          legend.title = element_blank(),
          panel.background = element_rect(fill = NA),
          aspect.ratio = 0.1,
          plot.title = element_text(family = "serif",
                                    hjust = 0.5,
                                    face = "bold",
                                    size = 16),
          plot.subtitle = element_text(hjust = 0.5,
                                       face = "bold",
                                       size = 11,
                                       color = "#7f7f7f"),
          axis.ticks = element_blank())
    
  favPlot2
  ggsave("favPlot2.png",
         width = 7,
         height = 12,
         units = "in")
    
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  