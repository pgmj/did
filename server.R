# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # thematic::thematic_shiny(font = "auto")


  # Kommunöversikt ----------------------------------------------------------

  ## circlePlot  -------------------------------------------------------------
  # inspired by https://debruine.github.io/30DCC-2022/11-circular.html
  output$circlePlot <- renderPlot({
    df.risk %>%
      filter(Kommun == input$enkommun) %>%
      filter(År == input$years0) %>%
      filter(!Index %in% c("Välbefinnande", "Positiv skolanknytning")) %>%
      mutate(riskLevel = car::recode(riskLevel,"NA='För få svar';
                                   'Medelhög risk'='Något förhöjd risk';
                                   'Hög risk'='Förhöjd risk'")) %>% 
      mutate(Risknivå = factor(riskLevel, levels = c("För få svar", "Låg risk", "Något förhöjd risk", "Förhöjd risk"))) %>%
      ggplot(aes(x = Index, y = Andel, fill = Risknivå)) +
      geom_col(alpha = 0.8) +
      geom_textpath(aes(label = Index, group = Index),
                    text_only = T,
                    position = "stack",
                    hjust = 0,
                    size = 6
      ) +
      coord_polar(theta = "y") +
      scale_fill_manual(values = c("lightgrey","#009E73", "#F0E442", "#D55E00")) +
      theme_bw() +
      scale_x_discrete(
        expand = expansion(add = c(3, 0)),
        limits = rev,
        labels = NULL
      ) +
      scale_y_continuous(
        breaks = seq(0, 90, 10),
        labels = paste0(seq(0, 90, 10), "%"),
        limits = c(0, 100)
      ) +
      labs(title = paste0(input$enkommun, " - ", input$years0),
           subtitle = "Samtliga respondenter") +
      geom_texthline(
        yintercept = 10, color = "black",
        linetype = 2, size = 4, alpha = 0.6,
        label = "Förhöjd risk",
        hjust = 0.05
      ) +
      geom_texthline(
        yintercept = 25, color = RISEprimRed,
        linetype = 2, size = 4, alpha = 0.6,
        label = "Något förhöjd risk",
        hjust = 0.15
      ) +
      theme(
        axis.text.x = element_text(size = ax.size, family = "sans"),
        axis.text.y = element_text(size = ax.size, family = "sans"),
        title = element_text(size = title.size),
        legend.text = element_text(size = legend.size),
        strip.text.x = element_text(size = 14),
        panel.spacing = unit(0.7, "cm", data = NULL)
      ) +
      xlab("") +
      ylab("")
  })
  

  ## stackedArea -------------------------------------------------------------

  output$stackedArea <- renderPlot({
    df.plot <- df %>%
      filter(Kommun == input$enkommun) %>%
      mutate(
        Risknivå = case_when(
          .data[[input$factor]] < rslimits |>
            select(input$factor) |>
            slice(1) |>
            pull() ~ "Låg risk",
          .data[[input$factor]] >= rslimits |>
            select(input$factor) |>
            slice(1) |>
            pull() &
            .data[[input$factor]] < rslimits |>
              select(input$factor) |>
              slice(2) |>
              pull() ~ "Något förhöjd risk",
          .data[[input$factor]] >= rslimits |>
            select(input$factor) |>
            slice(2) |>
            pull() ~ "Förhöjd risk",
          TRUE ~ "Otillräckliga svar"
        )
      )

    df.plot %>%
      filter(Kön %in% c("Pojke", "Flicka")) %>%
      # filter(ar %in% input$years0) %>% # allow selection of span of years?
      mutate(Risknivå = factor(Risknivå, levels = c("Förhöjd risk", "Något förhöjd risk", "Låg risk","Otillräckliga svar"))) %>%
      group_by(ar, Kön) %>%
      count(Risknivå, .drop = FALSE) %>%
      mutate(Andel = (100 * n / sum(n)) %>% round(digits = 1)) %>%
      ggplot(aes(x = ar, y = Andel)) +
      geom_area(aes(fill = Risknivå),
                position = "stack",
                alpha = 0.8
      ) +
      scale_fill_manual(values = c("#D55E00", "#F0E442", "#009E73", "lightgrey")) +
      geom_hline(yintercept = 90, color = "black", linetype = 2, linewidth = 0.66) +
      geom_hline(yintercept = 75, color = RISEprimRed, linetype = 2, linewidth = 0.66) +
      scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100)) +
      scale_x_continuous(breaks = årtal, guide = guide_axis(n.dodge = 2)) +
      xlab("Årtal") +
      ylab("Andel i %") +
      theme(
        axis.text.x = element_text(size = ax.size, family = "sans"),
        axis.text.y = element_text(size = ax.size, family = "sans"),
        title = element_text(size = title.size),
        legend.text = element_text(size = legend.size),
        strip.text.x = element_text(size = 14),
        panel.spacing = unit(pandist, "cm", data = NULL)
      ) +
      labs(
        title = paste0(input$factor, " - ", input$enkommun),
        subtitle = "Uppdelat på kön",
        caption = str_wrap("Svart streckad linje = referensvärde för 10% med högst risk. 
      Röd linje = referensvärde för 25% med högst risk",
                           width = 60)
      ) +
      facet_wrap(~Kön)
  })
  

## stackedAreaÅK -----------------------------------------------------------
  output$stackedAreaÅK <- renderPlot({
    df.plot <- df %>%
      filter(Kommun == input$enkommun) %>%
      mutate(
        Risknivå = case_when(
          .data[[input$factor]] < rslimits |>
            select(input$factor) |>
            slice(1) |>
            pull() ~ "Låg risk",
          .data[[input$factor]] >= rslimits |>
            select(input$factor) |>
            slice(1) |>
            pull() &
            .data[[input$factor]] < rslimits |>
            select(input$factor) |>
            slice(2) |>
            pull() ~ "Något förhöjd risk",
          .data[[input$factor]] >= rslimits |>
            select(input$factor) |>
            slice(2) |>
            pull() ~ "Förhöjd risk",
          TRUE ~ "Otillräckliga svar"
        )
      )
    
    df.plot %>%
      filter(Kön %in% c("Pojke", "Flicka")) %>%
      mutate(Risknivå = factor(Risknivå, levels = c("Förhöjd risk", "Något förhöjd risk", "Låg risk","Otillräckliga svar"))) %>%
      #filter(!Risknivå == "Otillräckliga svar") %>% 
      # filter(ar %in% input$years0) %>% # allow selection of span of years?
      group_by(ar, ARSKURS) %>%
      count(Risknivå, .drop = FALSE) %>%
      mutate(Andel = (100 * n / sum(n)) %>% round(digits = 1)) %>%
      ggplot(aes(x = ar, y = Andel)) +
      geom_area(aes(fill = Risknivå),
                position = "stack",
                alpha = 0.8
      ) +
      scale_fill_manual(values = c("#D55E00", "#F0E442", "#009E73", "lightgrey")) +
      geom_hline(yintercept = 90, color = "black", linetype = 2, linewidth = 0.66) +
      geom_hline(yintercept = 75, color = RISEprimRed, linetype = 2, linewidth = 0.66) +
      scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100)) +
      scale_x_continuous(breaks = årtal, guide = guide_axis(n.dodge = 2)) +
      xlab("Årtal") +
      ylab("Andel i %") +
      theme(
        axis.text.x = element_text(size = ax.size, family = "sans"),
        axis.text.y = element_text(size = ax.size, family = "sans"),
        title = element_text(size = title.size),
        legend.text = element_text(size = legend.size),
        strip.text.x = element_text(size = 14),
        panel.spacing = unit(pandist, "cm", data = NULL)
      ) +
      labs(
        title = paste0(input$factor, " - ", input$enkommun),
        subtitle = "Uppdelat på årskurs",
        caption = str_wrap("Svart streckad linje = referensvärde för 10% med högst risk. 
      Röd linje = referensvärde för 25% med högst risk",
                           width = 60)
      ) +
      facet_wrap(~ARSKURS)
  })
  

  ## ProtAreaPlot --------------------------------------------------------------
  
  output$ProtAreaPlot <- renderPlot({
    df.plot <- df %>%
      filter(Kommun == input$enkommun) %>%
      mutate(
        Risknivå = case_when(
          .data[[input$pfactor]] < rslimits.prot |>
            select(input$pfactor) |>
            slice(1) |>
            pull() ~ "Lågt skydd",
          .data[[input$pfactor]] >= rslimits.prot |>
            select(input$pfactor) |>
            slice(1) |>
            pull() &
            .data[[input$pfactor]] < rslimits.prot |>
            select(input$pfactor) |>
            slice(2) |>
            pull() ~ "Neutral",
          .data[[input$pfactor]] >= rslimits.prot |>
            select(input$pfactor) |>
            slice(2) |>
            pull() ~ "Högt skydd",
          TRUE ~ "Otillräckliga svar"
        )
      )
    
    df.plot %>%
      filter(Kön %in% c("Flicka","Pojke")) %>%
      filter(!Risknivå == "Otillräckliga svar") %>% 
      group_by(ar, Kön) %>%
      count(Risknivå) %>%
      mutate(Andel = (100 * n / sum(n)) %>% round(digits = 1)) %>%
      mutate(Risknivå = factor(Risknivå, levels = c("Högt skydd", "Neutral", 
                                                    "Lågt skydd"))) %>%
      ungroup() %>% 
      ggplot(aes(x = ar, y = Andel)) +
      geom_area(aes(color = Risknivå, fill = Risknivå), position = "stack") +
      scale_fill_manual(values = c("#00C18D","#F1E755","#F36B00")) +
      scale_color_manual(values = c("#00C18D","#F1E755","#F36B00")) +
      geom_hline(yintercept = 80, color = "black", linetype = 2, linewidth = 0.66) +
      geom_hline(yintercept = 20, color = RISEprimRed, linetype = 2, linewidth = 0.66) +
      scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100)) +
      #scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
      xlab("Årtal") +
      ylab("Andel i %") +
      theme(
        axis.text.x = element_text(size = ax.size, family = "sans"),
        axis.text.y = element_text(size = ax.size, family = "sans"),
        title = element_text(size = title.size),
        legend.text = element_text(size = legend.size),
        strip.text.x = element_text(size = 14),
        panel.spacing = unit(pandist, "cm", data = NULL)
      ) +
      labs(
        title = paste0(input$pfactor, " - ", input$enkommun),
        subtitle = "Uppdelat på kön",
        caption = str_wrap("Svart streckad linje = referensvärde för 20% med högst skydd.\nRöd linje indikerar referens värde för 20% med lägst skydd.")
      ) +
      facet_wrap(~Kön)
  })  
  
  ## ProtAreaPlotÅK --------------------------------------------------------------
  
  output$ProtAreaPlotÅK <- renderPlot({
    df.plot <- df %>%
      filter(Kommun == input$enkommun) %>%
      mutate(
        Risknivå = case_when(
          .data[[input$pfactor]] < rslimits.prot |>
            select(input$pfactor) |>
            slice(1) |>
            pull() ~ "Lågt skydd",
          .data[[input$pfactor]] >= rslimits.prot |>
            select(input$pfactor) |>
            slice(1) |>
            pull() &
            .data[[input$pfactor]] < rslimits.prot |>
            select(input$pfactor) |>
            slice(2) |>
            pull() ~ "Neutral",
          .data[[input$pfactor]] >= rslimits.prot |>
            select(input$pfactor) |>
            slice(2) |>
            pull() ~ "Högt skydd",
          TRUE ~ "Otillräckliga svar"
        )
      )
    
    df.plot %>%
      filter(Kön %in% c("Flicka","Pojke")) %>%
      filter(!Risknivå == "Otillräckliga svar") %>% 
      group_by(ar, ARSKURS) %>%
      count(Risknivå) %>%
      mutate(Andel = (100 * n / sum(n)) %>% round(digits = 1)) %>%
      mutate(Risknivå = factor(Risknivå, levels = c("Högt skydd", "Neutral", 
                                                    "Lågt skydd"))) %>%
      ungroup() %>% 
      ggplot(aes(x = ar, y = Andel)) +
      geom_area(aes(color = Risknivå, fill = Risknivå), position = "stack") +
      scale_fill_manual(values = c("#00C18D","#F1E755","#F36B00")) +
      scale_color_manual(values = c("#00C18D","#F1E755","#F36B00")) +
      geom_hline(yintercept = 80, color = "black", linetype = 2, linewidth = 0.66) +
      geom_hline(yintercept = 20, color = RISEprimRed, linetype = 2, linewidth = 0.66) +
      scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100)) +
      #scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
      xlab("Årtal") +
      ylab("Andel i %") +
      theme(
        axis.text.x = element_text(size = ax.size, family = "sans"),
        axis.text.y = element_text(size = ax.size, family = "sans"),
        title = element_text(size = title.size),
        legend.text = element_text(size = legend.size),
        strip.text.x = element_text(size = 14),
        panel.spacing = unit(pandist, "cm", data = NULL)
      ) +
      labs(
        title = paste0(input$pfactor, " - ", input$enkommun),
        subtitle = "Uppdelat på årskurs",
        caption = str_wrap("Svart streckad linje = referensvärde för 20% med högst skydd. Röd linje indikerar referens värde för 20% med lägst skydd.")
      ) +
      facet_wrap(~ARSKURS)
  })  

# Demografi ---------------------------------------------------------------

  output$demogrF5 <- renderPlot({
  
  df %>% 
    select(input$demogrVar,ar,Kommun) %>%
    filter(Kommun %in% input$kommunDemo) %>%
    #filter(ar >= yearsDemo[1] | ar <= yearsDemo[2]) %>% 
    #na.omit() %>%
    group_by(ar,Kommun) %>%
    pivot_longer(input$demogrVar) %>% 
    count(name, value) %>% 
    mutate(percent = (100 * n / sum(n)) %>% round(digits = 3)) %>% 
    mutate(proportion = (n/sum(n) %>% round(digits = 3))) %>% 
    mutate(sem = sqrt(proportion*(1-proportion)/sum(n))) %>% 
    mutate(lower.95ci = proportion - sem*1.96,
           upper.95ci = proportion + sem*1.96
    ) %>% 
    rename(Svarsalternativ = value,
           'Antal svar' = n,
           Procent = percent) %>% 
    mutate(Procent = round(Procent,1)) %>% 
    mutate(procentText = sprintf("%1.1f%%", Procent)) %>% 
    mutate(År = factor(ar)) %>% 
    ggplot(aes(x = Svarsalternativ, y = Procent, fill = År, group = År)) +
    geom_bar(position=position_dodge(), stat = 'identity') +
    # geom_errorbar(aes(ymin=lower.95ci*100, ymax=upper.95ci*100),
    #               width=.2,
    #               position = position_dodge(.9)) +
    # geom_text(aes(label = .data$'Antal svar'), 
    #           position = position_dodge(width = 1),
    #           hjust = -0.5, vjust = -0.9) +
    # #geom_text(aes(label = paste0(procentText," +/- ",sprintf("%1.1f%%",round(sem*1.96*100,2))), y = 7, angle = 90),
    # geom_text(aes(label = procentText, y = 2, angle = 0),
    #           position = position_dodge(width = 0.9),
    #           hjust = 0.5,
    #           color = 'white') +
    scale_fill_viridis_d(begin = 0.3, end = 0.9, option = 7) +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
      theme(
        axis.text.x = element_text(size = ax.size, family = "sans"),
        axis.text.y = element_text(size = ax.size, family = "sans"),
        title = element_text(size = title.size),
        legend.text = element_text(size = legend.size),
        strip.text.x = element_text(size = 14),
        panel.spacing = unit(pandist, "cm", data = NULL),
        legend.position = "right",
        plot.caption = element_text(face = 'italic', size = 8)
      ) +
    labs(title = str_wrap(paste0(input$demogrVar)),
         caption = "Siffrorna ovanför staplarna anger antalet respondenter i varje svarskategori.") +
    facet_wrap(~Kommun)  
  })
  
  output$demogrAntal <- renderPlot({
    
    df %>% 
      select(ar,Kommun,Kön,ARSKURS) %>%
      filter(!Kommun == "Stockholm") %>% 
      filter(Kommun %in% input$kommunDemo) %>% 
      na.omit() %>% 
      group_by_all() %>% 
      summarise(Antal = n()) %>% 
      mutate(År = factor(ar)) %>% 
      ggplot(aes(x = År, y = Antal, fill = Kön)) +
      geom_col(position = "stack") +
      scale_fill_viridis_d(begin = 0.3, end = 0.9, option = 7) +
      scale_x_discrete(breaks = årtal, guide = guide_axis(n.dodge = 2)) +
      theme(
        axis.text.x = element_text(size = ax.size, family = "sans"),
        axis.text.y = element_text(size = ax.size, family = "sans"),
        title = element_text(size = title.size),
        legend.text = element_text(size = legend.size),
        strip.text = element_text(size = 14),
        panel.spacing = unit(pandist, "cm", data = NULL)
      ) +
      labs(title = "Antal respondenter per år",
           subtitle = "OBS att y-axeln kan skilja sig åt mellan kommuner") +
      facet_grid(Kommun~ARSKURS,
                 scales = "free")
      
  })
  

## Antal svar --------------------------------------------------------------

  # output$responsesGender <- renderGirafe({
  #   responsesGender <- df %>% 
  #     filter(Kommun == input$kommunDemo,
  #            !Kön == "<NA>") %>% 
  #     rename(År = ar,
  #            Årskurs = ARSKURS) %>% 
  #     group_by(År,Kön) %>% 
  #     summarise(Antal = n()) %>% 
  #     ggplot(aes(x = År, y = Antal, color = Kön)) + 
  #     geom_line(linewidth = 1) +
  #     geom_point_interactive(aes(tooltip = Antal),
  #                            size = 2.5) +
  #     scale_color_brewer(type = "qual", palette = "Dark2") +
  #     scale_x_continuous(guide = guide_axis(n.dodge = 1), breaks = årtal) +
  #     scale_y_continuous(limits = c(0,1000)) +
  #     labs(title = "Antal respondenter per år",
  #          subtitle = "Fördelat på kön")
  #   
  #   girafe(ggobj = responsesGender)
  # })
  
  
# Jämförelser  ----------------------------------------------------------------


## radarPlot ---------------------------------------------------------------

  output$radarPlot <- renderPlot({
    sums.index %>%
      filter(Kommun %in% input$kommun) %>%
      filter(Faktor %in% rfactors) %>% 
      filter(Kön %in% c("Flicka","Pojke")) %>%
      filter(År == input$year2) %>% 
      ggplot(aes(x = Faktor, y = Medel, group = Kommun, color = Kommun, fill = Kommun)) + # make plot, with area color
      geom_line(linewidth = 1.2,
                alpha = 0.8) +
      geom_point(size = 3) +
      see::coord_radar(theta = "x", start = 3, clip = "off") +
      scale_y_continuous(limits = c(-3, NA), expand = c(0,0, 0, 0)) +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
      theme(
        axis.text.x = element_text(size = ax.size, family = "sans"),
        axis.text.y = element_blank(),
        title = element_text(size = title.size),
        legend.text = element_text(size = legend.size),
        strip.text.x = element_text(size = 14),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(color = 'grey', linetype = 'dotted'),
        panel.grid.minor = element_line(color = 'grey', linewidth = 2),
        panel.spacing = unit(pandist, "cm", data = NULL)
      ) +
      labs(title = paste0("Riskfaktorer ",input$year2),
           subtitle = "Högre värde = större risk",
           y = "", x = "") +
      facet_wrap(~Kön)
  })
  
  
  ## circlePlotWrap ----------------------------------------------------------
  
  output$circlePlotWrap <- renderPlot(
    {
      df.risk %>%
        #filter(!riskLevel == "NA") %>%
        filter(!Kommun == "Stockholm") %>%
        filter(År == input$year2) %>%
        filter(!Index %in% c("Välbefinnande","Positiv skolanknytning")) %>%
        mutate(riskLevel = car::recode(riskLevel,"NA='För få svar';
                                   'Medelhög risk'='Något förhöjd risk';
                                   'Hög risk'='Förhöjd risk'")) %>% 
        mutate(Risknivå = factor(riskLevel, levels = c("För få svar", "Låg risk", "Något förhöjd risk", "Förhöjd risk"))) %>%
        ggplot(aes(x = Index, y = Andel, fill = Risknivå)) +
        geom_col(alpha = 0.8) +
        geom_textpath(aes(label = Index, group = Index),
                      text_only = T,
                      position = "stack",
                      hjust = 0
        ) +
        coord_polar(theta = "y") +
        scale_fill_manual(values = c("lightgrey","#009E73", "#F0E442", "#D55E00")) +
        theme_bw() +
        scale_x_discrete(
          expand = expansion(add = c(3, 0)),
          limits = rev,
          labels = NULL
        ) +
        scale_y_continuous(
          breaks = seq(0, 90, 10),
          labels = paste0(seq(0, 90, 10), "%"),
          limits = c(0, 100)
        ) +
        labs(title = paste0(input$year2)) +
        geom_texthline(
          yintercept = 10, color = "black",
          linetype = 2, size = 4, alpha = 0.6,
          label = "Förhöjd risk",
          hjust = 0.05
        ) +
        geom_texthline(
          yintercept = 25, color = RISEprimRed,
          linetype = 2, size = 4, alpha = 0.6,
          label = "Något förhöjd risk",
          hjust = 0.15
        ) +
        theme(
          axis.text.x = element_text(size = ax.size, family = "sans"),
          axis.text.y = element_text(size = ax.size, family = "sans"),
          title = element_text(size = title.size),
          legend.text = element_text(size = legend.size),
          strip.text.x = element_text(size = 14),
          panel.spacing = unit(pandist, "cm", data = NULL)
        ) +
        xlab("") +
        ylab("") + 
        facet_wrap(~Kommun)
    }
  )

  ## mpanel90 ------------------------------------------------------------

  output$mpanel90 <- renderPlot({
    minyear <- input$years[1]
    maxyear <- input$years[2]

    df.plot <- sums.index %>%
      filter(Kommun == input$enkommun2) %>%
      filter(Kön %in% c("Flicka", "Pojke")) %>%
      filter(
        År >= minyear,
        År <= maxyear
      ) %>%
      mutate(År = as.factor(År))

    df.plot <- df.plot %>%
      filter(Faktor %in% input$faktorer)

    ggplot(df.plot, aes(x = År, y = n.90, tooltip = n, data_id = n)) +
      geom_bar(aes(group = Kön, color = Kön, fill = Kön),
        position = "dodge",
        stat = "identity"
      ) +
      # geom_bar(aes_string())
      scale_y_continuous(limits = c(0, 30)) +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
      # scale_color_manual(values = c(RISEprimGreen,RISEcompPurple)) +
      # scale_fill_manual(values = c(RISEprimGreen,RISEcompPurple)) +
      scale_fill_viridis_d(begin = 0.3, end = 0.8) +
      scale_color_viridis_d(begin = 0.3, end = 0.8) +
      theme(
        axis.text.x = element_text(size = ax.size, family = "sans"),
        axis.text.y = element_text(size = ax.size, family = "sans"),
        title = element_text(size = title.size),
        legend.text = element_text(size = legend.size),
        strip.text.x = element_text(size = 12)
      ) +
      geom_hline(yintercept = 10, color = RISEprimRed, linetype = 2, size = 0.66) +
      # geom_hline(yintercept = 30, color = RISEprimRed, linetype = 2, size = 0.66) +
      ggtitle(paste0("Andel högrisk, per år", " - ", input$enkommun2)) +
      xlab("") +
      ylab("") +
      facet_wrap(~Faktor, labeller = labeller(Faktor = label_wrap_gen(10)))
  })



  ## lineEduLevel ------------------------------------------------------------

  output$lineEduLevel <- renderPlot({
    df.plot <- df %>%
      filter(Kommun %in% c("Vaxholm", "Vallentuna")) %>%
      group_by(ar, Kommun) %>%
      count(eduLevel) %>%
      # spread(ar,n) %>%
      rename(
        År = ar,
        Antal = n
      ) %>%
      mutate(År = as.factor(År))

    ggplot(df.plot, aes(x = År, y = Antal, group = eduLevel, color = eduLevel)) + # make plot, with area color
      geom_line(size = 1.5) +
      geom_point(size = 3) +
      # scale_y_continuous(limits = c(-2,2)) +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
      scale_fill_viridis_d(begin = 0.3, end = 0.8) +
      scale_color_viridis_d(begin = 0.3, end = 0.8) +
      theme(
        axis.text.x = element_text(size = ax.size, family = "sans"),
        axis.text.y = element_text(size = ax.size, family = "sans"),
        title = element_text(size = title.size),
        legend.text = element_text(size = legend.size)
      ) +
      facet_wrap(~Kommun) +
      # geom_hline(yintercept = 10, color = RISEprimRed, linetype = 2, size = 0.4) +
      labs(title = "Respondenternas föräldrars utbildningsnivå", subtitle = "") +
      xlab("Årtal") +
      ylab("Antal")
  })

  ## lineBottSE ------------------------------------------------------------


  output$lineBottSE <- renderPlot({
    df.plot <- df %>%
      filter(Kommun %in% c("Vaxholm", "Vallentuna")) %>%
      group_by(ar, Kommun) %>%
      count(bottSE) %>%
      # spread(ar,n) %>%
      rename(
        År = ar,
        Antal = n
      ) %>%
      mutate(År = as.factor(År))

    ggplot(df.plot, aes(x = År, y = Antal, group = bottSE, color = bottSE)) + # make plot, with area color
      geom_line(size = 1.5) +
      geom_point(size = 3) +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
      scale_fill_viridis_d(begin = 0.3, end = 0.8) +
      scale_color_viridis_d(begin = 0.3, end = 0.8) +
      theme(
        axis.text.x = element_text(size = ax.size, family = "sans"),
        axis.text.y = element_text(size = ax.size, family = "sans"),
        title = element_text(size = title.size),
        legend.text = element_text(size = legend.size)
      ) +
      facet_wrap(~Kommun) +
      labs(title = "Hur länge respondenterna bott i Sverige", subtitle = "") +
      xlab("Årtal") +
      ylab("Antal")
  })


  # Social gradient -------------------------------------------------------------

  ## SoclineYearsMean --------------------------------------------------------------
  
  
  output$SocLineYearsMean <- renderPlot({
    
    df %>% 
      filter(Kommun == input$enkommun4) %>% 
      select(input$factor4,ar,Kommun,input$socialg) %>% 
      rename(SocialG = input$socialg,
             Faktor = input$factor4) %>% 
      na.omit(SocialG) %>% 
      group_by(ar,Kommun,SocialG) %>% 
      summarise(Medel = mean(Faktor, na.rm = T),
                StDev = sd(Faktor,  na.rm = T),
                sd.lo = Medel-StDev,
                sd.hi = Medel+StDev,
                n = n()) %>% 
      filter(n > 10) %>% 
      mutate(År = factor(ar)) %>% 
      mutate(across(where(is.numeric), round, 2)) %>% 
      as.data.frame() %>% 
      ggplot(aes(x = År, y = Medel, group = SocialG, color = SocialG, fill = SocialG)) + # make plot, with area color
      geom_line(linewidth = 1.5) +
      geom_point(size = 3) +
      geom_ribbon(aes(ymin = sd.lo, ymax = sd.hi),
                  alpha = 0.1, linetype = 0
      ) +
      geom_text_repel(aes(label = n)) +
      scale_y_continuous(limits = c(-2, 2)) +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
      # scale_fill_viridis_d(begin = 0.3, end = 0.8) +
      # scale_color_brewer(type = 'qual', palette = 'Dark2') +
      # scale_fill_brewer(type = 'qual', palette = 'Dark2') +
      # scale_color_viridis_d(begin = 0.3, end = 0.8) +
      theme(
        axis.text.x = element_text(size = ax.size, family = "sans"),
        axis.text.y = element_text(size = ax.size, family = "sans"),
        title = element_text(size = title.size),
        legend.text = element_text(size = legend.size),
        panel.spacing = unit(pandist, "cm", data = NULL)
      ) +
      # facet_wrap(~ Kommun) +
      # geom_hline(yintercept = 10, color = RISEprimRed, linetype = 2, size = 0.4) +
      labs(title = paste0("Medelvärde över tid", " - ", input$socialg), 
           subtitle = "Skuggat fält indikerar en standardavvikelse (~ 68%)") +
      xlab("Årtal") +
      ylab(paste0(input$factor4))
  })
  

  ## slab2multi ----------------------------------------------------------

  output$slab2multi <- renderPlot({
    df.plot <- df %>%
      filter(ar == input$year2) %>% # filter the year selected
      # filter(Kön %in% input$gender) %>%  # and gender(s) selected
      filter(Kommun %in% input$kommun) %>%
      select(input$factor2, Kommun, ar) %>%
      rename(RSfaktor = input$factor2)
    varmean <- sums.index %>% # get the mean for all of Stockholm, which will be the reference mean for all
      filter(Faktor == input$factor2) %>%
      filter(Kommun == "Stockholm Stad") %>%
      filter(År == input$year2) %>%
      filter(Kön == "Flickor och pojkar") %>% # for allsums.index (file with gender averages also included)
      pull(Medel)
    var90 <- rslimits %>% # get the cutoff point for 90% percentile, based on all years of Stockholm data
      select(input$factor2) %>%
      slice(2) %>%
      pull() # row 2 has 90% percentile
    n90 <- sums.index %>%
      filter(Faktor == input$factor2) %>%
      filter(Kommun %in% input$kommun) %>%
      filter(År == input$year2) %>%
      select(n.90) %>%
      round(2) %>%
      pull()

    ggplot(df.plot, aes(x = RSfaktor, y = Kommun, fill = Kommun)) + # make plot, with fill color by municipality
      stat_slab(aes(fill_ramp = after_stat(level), fill = Kommun),
        side = "right", show.legend = F,
        scale = 0.6, # defines the height that a slab can reach
        position = position_dodge(width = .6), # distance between elements for dodging
        .width = c(.50, .90, 1)
      ) + # set shading
      stat_dots(aes(color = Kommun),
        side = "left",
        scale = 0.9,
        show.legend = F,
        position = position_dodge(width = .9),
        alpha = 0.15
      ) +
      stat_summary(
        fun.data = "mean_cl_normal",
        show.legend = F,
        size = .4,
        position = position_dodge2nudge(x = .05, width = .8)
      ) +
      scale_fill_ramp_discrete(from = "black", aesthetics = "fill_ramp") +
      # scale_fill_viridis_d(begin = 0.4, end = 0.8) +
      #geom_vline(xintercept = varmean, color = RISEprimGreen, linetype = 2, linewidth = 0.8) +
      geom_vline(xintercept = var90, color = RISEprimRed, linetype = 2, linewidth = 0.8) +
      # annotate("text", x = var90+0.52, y = 1.91, label=paste0(n90[2],"%"), na.rm = T, color = RISEprimRed, size = 6) +
      # annotate("text", x = var90+0.52, y = 0.91, label=paste0(n90[1],"%"), na.rm = T, color = RISEprimRed, size = 6) +
      # scale_x_continuous(limits = c(0,100)) +
      # styling
      theme(
        axis.text.x = element_text(size = ax.size, family = "sans"),
        axis.text.y = element_text(size = ax.size, family = "sans"),
        title = element_text(size = title.size),
      ) +
      xlab("") +
      ylab("") +
      labs(
        title = paste0(input$factor2, " - ", input$year2),
        caption = str_wrap("Röd streckad linje = referensvärde 90:e percentilen.
                                Fältet i mitten visar fördelningen av 50% av respondenterna, nästa skuggade fält visar 90%.
                                Pricken i mitten visar medelvärdet.")
      )
  })


  ## lineYearsMean --------------------------------------------------------------


  output$lineYearsMean <- renderPlot({
    df.plot <- sums.index %>%
      filter(Faktor == input$factor2) %>%
      filter(Kommun %in% input$kommun) %>%
      filter(Kön == "Flickor och pojkar") %>%
      mutate(År = as.factor(År))
    # mutate(År = paste0(År, "\n", "n=", n))

    ggplot(df.plot, aes(x = År, y = Medel, group = Kommun, color = Kommun, fill = Kommun)) + # make plot, with area color
      geom_line(linewidth = 1.5) +
      geom_point(size = 3) +
      geom_ribbon(aes(ymin = sd.lo, ymax = sd.hi),
        alpha = 0.1, linetype = 0
      ) +
      scale_y_continuous(limits = c(-2, 2)) +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
      # scale_fill_viridis_d(begin = 0.3, end = 0.8) +
      # scale_color_brewer(type = 'qual', palette = 'Dark2') +
      # scale_fill_brewer(type = 'qual', palette = 'Dark2') +
      # scale_color_viridis_d(begin = 0.3, end = 0.8) +
      theme(
        axis.text.x = element_text(size = ax.size, family = "sans"),
        axis.text.y = element_text(size = ax.size, family = "sans"),
        title = element_text(size = title.size),
        legend.text = element_text(size = legend.size),
        panel.spacing = unit(pandist, "cm", data = NULL)
      ) +
      # facet_wrap(~ Kommun) +
      # geom_hline(yintercept = 10, color = RISEprimRed, linetype = 2, size = 0.4) +
      labs(title = "Medelvärde över tid", subtitle = "Skuggat fält indikerar en standardavvikelse (~ 68%)") +
      xlab("Årtal") +
      ylab(paste0(input$factor2))
  })

  ## line90 ------------------------------------------------------------------

  output$line90 <- renderPlot({
    minyear <- input$years2[1]
    maxyear <- input$years2[2]
    df.plot <- sums.index %>%
      filter(Faktor == input$factor2) %>%
      filter(Kommun %in% input$kommun) %>%
      filter(Kön %in% c("Flicka", "Pojke")) %>%
      filter(
        År >= minyear,
        År <= maxyear
      ) %>%
      mutate(År = as.factor(År))

    ggplot(df.plot, aes(x = År, y = n.90, group = Kön, color = Kön, tooltip = n, data_id = n)) + # make plot, with area color
      geom_line(linewidth = 1.5) +
      geom_point(size = 3) +
      # geom_point(size = 3) +
      scale_y_continuous(limits = c(0, 30)) +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
      # scale_fill_viridis_d(begin = 0.3, end = 0.8) +
      # scale_color_viridis_d(begin = 0.3, end = 0.8) +
      # scale_color_manual(values = RISEpalette1) +
      # scale_color_hue(direction = -1, h.start=30) +
      scale_color_brewer(type = "qual", palette = "Dark2") +
      theme(
        axis.text.x = element_text(size = ax.size, family = "sans"),
        axis.text.y = element_text(size = ax.size, family = "sans"),
        title = element_text(size = title.size),
        legend.text = element_text(size = legend.size),
        strip.text.x = element_text(size = 12),
        panel.spacing = unit(pandist, "cm", data = NULL)
      ) +
      # facet_wrap(~ Kommun) +
      geom_hline(yintercept = 10, color = RISEprimRed, linetype = 2, linewidth = 0.4) +
      ggtitle("Procent över percentil 90, per år") +
      xlab("") +
      ylab(paste0(input$factor2)) +
      facet_wrap(~Kommun, labeller = labeller(Kommun = label_wrap_gen(12))) # wrap text to fit facet box
  })



  # Exempel enkät -----------------------------------------------------------

  ## personaTable -----------------------------------------------------------------

  output$personaTable <- renderFormattable({
    # typical response patterns for different risk levels
    if (input$risknivå == "Låg risk") {
      #p.resp <- c(1, 1, 1, 2, 2, 1, 1, 3)
      p.resp <- psf.modes$V1 # for using actual mode data
    } else if (input$risknivå == "Något förhöjd risk") {
      #p.resp <- c(2, 1, 1, 2, 3, 2, 2, 2)
      p.resp <- psf.modes$V2 # for using actual mode data
    } else if (input$risknivå == "Förhöjd risk") {
      #p.resp <- c(3, 2, 2, 3, 3, 2, 2, 1)
      p.resp <- psf.modes$V3 # for using actual mode data
    }

    psf.table <- itemlabels.final %>%
      filter(Index == "PsykSomBesv") %>%
      select(!Index)

    itemresponses %>%
      filter(itemnr %in% psf.table$itemnr) %>%
      select(itemnr, starts_with("Svarskategori")) %>%
      mutate(across(everything(), ~ car::recode(.x,"NA=''"))) %>% 
      left_join(psf.table) %>%
      relocate(item, .after = itemnr) %>%
      formattable(.,
        align = c("r", "l", "c", "c", "c", "c", "c"), list(
          formattable::area(row = 1, col = 3 + p.resp[1]) ~ color_tile("lightblue", "lightpink"),
          formattable::area(row = 2, col = 3 + p.resp[2]) ~ color_tile("lightblue", "lightpink"),
          formattable::area(row = 3, col = 3 + p.resp[3]) ~ color_tile("lightblue", "lightpink"),
          formattable::area(row = 4, col = 3 + p.resp[4]) ~ color_tile("lightblue", "lightpink"),
          formattable::area(row = 5, col = 3 + p.resp[5]) ~ color_tile("lightblue", "lightpink"),
          formattable::area(row = 6, col = 3 + p.resp[6]) ~ color_tile("lightblue", "lightpink"),
          formattable::area(row = 7, col = 3 + p.resp[7]) ~ color_tile("lightblue", "lightpink"),
          formattable::area(row = 8, col = 3 + p.resp[8]) ~ color_tile("lightblue", "lightpink")
        ),
        table.attr = 'class=\"table table-striped\" style="font-size: 11px; font-family: Lato"'
      )
  })


  ## personaPlot -------------------------------------------------------------

  output$personaPlot <- renderPlot({

    # typical response patterns for different risk levels
    if (input$risknivå == "Låg risk") {
      #p.resp <- c(1, 1, 1, 2, 2, 1, 1, 3)
      p.resp <- psf.modes$V1 # for using actual mode data
    } else if (input$risknivå == "Något förhöjd risk") {
      #p.resp <- c(2, 1, 1, 2, 3, 2, 2, 2)
      p.resp <- psf.modes$V2 # for using actual mode data
    } else if (input$risknivå == "Förhöjd risk") {
      #p.resp <- c(3, 2, 2, 3, 3, 2, 2, 1)
      p.resp <- psf.modes$V3 # for using actual mode data
    }
    # add code for other options

    avg1<-thetaEst(psf.itemps, p.resp, model = "PCM", method = "WL")
    sem1<-semTheta(thEst = avg1, it = psf.itemps, x = p.resp, model = "PCM", method = "WL")
    sem.lo <- avg1-sem1
    sem.hi <- avg1+sem1

    # these are not used currently, remove if they keep being unused :)
    # var90 <- rslimits %>% # get the cutoff point for 90% percentile, based on all years of Stockholm data
    #   select(input$factor) %>%
    #   slice(2) %>% pull() # row 2 has 90% percentile
    # var70 <- rslimits %>% # get the cutoff point for 90% percentile, based on all years of Stockholm data
    #   select(input$factor) %>%
    #   slice(1) %>% pull() # row 1 has 70% percentile

    df %>%
      filter(Kommun == "Stockholm",
             ar == "2020") %>%
      # first a test with histogram like in RItargeting():
      # ggplot() +
      #   geom_histogram(aes_string(rlang::sym(input$factor), y= "..count..")) +
      #   xlab(paste0(input$factor)) +
      #   ylab('Antal individer') +
      #   scale_x_continuous(limits = c(-4,4), breaks = scales::pretty_breaks(n = 8)) +
      #   geom_vline(xintercept = avg1, color = RISEcompGreenDark, linetype = 2) +
      #   annotate("rect", ymin = 0, ymax = Inf, xmin = sem.lo, xmax = sem.hi, alpha = .2) +
      #   #geom_text(hjust = 1.1, vjust = 1) +
      #   theme_bw() +
      #   theme(legend.position = 'none',
    #         text=element_text(family = "sans"),
    #         axis.text.x = element_text(size=ax.size, family = "sans"),
    #         axis.text.y = element_text(size=ax.size, family = "sans"),
    #         title = element_text(size=title.size),
    #         legend.text = element_text(size=legend.size))
    ### slab plot
    ggplot(aes(x = `Psykiska/ psykosomatiska besvär`, Kommun, fill = Kommun)) + # make plot, with area color
      stat_slab(side = "right", show.legend = F,
                scale = 0.6, # defines the height that a slab can reach
                position = position_dodge(width=.6), # distance between elements for dodging
                aes(fill_ramp = after_stat(level), fill=Kommun),
                .width = c(.50,.75,1)) +  # set shading
      # stat_dots(side = "left",scale = 0.9, show.legend = F,
      #           position = position_dodge(width = .9),aes(color = Kommun),
      #           alpha = 0.15) +
      stat_summary(fun.data = "mean_cl_normal",show.legend = F, size = .4,
                   position = position_dodge2nudge(x=.05,width = .8)) +
      scale_fill_ramp_discrete(from='black', aesthetics = "fill_ramp") +
      scale_fill_viridis_d(begin = 0.4) +
      #geom_vline(xintercept = var70, color = "orange", linetype = 2, size = 0.8) +
      #geom_vline(xintercept = var90, color = RISEprimRed, linetype = 2, size = 0.8) +
      geom_vline(xintercept = avg1, color = "orange", linetype = 2) +
      annotate("rect", ymin = 0, ymax = Inf, xmin = sem.lo, xmax = sem.hi, alpha = .1) +
      # styling
      theme(axis.text.x = element_text(size=ax.size, family = "sans"),
            axis.text.y = element_blank(),
            title = element_text(size=title.size),
      )+
      xlab("Psykiska/ psykosomatiska besvär, Stockholm 2020") +
      ylab("Antal personer")

  })

  

# Skolinspektionen --------------------------------------------------------


## SIyearsMean -------------------------------------------------------------

  
  output$SIyearsMean <- renderPlot({
    
    minyear <- input$SIyears[1]
    maxyear <- input$SIyears[2]
    df.plot <- sums.si %>%
      filter(Kommun %in% input$SIkommuner) %>%
      filter(Kön %in% c("Flicka", "Pojke"))
    # filter(
    #   ar >= minyear,
    #   ar <= maxyear
    # )

    ggplot(df.plot, aes(x = År, y = Medel, group = Kommun, color = Kommun, fill = Kommun)) + # make plot, with area color
      geom_line(linewidth = 1.5) +
      geom_point(size = 4) +
      geom_ribbon(aes(ymin = sd.lo, ymax = sd.hi),
                  alpha = 0.1, linetype = 0
      ) +
      scale_y_continuous(limits = c(-2, 4)) +
      #scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
      theme(
        axis.text.x = element_text(size = ax.size, family = "sans"),
        axis.text.y = element_text(size = ax.size, family = "sans"),
        title = element_text(size = title.size),
        legend.text = element_text(size = legend.size),
        panel.spacing = unit(pandist, "cm", data = NULL)
      ) +
      labs(title = "Medelvärde över tid", subtitle = "Skuggat fält indikerar en standardavvikelse (~ 68%)") +
      xlab("Årtal") +
      ylab("Positiv skolanknytning åk 5") +
      facet_wrap(~Kön)
  })
  

## SIareaPlot --------------------------------------------------------------

  output$SIareaPlot <- renderPlot({
    df.plot <- df.si %>%
      filter(Kommun == input$SIenkommun) %>%
      mutate(
        Risknivå = case_when(
          Indexvärde < rslimits.si |>
            select(`Positiv skolanknytning åk 5`) |>
            slice(1) |>
            pull() ~ "Lågt skydd",
          Indexvärde >= rslimits.si |>
            select(`Positiv skolanknytning åk 5`) |>
            slice(1) |>
            pull() &
            Indexvärde < rslimits.si |>
            select(`Positiv skolanknytning åk 5`) |>
            slice(2) |>
              pull() ~ "Neutral",
            Indexvärde >= rslimits.si |>
              select(`Positiv skolanknytning åk 5`) |>
              slice(2) |>
              pull() ~ "Högt skydd",
            TRUE ~ "Otillräckliga svar"
          )
        )
      
    df.plot %>%
      filter(Kön %in% c("Flicka","Pojke")) %>%
      filter(!Risknivå == "Otillräckliga svar") %>% 
      group_by(ar, Kön) %>%
      count(Risknivå) %>%
      mutate(Andel = (100 * n / sum(n)) %>% round(digits = 1)) %>%
      mutate(Risknivå = factor(Risknivå, levels = c("Högt skydd", "Neutral", 
                                                    "Lågt skydd"))) %>%
      ungroup() %>% 
      ggplot(aes(x = ar, y = Andel)) +
      geom_area(aes(color = Risknivå, fill = Risknivå), position = "stack") +
      scale_fill_manual(values = c("#00C18D","#F1E755","#F36B00")) +
      scale_color_manual(values = c("#00C18D","#F1E755","#F36B00")) +
      geom_hline(yintercept = 80, color = "black", linetype = 2, linewidth = 0.66) +
      geom_hline(yintercept = 20, color = RISEprimRed, linetype = 2, linewidth = 0.66) +
      scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100)) +
      #scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
      xlab("Årtal") +
      ylab("Andel i %") +
      theme(
        axis.text.x = element_text(size = ax.size, family = "sans"),
        axis.text.y = element_text(size = ax.size, family = "sans"),
        title = element_text(size = title.size),
        legend.text = element_text(size = legend.size),
        strip.text.x = element_text(size = 14),
        panel.spacing = unit(pandist, "cm", data = NULL)
      ) +
        labs(
          title = paste0("Positiv skolanknytning åk 5", " - ", input$SIenkommun),
          subtitle = "Uppdelat på kön",
          caption = str_wrap("Svart streckad linje = referensvärde för 10% med högst risk. Röd linje indikerar 30% (~ en standardavvikelse).
                                  När kommunens fält är mindre än referensvärdet har man färre respondenter med den risknivån.")
        ) +
        facet_wrap(~Kön)
  })  


## siLine90 ----------------------------------------------------------------
  
  output$siLine90 <- renderPlot({
    minyear <- input$SIyears[1]
    maxyear <- input$SIyears[2]
    df.plot <- sums.si %>%
      filter(Kommun %in% input$SIkommuner) %>%
      filter(Kön %in% c("Flicka", "Pojke"))
      # filter(
      #   ar >= minyear,
      #   ar <= maxyear
      # )
    
    ggplot(df.plot, aes(x = År, y = n.90, group = Kön, color = Kön)) + # make plot, with area color
      geom_line(linewidth = 1.5) +
      geom_point(size = 4) +
      scale_y_continuous(limits = c(0, 30)) +
      #scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
      scale_color_brewer(type = "qual", palette = "Dark2") +
      theme(
        axis.text.x = element_text(size = ax.size, family = "sans"),
        axis.text.y = element_text(size = ax.size, family = "sans"),
        title = element_text(size = title.size),
        legend.text = element_text(size = legend.size),
        strip.text.x = element_text(size = 12),
        panel.spacing = unit(pandist, "cm", data = NULL)
      ) +
      geom_hline(yintercept = 10, color = RISEprimRed, linetype = 2, linewidth = 0.4) +
      ggtitle("Procent över percentil 90, per år") +
      xlab("") +
      ylab("Positiv skolanknytning åk 5") +
      facet_wrap(~Kommun, labeller = labeller(Kommun = label_wrap_gen(12))) # wrap text to fit facet box
  })
  

# KOLADA ------------------------------------------------------------------


## kolaMpanelAll -----------------------------------------------------------

  output$kolaMpanelAll <- renderPlot({
    
    KOLADA %>% 
      filter(Kön == "Alla") %>% 
      filter(Kommun %in% input$kolaKommuner) %>% 
      filter(KPI %in% input$kolaKPI) %>% 
      ggplot(aes(x = År, y = Andel, group = Kommun, color = Kommun)) +
      geom_line() +
      geom_point() +
      scale_x_continuous(guide = guide_axis(n.dodge = 2)) +
      scale_y_continuous(limits = c(0, 100)) +
      theme(
        axis.text.x = element_text(size = ax.size, family = "sans"),
        axis.text.y = element_text(size = ax.size, family = "sans"),
        title = element_text(size = title.size),
        legend.text = element_text(size = legend.size),
        strip.text.x = element_text(size = 14),
        panel.spacing = unit(0.5, "cm", data = NULL)
      ) +
      facet_wrap(~KPI,
                 ncol = 4,
                 scales = "free",
                 labeller = labeller(KPI = label_wrap_gen(25)))
    
  })

## kolaBarsAll -------------------------------------------------------------

  
  output$kolaMpanelAllBars <- renderPlot({
    
    KOLADA %>% 
      filter(Kön == "Alla") %>% 
      filter(Kommun %in% input$kolaKommuner) %>% 
      filter(KPI %in% input$kolaKPI) %>% 
      ggplot(aes(x = Kommun, y = Andel, group = factor(År), fill = Kommun, label = factor(År))) +
      geom_col(position = "dodge") +
      geom_text(aes(y = 8),
                position = position_dodge(width = 0.9), 
                angle = 90) +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
      scale_y_continuous(limits = c(0, 100)) +
      theme(
        axis.text.x = element_text(size = ax.size, family = "sans"),
        axis.text.y = element_text(size = ax.size, family = "sans"),
        title = element_text(size = title.size),
        legend.text = element_text(size = legend.size),
        strip.text.x = element_text(size = 14),
        panel.spacing = unit(0.5, "cm", data = NULL),
        legend.position = "none"
      ) +
      facet_wrap(~KPI,
                 ncol = 4,
                 scales = "free",
                 labeller = labeller(KPI = label_wrap_gen(25)))
    
  })

## kolaMpanelGender --------------------------------------------------------

  output$kolaMpanelGender <- renderPlot({
    
    KOLADA %>% 
      filter(!Kön == "Alla") %>% 
      filter(Kommun %in% input$kolaKommuner) %>% 
      filter(KPI %in% input$kolaKPI) %>% 
      ggplot(aes(x = År, y = Andel, group = Kön, color = Kön)) +
      geom_line() +
      geom_point() +
      scale_color_brewer(type = "qual", palette = "Dark2") +
      scale_x_continuous(guide = guide_axis(n.dodge = 2)) +
      scale_y_continuous(limits = c(0, 100)) +
      theme(
        axis.text.x = element_text(size = ax.size, family = "sans"),
        axis.text.y = element_text(size = ax.size, family = "sans"),
        title = element_text(size = title.size),
        legend.text = element_text(size = legend.size),
        strip.text = element_text(size = 14),
        panel.spacing = unit(0.5, "cm", data = NULL)
      ) +
      facet_grid(Kommun~KPI, 
                 scales = "free",
                 labeller = labeller(KPI = label_wrap_gen(25)))
    
  }) 
  
  # rsFigure -------------------------------------------------------------

  output$rsFigure <- renderUI({
    lst.kontext <- subset(lst.data, Kontext == input$context & RSfaktor == input$factortype)
    # koden nedan är lånad från nedanstående källor, och modifierad:
    # https://www.r-graph-gallery.com/322-custom-colours-in-sankey-diagram
    # https://medium.com/@emtiazahmed.cs/sankey-diagram-step-by-step-using-r-b3e7bea53224
    # https://christophergandrud.github.io/networkD3/

    # extrahera vektorer med unika Spetsar & RS-faktorer
    spetsar <- lst.kontext %>%
      distinct(Spets) %>%
      dplyr::rename(label = Spets)
    faktorer <- lst.kontext %>%
      distinct(Faktor) %>%
      dplyr::rename(label = Faktor)

    # sammanfoga dem
    rsfaktorer <- full_join(faktorer, spetsar, by = "label")
    rsfaktorer <- rsfaktorer %>% rowid_to_column("id")

    # skapa table som visar hur många gånger varje rsfaktor kopplas till en spets
    per_route <- lst.kontext %>%
      group_by(Faktor, Spets) %>%
      dplyr::summarise(count = n()) %>%
      ungroup()

    # ta fram variabler för nätverksmodeller och liknande visualisering
    edges <- per_route %>%
      left_join(rsfaktorer, by = c("Faktor" = "label")) %>%
      dplyr::rename(from = id)
    edges <- edges %>%
      left_join(rsfaktorer, by = c("Spets" = "label")) %>%
      dplyr::rename(to = id)

    edges <- select(edges, from, to, count)
    edges <- mutate(edges, width = count + 1)
    nodes_d3 <- mutate(rsfaktorer, id = id - 1)
    edges_d3 <- mutate(edges, from = from - 1, to = to - 1)
    edges_d3$group <- per_route$Spets
    nodes_d3$nodecolor <- c("allsamecolor")

    # färgsättning av flöden (edges) utifrån spetsarna och rätblocken intill faktorer & spetsar i diagrammet
    # kulörer lånade bl.a. från http://opencolor.tools/palettes/wesanderson/
    my_color <- 'd3.scaleOrdinal() .domain(["Psyk. ohälsa","Utanförskap","Våld","Kriminalitet","Missbruk/ANDTS", "allsamecolor"]) 
              .range(["lightblue", "#F5CDB6", "#F7B0AA", "#FDDDA4", "#76A08A", "#FCD16B"])'
    # färgkod #FCD16B för skyddsfaktorer (förvalt i koden ovanför) och #D8A49B för riskfaktorer
    # ändra "#FCD16B" på rad 91 till "#D8A49B" för att byta färg på rätblocken

    # skapa ett interaktivt Sankey-diagram där spetsarna i preventionsstjärnan finns till höger.
    sankeyNetwork(
      Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to",
      NodeID = "label", Value = "count", fontSize = 20, unit = "Antal",
      fontFamily = "sans-serif", LinkGroup = "group", colourScale = my_color,
      nodeWidth = 13, NodeGroup = "nodecolor", nodePadding = 18
    )
  })
  # Insatser ----------------------------------------------------------------
  
  output$insatserTable <- render_gt({
    gt(insatserMain,
       groupname_col = "Kontext") %>% 
      gt_theme_espn() %>% 
      tab_options(table.font.name = "Lato",
                  container.width = 500,
                  heading.align = "left") %>% 
      cols_align(align = "right",
                 columns = "Insats") %>% 
      tab_footnote(insatserFotnot$fotnot)
  })
}




# Run the application
# shinyApp(ui = ui, server = server)
