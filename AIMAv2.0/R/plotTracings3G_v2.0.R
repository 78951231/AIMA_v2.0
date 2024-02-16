#' makes bar plots along with tracing plots.
#' this assumes data of 3 groups with 2 conditions.

plotTracings3G_v2.0 = function(groupAvg_channelTracings,
                             channelTracings_groups,
                             deltaChannel_groups,
                             avgChannelvalues_groups,
                             groupNames,
                             trial_length,
                             conditionNames,
                             refGroup) {

  groupAvg_channelTracings = as.data.frame(groupAvg_channelTracings)
  groupAvg_channelTracings = groupAvg_channelTracings %>% mutate(rng = row_number()) %>% pivot_longer(cols = -rng,
                                                                                              names_to = "name1",
                                                                                              values_to = "value1")
  channelTracings_groups = as.data.frame(channelTracings_groups)
  channelTracings_groups = channelTracings_groups %>% mutate(rn = row_number()) %>% pivot_longer(cols = -rn,
                                                                                         names_to = "name",
                                                                                         values_to = "value")

  min_Xaxis = 0
  max_Xaxis = trial_length*60*2
  min_Yaxis = -60
  max_Yaxis = 30
  noSegments_Xaxis = (max_Xaxis - min_Xaxis)/4
  noSegments_Yaxis = (max_Yaxis - min_Yaxis)/3
  xlocationChamberON = trial_length*60
  ylocationChamberON = max_Yaxis - 5
  locationOH = min_Yaxis + 15
  maxYaxis_barGraph = ceiling((max(as.numeric(avgChannelvalues_groups))+30)/10)*10
  #refGroup = groupNames[1]
  maxYaxis_deltaGraph = ceiling((max(as.numeric(deltaChannel_groups))+15)/10)*10
  minYaxis_deltaGraph = floor((min(as.numeric(deltaChannel_groups))-10)/10)*10

  tracingGraph = ggplot() +
    geom_smooth(
      data = channelTracings_groups,
      aes(x = rn, y = value, group = name),
      color = 'snow2',
      method = 'loess',
      span = 0.1,
      linewidth = 1.0,
      se = FALSE
    ) +
    geom_smooth(
      data = groupAvg_channelTracings,
      aes(
        x = rng,
        y = value1,
        group = name1,
        color = name1
        ),
      method = 'loess',
      span = 0.1,
      linewidth = 2.5,
      se = FALSE
    ) +
    scale_color_manual(
      #labels = groupNames, # c("SCI", "SCI-EES", "healthy")
      values = c("grey75", "steelblue", "plum1")
    ) +
    labs(x = "time (sec)", y = expression(Delta * SBP (mmHg))) +
    #ggtitle("Week 5") +
    annotate(
      geom = "text",
      x = xlocationChamberON,
      y = ylocationChamberON,
      family = "Helvetica",
      label = "Chamber ON",
      size = 6,
      color = "black",
      hjust = -0.03,
      fontface = "plain"
    ) +
    annotate(
      geom = "text",
      x = min_Xaxis + 1,
      y = locationOH,
      family = "Helvetica",
      label = "Orthostatic hypotension",
      size = 6,
      color = "red",
      hjust = -0.02
    ) +
    theme_prism() +
    theme(plot.title = element_text(
      color = "black",
      size = 20,
      face = "plain",
      hjust = 0.5
    )) +
    theme(
      axis.title.x = element_text(
        color = "black",
        face = "plain",
        family = "Helvetica",
        size = 20
      ),
      axis.title.y = element_text(
        color = "black",
        face = "plain",
        family = "Helvetica",
        size = 20
      )
    ) +
    theme(
      axis.text.x = element_text(
        color = "black",
        size = 20,
        family = "Helvetica",
        face = "plain"
      ),
      axis.text.y = element_text(
        color = "black",
        size = 20,
        hjust = 0,
        family = "Helvetica",
        face = "plain"
      )
    ) +
    scale_x_continuous(expand = c(0, 0), breaks = seq(min_Xaxis, max_Xaxis, by = noSegments_Xaxis)) +
    scale_y_continuous(expand = c(0, 0), breaks = seq(min_Yaxis, max_Yaxis, by = noSegments_Yaxis)) +
    coord_cartesian(xlim = c(min_Xaxis, max_Xaxis+5), ylim = c(min_Yaxis, max_Yaxis)) +
    #theme(axis.ticks.length.x  = unit(0.2, "cm")) +
    theme(axis.ticks.length.y  = unit(0.2, "cm")) +
    # theme(axis.ticks.x = element_line(color = "black",
    #                                   linewidth = 1.0)) +
    theme(axis.ticks.y = element_line(color = "black",
                                      linewidth = 1.0)) +
    theme(axis.line.x = element_line(
      color = "black",
      linewidth = 1.0,
      linetype = 1
    )) +
    theme(axis.line.y = element_line(
      color = "black",
      linewidth = 1.0,
      linetype = 1
    )) +
    theme(panel.grid = element_blank()) +
    theme(
      plot.background = element_blank(),
      panel.border = element_blank(),
      legend.title = element_blank(),
      legend.box.background = element_blank(),
      legend.key = element_rect(fill = "transparent", colour = "transparent"),
      legend.background = element_blank()
    ) +
    geom_vline(
      xintercept = (trial_length*60),
      linetype = "dotted",
      color = "grey75",
      linewidth = 1.0
    ) +
    annotate(
      'rect',
      xmin = 0,
      xmax = (trial_length*60*2)+5,
      ymin = -60,
      ymax = -20,
      alpha = .5,
      fill = 'pink'
    )

  ##############################################################################
  #########. bar graphs for conditions comparisons #############################
  ##############################################################################
  BaselineTilt = avgChannelvalues_groups
  No_of_Conditions = length(conditionNames)
  No_of_Subjects = length(avgChannelvalues_groups)/(length(groupNames)*No_of_Conditions)
  df1 <-
    data.frame(
      groups_dat = rep(groupNames, each = No_of_Subjects * No_of_Conditions),
      conditions = rep(conditionNames, each = No_of_Subjects),
      sbp = as.numeric(BaselineTilt)
    )

  df1_pVal <- df1 %>%
    rstatix::group_by(groups_dat) %>%
    rstatix::t_test(sbp ~ conditions) %>%
    rstatix::adjust_pvalue(p.col = "p", method = "bonferroni") %>%
    rstatix::add_significance(p.col = "p.adj") %>%
    rstatix::add_xy_position(x = "groups_dat", dodge = 0.8) # important for positioning!

  barFigure = ggplot(data = df1, aes(x = groups_dat, y = sbp)) +
    geom_bar(aes(fill = conditions),
             stat = "summary",
             position = "dodge",
             width = 0.9) +
    geom_point(
      aes(color = conditions),
      shape = 19,
      size = 4,
      position = position_dodge(0.9)
    ) +
    scale_color_manual(values = c('gray80', 'dodgerblue', 'pink')) +
    theme_prism() +
    scale_fill_brewer(palette = "Paired") +
    labs(title = "",
         x = "", y = "SBP (mmHg)") +
    geom_blank() +
    #scale_x_continuous(expand = c(0, 0), breaks = seq(0, 200, by = 150)) +
    scale_y_continuous(expand = c(0, 0),
                       breaks = seq(0, maxYaxis_barGraph, by = maxYaxis_barGraph/2)) +
    coord_cartesian(ylim = c(0, maxYaxis_barGraph))

  barGraph = barFigure + scale_fill_manual(values = c('gray90', 'deepskyblue')) +
    theme(
      axis.line.x = element_blank(),
      #axis.text.x=element_blank(),
      axis.ticks.x = element_blank(),
      #axis.title.x=element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.title = element_blank(),
      legend.box.background = element_blank(),
      legend.key = element_rect(fill = "transparent", colour = "transparent"),
      legend.background = element_blank(),
      legend.position = c(0.5, 0.95),
      legend.direction = "horizontal",
      legend.key.height = unit(0.1, 'cm'),
      legend.key.width = unit(1.5, 'cm')
    ) +
    #theme(axis.ticks.length.x  = unit(0.2, "cm")) +
    theme(axis.ticks.length.y  = unit(0.2, "cm")) +
    # theme(axis.ticks.x = element_line(color = "black",
    #                                   linewidth = 1.0)) +
    theme(axis.ticks.y = element_line(color = "black",
                                      linewidth = 1.0)) +
    # theme(plot.title = element_text(
    #   color = "black",
    #   size = 20,
    #   face = "plain",
    #   hjust = 0.5
    # )) +
    theme(# axis.title.x = element_text(
      #   color = "black",
      #   face = "plain",
      #   family = "Helvetica",
      #   size = 20
      # ),
      axis.title.y = element_text(
        color = "black",
        face = "plain",
        family = "Helvetica",
        size = 20
      )) +
    theme(
      axis.text.x = element_text(
        color = "black",
        size = 20,
        family = "Helvetica",
        face = "plain"
      ),
      axis.text.y = element_text(
        color = "black",
        size = 20,
        hjust = 0,
        family = "Helvetica",
        face = "plain"
      )
    ) +
    theme(axis.line.y = element_line(
      color = "black",
      linewidth = 1.0,
      linetype = 1
    ))

  barGraph = barGraph + add_pvalue(df1_pVal,
                 xmin = "xmin",
                 xmax = "xmax",
                 label = "p = {p.adj}",
                 # label = "p.adj.signif",
                 tip.length = 0,
                 bracket.colour = "black",
                 label.size = 4)

  ##############################################################################
  ######### bar graphs for delta values ########################################
  ##############################################################################
  BaselineTilt = deltaChannel_groups
  No_of_Conditions = 1
  df2 <-
    data.frame(
      groups_dat2 = rep(groupNames, each = No_of_Subjects * No_of_Conditions),
      conditions = rep(groupNames, each = No_of_Subjects),
      sbp = as.numeric(BaselineTilt)
    )

  df2_pVal <- rstatix::t_test(df2, sbp ~ groups_dat2, ref.group = refGroup) %>%
    rstatix::add_xy_position()

  #df2$conditions <- as.factor(df2$conditions)
  barDeltaFigure = ggplot(data = df2, aes(x = groups_dat2, y = sbp)) +
    geom_bar(aes(fill = groups_dat2),
             stat = "summary",
             position = "dodge",
             width = 0.9) +
    geom_point(
      aes(color = groups_dat2),
      shape = 19,
      size = 4,
      position = position_dodge(0.9)
    ) +
    scale_color_manual(values = c('gray80', 'steelblue4', 'plum3')) +
    theme_prism() +
    scale_fill_brewer(palette = "Paired") +
    labs(title = "",
         x = "",
         y = expression(Delta * SBP (mmHg))) +
    geom_blank() +
    scale_y_continuous(expand = c(0, 0),
                       breaks = seq(minYaxis_deltaGraph, maxYaxis_deltaGraph,
                       by = (maxYaxis_deltaGraph-minYaxis_deltaGraph)/2)) +
    coord_cartesian(ylim = c(minYaxis_deltaGraph, maxYaxis_deltaGraph))


  barDeltaGraph = barDeltaFigure + scale_fill_manual(values = c("grey75", "steelblue", "plum1")) +
    theme(
      axis.line.x = element_blank(),
      #axis.text.x=element_blank(),
      axis.ticks.x = element_blank(),
      #axis.title.x=element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = "none"
    ) +
    #theme(axis.ticks.length.x  = unit(0.2, "cm")) +
    theme(axis.ticks.length.y  = unit(0.2, "cm")) +
    # theme(axis.ticks.x = element_line(color = "black",
    #                                   linewidth = 1.0)) +
    theme(axis.ticks.y = element_line(color = "black",
                                      linewidth = 1.0)) +

    theme(# axis.title.x = element_text(
      #   color = "black",
      #   face = "plain",
      #   family = "Helvetica",
      #   size = 20
      # ),
      axis.title.y = element_text(
        color = "black",
        face = "plain",
        family = "Helvetica",
        size = 20
      )) +
    theme(
      axis.text.x = element_text(
        color = "black",
        size = 20,
        family = "Helvetica",
        face = "plain"
      ),
      axis.text.y = element_text(
        color = "black",
        size = 20,
        hjust = 0,
        family = "Helvetica",
        face = "plain"
      )
    ) +
    theme(axis.line.y = element_line(
      color = "black",
      linewidth = 1.0,
      linetype = 1
    ))

  barDeltaGraph = barDeltaGraph + add_pvalue(df2_pVal,
                                             #label = "p.adj.signif",
                                             label = "p = {p.adj}",
                                             tip.length = 0,
                                             bracket.colour = "black",
                                             label.size = 4)

  ##############################################################################
  ######### all graphs on one figure ########################################
  ##############################################################################

  graphs = ggdraw() +
    draw_plot(
      barGraph,
      x = 0,
      y = .5,
      width = .45,
      height = .5
    ) +
    draw_plot(
      barDeltaGraph,
      x = .5,
      y = .5,
      width = .27,
      height = .5
    ) +
    draw_plot(
      tracingGraph,
      x = 0,
      y = 0,
      width = 1,
      height = 0.5
    ) +
    draw_plot_label(
      label = c("A", "B", "C"),
      size = 20,
      x = c(0, 0.5, 0),
      y = c(1, 1, 0.5)
    )

  return(graphs)
}
