# load packages
pacman::p_load(tidyverse,
               dplyr,
               anchors, 
               ltm,
               srvyr, 
               haven,
               essurvey,
               gganimate,
               ggthemes,
               ggthemr)

# dont update packages
#devtools::install_github('cttobin/ggthemr')


# ---------------------------
# Load in all rounds of ESS #
# ---------------------------

# set your email
set_email("roy.tim@outlook.com")

# import all rounds of ESS
all_ESS <- import_all_rounds()

# clean rounds, making 2 = 0
ESS <- list() # create new list
for(i in 1:length(all_ESS)) {
  ESS[[i]] <- replace.value(all_ESS[[i]], "dscrgrp", from = 2, to = 0)
}              

# ---------------------------
# Create survey environment #
# ---------------------------

# need to remove Latvia and Romania, no design weights in ESS 3
ESS[[3]] <- filter(ESS[[3]], cntry != "LV" & cntry != "RO")
stopifnot(sum(is.na(ESS[[3]]$dweight)) == 0)

sv <- list()
for(i in 1:length(all_ESS)) {
  sv[[i]] <- ESS[[i]] %>% as_survey(weights = c(pweight, dweight))
}

# ------------------------------------------------
# Get proportion of people discriminated against #
# ------------------------------------------------

# Subset for ethnic minorities
sv_etmg <- list()
for(i in 1:length(sv)) {
  sv_etmg[[i]] <- filter(sv[[i]], blgetmg == 1)
}

# # Subset for Muslim people
# sv_etmg <- list()
# for(i in 1:length(sv)) {
#   sv_etmg[[i]] <- filter(sv[[i]], rlgdnm == 6)
# }

# set grouping variable
grouping_var <- sym("cntry")

# create lists for output from for() loop
cntry_sums <- list()
pes <- list()
upperCIs <- list()
lowerCIs <- list()
cntry_means <- list()

# for loop, gets proportions and CIs
for(i in 1:length(sv_etmg)) {
  # means by country
  cntry_sums[[i]] <- 
    sv_etmg[[i]] %>% 
    group_by(!!grouping_var) %>% 
    summarize(
      dscrrlg = survey_mean(dscrrlg, na.rm = T, vartype = "ci"), # religion
      dscrrce = survey_mean(dscrrce, na.rm = T, vartype = "ci"), # color or race
      dscrntn = survey_mean(dscrntn, na.rm = T, vartype = "ci"), # nationality
      dscrlng = survey_mean(dscrlng, na.rm = T, vartype = "ci"), # language
      dscretn = survey_mean(dscretn, na.rm = T, vartype = "ci") # ethnic group
    )
  # separate PEs and CIs
  pes[[i]] <- cntry_sums[[i]] %>% dplyr::select(cntry, 
                                                dscrrlg, # religion
                                                dscrrce, # color or race 
                                                dscrntn, # nationality
                                                dscrlng, # language
                                                dscretn # ethnic group
  )
  upperCIs[[i]] <- cntry_sums[[i]] %>% dplyr::select(cntry, ends_with("upp"))
  lowerCIs[[i]] <- cntry_sums[[i]] %>% dplyr::select(cntry, ends_with("low"))
  # Pivot longer
  pes[[i]] <- pes[[i]] %>% 
    pivot_longer(-cntry, 
                 names_to = "outcome", 
                 values_to = "discr_ratio")
  
  upperCIs[[i]] <- upperCIs[[i]] %>% 
    pivot_longer(-cntry, 
                 names_to = "outcome", 
                 values_to = "upper")
  
  lowerCIs[[i]] <- lowerCIs[[i]] %>% 
    pivot_longer(-cntry,
                 names_to = "outcome",
                 values_to = "lower")
  # bind long PEs and CIs
  cntry_means[[i]] <- cbind(pes[[i]], 
                            upper = upperCIs[[i]]$upper, 
                            lower = lowerCIs[[i]]$lower)
  # get average outcome
  cntry_means[[i]] <- cntry_means[[i]] %>% 
    group_by(!!grouping_var) %>% 
    mutate(outcome_avg = mean(discr_ratio))
}

# ----------------------------------------------------
# Plotting all outcomes and countries for each round #
# ----------------------------------------------------

p <- list()

for(i in 1:length(sv)) {
  p[[i]] <- 
    ggplot(cntry_means[[i]],
           aes(x = reorder(cntry, -outcome_avg),
               y = discr_ratio,
               group = outcome)) +
    theme_minimal() +
    geom_linerange(aes(ymin = 0, ymax = discr_ratio),
                   color = "grey", size = 1.2) +
    geom_linerange(aes(color = outcome,
                       ymin = lower, 
                       ymax = upper),
                   position = position_dodge(width = 0.7), 
                   size = 1.2)# +
  #coord_flip()
}

# filter out israel maybe?

# ----------------------------------
# plotting just France and England #
# ----------------------------------
# No French Muslims in Round 1,2
france <- list()
for(i in 1:length(sv)) { # for ethnic minorities
#for(i in 3:9) {         # for muslims
  france[[i]] <- filter(cntry_means[[i]], cntry == "FR") # subset for France
  france[[i]]$round <- i # create round variable
}

# create single dataframe for france
france_long <- rbind(france[[1]], france[[2]], france[[3]],
                     france[[4]], france[[5]], france[[6]],
                     france[[7]], france[[8]], france[[9]])

# No English Muslims in round 2,3
rm(england)
england <- list()
for(i in 1:length(sv)) { # for ethnic minorities
#for(i in c(1, 4:9)) {     # for Muslims
  england[[i]] <- filter(cntry_means[[i]], cntry == "GB") # subset for england
  england[[i]]$round <- i # create round variable
}

# create single dataframe for england
england_long <- rbind(england[[1]], england[[2]], england[[3]],
                      england[[4]], england[[5]], england[[6]],
                      england[[7]], england[[8]], england[[9]])

# list for plots
df_list <- list(france_long, england_long)

# create lists for for() loop output
plot_list <- list()
round_list <- list()
outcome_list <- list()

# set plot subtitles
plot_subtitle <- c("France, 2002-2018", "England, 2002-2018")
# Set round animation paths
plot_round_paths <- c("./discr_figs/france_by_round.gif",
                      "./discr_figs/england_by_round.gif")
# set outcome animation paths
plot_outcome_paths <- c("./discr_figs/france_by_outcome.gif",
                        "./discr_figs/england_by_outcome.gif")


# get the average outcome ratio over 9 rounds
for(i in 1:length(df_list)) {
  grouping_var <- sym("outcome") # set grouping var
  df_list[[i]] <- df_list[[i]] %>% 
    group_by(!!grouping_var) %>% # group by outcome
    mutate(avg_by_out = mean(discr_ratio)) %>% 
    ungroup()
  
  # set themes
  earth <- ggthemr("earth", type = "outer", set_theme = FALSE)
  flat_dark <- ggthemr("flat dark", type = "outer", set_theme = FALSE)
  
  # map plot
  plot_list[[i]] <- 
    ggplot(df_list[[i]], aes(round, discr_ratio, group = outcome)) +
    #theme_solarized(light = F) +
    geom_hline(aes(yintercept = avg_by_out, color = outcome), 
               linetype = "dashed", size = 1, alpha = 0.4) +
    #geom_linerange(aes(ymin = 0, ymax = discr_ratio),
    #               color = "grey", size = 1.2) +
    geom_rect(aes(color = outcome,
                  ymin = lower, 
                  ymax = upper,
                  xmin = round - 0.05,
                  xmax = round + 0.05),
              position = position_dodge(width = 0.8), 
              size = 1.2, alpha = 0.6) +
    #geom_smooth(aes(color = outcome), 
    #                method = "loess", se = FALSE) +
    geom_point(aes(color = outcome), 
               shape = 20, size = 4, 
               position = position_dodge(width = 0.8),
               alpha = 0.8) +
    #geom_line(aes(color = outcome)) +
    scale_x_continuous(breaks = seq(1, 9, 1)) +
    scale_color_discrete(name = "Form of Discrimination", 
                         labels = c("Ethnic Discrimination", 
                                    "Linguistic Discrimination",
                                    "Nationality Discrimination",
                                    "Racial Discrimination",
                                    "Religious Discrimination")) +
    labs(title = "Proportion of Ethnic Minorities Discriminated Against", 
         subtitle = plot_subtitle[i], 
         caption = "Dashed lines = average of each form of discrimination",
         y = "Proportion Discriminated Against",
         x = "Round of ESS") 

  # --------------------------------
  # animate, transition by round #
  # --------------------------------
  round_list[[i]] <- animate(
    plot_list[[i]] + 
      flat_dark$theme + transition_states(factor(df_list[[i]]$round),
                                          transition_length = 4,
                                          state_length = 5),
    width = 900, height = 500, res = 120)
  
  # --------------------------------
  # animate, transition by outcome #
  # --------------------------------
  outcome_list[[i]] <- animate(
    plot_list[[i]] + stat_smooth(aes(color = outcome), se = F) + 
      flat_dark$theme + transition_states(factor(df_list[[i]]$outcome),
                                          transition_length = 4,
                                          state_length = 5),
    width = 900, height = 500, res = 120
  )
  # -------------------
  # Saving Animations #
  # -------------------
  anim_save(plot_round_paths[[i]], animation = round_list[[i]])
  anim_save(plot_outcome_paths[[i]], animation = outcome_list[[i]])
}