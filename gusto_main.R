  
  
    ##################################################################################################
    # Created  by D.M.T. on AUGUST 2021                                                           
    ##################################################################################################
    #                                      PRELIMINARY STUFF ----------------------------------------
    
    #load libraries
    
    if(!require(pacman)) {
      install.packages("pacman")
      install.packages("devtools")
      library(pacman)
    }
    
    #get packages
    pacman::p_load(tidyverse, dplyr, plyr, Rmisc, afex, BayesFactor, ggpubr) 
    
    # get tools
    devtools::source_gist("383aa93ffa161665c0dca1103ef73d9d", 
                          filename = "effect_CI.R")
    devtools::source_gist("2a1bb0133ff568cbe28d", 
                          filename = "geom_flat_violin.R")


  
  # -------------------------------------------------------------------------
  # *************************************** SETUP **************************************
  # -------------------------------------------------------------------------
  
  
  # Set path
    analysis_path = getwd()

  
  # Set working directory
  figures_path  <- file.path(analysis_path, '/FIGURES') 
  setwd(analysis_path)
  
  #datasets dictory
  data_path <- file.path(analysis_path,'/DATA') 
  
  # open datasets
  HED  <- read.delim(file.path(data_path,'OBIWAN_HEDONIC.txt'), header = T, sep ='') # 
  info <- read.delim(file.path(data_path,'info_expe.txt'), header = T, sep ='') # 
  
  #subset only pretest
  HED = subset(HED, session == 'second')
  
  #exclude participants (242 really outlier everywhere, 256 can't do the task, 114 & 228 REALLY hated the solution and thus didn't "do" the conditioning) & 123, 124 and 226 have imcomplete data
  `%notin%` <- Negate(`%in%`)
  HED = filter(HED, id %notin% c(242, 256, 114, 228, 123, 124, 226))
  
  #merge with info
  HED = merge(HED, info, by = "id")
  
  # Check Demo
  AGE = ddply(HED,.(), summarise,mean=mean(age),sd=sd(age), min = min(age), max = max(age)); AGE
  GENDER = ddply(HED, .(id), summarise, gender=mean(as.numeric(gender)))  %>%
    group_by(gender) %>%
    tally() ; GENDER #1 = women
  
  cov = ddply(HED, .(id),  summarize, age = mean(age, na.rm = TRUE), gender = mean(as.numeric(gender), na.rm = TRUE)) ; cov$age = scale(cov$age)
  
  write.table(cov, (file.path(analysis_path, "/fMRI/covariate.txt")), row.names = F, sep="\t")
  
  
  
  # -------------------------------------- PLOTS -----------------------------------------------
  
  
  # -------------------------------------- themes for plots --------------------------------------------------------
  averaged_theme <- theme_bw(base_size = 32, base_family = "Helvetica")+
    theme(strip.text.x = element_text(size = 32, face = "bold"),
          strip.background = element_rect(color="white", fill="white", linetype="solid"),
          legend.position=c(.9,.9),
          legend.title  = element_text(size = 12),
          legend.text  = element_text(size = 10),
          legend.key.size = unit(0.2, "cm"),
          legend.key = element_rect(fill = "transparent", colour = "transparent"),
          panel.grid.major.x = element_blank() ,
          panel.grid.major.y = element_line(size=.2, color="lightgrey") ,
          panel.grid.minor = element_blank(),
          axis.title.x = element_text(size = 30),
          axis.title.y = element_text(size =  30),
          axis.line = element_line(size = 0.5),
          panel.border = element_blank())
  
  pal = viridis::inferno(n=5) # specialy conceived for colorblindness  
  
  HED.means <- aggregate(list(HED$perceived_liking, HED$perceived_intensity), by = list(HED$id, HED$condition), FUN='mean') # extract means
  colnames(HED.means) <- c('id','condition','perceived_liking', 'perceived_intensity')
  
  HED.means$int =  HED.means$perceived_intensity[HED.means$condition=="MilkShake"] - HED.means$perceived_intensity[HED.means$condition=="Empty"]; HED.means$int = scale(HED.means$int)
  HED.means$lik =  HED.means$perceived_liking[HED.means$condition=="MilkShake"] - HED.means$perceived_liking[HED.means$condition=="Empty"]; HED.means$lik = scale(HED.means$lik)
  
  HED.means$id = as.factor(HED.means$id); HED.means$condition = as.factor(HED.means$condition)
  
  
  # AVERAGED EFFECT INTENSITY
  
  dfH <- summarySEwithin(HED.means,
                         measurevar = "perceived_intensity",
                         withinvars = "condition", 
                         idvar = "id")
  
  dfH$cond <- ifelse(dfH$condition == "MilkShake", -0.25, 0.25)
  HED.means$cond <- ifelse(HED.means$condition == "MilkShake", -0.25, 0.25)
  set.seed(666)
  HED.means <- HED.means %>% mutate(condjit = jitter(as.numeric(cond), 0.3),
                                    grouping = interaction(id, cond))
  
  
  pp <- ggplot(HED.means, aes(x = cond, y = perceived_intensity, 
                              fill = condition, color = condition)) +
    geom_point(data = dfH, alpha = 0.5) +
    geom_line(aes(x = condjit, group = id, y = perceived_intensity), alpha = .3, size = 0.5, color = 'gray') +
    geom_flat_violin(scale = "count", trim = FALSE, alpha = .2, aes(fill = condition, color = NA))+
    geom_point(aes(x = condjit), alpha = .3,) +
    geom_crossbar(data = dfH, aes(y = perceived_intensity, ymin=perceived_intensity-ci, ymax=perceived_intensity+ci,), width = 0.2 , alpha = 0.1)+
    ylab('Perceived taste intesity') +
    xlab('') +
    scale_y_continuous(expand = c(0, 0), breaks = c(seq.int(0,100, by = 20)), limits = c(-0.5,100.5)) +
    scale_x_continuous(labels=c("Milkshake", "Tasteless"),breaks = c(-.25,.25), limits = c(-.5,.5)) +
    scale_fill_manual(values=c("MilkShake"= pal[3], "Empty"=pal[1]), guide = 'none') +
    scale_color_manual(values=c("MilkShake"=pal[3], "Empty"=pal[1]), guide = 'none') +
    theme_bw()
  
  ppp1 <- pp + averaged_theme + theme(plot.margin = margin(3,0.1,0.1,0.1, "cm")) 
  ppp1
  
  
  
  # STATS -------------------------------------------------------------------
  HED.means$condition <- relevel(HED.means$condition, "MilkShake") # Make MilkShake first
  
  # intensity
  t.test(perceived_intensity ~ condition, data = HED.means, paired = T)
  
  
  cohen_d_ci(HED.means$perceived_intensity[HED.means$condition == "MilkShake"], HED.means$perceived_intensity[HED.means$condition == "Empty"], paired=T)
    
    anova.HED <- aov_car(perceived_intensity ~ condition  + Error (id/condition), data = HED.means, factorize = F, anova_table = list(correction = "GG", es = "none")); summary(anova.HED)
  
  pes_ci(perceived_intensity ~ condition + Error (id/condition), data = HED.means, factorize = F, epsilon = "GG")
  
  # Bayes factors
  BF = anovaBF(perceived_intensity ~ condition + id, data = HED.means, whichRandom = "id"); BF
  
  
  #ANCOVA pleasantnes accounting for intensity
  anova.HED2 <- aov_car(perceived_liking ~ condition  + int + Error (id/condition), data = HED.means, factorize = F, anova_table = list(correction = "GG", es = "none")); summary(anova.HED2)

  pes_ci(perceived_liking ~ condition + int + Error (id/condition), data = HED.means, factorize = F, epsilon = "GG")

  # Bayes factors
  test = extractBF(generalTestBF(perceived_liking ~ condition*int + id, data = HED.means, whichRandom = "id", neverExclude="id", progress=T,whichModels ="top")); BF2 = 1/test[1] #switch to BF10 inclusion
  BF2


  mod = lmer(data =HED, perceived_liking ~ condition + perceived_intensity +  (condition|id) + (1|trialxcondition), REML = F)
  
  
  
  # PLOT fMRI exctracted BETAS INSULA ----------------------------------------------------------------
  
  mri_path <- file.path(home_path,'DERIVATIVES/GLM/SPM/hedonicreactivity/GLM_GUSTO/group/ROI') 
  
  # open datasets
  rew_mri  <- read.delim(file.path(mri_path,'reward/insula_betas.csv'), header = T, sep =',') # 
  rew_mri$id = unique(HED.means$id); rew_mri$condition = 'MilkShake'
  neu_mri  <- read.delim(file.path(mri_path,'/neutral/insula_betas.csv'), header = T, sep =',') # 
  neu_mri$id = unique(HED.means$id); neu_mri$condition = 'Empty'
  df1 = rbind(rew_mri, neu_mri); df1$region = "insula"
  
  rew_mri  <- read.delim(file.path(mri_path,'reward/Pirif_betas.csv'), header = T, sep =',') # 
  rew_mri$id = unique(HED.means$id); rew_mri$condition = 'MilkShake'
  neu_mri  <- read.delim(file.path(mri_path,'/neutral/Pirif_betas.csv'), header = T, sep =',') # 
  neu_mri$id = unique(HED.means$id); neu_mri$condition = 'Empty'
  df2 = rbind(rew_mri, neu_mri); df2$region = "piriform"
  
  df = rbind(df1, df2)
  
  diff =  subset(df, condition=="MilkShake"); y =  subset(df, condition=="Empty"); diff$betas = diff$betas - y$betas; df = as_tibble(diff) 
  
  # AVERAGED EFFECT

  dfH <- summarySE(df, measurevar = "betas",
                         groupvars = "region")

  pp <- ggplot(df, aes(x =region, y = betas)) +
    geom_hline(yintercept = 0, linetype =2, alpha = 0.5, size = 0.5) + 
    geom_point(data = dfH, alpha = 0.5) +
    #geom_line(aes(x = condjit, group = id, y = betas), alpha = .3, size = 0.5, color = 'gray') +
    geom_flat_violin(scale = "count", trim = FALSE, alpha = .2, fill=pal[2], color=NA)+
    geom_point(alpha = .3, position = position_jitter(seed=666,width = 0.09), color=pal[2]) +
    geom_crossbar(data = dfH, aes(y = betas, ymin=betas-ci, ymax=betas+ci), width = 0.4 , alpha = 0.1, fill=pal[2], color=pal[2])+
    ylab('Beta estimates (a.u.) \n Milkshake > Tasteless') +
    xlab('') +
    scale_x_discrete(labels=c("Insular Cortex", "Piriform Cortex")) +
    theme_bw() 
  
  ppp2 <- pp + averaged_theme + theme(plot.margin = margin(3,0.1,0.1,0.1, "cm")) 
  ppp2
  

  
  
  figure <- ggarrange(ppp1, ppp2,
                      font.label = list(size = 32, color = "black", face = "bold"),
                      labels = c("A", "B"),
                      ncol = 2, nrow =1) 

  figure
  
  cairo_pdf(file.path(figures_path,'Figure_HEDONIC.pdf'))
  print(figure)
  dev.off()
  
  #tests
  
  # bf = ttestBF(formula = betas ~ condition, data = subset(df, region == 'insula')); bf
  # t = t.test(formula = betas ~ condition, data = df); t
  
  
  # PubMed count ------------------------------------------------------------
  df_pub <- read_csv("pub.csv")
  
  df_pub = df_pub %>% drop_na(Year)
  df = count(df_pub, "Year")
  
  plot_pub = ggplot(df,aes(Year, freq))+geom_bar(stat="identity", fill="#21908CFF") +
    scale_x_continuous(expand = c(0, 0), breaks = c(seq.int(1960,2020, by = 10)), limits = c(1958,2022)) +  ylab('Publications')+
    theme_bw(base_size = 32, base_family = "Helvetica") +
    theme(panel.grid.major = element_line(size=.8, color="lightgrey"),axis.title.x = element_text(size = 30),
          axis.title.y = element_text(size =  30),
          axis.line = element_line(size = 0.5),
          panel.border = element_blank())
  
  cairo_pdf(file.path(figures_path,'Figure_PubMed.pdf'))
  print(plot_pub)
  dev.off()
  