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
  
  pacman::p_load(tidyverse, dplyr, plyr, Rmisc, afex, BayesFactor, ggpubr) 
  
  # get tool
  devtools::source_gist("2a1bb0133ff568cbe28d", 
                        filename = "geom_flat_violin.R")
  source('~/OBIWAN/GUSTO/cohen_d_ci.R', echo=F)
  source('~/OBIWAN/GUSTO/pes_ci.R', echo=F)
  
  # -------------------------------------------------------------------------
  # *************************************** SETUP **************************************
  # -------------------------------------------------------------------------
  
  
  # Set path
  home_path       <- '~/OBIWAN'
  
  # Set working directory
  analysis_path <- file.path(home_path, 'GUSTO')
  figures_path  <- file.path(home_path, 'GUSTO/FIGURES') 
  setwd(analysis_path)
  
  #datasets dictory
  data_path <- file.path(home_path,'DERIVATIVES/BEHAV') 
  
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
  
  write.table(cov, (file.path(analysis_path, "covariate.txt")), row.names = F, sep="\t")
  
  
  
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
  
  # se <- function (x,na.rm=TRUE) {
  #   if (!is.vector(x)) STOP("'x' must be a vector.")
  #   if (!is.numeric(x)) STOP("'x' must be numeric.")
  #   if (na.rm) x <- x[stats::complete.cases(x)]
  #   sqrt(stats::var(x)/length(x))
  # }
  
  HED.means <- aggregate(list(HED$perceived_liking, HED$perceived_intensity), by = list(HED$id, HED$condition), FUN='mean') # extract means
  colnames(HED.means) <- c('id','condition','perceived_liking', 'perceived_intensity')
  
  #HED.means$perceived_intensity =  HED.means$perceived_intensity[HED.means$condition=="MilkShake"] -HED.means$perceived_intensity[HED.means$condition=="Empty"]; HED.means$perceived_intensity = scale(HED.means$perceived_intensity)
  
  HED.long = gather(HED.means, key = "rating", value = "measurement",
         perceived_liking, perceived_intensity)
  
  
  # AVERAGED EFFECT
  
  dfH <- summarySEwithin(HED.long,
                         measurevar = "measurement",
                         withinvars = "condition", 
                         betweenvars = "rating",
                         idvar = "id")
  
  dfH$cond <- ifelse(dfH$condition == "MilkShake", -0.25, 0.25)
  HED.long$cond <- ifelse(HED.long$condition == "MilkShake", -0.25, 0.25)
  set.seed(666)
  HED.long <- HED.long %>% mutate(condjit = jitter(as.numeric(cond), 0.3),
                                    grouping = interaction(id, cond))
  
  labels = c("perceived_liking"="Pleasantness", "perceived_intensity"="Intensity")
  
  pp <- ggplot(HED.long, aes(x = cond, y = measurement, 
                              fill = condition, color = condition)) +
    geom_point(data = dfH, alpha = 0.5) +
    geom_line(aes(x = condjit, group = id, y = measurement), alpha = .3, size = 0.5, color = 'gray') +
    geom_flat_violin(scale = "count", trim = FALSE, alpha = .2, aes(fill = condition, color = NA))+
    geom_point(aes(x = condjit), alpha = .3,) +
    geom_crossbar(data = dfH, aes(y = measurement, ymin=measurement-se, ymax=measurement+se,), width = 0.2 , alpha = 0.1)+
    ylab('Behavioral Ratings') +
    xlab('') +
    scale_y_continuous(expand = c(0, 0), breaks = c(seq.int(0,100, by = 20)), limits = c(-0.5,100.5)) +
    scale_x_continuous(labels=c("MilkShake", "Tasteless"),breaks = c(-.25,.25), limits = c(-.5,.5)) +
    scale_fill_manual(values=c("MilkShake"= pal[3], "Empty"=pal[1]), guide = 'none') +
    scale_color_manual(values=c("MilkShake"=pal[3], "Empty"=pal[1]), guide = 'none') +
    theme_bw() + facet_wrap(~rating, labeller=labeller(rating =labels))
  
  ppp1 <- pp + averaged_theme
  ppp1
  
  
  
  # STATS -------------------------------------------------------------------
  # Liking
  HED.means$id = as.factor(HED.means$id); HED.means$condition = as.factor(HED.means$condition)
  
  anova.HED <- aov_car(perceived_liking ~ condition  + Error (id/condition), data = HED.means, factorize = F, anova_table = list(correction = "GG", es = "none")); summary(anova.HED)
  
  pes_ci(perceived_liking ~ condition + Error (id/condition), data = HED.means, factorize = F, epsilon = "GG")
  
  # Bayes factors
  test = anovaBF(perceived_liking ~ condition + id, data = HED.means, whichRandom = "id"); test
  
  # intensity
  
  anova.HED <- aov_car(perceived_intensity ~ condition  + Error (id/condition), data = HED.means, factorize = F, anova_table = list(correction = "GG", es = "none")); summary(anova.HED)
  
  pes_ci(perceived_intensity ~ condition + Error (id/condition), data = HED.means, factorize = F, epsilon = "GG")
  
  # Bayes factors
  test = anovaBF(perceived_intensity ~ condition + id, data = HED.means, whichRandom = "id")
  
  
  #ANCOVA
  # anova.HED <- aov_car(perceived_liking ~ condition  +perceived_intensity + Error (id/condition), data = HED.means, factorize = F, anova_table = list(correction = "GG", es = "none")); summary(anova.HED)
  # 
  # pes_ci(perceived_liking ~ condition + perceived_intensity + Error (id/condition), data = HED.means, factorize = F, epsilon = "GG")
  # 
  # # Bayes factors
  # test = extractBF(generalTestBF(perceived_liking ~ condition*perceived_intensity + id, data = HED.means, whichRandom = "id", neverExclude="id", progress=T,whichModels ="top")); BF = 1/test[1] #switch to BF10 inclusion
  # BF
  
  
  # diff =  HED.means$perceived_liking[HED.means$condition=="MilkShake"] -HED.means$perceived_liking[HED.means$condition=="Empty"]
  # bf = ttestBF(x = HED.means$perceived_liking[HED.means$condition=="MilkShake"], y = HED.means$perceived_liking[HED.means$condition=="Empty"], paired=TRUE); bf
  # t = t.test(formula = perceived_liking ~ condition, data = HED.means, paired = T); t
  # se(diff)
  # effect size
  #cohen_d_ci(diff, conf.level = .95)
  #mod = lmer(data =HED, perceived_liking ~ condition + (condition|id) + (1|trialxcondition), REML = F)
  
  
  
  # PLOT fMRI exctracted BETAS INSULA ----------------------------------------------------------------
  
  mri_path <- file.path(home_path,'DERIVATIVES/GLM/SPM/hedonicreactivity/GLM_GUSTO/group/ROI') 
  
  # open datasets
  rew_mri  <- read.delim(file.path(mri_path,'reward/insula_betas.csv'), header = T, sep =',') # 
  rew_mri$id = unique(HED.means$id); rew_mri$condition = 'MilkShake'
  neu_mri  <- read.delim(file.path(mri_path,'/neutral/insula_betas.csv'), header = T, sep =',') # 
  neu_mri$id = unique(HED.means$id); neu_mri$condition = 'Empty'
  df = rbind(rew_mri, neu_mri); df = as_tibble(df)
  
  
  # AVERAGED EFFECT
  dfH <- summarySEwithin(df,
                         measurevar = "betas",
                         withinvars = "condition", 
                         idvar = "id")
  
  dfH$cond <- ifelse(dfH$condition == "MilkShake", -0.25, 0.25)
  df$cond <- ifelse(df$condition == "MilkShake", -0.25, 0.25)
  set.seed(666)
  df <- df %>% mutate(condjit = jitter(as.numeric(cond), 0.3),
                      grouping = interaction(id, cond))
  
  
  pp <- ggplot(df, aes(x = cond, y = betas, 
                       fill = condition, color = condition)) +
    geom_point(data = dfH, alpha = 0.5) +
    geom_line(aes(x = condjit, group = id, y = betas), alpha = .3, size = 0.5, color = 'gray') +
    geom_flat_violin(scale = "count", trim = FALSE, alpha = .2, aes(fill = condition, color = NA))+
    geom_point(aes(x = condjit), alpha = .3,) +
    geom_crossbar(data = dfH, aes(y = betas, ymin=betas-se, ymax=betas+se), width = 0.2 , alpha = 0.1)+
    ylab('Beta estimates (a.u.)') +
    xlab('') +
    #scale_y_continuous(expand = c(0, 0), breaks = c(seq.int(0,100, by = 20)), limits = c(-0.5,100.5)) +
    scale_x_continuous(labels=c("MilkShake", "Tasteless"),breaks = c(-.25,.25), limits = c(-.5,.5)) +
    scale_fill_manual(values=c("MilkShake"= pal[3], "Empty"=pal[1]), guide = 'none') +
    scale_color_manual(values=c("MilkShake"=pal[3], "Empty"=pal[1]), guide = 'none') +
    theme_bw()
  
  ppp2 <- pp + averaged_theme
  ppp2
  
  
  bf = ttestBF(formula = betas ~ condition, data = df); bf
  t = t.test(formula = betas ~ condition, data = df); t
  


# PLOT fMRI exctracted BETAS Piriform ----------------------------------------------------------------

# open datasets
rew_mri  <- read.delim(file.path(mri_path,'reward/Pirif_betas.csv'), header = T, sep =',') # 
rew_mri$id = unique(HED.means$id); rew_mri$condition = 'MilkShake'
neu_mri  <- read.delim(file.path(mri_path,'/neutral/Pirif_betas.csv'), header = T, sep =',') # 
neu_mri$id = unique(HED.means$id); neu_mri$condition = 'Empty'
df = rbind(rew_mri, neu_mri); df = as_tibble(df)


# AVERAGED EFFECT
dfH <- summarySEwithin(df,
                       measurevar = "betas",
                       withinvars = "condition", 
                       idvar = "id")

dfH$cond <- ifelse(dfH$condition == "MilkShake", -0.25, 0.25)
df$cond <- ifelse(df$condition == "MilkShake", -0.25, 0.25)
set.seed(666)
df <- df %>% mutate(condjit = jitter(as.numeric(cond), 0.3),
                                  grouping = interaction(id, cond))


pp <- ggplot(df, aes(x = cond, y = betas, 
                            fill = condition, color = condition)) +
  geom_point(data = dfH, alpha = 0.5) +
  geom_line(aes(x = condjit, group = id, y = betas), alpha = .3, size = 0.5, color = 'gray') +
  geom_flat_violin(scale = "count", trim = FALSE, alpha = .2, aes(fill = condition, color = NA))+
  geom_point(aes(x = condjit), alpha = .3,) +
  geom_crossbar(data = dfH, aes(y = betas, ymin=betas-se, ymax=betas+se), width = 0.2 , alpha = 0.1)+
  ylab('Beta estimates (a.u.)') +
  xlab('') +
  #scale_y_continuous(expand = c(0, 0), breaks = c(seq.int(0,100, by = 20)), limits = c(-0.5,100.5)) +
  scale_x_continuous(labels=c("Pleasant", "Neutral"),breaks = c(-.25,.25), limits = c(-.5,.5)) +
  scale_fill_manual(values=c("MilkShake"= pal[3], "Empty"=pal[1]), guide = 'none') +
  scale_color_manual(values=c("MilkShake"=pal[3], "Empty"=pal[1]), guide = 'none') +
  theme_bw()

ppp3 <- pp + averaged_theme
ppp3


bf = ttestBF(formula = betas ~ condition, data = df); bf
t = t.test(formula = betas ~ condition, data = df); t
# 
# figure <- ggarrange(ppp1, ppp2, 
#                     font.label = list(size = 32, color = "black", face = "bold"),
#                     labels = c("A", "B"),
#                     ncol = 2, nrow =1)
# 
# figure
# cairo_pdf(file.path(figures_path,'Figure_HEDONIC.pdf'))
# print(figure)
# dev.off()





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
