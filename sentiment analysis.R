# Leslie Huang
# MA paper
# LIWC analysis of FARC communiques

rm(list=ls())
setwd("/Users/lesliehuang/Dropbox/MA-thesis-analysis/")

set.seed(1234)

libraries <- c("foreign", "utils", "stargazer", "dplyr", "devtools", "quanteda", "ggplot2", "stringr", "LIWCalike", "austin", "forecast", "lmtest", "strucchange", "vars", "tseries", "urca", "depmixS4", "rrcov", "mlogit", "reshape2")
lapply(libraries, require, character.only=TRUE)

devtools::install_github("ggbiplot", "vqv")
library(ggbiplot)

devtools::install_github("leeper/margins")
library("margins")

# get LIWC dict
spanish_dict <- dictionary(file = "../LIWC/Spanish_LIWC2007_Dictionary.dic", format = "LIWC")

################################################################################## 
################################################################################## 
# get monthly levels of violence
monthly_viol <- read.csv("../MA-datasets/violence_stats.csv", stringsAsFactors = FALSE)
monthly_viol$date <- as.Date(as.yearmon(monthly_viol$date, "%Y-%m"))
monthly_viol[,2:4] <- sapply(monthly_viol[,2:4], function(x) { as.numeric(x)})
monthly_viol <- subset(monthly_viol, select = c(2:4, 1))

# some major dates for plotting
major_violence <- as.Date(c("7/20/13", "1/16/13", "7/29/14", "11/16/14", "4/15/15", "5/31/15", "6/15/15", "6/22/15"), "%m/%d/%y")
major_agree <- as.Date(c("8/26/12", "5/26/13", "11/6/13", "5/16/14", "3/7/15", "6/2/15", "9/23/15"), "%m/%d/%y")

cf_start <- as.Date(c("11/20/12", "12/15/13", "5/16/14", "12/20/14", "7/20/15"), "%m/%d/%y")
cf_end <- as.Date(c("1/20/13", "1/15/14", "5/28/14", "5/22/15", "1/1/16"), "%m/%d/%y")
ceasefires <- data.frame(start = as.Date(c("11/20/12", "12/15/13", "5/16/14", "12/20/14", "7/20/15"), "%m/%d/%y"), end = as.Date(c("1/20/13", "1/15/14", "5/28/14", "5/22/15", "1/1/16"), "%m/%d/%y"))

# df of all dates
dates <- rbind(data.frame(date = major_violence, group = "major_viol"), data.frame(date = major_agree, group = "major_agree"), data.frame(date = cf_start, group = "ceasefire_start"), data.frame(date = cf_end, group = "ceasefire_end"))
dates <- arrange(dates, date)

stargazer(dates, title="Labeling states using major events", digits = 2, digit.separator = "", summary = FALSE)


################################################################################## 
# Some functions to extract sentiment, loess it, and return results

# Function to get raw LIWC measures
liwc_extractor <- function(df) {
  # run liwc
  liwc_results <- liwcalike(df$text, spanish_dict)
  
  # get date metadata
  df_dates <- dplyr::select(df, date)
  date <- as.Date(df_dates[[1]], "%Y-%m-%d")
  
  # extract the measures we want
  liwc_results <- dplyr::select(liwc_results, EmoNeg, EmoPos, Ellos, Muerte)
  
  # make the dataframe
  results_df <- data.frame(cbind(sapply(liwc_results, function(x) {as.numeric(x)})))
  results_df$date <- as.Date(date, "%Y-%m-%d")
  return(results_df)
}

# Function to take a df of raw LIWC values and return loessed values
liwc_loess <- function(liwc_results) {
  date <- as.Date(liwc_results$date, origin = "1970-01-01")
  results_df <- data.frame(cbind(sapply(liwc_results[,1:4], function(x) { loess(x ~ as.numeric(liwc_results$date), control = loess.control(surface = "direct"))$y})))
  results_df$date <- date
  return(results_df)
}

# Function to return loess predictions as a list
loess_lines <- function(liwc_results) {
  
  list_models <- list(sapply(liwc_results[,1:4], function(x) {loess(x ~ as.numeric(liwc_results$date), control=loess.control(surface="direct"))}))
  return(list_models)
}

#################################################################################
# import FARC communiques
FARC <- read.csv("../MA-datasets/FARC_communiques.csv", stringsAsFactors = FALSE)

# fill in a document that didn't get scraped
FARC[120, 1] <- "2016-05-30"
FARC[120, 2] <- "Informamos sobre la muerte del Camarada Bernardo Peñaloza"
FARC[120, 3] <- ""
FARC[120, 4] <- "COMUNICADO: Mayo 30 de 2016 Lamentamos informar a toda la guerrillerada y a la opinión pública nacional e internacional, que el día 25 de Mayo el camarada Bernardo Peñaloza, ex integrante de la Comisión de Paz en los diálogos de la Habana, tras una larga y fructífera vida entregada a la lucha por la revolución y los cambios sociales, en territorio colombiano y en cumplimiento de sus tareas, falleció como consecuencia de un paro cardiaco.Expresamos nuestras condolencias a sus familiares y amigos que lo conocieron. Rendimos homenaje a su memoria y a su ejemplo de dedicación y lealtad a la causa de los oprimidos. Paz en su tumba. Estado Mayor Central de las FARC EP."

# exclude documents prior to 9/2012
FARC$date <- as.Date(FARC$date, "%Y-%m-%d")
FARC <- filter(FARC, date >= "2012-09-01")

# raw LIWC measures
FARC_raw <- liwc_extractor(FARC)

# loess it
FARC_results <- liwc_loess(FARC_raw)

# get the loess lines for plotting
FARC_lines <- loess_lines(FARC_raw)

#################################################################################
# do the same for joint communiques
joint <- read.csv("../MA-datasets/jointstatements.csv", stringsAsFactors = FALSE)

joint[18, 2] <- "Las delegaciones del Gobierno y las Farc-ep informan que la Mesa de Conversaciones retomará las sesiones de trabajo el próximo viernes 3 de julio para continuar discutiendo el tema de reparación que hace parte del Punto 5 de la Agenda “Víctimas”."

joint[20, 2] <- "Las delegaciones del Gobierno y las FARC-EP, en el marco del “ACUERDO SOBRE LIMPIEZA Y DESCONTAMINACIÓN DEL TERRITORIO DE LA PRESENCIA DE MINAS ANTIPERSONAL (MAP), ARTEFACTOS EXPLOSIVOS IMPROVISADOS (AEI) Y MUNICIONES SIN EXPLOTAR (MUSE) O RESTOS EXPLOSIVOS DE GUERRA (REG) EN GENERAL”, anunciado de manera conjunta el pasado 7 de marzo de 2015, se permiten informar los siguientes avances: 1. Como es de conocimiento público, las delegaciones seleccionaron la vereda El Orejón ubicada en el municipio de Briceño, Antioquia, para iniciar el proyecto piloto de desminado. 2. Hasta la zona viajó el grupo de gestión conformado por: delegados del Gobierno (Dirección para la Acción Integral contra Minas Antipersonal y el Batallón Humanitario de Desminado del Ejército Nacional), miembros representantes de las FARC-EP, técnicos de la organización Ayuda Popular Noruega (APN) con el acompañamiento de Cuba y Noruega, países garantes; y del Comité Internacional de la Cruz Roja (CICR). 3. Luego de la selección de sitios, se inició la segunda fase del proyecto denominada Estudio No Técnico (ENT) con el fin de recopilar la información para identificar las áreas realmente contaminadas por minas antipersonal (MAP), artefactos explosivos improvisados (AEI) y municiones sin explotar (MUSE) o restos explosivos de guerra (REG). Un equipo liderado por la Ayuda Popular Noruega (APN) entró en contacto con las comunidades para realizar entrevistas con el fin de recopilar información y socializar el proyecto. 4. El Estudio No Técnico (ENT) tuvo una duración de cerca de 7 días en campo. Pese a la complejidad del terreno,  las condiciones climáticas y la naturaleza del proyecto se registraron resultados satisfactorios. 5. Como resultado de un trabajo coordinado entre el Gobierno y las FARC-EP, se identificaron 4 áreas peligrosas que suman aproximadamente 12 mil metros cuadrados. El trabajo de limpieza de estas áreas contaminadas que serán despejadas como producto del Acuerdo, facilitarán la movilidad de la comunidad en riesgo y permitirán la restauración de derechos de las comunidades en términos de movilidad, esparcimiento, acceso a vías terrestres y uso productivo de la tierra. 6. Durante el Estudio No Técnico (ENT) se recogió la información precisa y útil para la siguiente fase de “limpieza y descontaminación”. 7. Como quedó establecido en el Acuerdo, el compromiso es mantener las áreas intervenidas libres de minas y artefactos explosivos, como una garantía de no repetición para el beneficio de las comunidades. 8. Agradecemos a la Ayuda Popular Noruega (APN), a Cuba y Noruega, países garantes; al Comité Internacional de la Cruz Roja (CICR), la Gobernación de Antioquia y a las autoridades locales por su apoyo y colaboración. 9. Queremos extender un especial agradecimiento a la comunidad de la vereda El Orejón por su disposición, respaldo y conciencia al adoptar comportamientos seguros ante el riesgo de las minas antipersonal. 10. Confiamos en que esta primera medida conjunta de desescalamiento permita en un tiempo prudencial llevar alivio a las comunidades más afectadas por el conflicto y avanzar hacia la solución de este."

# LIWC estimates
joint_raw <- liwc_extractor(joint)

# loessed point estimates
joint_results <- liwc_loess(joint_raw)

# get the loess lines for plotting
joint_lines <- loess_lines(joint_raw)

#################################################################################
# and the same for govt statements
govt <- read.csv("govtstatements.csv", stringsAsFactors = FALSE)

# LIWC estimates
govt_raw <- liwc_extractor(govt)

# loessed point estimates
govt_results <- liwc_loess(govt_raw)

# get the loess lines for plotting
govt_lines <- loess_lines(govt_raw)
  

#################################################################################
#################################################################################
# Find structural breakpoints

# Function to find breakpoints for each column of a dataframe. Takes one argument: a dataframe whose last column is the date
break_finder <- function(df) {
  # make a list to contain the breakdates
  break_obs <- vector("list", length(df) - 1)
  
  # get breakpoints
  for (i in 1:(length(df)-1) ) {
   break_obs[i] <- (list(breakpoints(df[[i]] ~ 1)$breakpoints))
  }
  return(break_obs)
}

# Function to convert break obs to dates. Takes two arguments: a list of lists (shudder), and an original df
get_breakdate <- function(listoflists, df) {
  # for each list of break obs
  for (i in 1:length(listoflists)) {
    # if it's not empty
    if (!is.na(listoflists[i])) {
      
      # get the observations
      unlisted_obs <- unlist(listoflists[i])
      dates_list <- vector("list", 0)
      
      # and then for each of the observations
      for (j in 1:length(unlisted_obs)) {
        # get the date from the dataframe and add it to the list
        current_date <- df$date[unlisted_obs[j]]
        dates_list <- append(dates_list, current_date)
      }
      listoflists[i] <- list(dates_list)
    }
  }
  
  return(listoflists)
}

FARC_breaks <- get_breakdate(break_finder(FARC_results), FARC_results)
govt_breaks <- get_breakdate(break_finder(govt_results), govt_results)
joint_breaks <- get_breakdate(break_finder(joint_results), joint_results)

# Now get the breakpoints into a list we can graph
# convert breaks to a df
convert_breaks <- function(listoflists) {
  df <- as.data.frame(unlist(listoflists))
  df$group <- NA
  # now fill in by type
  negs <- rep("neg_break", length(listoflists[[1]]))
  poss <- rep("pos_break", length(listoflists[[2]]))
  pp3 <- rep("pp3_break", length(listoflists[[3]]))
  death <- rep("death_break", length(listoflists[[4]]))
  groups <- c(negs, poss, pp3, death)
  df$group <- groups
  df <- na.omit(df)
  colnames(df)[1] <- "date" 
  df$date <- as.Date(df$date, origin = "1970-01-01")
  return(df)
}

# get the dataframes of breaks, sorted by F/G/J stream
FARC_breaks_df <- convert_breaks(FARC_breaks)
govt_breaks_df <- convert_breaks(govt_breaks)
joint_breaks_df <- convert_breaks(joint_breaks)

# Function to make dataframe of all breaks of a type (e.g. neg)
break_sorter <- function(a,b,c,d) {
  df1 <- filter(a, group == d)
  if (nrow(df1) != 0) {
    df1$group <- "FARC"
  }
  df2 <- filter(b, group == d)
  if (nrow(df2) != 0) {
    df2$group <- "govt"
  }  
  df3 <- filter(c, group == d)
  if (nrow(df3) != 0) {
    df3$group <- "joint"
  }  
  df <- rbind(df1, df2, df3)
  if (nrow(df) > 0) {
    return(df)
  }
}

neg_breaks <- break_sorter(FARC_breaks_df, govt_breaks_df, joint_breaks_df, "neg_break")
pos_breaks <- break_sorter(FARC_breaks_df, govt_breaks_df, joint_breaks_df, "pos_break")
pp3_breaks <- break_sorter(FARC_breaks_df, govt_breaks_df, joint_breaks_df, "pp3_break")
death_breaks <- break_sorter(FARC_breaks_df, govt_breaks_df, joint_breaks_df, "death_break")

#################################################################################
#################################################################################
# Let's look at violence
# let's find the structural breaks ** ignore the "day"
viol_breaks <- get_breakdate(break_finder(monthly_viol), monthly_viol)

# Function to make list of violence breaks with categorical var for the measure of violence
convert_vbreaks <- function(listoflists) {
  df <- as.data.frame(unlist(listoflists))
  df$group <- NA
  # now fill in by type
  farc_actions <- rep("farc_action", length(listoflists[[1]]))
  casualties <- rep("casualties", length(listoflists[[2]]))
  desmovilizados <- rep("desmovilizados", length(listoflists[[3]]))
  groups <- c(farc_actions, casualties, desmovilizados)
  df$group <- groups
  df <- na.omit(df)
  colnames(df)[1] <- "date" 
  df$date <- as.Date(df$date, origin = "1970-01-01")
  return(df)
}

# get structural breaks in violence trends for graphing
viol_breaks_list <- convert_vbreaks(viol_breaks)

#################################################################################
#################################################################################
# TIME SERIES ANALYSIS
#################################################################################
#################################################################################

# let's check out these time series of data

# ADF tests for stationarity
sapply(monthly_viol[2:4], function(x) {summary(ur.df(na.omit(x), type = "trend", lags = 1))})
# Results: FARC actions / casualties / demobilization: unit root, trend, and drift / unit root, trend, and drift / unit root, no trend, no drift
sapply(monthly_viol[2:4], function(x) {summary(ur.df(na.omit(x), type = "drift", lags = 1))})
# FARC actions / casualties / demobilization: can't reject null / reject null / reject null: unit root, and no drift
sapply(monthly_viol[2:4], function(x) {summary(ur.df(na.omit(x), type = "none", lags = 1))})
# FARC actions / casualties / demobilization: reject null / can't reject / can't reject

# KPSS
sapply(monthly_viol[2:4], function(x) {kpss.test(x, null = "T")})

# check ndiffs: 1 differencing needed for each trend to make stationary
sapply(monthly_viol[2:4], function(x) { ndiffs(x)})

# Now let's see if FARC actions and army casualties are cointegrated.
viol_VAR <- VAR(na.omit(monthly_viol[,2:3]), p = 2, type = "both")

# test for serial correlation of residuals. Increase lags p = 2 in the VAR model to get p val = 0.94: no autocorrelation
serial.test(viol_VAR)

# Now let's run the Johansen cointgration test
summary(ca.jo(na.omit(monthly_viol[,2:3]), type = "trace", K = 2, ecdet = "trend"))
# result: cointegration

# test Granger causality both ways

# significant only when lags = 3, with p = 0.01
grangertest(na.omit(monthly_viol[,2]) ~ na.omit(monthly_viol[,3]), order = 3)

# significant with p = 0.0007
grangertest(na.omit(monthly_viol[,3]) ~ na.omit(monthly_viol[,2]), order = 1)
# result: army deaths are Granger caused by FARC actions



#################################################################################
#################################################################################
# Comparison of means: let's look at the means before and after structural breaks

# Function to calculate means in each type of sentiment during "regimes" bounded by structural breaks. Takes two arguments: df of dates with a group ID of break type, and df of loess values. Returns one argument: list of lists of Sentiment Type, Breakdate, and Means
calculate_breakmeans <- function(df, loessed) {
  # need to rename groups so they match up with columns in the loess df
  df$group <- gsub("neg_break", "EmoNeg", df$group)
  df$group <- gsub("pos_break", "EmoPos", df$group)
  df$group <- gsub("pp3_break", "Ellos", df$group)
  df$group <- gsub("death_break", "Muerte", df$group)
  
  # which types of sentiment have breaks?
  groups <- distinct(df, group)$group
  # list to contain the means for each sentiment
  listofmeans <- vector("list", length(groups))

  # for each type of sentiment
  for (i in 1:length(groups)) {
    # get the breakdates for that type
    breaks <- filter(df, group == groups[i])
    # get the name of the type
    senti_name <- groups[i]
    
    # get the correct columns from the loess df, supplied as an argument to the function
    data <- cbind(loessed["date"], loessed[senti_name])
    
    # number of structural breaks
    break_len <- length(breaks[[1]])
    
    # IDs to return
    IDs <- list(senti_name, breaks["date"])
    # Case #1: 2 breaks, 3 regimes
    if (break_len == 2) {
      data1 <- filter(data, date < breaks[1,1])
      mean1 <- mean(unlist(data1[senti_name]))
      
      data2 <- filter(data, date >= breaks[1,1], date < breaks[2,1])
      mean2 <- mean(unlist(data2[senti_name]))
      
      data3 <- filter(data, date >= breaks[2,1])
      mean3 <- mean(unlist(data3[senti_name]))
      
      means <- c(mean1, mean2, mean3)
    }
    
    # Case #2: only 1 break, 2 regimes
    else {
      data1 <- filter(data, date < breaks[1,1])
      mean1 <- mean(unlist(data1[senti_name]))
      
      data2 <- filter(data, date >= breaks[1,1])
      mean2 <- mean(unlist(data2[senti_name]))
      
      means <- c(mean1, mean2)
    }
    
    listofmeans[[i]] <- list(IDs, means)
  }
  return(listofmeans)
}

#################################################################################
#################################################################################
# get all the means of regimes defined by structural breakpoints in emotion
FARC_means <- calculate_breakmeans(FARC_breaks_df, FARC_results)
govt_means <- calculate_breakmeans(govt_breaks_df, govt_results)
joint_means <- calculate_breakmeans(joint_breaks_df, joint_results)

neg_breaks_gg
pos_breaks_gg
ellos_breaks_gg
death_breaks_gg

#################################################################################
#################################################################################
# now let's do the same for structural breaks in the violence time series

# modified version of the breakmeans function
calculate_viol_breakmeans <- function(df, loessed) {
  loessed <- na.omit(loessed)
  df$group <- gsub("farc_action", "FARC_actions", df$group)
  df$group <- gsub("casualties", "deaths_fuerzapublica", df$group)

  # which types of sentiment have breaks?
  groups <- distinct(df, group)$group
  
  # list to contain the means for each sentiment
  listofmeans <- vector("list", length(groups))
  
  # for each type of sentiment
  for (i in 1:length(groups)) {
    # get the breakdates for that type
    breaks <- filter(df, group == groups[i])
    # get the name of the type
    senti_name <- groups[i]
    # get the correct columns from the loess df, supplied as an argument to the function
    data <- cbind(loessed["date"], loessed[senti_name])
    # IDs to return
    IDs <- list(senti_name, breaks["date"])

    data1 <- filter(data, date < breaks[1,1])
    mean1 <- mean(unlist(data1[senti_name]))
    
    data2 <- filter(data, date >= breaks[1,1], date < breaks[2,1])
    mean2 <- mean(unlist(data2[senti_name]))
    
    data3 <- filter(data, date >= breaks[2,1])
    mean3 <- mean(unlist(data3[senti_name]))
      
    means <- c(mean1, mean2, mean3)
    
    listofmeans[[i]] <- list(IDs, means)
  }
  
  return(listofmeans)
}

viol_means <- calculate_viol_breakmeans(viol_breaks_list, monthly_viol)



#################################################################################
#################################################################################
# Introducing... public opinion
public_op <- read.csv("../MA-datasets/public opinion.csv", stringsAsFactors = FALSE)

public_op <- public_op[,1:3]
public_op$date <- as.Date(as.yearmon(public_op$date, "%Y-%m"))
public_op[,2:3] <- sapply(public_op[,2:3], function(x) { as.numeric(x)})
public_op <- subset(public_op, select = c(2:3, 1))

# graph it
base_opinion = ggplot(public_op, aes(x = as.Date(date, origin = "1970-01-01"), y = santos_positive_image, color = "Positive image of Pres. Santos")) +
  geom_smooth(method = "loess", se = FALSE) +
  geom_jitter() +
  geom_point(data = public_op, aes(x = as.Date(date, origin = "1970-01-01"), y = approve_santos_decision_talks, color = "Approve of negotiations with guerrillas")) +
  geom_smooth(method = "loess", se = FALSE, data = public_op, aes(x = as.Date(date, origin = "1970-01-01"), y = approve_santos_decision_talks, color = "Approve of negotiations with guerrillas")) +
  labs(
    x = "Date",
    y = "Percent Approve/Positive Image",
    color = "Legend") +
  scale_x_date(date_minor_breaks = "1 month",
               limits = c(as.Date("2012-01-01", "%Y-%m-%d"), NA)) +
  ggtitle("Public Opinion")

# public opinion and ceasefires
opinion_cf = base_opinion +
  ggtitle("Public Opinion and Ceasefires") +
  geom_rect(aes(xmin=cf_start[1], xmax=cf_end[1], ymin=-Inf, ymax=Inf), fill = "yellow", linetype = 0, alpha = 0.01) + 
  geom_rect(aes(xmin=cf_start[2], xmax=cf_end[2], ymin=-Inf, ymax=Inf), fill = "yellow", linetype = 0, alpha = 0.01) +
  geom_rect(aes(xmin=cf_start[3], xmax=cf_end[3], ymin=-Inf, ymax=Inf), fill = "yellow", linetype = 0, alpha = 0.01) + 
  geom_rect(aes(xmin=cf_start[4], xmax=cf_end[4], ymin=-Inf, ymax=Inf), fill = "yellow", linetype = 0, alpha = 0.01) + 
  geom_rect(aes(xmin=cf_start[5], xmax=cf_end[5], ymin=-Inf, ymax=Inf), fill = "yellow", linetype = 0, alpha = 0.01)

# public opinion and major events
opinion_major = base_opinion + 
  ggtitle("Major Events and Public Opinion Trends") +
  geom_vline(data = filter(dates, group == "major_agree"), mapping = aes(xintercept = as.numeric(date), color = "Major agreement"), linetype = 2) +
  geom_vline(data = filter(dates, group == "major_viol"), mapping = aes(xintercept = as.numeric(date), color = "Major violence"), linetype = 1)

# get breakdates in public opinion
opinion_breakd <- get_breakdate(break_finder(na.omit(public_op)), public_op)

#################################################################################
#################################################################################
# Function takes 1 parameter: a dataframe, and returns one parameter with monthly stats for violence and public opinion added: a dataframe
add_monthlies <- function(df) {
  dates <- df["date"]
  
  # add columns for the monthly data we're adding
  col_names <- c("FARC_actions", "army_casualties", "pres_approve", "peace_approve")
  df[, col_names] <- NA
  for (i in 1:length(dates[[1]])) {
    date <- dates[i, 1]
    year <- format(date, "%Y")
    month <- format(date, "%m")
    
    monthly_date <- as.Date(paste(year, month, "01", sep = "-"))
    
    # get the stats from violence and opinion dfs
    viol <- filter(monthly_viol, date == monthly_date)
    public <- filter(public_op, date == monthly_date)
    
    # write them to new df
    df["FARC_actions"][i, 1] <- as.numeric(viol[1])
    df["army_casualties"][i, 1] <- as.numeric(viol[2])
    df["pres_approve"][i, 1] <- as.numeric(public[1])
    df["peace_approve"][i, 1] <- as.numeric(public[2])
  }
  
  return(df)
}


#################################################################################
#################################################################################
# Markov models

# Hidden Markov model: FARC
# let's limit it to just 2 sentiment measures
FARC_results1 <- FARC_results[, -(3:4)]

# number of states
num_states <- seq(1, 6, by = 1)

#################################################################################
#################################################################################
# What is the optimal number of states? We will optimize for
# (1) BIC, fitted model
# (2) AIC, fitted model
# (3) BIC, fitted model w/ covars
# (4) AIC, fitted model w/ covars

#################################################################################

# formulas for the model
forms1 <- list(FARC_results1$EmoNeg ~ 1, FARC_results1$EmoPos ~ 1)

# Optimize BIC vals for fitted model, no covars
BIC_vals1 <- sapply(num_states, function(x) {BIC(fit(depmix(forms1, family = list(gaussian(), gaussian()), nstates = x, data = FARC_results1)))})

BIC_df1 <- data.frame(cbind(num_states, BIC_vals1))

# plot the BIC values to select the optimal number of states
BIC_plot1 <- ggplot(BIC_df1, aes(x = num_states, y = BIC_vals1)) +
  geom_point() +
  ggtitle("BIC Values for n = 2:10 Latent States Fitted HMM")

BIC_plot1

# Optimize AIC vals for same fitted model
AIC_vals1 <- sapply(num_states, function(x) {AIC(fit(depmix(forms1, family = list(gaussian(), gaussian()), nstates = x, data = FARC_results1)))})

AIC_df1 <- data.frame(cbind(num_states, AIC_vals1))

# plot the AIC values to select the optimal number of states
AIC_plot1 <- ggplot(AIC_df1, aes(x = num_states, y = AIC_vals1)) +
  geom_point() +
  ggtitle("AIC Values for n = 2:10 Latent States Fitted HMM")

AIC_plot1


#################################################################################
#################################################################################
# now optimize for the government

govt_results1 <- govt_results[, -(3:4)]
forms1_govt <- list(govt_results1$EmoNeg ~ 1, govt_results1$EmoPos ~ 1)

BIC_vals1_govt <- sapply(num_states, function(x) {BIC(fit(depmix(forms1_govt, family = list(gaussian(), gaussian()), nstates = x, data = govt_results1)))})
AIC_vals1_govt <- sapply(num_states, function(x) {AIC(fit(depmix(forms1_govt, family = list(gaussian(), gaussian()), nstates = x, data = govt_results1)))})


#################################################################################
#################################################################################
# do the results differ when covariates are added?

# Run function to add violence/public opinion levels to FARC df
FARC_results2 <- add_monthlies(FARC_results1)
govt_results2 <- add_monthlies(govt_results1)

# take the log of the monthly stats
FARC_results2[, 4:7] <- log(FARC_results2[, 4:7])
govt_results2[, 6:9] <- log(govt_results2[, 4:7])

BIC_vals_fitted <- sapply(num_states, function(x) {BIC(fit(depmix(forms1, family = list(gaussian(), gaussian()), nstates = x, transitions = list(~ FARC_actions, ~ pres_approve), data = FARC_results2)))})
AIC_vals_fitted <- sapply(num_states, function(x) {AIC(fit(depmix(forms1, family = list(gaussian(), gaussian()), nstates = x, transitions = list(~ FARC_actions, ~ pres_approve), data = FARC_results2)))})

BIC_vals_fitted_govt <- sapply(num_states, function(x) {BIC(fit(depmix(forms1_govt, family = list(gaussian(), gaussian()), nstates = x, transitions = list(~ FARC_actions, ~ pres_approve), data = govt_results2)))})
AIC_vals_fitted_govt <- sapply(num_states, function(x) {AIC(fit(depmix(forms1_govt, family = list(gaussian(), gaussian()), nstates = x, transitions = list(~ FARC_actions, ~ pres_approve), data = govt_results2)))})

## make a table for the paper
hmm_comparison <- data.frame(BIC_vals1, AIC_vals1, BIC_vals_fitted, AIC_vals_fitted, BIC_vals1_govt, AIC_vals1_govt, BIC_vals_fitted_govt, AIC_vals_fitted_govt)
colnames(hmm_comparison) <- c("BIC", "AIC", "BIC", "AIC", "BIC", "AIC", "BIC", "AIC")
stargazer(hmm_comparison, title="Comparison of Hidden Markov Models", column.labels=c("BIC", "AIC", "BIC", "AIC", "BIC", "AIC", "BIC", "AIC"), summary = FALSE, digits = 2, digit.separator = "")


#################################################################################
#################################################################################
# HMM wih 3 states

# for FARC: model w/o covars
hmm_F <- fit(depmix(forms1, family = list(gaussian(), gaussian()), nstates = 3, data = FARC_results1))
summary(hmm_F)

# what is the probability of being in a given state over time?
prob_HMM_F <- posterior(hmm_F)

# check that the rows sum to 1
rowSums(head(prob_HMM_F)[,2:4])


# plot probability of being in a state over time against the sentiment measures
colnames(prob_HMM_F) <- c("est_state", paste("P",1:3, sep="_state"))
prob_HMM_F <- cbind(FARC_results1, prob_HMM_F)

# label the states with substantive labels based on mean sentiment
# function to calculate measures of central tendency
mean_of_states <- function(df) {
  n1 <- mean(filter(df, est_state == 1)$EmoNeg)
  p1 <- mean(filter(df, est_state == 1)$EmoPos)
  
  n2 <- mean(filter(df, est_state == 2)$EmoNeg)
  p2 <- mean(filter(df, est_state == 2)$EmoPos)
  
  n3 <- mean(filter(df, est_state == 3)$EmoNeg)
  p3 <- mean(filter(df, est_state == 3)$EmoPos)
  
  sdn1 <- sd(filter(df, est_state == 1)$EmoNeg)
  sdp1 <- sd(filter(df, est_state == 1)$EmoPos)
  
  sdn2 <- sd(filter(df, est_state == 2)$EmoNeg)
  sdp2 <- sd(filter(df, est_state == 2)$EmoPos)
  
  sdn3 <- sd(filter(df, est_state == 3)$EmoNeg)
  sdp3 <- sd(filter(df, est_state == 3)$EmoPos)
  
  mdn1 <- median(filter(df, est_state == 1)$EmoNeg)
  mdn2 <- median(filter(df, est_state == 2)$EmoNeg)
  mdn3 <- median(filter(df, est_state == 3)$EmoNeg)
  
  mdp1 <- median(filter(df, est_state == 1)$EmoPos)
  mdp2 <- median(filter(df, est_state == 2)$EmoPos)
  mdp3 <- median(filter(df, est_state == 3)$EmoPos)
  
  results1 <- c(n1, sdn1, mdn1, p1, sdp1, mdp1)
  results2 <- c(n2, sdn2, mdn2, p2, sdp2, mdp2)
  results3 <- c(n3, sdn3, mdn3, p3, sdp3, mdp3)
  
  return(rbind(results1, results2, results3))
  
}

FARC_hmm_means <- mean_of_states(prob_HMM_F)

# what state are we in at a given time?

HMM_F_est_state_gg = ggplot(prob_HMM_F, aes(x = as.Date(date, origin = "1970-01-01"), y = est_state)) +
  geom_step(color = "#000000") +
  labs(
    x = "",
    y = "State",
    color = "Legend") +
  scale_x_date(date_minor_breaks = "1 month",
               limits = c(as.Date("2012-09-01", "%Y-%m-%d"), NA)) +
  scale_y_continuous(breaks = c(1, 2, 3)) +
  ggtitle("Estimated Hardline/Moderate/Conciliatory States of FARC")



#################################################################################
## now do the same for the government

hmm_g <- fit(depmix(forms1_govt, family = list(gaussian(), gaussian()), nstates = 3, data = govt_results1))
summary(hmm_g)

# what is the probability of being in a given state over time?
prob_HMM_g <- posterior(hmm_g)

# check that the rows sum to 1
rowSums(head(prob_HMM_g)[,2:4])

# plot probability of being in a state over time against the sentiment measures
colnames(prob_HMM_g) <- c("est_state", paste("P",1:3, sep="_state"))
prob_HMM_g <- cbind(govt_results1, prob_HMM_g)

# get means
govt_hmm_means <- mean_of_states(prob_HMM_g)

# cbind it into a table for stargazer
hmm_all_means <- cbind(FARC_hmm_means, govt_hmm_means)

stargazer(hmm_all_means, summary = FALSE, title = "Mean Sentiment Scores for Latent States", digits = 2)

# use hungarian algorithm to optimize labels
beta1 <- FARC_hmm_means[, 3]
beta2 <- govt_hmm_means[, 3]
align <- clue::solve_LSAP(beta1%*%t(beta2), maximum=TRUE)

# what state are we in at a given time?

HMM_g_est_state_gg = ggplot(prob_HMM_g, aes(x = as.Date(date, origin = "1970-01-01"), y = est_state)) +
  geom_step(color = "#000000") +
  labs(
    x = "",
    y = "State",
    color = "Legend") +
  scale_x_date(date_minor_breaks = "1 month",
               limits = c(as.Date("2012-09-01", "%Y-%m-%d"), NA)) +
  scale_y_continuous(breaks = c(1, 2, 3)) +
  ggtitle("Estimated Hardline/Moderate/Conciliatory States of Government")

#################################################################################
#################################################################################
# Transition model: sentiment-responds-to-sentiment

# Classify overall sentiment as "high" or "low": Compare negative and positive emotion proportions within a given document. 1 = high, 0 = low
FARC_results3 <- FARC_results
FARC_results3["sentiment_level"] <- as.numeric(2 * FARC_results3$EmoNeg <= FARC_results3$EmoPos)
FARC_results3["side"] <- "FARC"
govt_results3 <- govt_results
govt_results3["side"] <- "govt"
govt_results3["sentiment_level"] <- as.numeric(2 * govt_results3$EmoNeg <= govt_results3$EmoPos)

# combine them into one stream
transition_chain <- rbind(FARC_results3, govt_results3)
# sort by date
transition_chain <- transition_chain[order(as.Date(transition_chain$date, format = "%Y-%m-%d")), ]
# filter dates to the peace process
transition_chain <- filter(transition_chain, date > "2012-01-01")

# pairwise comparison to gauge "responsiveness": how often are the parties giving statements at t and t-1 different?

get_responsiveness <- function(df) {
  # put boolean of pairwise comparisons here
  vect <- rep(NA, (length(df[ , 1]) - 1))
  
  for (i in 2:length(df[ , 1])) {
    side_t <- df["side"][i, ]
    side_t_1 <- df["side"][i-1, ]
    
    vect[i] <- side_t != side_t_1
  }
  
  return(vect)
}

# how frequent are "responding pairs"?
pairwise_responsiveness <- get_responsiveness(transition_chain)
pairwise_num <- sum(na.omit(as.numeric(pairwise_responsiveness)))
pairwise_num


#################################################################################
#################################################################################
# Multinomial logit fitted MLE

# Let's get our dataset!!!!!!
mnl_df <- dplyr::select(transition_chain, date, sentiment_level, side)

# state_x = current state at t, state_y = next state at t+1
# State1 = FARC-low, State 2 = FARC-high, State 3 = govt-low, State 4 = govt-high

# Function takes 1 parameter: a df with "side" and "sentiment_level" variables
state_maker <- function(df) {
  df["state_x"] <- NA
  
  # fill in the appropriate states
  for (i in 1:length(df[, 1])) {
    
    # Divide FARC into states 1 and 2
    if (df["side"][i, ] == "FARC") {
      
      if (df["sentiment_level"][i, ] == 0) {
        df["state_x"][i, ] <- 1
      }
      
      else {
        df["state_x"][i, ] <- 2
      }
    }
    
    # Divide govt into states 3 and 4
    if (df["side"][i, ] == "govt") {
      
      if (df["sentiment_level"][i, ] == 0) {
      df["state_x"][i, ] <- 3
      }
      
    else {
      df["state_x"][i, ] <- 4
    }
      
  }
  }
  
  df["state_y"] <- NA
  
  for (i in 1:length(df[, 1]) -1) {
    df["state_y"][i, ] <- df["state_x"][i+1, ]
  }
  
  return(df)
  }

# Run function to create dataset
mnl_df <- state_maker(mnl_df)

# add the violence and public opinion stats to the dataset (for fixed effects) and then take log(x+1) because of zeroes
mnl_df <- add_monthlies(mnl_df)
mnl_df[6:9] <- log(mnl_df[6:9] + 1)

# add year for random/fixed effects
mnl_df$year <- mnl_df$date
mnl_df$year <- sapply(mnl_df$year, function(x) {substr(toString(x), 1, 4)})
mnl_df$year <- factor(mnl_df$year)

# factor and relevel
mnl_df$state_y <- factor(mnl_df$state_y)
mnl_df$state_x <- factor(mnl_df$state_x)
mnl_df$state_y2 <- relevel(mnl_df$state_y, ref = "1")


# Export to Stata.... sigh
write.dta(mnl_df, "mnl_data.dta")

# can't use multinom because fitting is not with MLE
# mnl_mod <- multinom(state_y2 ~ state_x + FARC_actions + pres_approve + year, data = mnl_df, na.action = na.omit)
# summary(mnl_mod)

# get the relative risk ratios
# exp(coef(mnl_mod))

# Convert for mlogit pkg
ml_df <- mnl_df
ml_df <- mlogit.data(ml_df, choice = "state_y2", shape = "wide")

# Model #1 specification
ml_mod1 <- mlogit::mlogit(formula = state_y2 ~ 1 | state_x, data = ml_df, reflevel = "4")
summary(ml_mod1)

ml_mod2 <- mlogit::mlogit(formula = state_y2 ~ 1 | state_x + FARC_actions + pres_approve + year, data = ml_df, reflevel = "4")

# relative risk ratio
mod1_rrr <- exp(coef(ml_mod1))

# predicted vals
mod1_fit <- fitted(ml_mod1)

hmftest(ml_mod1, ml_mod2)