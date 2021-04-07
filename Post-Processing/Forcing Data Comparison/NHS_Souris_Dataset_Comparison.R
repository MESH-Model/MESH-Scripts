# -------------------------------------------------------------------------
# Date created:      08-12-2020
# Last modified:     18-02-2021
# Version:           1.3
# Author:            Nathan Riis
# Description:       Compares historical (1979-2016) values of different forcing
#                    data sets for bias comparisons.
# 
# Required input(s): Water and energy balance for two sets of climate forcing
#                    files for each of the 15 runs. It can be set up for less by
#                    hardcoding fewer runs into the run list.
# 
# Outputs:           Plots boxplots for the average difference by each season
#                    for any specified variable. Each plot can be personalized
#                    by hardcoding details such as title and labels.
# -------------------------------------------------------------------------


library(lubridate)
library(reshape2)
library(tidyverse)


isrsb_red = "#BA1200"
isrsb_orange = "#FF8C00"
isrsb_yellow = "#C9B22F"
isrsb_green = "#246230"
isrsb_blue = "#5085BF"
isrsb_grey = "#7D7E75"
isrsb_black = "#071E22"


# set up ------------------------------------------------------------------

# working directory - program looks in folders stored in this directory
# just leaving this code because the goal is to eventually use the script location to access the supplemental files
dir <- "C:/Users/riisn/Documents/GitHub/MESH_Souris_River/Code/Postprocessing/"
setwd( dir )
setwd( "../supplemental_files_for_scripts/" )
supp_dir <- "C:/Users/riisn/Documents/GitHub/MESH_Souris_River/Code/supplemental_files_for_scripts/"
setwd( supp_dir )

# reads in specified inputs from general input file
plot_inputs <- read.table( "plot_inputs.txt" )
row.names(plot_inputs) <- plot_inputs$V1
plot_inputs$V1 <- NULL
plot_inputs <- t(plot_inputs)

# input directory holds required input files
input_dir <- plot_inputs[,"input_dir"]
# output directory is where all output files will be written to
output_dir <- plot_inputs[,"output_dir"]

timezone <- plot_inputs[,"timezone"]

# list of the 15 climate runs to loop through them all
run_list <- c( "r8i2p1r1",  "r8i2p1r2",  "r8i2p1r3",  "r8i2p1r4",  "r8i2p1r5",
                "r9i2p1r1",  "r9i2p1r2",  "r9i2p1r3",  "r9i2p1r4",  "r9i2p1r5",
                "r10i2p1r1", "r10i2p1r2", "r10i2p1r3", "r10i2p1r4", "r10i2p1r5"
)

set_list <- c(
    "WFDEI",
    "GEM-CaPA",
    "WFDEI-GEM-CaPA",
    "CanRCM4",
    "CanRCM4-WFDEI-GEM-CaPA"
)

suffixes <- c(
    "WFDEI_1979-2016",
    "GEM-CaPA_2002-2017",
    "WFDEI-GEM-CaPA_1979-2016",
    "CanRCM4_run_1951-2100", # uncorrected CanRCM4
    "CanRCM4-WFDEI-GEM-CaPA_run_1951-2100" # CanRCM4 corrected to WFDEI-GEM-CaPA in full for testing
    # "CanRCM4-WFDEI-GEM-CaPA_run_1951-2020", # CanRCM4 corrected to WFDEI-GEM-CaPA part 1 from MESH
    # "CanRCM4-WFDEI-GEM-CaPA_run_2020-2100" # CanRCM4 corrected to WFDEI-GEM-CaPA part 2 from MESH
)

comparison_list <- c(
    "CanRCM4vsWGC",
    "CanRCM4WGCvsWGC"
)




all_sets <- data.frame()

for (set in suffixes){
    all_runs <- data.frame()
    
    # takes out the first four digits of the time period specified on each set title
    start_year <- as.numeric(substr(set, nchar(set)-8, nchar(set)-5))
    end_year <- as.numeric(substr(set, nchar(set)-3, nchar(set)))
    
    
    if ( grepl("run", set) ){
        start_date <- as.Date("1951-01-01")
        end_date <- as.Date("2100-12-31")
        
        for ( run in run_list ){
            # substitutes the word run for the run r#i#p#r# to find the correct input file
            filename <- paste0(gsub("run",run,set),".csv")
            
            print(filename)
            
            energy <- read.csv( paste0( input_dir,"Basin_average_energy_balance_",filename ) )
            if (energy$YEAR[1]==0){ mesh_run <- TRUE } else { mesh_run <- FALSE }
            if (colnames(energy)[2]=="DAY"){ old_mesh <- TRUE } else { old_mesh <- FALSE }
            
            # adjust date from year+day to y-m-d
            if ( mesh_run ){
                # data sets run through current MESH start at "day 0" and "year 0"
                # non-MESH output files start at the correct year and "day 1"
                energy$YEAR <- energy$YEAR+start_year
            }
            
            if ( old_mesh ){ energy$DATE <- as.Date(energy$DAY,origin=paste0(energy$YEAR-1,"-12-31")) }
            # new MESH runs use JDAY instead of DAY
            else { energy$DATE <- as.Date(energy$JDAY,origin=paste0(energy$YEAR-1,"-12-31")) }
            
            # reads in non-blank columns
            energy <- subset(energy, !is.na(DATE), select = c(DATE, YEAR, FSIN, FLIN, TA, QA, UV, PRES)) %>%
                mutate(MONTH = month(DATE)) %>%
                # adds 1 to December's year for keeping winter seasons together
                mutate(SEASON_YEAR = YEAR+floor(MONTH/12)) %>%
                # denoting which months fit into each season for by-season calculations
                mutate(SEASON = case_when(MONTH %in% c(1,2,12) ~ "Winter",
                                          MONTH %in% c(3:5) ~ "Spring",
                                          MONTH %in% c(6:8) ~ "Summer",
                                          MONTH %in% c(9:11) ~ "Fall"))
            
            # reads in non-blank columns, ignoring PREACC which is easily calculated later
            water <- read.csv( paste0( input_dir,"Basin_average_water_balance_",filename ) )
            # adjust date from year+day to y-m-d
            if ( mesh_run ){
                # data sets run through current MESH start at "day 0" and "year 0"
                # non-MESH output files start at the correct year and "day 1"
                water$YEAR <- water$YEAR+start_year
            }
            
            if ( old_mesh ){
                water$DATE <- as.Date(water$DAY,origin=paste0(energy$YEAR-1,"-12-31"))
            } else {
                # new MESH runs use JDAY instead of DAY
                water$DATE <- as.Date(water$JDAY,origin=paste0(energy$YEAR-1,"-12-31"))
                # current versions of MESH also use PREC instead of PRE for precipitation output
                water <- rename(water, c("PRE" = "PREC"))
            }
            
            water <- subset(water, !is.na(DATE), select = c(DATE, YEAR, PRE)) %>%
                group_by(YEAR) %>% mutate(PREACC = sum(PRE)) %>% ungroup() %>%
                mutate(MONTH = month(DATE)) %>%
                # adding cumulative precipitation by month
                group_by(YEAR,MONTH) %>% mutate(M_PREACC = sum(PRE)) %>% ungroup() %>%
                # adds 1 to December's year for keeping winter seasons together
                mutate(SEASON_YEAR = YEAR+floor(MONTH/12)) %>%
                # denoting which months fit into each season for by-season calculations
                mutate(SEASON = case_when(MONTH %in% c(1,2,12) ~ "Winter",
                                          MONTH %in% c(3:5) ~ "Spring",
                                          MONTH %in% c(6:8) ~ "Summer",
                                          MONTH %in% c(9:11) ~ "Fall"))
            
            # adding cumulative precipitation by season
            water <- water %>% group_by(SEASON,SEASON_YEAR) %>% mutate(S_PREACC = sum(PRE)) %>% ungroup()
            
            # sets the SET category depending on whether the set being read is corrected or not (-WFDEI-GEM-CaPA)
            if ( grepl("WFDEI-GEM-CaPA", set) ){ set_label <- set_list[5] }
            else { set_label <- set_list[4] }
            
            # combining all data into a single dataframe
            run_data <- full_join(energy,water,by=c("DATE","SEASON","YEAR","SEASON_YEAR","MONTH")) %>%
                subset(DATE>=start_date & DATE<=end_date) %>%
                melt(id.vars = c("DATE","SEASON","YEAR","SEASON_YEAR","MONTH")) %>%
                mutate(RUN = run) %>%
                mutate(SET = set_label)
            
            # appends the data for each run
            all_runs <- rbind(all_runs,run_data)
        }
    } else {
        if ( grepl("WFDEI",set) ){
            start_date <- as.Date("1979-01-01")
            end_date <- as.Date("2016-12-31")
        } else if ( grepl("GEM-CaPA",set) ){
            # GEM-CaPA files contain data from 2002, but there are issues with the CaPA data until 2005
            start_date <- as.Date("2005-01-01")
            end_date <- as.Date("2017-09-01")
        }
        
        print(set)
        
        energy <- read.csv( paste0(input_dir,"Basin_average_energy_balance_",set,".csv") )
        if (energy$YEAR[1]==0){ mesh_run <- TRUE } else { mesh_run <- FALSE }
        if (colnames(energy)[2]=="DAY"){ old_mesh <- TRUE } else { old_mesh <- FALSE }
        
        # adjust date from year+day to y-m-d
        
        # data sets run through current MESH start at "day 0" and "year 0"
        # non-MESH output files start at the correct year and "day 1"
        if ( mesh_run ){ energy$YEAR <- energy$YEAR+start_year }
        
        if ( old_mesh ){ energy$DATE <- as.Date(energy$DAY,origin=paste0(energy$YEAR-1,"-12-31")) }
        # new MESH runs use JDAY instead of DAY
        else { energy$DATE <- as.Date(energy$JDAY,origin=paste0(energy$YEAR-1,"-12-31")) }
        
        energy <- subset(energy, !is.na(DATE), select = c(DATE, YEAR, FSIN, FLIN, TA, QA, UV, PRES)) %>%
            mutate(MONTH = month(DATE)) %>%
            # adds 1 to December's year for keeping winter seasons together
            mutate(SEASON_YEAR = YEAR+floor(MONTH/12)) %>%
            # denoting which months fit into each season for by-season calculations
            mutate(SEASON = case_when(MONTH %in% c(1,2,12) ~ "Winter",
                                      MONTH %in% c(3:5) ~ "Spring",
                                      MONTH %in% c(6:8) ~ "Summer",
                                      MONTH %in% c(9:11) ~ "Fall"))
        
        # reads in non-blank columns, ignoring PREACC which is easily calculated later
        water <- read.csv( paste0(input_dir,"Basin_average_water_balance_",set,".csv") )
        
        # adjust date from year+day to y-m-d
        
        # data sets run through current MESH start at "day 0" and "year 0"
        # non-MESH output files start at the correct year and "day 1"
        if ( mesh_run ){ water$YEAR <- water$YEAR+start_year }
        
        if ( old_mesh ){
            water$DATE <- as.Date(water$DAY,origin=paste0(energy$YEAR-1,"-12-31"))
        } else {
            # new MESH runs use JDAY instead of DAY
            water$DATE <- as.Date(water$JDAY,origin=paste0(energy$YEAR-1,"-12-31"))
            # current versions of MESH also use PREC instead of PRE for precipitation output
            water <- rename(water, c("PRE" = "PREC"))
        }
        
        water <- subset(water, !is.na(DATE), select = c(DATE, YEAR, PRE)) %>%
            group_by(YEAR) %>% mutate(PREACC = sum(PRE)) %>% ungroup() %>%
            mutate(MONTH = month(DATE)) %>%
            # adding cumulative precipitation by month
            group_by(YEAR,MONTH) %>% mutate(M_PREACC = sum(PRE)) %>% ungroup() %>%
            # adds 1 to December's year for keeping winter seasons together
            mutate(SEASON_YEAR = YEAR+floor(MONTH/12)) %>%
            # denoting which months fit into each season for by-season calculations
            mutate(SEASON = case_when(MONTH %in% c(1,2,12) ~ "Winter",
                                      MONTH %in% c(3:5) ~ "Spring",
                                      MONTH %in% c(6:8) ~ "Summer",
                                      MONTH %in% c(9:11) ~ "Fall"))
        
        # adding cumulative precipitation by season
        water <- water %>% group_by(SEASON,SEASON_YEAR) %>% mutate(S_PREACC = sum(PRE)) %>% ungroup()
        
        if ( grepl("WFDEI",set) ){
            if ( grepl("GEM-CaPA",set) ){ set_label <- set_list[3] } # WFDEI-GEM-CaPA
            else { set_label <- set_list[1] } # WFDEI
        } else { set_label <- set_list[2] } # GEM-CaPA
        
        # combining all data into a single dataframe
        set_data <- full_join(energy,water,by=c("DATE","SEASON","YEAR","SEASON_YEAR","MONTH")) %>%
            subset(DATE>=start_date & DATE<=end_date) %>%
            melt(id.vars = c("DATE","SEASON","YEAR","SEASON_YEAR","MONTH")) %>%
            mutate(RUN = "NA") %>%
            mutate(SET = set_label)
        
        all_runs <- set_data
    }
    
    # appends the data for each data set
    all_sets <- rbind(all_sets,all_runs)
}

# setting up historical and projected periods
# all_sets$TYPE[all_sets$YEAR < 2006] <- "Hist"
# all_sets$TYPE[all_sets$YEAR > 2005] <- "Proj"
all_sets$TYPE[all_sets$YEAR < 2017] <- "Hist"
all_sets$TYPE[all_sets$YEAR > 2016] <- "Proj"

s <- all_sets # for testing


# separated out because of time period difference
monthly_avgs <- subset(all_sets,YEAR %in% c(2005:2016)) %>%
    group_by(SET,RUN,MONTH,variable) %>% mutate(HIST_MONTHLY_AVG = mean(value)) %>% ungroup()


cc_signal <- subset(all_sets,grepl("CanRCM",SET) & YEAR %in% c(1979:2008,2071:2100)) %>%
    group_by(SET,RUN,TYPE,MONTH,variable) %>% mutate(MONTHLY_AVG = mean(value)) %>% ungroup()
cc_signal <- unique(subset(cc_signal,select=c(SET,RUN,TYPE,MONTH,variable,MONTHLY_AVG)))

cc_signal <- subset(cc_signal,TYPE=="Proj",select=c(SET,RUN,MONTH,variable)) %>%
    mutate(CC_SIGNAL = (subset(cc_signal,TYPE=="Proj",select=c(MONTHLY_AVG)) - subset(cc_signal,TYPE=="Hist",select=c(MONTHLY_AVG)))[,"MONTHLY_AVG"]) %>%
    subset(RUN=="r8i2p1r1")
    # group_by(SET,MONTH,variable) %>% mutate(CC_SIGNAL = mean(CC_SIGNAL)) # removes run specification by taking the average


all_sets <- all_sets %>%
    group_by(SET,RUN,YEAR,variable) %>% mutate(ANNUAL_AVG = mean(value)) %>% ungroup() %>%
    group_by(SET,RUN,SEASON,SEASON_YEAR,variable) %>% mutate(SEASONAL_AVG = mean(value)) %>% ungroup()

all_sets_08 <- subset(all_sets,YEAR %in% c(1979:2008)) %>%
    group_by(SET,RUN,variable) %>% mutate(HIST_ANNUAL_AVG = mean(ANNUAL_AVG)) %>% ungroup() %>%
    group_by(SET,RUN,SEASON,variable) %>% mutate(HIST_SEASONAL_AVG = mean(SEASONAL_AVG)) %>% ungroup() %>%
    group_by(SET,MONTH,variable) %>% mutate(HIST_MONTHLY_AVG = mean(value)) %>% ungroup()

all_sets_16 <- subset(all_sets,YEAR %in% c(1979:2016)) %>%
    group_by(SET,RUN,variable) %>% mutate(HIST_ANNUAL_AVG = mean(ANNUAL_AVG)) %>% ungroup() %>%
    group_by(SET,RUN,SEASON,variable) %>% mutate(HIST_SEASONAL_AVG = mean(SEASONAL_AVG)) %>% ungroup() %>%
    group_by(SET,MONTH,variable) %>% mutate(HIST_MONTHLY_AVG = mean(value)) %>% ungroup()


conversion <- 273.16

crcm_seasonal <- unique(subset(all_sets_16,SET=="CanRCM4" | SET=="CanRCM4-WFDEI-GEM-CaPA",
                               select=c(SET,RUN,SEASON,variable,HIST_SEASONAL_AVG)))
wgc_seasonal <- unique(subset(all_sets_16,SET=="WFDEI-GEM-CaPA",
                              select=c(SEASON,variable,HIST_SEASONAL_AVG))) %>%
    rename(WGC_HIST_SEASONAL_AVG = HIST_SEASONAL_AVG)
seasonal <- right_join(crcm_seasonal,wgc_seasonal,by=c("SEASON","variable")) %>%
    mutate(HIST_COMPARISON = HIST_SEASONAL_AVG - WGC_HIST_SEASONAL_AVG) %>%
    mutate(HIST_PCT_DIFF = 100 * (HIST_SEASONAL_AVG - WGC_HIST_SEASONAL_AVG)/WGC_HIST_SEASONAL_AVG )
# correcting percent difference for temperature, which is given in celsius not kelvin
temp <- seasonal$WGC_HIST_SEASONAL_AVG[seasonal$variable=="TA"]
seasonal$HIST_PCT_DIFF[seasonal$variable=="TA"] <- seasonal$HIST_PCT_DIFF[seasonal$variable=="TA"] * temp/(temp+conversion)

crcm_annual <- unique(subset(all_sets_16,SET=="CanRCM4" | SET=="CanRCM4-WFDEI-GEM-CaPA",
                             select=c(SET,RUN,variable,HIST_ANNUAL_AVG)))
wgc_annual <- unique(subset(all_sets_16,SET=="WFDEI-GEM-CaPA",
                            select=c(variable,HIST_ANNUAL_AVG))) %>%
    rename(WGC_HIST_ANNUAL_AVG = HIST_ANNUAL_AVG)
annual <- right_join(crcm_annual,wgc_annual,by=c("variable")) %>%
    mutate(HIST_COMPARISON = HIST_ANNUAL_AVG - WGC_HIST_ANNUAL_AVG) %>%
    mutate(HIST_PCT_DIFF = 100 * (HIST_ANNUAL_AVG - WGC_HIST_ANNUAL_AVG)/WGC_HIST_ANNUAL_AVG ) %>%
    mutate(SEASON = "Annual")
# correcting percent difference for temperature, which is given in celsius not kelvin
temp <- annual$WGC_HIST_ANNUAL_AVG[annual$variable=="TA"]
annual$HIST_PCT_DIFF[annual$variable=="TA"] <- annual$HIST_PCT_DIFF[annual$variable=="TA"] * temp/(temp+conversion)

comparison <- rbind(subset(seasonal,select=c(SET,RUN,SEASON,variable,HIST_COMPARISON,HIST_PCT_DIFF)),
                    subset(annual,select=c(SET,RUN,SEASON,variable,HIST_COMPARISON,HIST_PCT_DIFF))) %>%
    ungroup()



# plotting boxplot data of variables, mean difference between CanRCM4-WFDEI-GEM-CaPA and WFDEI-GEM-CaPA

for (var in c("FSIN", "FLIN", "TA", "QA", "UV", "PRES", "PREACC")){
# for (var in c("FSIN", "FLIN", "TA", "QA", "UV", "PRES", "PRE", "PREACC")){
    if (var == "TA"){
        # ylabel <- "(°C)"
        ylabel <- "(K)"
        # plot_label <- "Surface Air Temperature"
        plot_label <- "Temperature"
    } else if (var == "PRE"){
        ylabel <- "(mm)"
        plot_label <- "Precipitation"
    } else if (var == "PREACC"){
        ylabel <- "(mm)"
        # plot_label <- "Accumulated Precipitation"
        plot_label <- "Total Precipitation"
    } else if (var == "QA"){
        ylabel <- "(kg per kg)"
        plot_label <- "Humidity"
    } else if (var == "FSIN"){
        ylabel <- "(W per m^2)"
        # plot_label <- "Downwelling Shortwave"
        plot_label <- "Shortwave"
    } else if (var == "FLIN"){
        ylabel <- "(W per m^2)"
        # plot_label <- "Downwelling Longwave"
        plot_label <- "Longwave"
    } else if (var == "UV"){
        ylabel <- "(m per s)"
        plot_label <- "Wind"
    } else if (var == "PRES"){
        ylabel <- "(Pa)"
        # plot_label <- "Atmospheric Pressure"
        plot_label <- "Pressure"
    }
    
    # plots individual boxplots
    for (comp in comparison_list){
        if (comp=="CanRCM4vsWGC"){ set <- "CanRCM4" }
        else { set <- "CanRCM4-WFDEI-GEM-CaPA" }
        
        if (var!="PREACC"){ plot_data <- subset(comparison,variable==var) }
        else { plot_data <- subset(comparison, (variable==var & SEASON=="Annual")
                                   | (variable=="S_PREACC" & SEASON!="Annual") ) }
        
        plot_data <- subset(plot_data,SET==set)
        
        filename <- paste0("NHS_Souris_HistvsHist_",comp,"_",var,"_by_season")
        # ycalc <- paste0("CanRCM4-WFDEI-GEM-CaPA - WFDEI-GEM-CaPA ",ylabel)
        ycalc <- paste0(plot_label," Difference ",ylabel)
        
        # separates each season's data to be plotted in the correct order
        plot_data$SEASON <- factor(plot_data$SEASON, levels=c("Winter","Spring","Summer","Fall","Annual"))
        
        plotname <- paste0(set," vs WFDEI-GEM-CaPA\n",plot_label)
        
        ggplot(plot_data) + geom_boxplot(aes(SEASON,HIST_COMPARISON,fill=SEASON)) +
            scale_y_continuous(name = ycalc, label=scales::comma) +
            labs(title = plotname) +
            scale_fill_manual(values=c("Winter" = isrsb_blue, "Spring" = isrsb_green,
                                       "Summer" = isrsb_red, "Fall" = isrsb_yellow, "Annual" = isrsb_orange)) +
            theme_bw() + theme(legend.position = "bottom", legend.title = element_blank(),
                               plot.title = element_text(hjust = 0.5))
        
        ggsave(paste0(output_dir,filename,".png"), width = 5, height = 4)
    }
    
    # if (var!="PREACC"){ plot_data <- subset(comparison,variable==var) }
    # else { plot_data <- subset(comparison, (variable==var & SEASON=="Annual")
    #                            | (variable=="S_PREACC" & SEASON!="Annual") ) }
    # 
    # filename <- paste0("NHS_Souris_1979-2008_CanRCMvsWGC_",var,"_by_season")
    # ycalc <- paste0(plot_label," Difference ",ylabel)
    # 
    # # separates each season's data to be plotted in the correct order
    # plot_data$SEASON <- factor(plot_data$SEASON, levels=c("Winter","Spring","Summer","Fall","Annual"))
    # 
    # set_labels <- c("CanRCM4 vs WFDEI-GEM-CaPA", "CanRCM4-WFDEI-GEM-CaPA vs WFDEI-GEM-CaPA")
    # names(set_labels) <- c("CanRCM4", "CanRCM4-WFDEI-GEM-CaPA")
    # 
    # ggplot(plot_data) + geom_boxplot(aes(SEASON,HIST_COMPARISON,fill=SEASON)) +
    #     scale_y_continuous(name = ycalc, label=scales::comma,
    #                        sec.axis = sec_axis(~ . , name = ycalc, label=scales::comma)) +
    #     scale_x_discrete(name = NULL) +
    #     facet_wrap(~SET, labeller = labeller(SET = set_labels)) +
    #     scale_fill_manual(values=c("Winter" = isrsb_blue, "Spring" = isrsb_green,
    #                                "Summer" = isrsb_red, "Fall" = isrsb_yellow, "Annual" = isrsb_orange)) +
    #     theme_bw() + theme(legend.position = "bottom", legend.title = element_blank())
    # 
    # ggsave(paste0(output_dir,filename,".png"), width = 10, height = 4)
    
    
    filename <- paste0("Souris_1979-2008_",var,"_Monthly")
    plotname <- paste0("Mean Monthly ",plot_label," (1979-2008)")
    
    ycalc <- paste0("Mean Monthly ",plot_label," ",ylabel)
    
    if (var!="PREACC"){ plot_data <- subset(all_sets_08,variable==var) }
    else { plot_data <- subset(all_sets_16,variable=="M_PREACC") }
    plot_data <- unique(subset(plot_data,(SET=="WFDEI" | SET=="WFDEI-GEM-CaPA"
                                          | SET=="CanRCM4" | SET=="CanRCM4-WFDEI-GEM-CaPA"),
                               select=c(SET,RUN,MONTH,HIST_MONTHLY_AVG)))
    plot_data$DATE <- as.Date(paste0("1979-",plot_data$MONTH,"-1"))
    
    ggplot(plot_data,aes(DATE,HIST_MONTHLY_AVG)) + geom_line(aes(colour=SET,linetype=SET),size=1) +
        scale_x_date(name = "Date", date_labels = "%b",date_breaks = "1 month",
                     expand = expansion(c(0,0.01),c(1,1))) +
        scale_y_continuous(name = ycalc, label=scales::comma) +
        labs(title = plotname) +
        scale_colour_manual(values=c("WFDEI" = isrsb_green, "WFDEI-GEM-CaPA" = isrsb_blue,
                                     "CanRCM4" = isrsb_orange, "CanRCM4-WFDEI-GEM-CaPA" = isrsb_red)) +
        scale_linetype_manual(values = c("WFDEI" = "solid", "WFDEI-GEM-CaPA" = "dashed",
                                         "CanRCM4" = "solid", "CanRCM4-WFDEI-GEM-CaPA" = "solid"),
                              guide = FALSE) +
        theme_bw() + theme(legend.position = "bottom", legend.title = element_blank(),
                           plot.title = element_text(hjust = 0.5))
    
    ggsave(paste0(output_dir,filename,".png"), width = 7, height = 5)
    
    
    filename <- paste0("Souris_",var,"_annual")
    plotname <- paste0("Mean Annual ",plot_label," (1979-2016)")
    # plotname <- "Souris Basin, RCP 8.5 Downscaled and Bias Corrected CanRCM4\nand WFDEI-GEM-CaPA (1979-2016)"
    
    ycalc <- paste0("Mean Annual ",plot_label," ",ylabel)
    
    plot_data <- unique(subset(all_sets_16,variable==var & (SET=="WFDEI" | SET=="GEM-CaPA" | SET=="WFDEI-GEM-CaPA"),
                               select=c(SET,RUN,YEAR,ANNUAL_AVG)))
    plot_data$DATE <- as.Date(paste0(plot_data$YEAR,"-01-01"))
    
    ggplot(plot_data,aes(DATE,ANNUAL_AVG)) + geom_line(aes(colour=SET),size=1) +
        scale_x_date(name = "Date", date_labels = "%Y",date_breaks = "1 year", expand = c(0,0)) +
        scale_y_continuous(name = ycalc, label=scales::comma) +
        # scale_y_continuous(name = "Mean Annual Precipitation (mm/yr)", expand = c(0,0)) +
        # coord_cartesian(ylim=c(0,800)) +
        labs(title = plotname) +
        scale_colour_manual(values=c("WFDEI" = isrsb_green, "GEM-CaPA" = isrsb_grey,
                                     "WFDEI-GEM-CaPA" = isrsb_blue)) +
        theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                           legend.position = "bottom", legend.title = element_blank(),
                           plot.title = element_text(hjust = 0.5))
    
    ggsave(paste0(output_dir,filename,".png"), width = 7, height = 5)
    
    
    filename <- paste0("Souris_",var,"_monthly")
    plotname <- paste0("Mean Monthly ",plot_label," (2005-2016)")
    # plotname <- "Souris Basin, RCP 8.5 Downscaled and Bias Corrected CanRCM4\nand WFDEI-GEM-CaPA (2005-2016)"
    
    ycalc <- paste0("Mean Monthly ",plot_label," ",ylabel)
    
    if (var!="PREACC"){ plot_data <- subset(monthly_avgs,variable==var) }
    else { plot_data <- subset(monthly_avgs,variable=="M_PREACC") }
    plot_data <- unique(subset(plot_data,(SET=="WFDEI" | SET=="GEM-CaPA" | SET=="WFDEI-GEM-CaPA"),
                               select=c(SET,RUN,MONTH,HIST_MONTHLY_AVG)))
    plot_data$DATE <- as.Date(paste0("2005-",plot_data$MONTH,"-1"))
    
    ggplot(plot_data,aes(DATE,HIST_MONTHLY_AVG)) + geom_line(aes(colour=SET),size=1) +
    # ggplot(plot_data,aes(DATE,HIST_MONTHLY_AVG)) + geom_line(aes(colour=SET,linetype=SET),size=1) +
        scale_x_date(name = "Date", date_labels = "%b",date_breaks = "1 month",
                     expand = expansion(c(0,0.01),c(1,1)), label=scales::comma) +
        scale_y_continuous(name = ycalc) +
        # scale_y_continuous(name = "Mean Monthly Precipitation\n(mm/month)", expand = c(0,0)) +
        # coord_cartesian(ylim=c(0,100)) +
        labs(title = plotname) +
        scale_colour_manual(values=c("WFDEI" = isrsb_green, "GEM-CaPA" = isrsb_grey, 
                                     "WFDEI-GEM-CaPA" = isrsb_blue)) +
        # scale_linetype_manual(values = c("WFDEI" = "solid", "GEM-CaPA" = "solid",
        #                                  "WFDEI-GEM-CaPA" = "dashed"), guide = FALSE) +
        theme_bw() + theme(legend.position = "bottom", legend.title = element_blank(),
                           plot.title = element_text(hjust = 0.5))
    
    ggsave(paste0(output_dir,filename,".png"), width = 5, height = 4)
    
    
    filename <- paste0("Souris_Climate_Signal_",var)
    plotname <- paste0("Climate Change Signal ",plot_label)
    # plotname <- "Souris Basin, RCP 8.5 Downscaled and Bias Corrected CanRCM4\nand CanRCM4-WFDEI-GEM-CaPA"
    
    ycalc <- paste0(plot_label," Difference ",ylabel)
    
    if (var!="PREACC"){ plot_data <- subset(cc_signal,variable==var) }
    else { plot_data <- subset(cc_signal,variable=="M_PREACC") }
    plot_data$DATE <- as.Date(paste0("2005-",plot_data$MONTH,"-1"))
    
    ggplot(plot_data,aes(DATE,CC_SIGNAL)) + geom_line(aes(colour=SET),size=1) +
        scale_x_date(name = "Date", date_labels = "%b",date_breaks = "1 month",
                     expand = expansion(c(0,0.02),c(1,1)), label=scales::comma) +
        scale_y_continuous(name = ycalc) +
        # scale_y_continuous(name = "Temperature Difference (K)", breaks = seq(0,9,by=1), expand = c(0,0)) +
        # coord_cartesian(ylim=c(0,9)) +
        labs(title = plotname) +
        scale_colour_manual(values=c("CanRCM4" = isrsb_orange, "CanRCM4-WFDEI-GEM-CaPA" = isrsb_red)) +
        theme_bw() + theme(legend.position = "bottom", legend.title = element_blank(),
                           plot.title = element_text(hjust = 0.5))
    
    ggsave(paste0(output_dir,filename,".png"), width = 5, height = 4)
}


# plotting differences against one another, in this case specifically mean temperature difference vs %diff precipitation
# points plotted by run and graphed by season

filename <- "Souris_HistvsHist_PREvsTA_CanRCM-WGC_vs_WGC"
plotname <- "Souris Basin, Historical RCP 8.5 Downscaled and Bias Corrected CanRCM4\nvs. WFDEI-GEM-CaPA (1979-2016)"

plot_data <- subset(seasonal, SET=="CanRCM4-WFDEI-GEM-CaPA" & (variable=="TA" | variable=="PREACC"),
                    select=c(-HIST_SEASONAL_AVG,-WGC_HIST_SEASONAL_AVG) )

tavspre <- cbind( subset(plot_data,variable=="TA",select=c(RUN,SEASON,HIST_COMPARISON)),
                  subset(plot_data,variable=="PREACC",select=c(HIST_PCT_DIFF)) )

ggplot(tavspre,aes(HIST_COMPARISON,HIST_PCT_DIFF)) + geom_point(aes(shape = RUN, fill = RUN), size = 2) +
    labs(title = plotname, x = "Mean Temperature Difference (deg C)", y = "Precipitation Difference (%)") +
    facet_wrap(~SEASON) +
    scale_shape_manual(values=c("circle open",   "triangle filled", "triangle down open",
                                "square filled", "diamond open",    "square cross",
                                "circle filled", "triangle open",   "triangle down filled",
                                "square open",   "diamond filled",  "diamond plus",
                                "plus",          "asterisk",        "cross"),
                       name = '') +
    scale_fill_manual(values=c("#000000","#999999","#000000",
                               "#0D00FF","#000000","#000000",
                               "#0A8214","#000000","#FFB433",
                               "#000000","#820A0A","#000000",
                               "#000000","#000000","#000000"),
                      name = '') +
    theme_bw() +
    theme(legend.position = "bottom", legend.title = element_blank())

ggsave(paste0(output_dir,filename,".png"), width = 10, height = 8)


# filename <- "Souris_PREACC_annual"
# plotname <- "Total Annual Precipitation (1979-2016)"
# # plotname <- "Souris Basin, RCP 8.5 Downscaled and Bias Corrected CanRCM4\nand WFDEI-GEM-CaPA (1979-2016)"
# 
# plotting_data <- unique(subset(all_sets,variable=="PREACC" & (SET=="WFDEI" | SET=="GEM-CaPA" | SET=="WFDEI-GEM-CaPA"),
#                                select=c(SET,RUN,YEAR,ANNUAL_AVG)))
# plotting_data$DATE <- as.Date(paste0(plotting_data$YEAR,"-01-01"))
# ggplot(plotting_data,aes(DATE,ANNUAL_AVG)) + geom_line(aes(colour=SET),size=1) +
#     scale_x_date(name = "Date", date_labels = "%Y",date_breaks = "1 year", expand = c(0,0)) +
#     scale_y_continuous(name = "Mean Annual Precipitation (mm/yr)", expand = c(0,0)) +
#     coord_cartesian(ylim=c(0,800)) +
#     labs(title = plotname) +
#     scale_colour_manual(values=c("WFDEI" = isrsb_green, "WFDEI-GEM-CaPA" = isrsb_blue, "GEM-CaPA" = isrsb_grey)) +
#     theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
#                        legend.position = "bottom", legend.title = element_blank(),
#                        plot.title = element_text(hjust = 0.5))
# 
# ggsave(paste0(output_dir,filename,".png"), width = 7, height = 5)
# 
# 
# filename <- "Souris_PREACC_monthly"
# plotname <- "Mean Monthly Total Precipitation (2005-2016)"
# # plotname <- "Souris Basin, RCP 8.5 Downscaled and Bias Corrected CanRCM4\nand WFDEI-GEM-CaPA (2005-2016)"
# 
# plotting_data <- unique(subset(monthly_avgs,variable=="M_PREACC" & (SET=="WFDEI" | SET=="GEM-CaPA" | SET=="WFDEI-GEM-CaPA"),
#                                select=c(SET,RUN,MONTH,HIST_MONTHLY_AVG)))
# plotting_data$DATE <- as.Date(paste0("2006-",plotting_data$MONTH,"-1"))
# ggplot(plotting_data,aes(DATE,HIST_MONTHLY_AVG)) + geom_line(aes(colour=SET),size=1) +
#     scale_x_date(name = "Date", date_labels = "%b",date_breaks = "1 month", expand = expansion(c(0,0.01),c(1,1))) +
#     scale_y_continuous(name = "Mean Monthly Precipitation\n(mm/month)", expand = c(0,0)) +
#     coord_cartesian(ylim=c(0,100)) +
#     labs(title = plotname) +
#     scale_colour_manual(values=c("WFDEI" = isrsb_green, "WFDEI-GEM-CaPA" = isrsb_blue, "GEM-CaPA" = isrsb_grey)) +
#     theme_bw() + theme(legend.position = "bottom", legend.title = element_blank(),
#                        plot.title = element_text(hjust = 0.5))
# 
# ggsave(paste0(output_dir,filename,".png"), width = 5, height = 4)
# 
# 
# filename <- "Souris_Climate_Signal"
# plotname <- "Climate Change Signal"
# # plotname <- "Souris Basin, RCP 8.5 Downscaled and Bias Corrected CanRCM4\nand CanRCM4-WFDEI-GEM-CaPA"
# 
# plotting_data <- cc_signal
# plotting_data$DATE <- as.Date(paste0("2006-",plotting_data$MONTH,"-1"))
# ggplot(plotting_data,aes(DATE,CC_SIGNAL)) + geom_line(aes(colour=SET),size=1) +
#     scale_x_date(name = "Date", date_labels = "%b",date_breaks = "1 month", expand = expansion(c(0,0.02),c(1,1))) +
#     scale_y_continuous(name = "Temperature Difference (K)", breaks = seq(0,9,by=1), expand = c(0,0)) +
#     coord_cartesian(ylim=c(0,9)) +
#     labs(title = plotname) +
#     scale_colour_manual(values=c("CanRCM4" = isrsb_orange, "CanRCM4-WFDEI-GEM-CaPA" = isrsb_red)) +
#     theme_bw() + theme(legend.position = "bottom", legend.title = element_blank())
# 
# ggsave(paste0(output_dir,filename,".png"), width = 5, height = 4)


# # q-q plot
# plotting_data <- subset(all_sets,)
# 
# ggplot(plotting_data, aes(station,M_PREACC,sample=mpg))+stat_qq()
