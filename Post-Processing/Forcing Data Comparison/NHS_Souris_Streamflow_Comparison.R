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

# input directory holds required input files
input_dir <- "C:\Users\riisn\Documents\Climate_Change\"
# output directory is where all output files will be written to
output_dir <- "C:\Users\riisn\Documents\Climate_Change\out\"

timezone <- plot_inputs[,"timezone"]

# list of the 15 climate runs to loop through them all
full_run_list <- c( "r8i2p1r1"#,  "r8i2p1r2",  "r8i2p1r3",  "r8i2p1r4",  "r8i2p1r5",
               # "r9i2p1r1",  "r9i2p1r2",  "r9i2p1r3",  "r9i2p1r4",  "r9i2p1r5",
               # "r10i2p1r1", "r10i2p1r2", "r10i2p1r3", "r10i2p1r4", "r10i2p1r5"
)

set_list <- c(
    "WFDEI-GEM-CaPA",
    "CanRCM4",
    "CanRCM4-WFDEI-GEM-CaPA"
)

suffixes <- c(
    "WFDEI-GEM-CaPA_1979-2016"#,
    # "CanRCM4_run_1951-2100", # uncorrected CanRCM4
    # "CanRCM4-WFDEI-GEM-CaPA_run_1951-2100" # CanRCM4 corrected to WFDEI-GEM-CaPA in full for testing
    # "CanRCM4-WFDEI-GEM-CaPA_run_1951-2020"#, # CanRCM4 corrected to WFDEI-GEM-CaPA part 1 from MESH
    # "CanRCM4-WFDEI-GEM-CaPA_run_2020-2100" # CanRCM4 corrected to WFDEI-GEM-CaPA part 2 from MESH
)



num_stations <- read.table(paste0(input_dir,"MESH_input_streamflow.txt"), skip = 1, nrows = 1)[1, 1]

station_list <- read.table(paste0(input_dir,"MESH_input_streamflow.txt"), skip = 2,
                                  stringsAsFactors = FALSE, nrows = num_stations) %>%
    mutate(num = c(1:num_stations)) %>%
    select(num, V3) %>%
    rename(station = V3)


all_flows <- data.frame()

for (set in suffixes){
    if ( grepl("CanRCM", set) ){ run_list <- full_run_list }
    else { run_list <- "None" }
    
    all_runs <- data.frame()
    
    for ( run in run_list ){
        # substitutes the word run for the run r#i#p#r# to find the correct input file
        filename <- paste0(gsub("run",run,set),".csv")
        
        print(filename)
        
        flow <- read.csv( paste0( input_dir,"MESH_output_streamflow_",filename ) )
        if (colnames(flow)[2]=="DAY"){ old_mesh <- TRUE } else { old_mesh <- FALSE }
        
        if ( old_mesh ){ flow$DATE <- as.Date(flow$DAY,origin=paste0(flow$YEAR-1,"-12-31")) }
        # new MESH runs use JDAY instead of DAY
        else { flow$DATE <- as.Date(flow$JDAY,origin=paste0(flow$YEAR-1,"-12-31")) }
        
        flow[c("YEAR",names(flow)[grep("QO",names(flow))])] <- apply(flow[c("YEAR",names(flow)[grep("QO",names(flow))])],2,as.numeric)
        
        for (station in c(1:num_stations)){
            flow[paste0("QOMEAS",station)][flow[paste0("QOMEAS",station)] < 0] <- NA
            flow[paste0("QOSIM",station)][flow[paste0("QOSIM",station)] < 0] <- NA
            
            flow[paste0("QOMEAS",station)] <- flow[paste0("QOMEAS",station)] / 0.02831685
            flow[paste0("QOSIM",station)] <- flow[paste0("QOSIM",station)] / 0.02831685
            
            # flow[names(flow)[grep("QO",names(flow))]] <- flow[names(flow)[grep("QO",names(flow))]] / .02831685
            # flow[flow[names(flow)[grep("QO",names(flow))]] < 0] <- NA
        }
        
        # reads in non-blank columns
        flow <- flow[c("DATE","YEAR",names(flow)[grep("QO",names(flow))])] %>%
            mutate(MONTH = month(DATE)) %>%
            mutate(MDAY = day(DATE)) %>%
            # adds 1 to December's year for keeping winter seasons together
            mutate(SEASON_YEAR = YEAR+floor(MONTH/12)) %>%
            # denoting which months fit into each season for by-season calculations
            mutate(SEASON = case_when(MONTH %in% c(1,2,12) ~ "Winter",
                                      MONTH %in% c(3:5) ~ "Spring",
                                      MONTH %in% c(6:8) ~ "Summer",
                                      MONTH %in% c(9:11) ~ "Fall"))
        
        # sets the SET category depending on whether the set being read is corrected or not (-WFDEI-GEM-CaPA)
        if ( grepl("WFDEI-GEM-CaPA", set) ){
            if ( grepl("CanRCM4", set) ){ set_label <- set_list[3] }
            else { set_label <- set_list[1] }
        } else { set_label <- set_list[2] }
        
        # combining all data into a single dataframe
        run_data <- flow %>%
            melt(id.vars = c("DATE","SEASON","YEAR","SEASON_YEAR","MONTH","MDAY"), na.rm = TRUE) %>%
            mutate(RUN = run) %>%
            mutate(SET = set_label)
        
        # appends the data for each run
        all_runs <- rbind(all_runs,run_data)
    }
    
    # appends the data for each data set
    all_flows <- rbind(all_flows,all_runs)
}


# reading in MESH input streamflow for observed values
obs <- read.table(paste0(input_dir,"MESH_input_streamflow.txt"), skip = 2+num_stations,
                  stringsAsFactors = FALSE)
names(obs) <- station_list$station
obs[obs < 0] <- NA
obs <- (obs / 0.02831685) %>% mutate(row = c(1:dim(obs)[1]))

obs_start <- read.table(paste0(input_dir,"MESH_input_streamflow.txt"), skip = 1, nrows = 1,
                             stringsAsFactors = FALSE)
# obs_start <- as.Date(paste0(obs_start[5],"-"obs_start))
# $DATE <- as.Date(water$JDAY,origin=paste0(energy$YEAR-1,"-12-31"))

obs <- obs %>% mutate(DATE = as.Date(row, origin = (as.Date(paste0(obs_start[5],"-01-",obs_start[6])) - 1) )) %>%
    subset(select=c(-row)) %>% mutate(YEAR = year(DATE)) %>%
    mutate(MONTH = month(DATE)) %>%
    mutate(MDAY = day(DATE)) %>%
    # adds 1 to December's year for keeping winter seasons together
    mutate(SEASON_YEAR = YEAR+floor(MONTH/12)) %>%
    # denoting which months fit into each season for by-season calculations
    mutate(SEASON = case_when(MONTH %in% c(1,2,12) ~ "Winter",
                              MONTH %in% c(3:5) ~ "Spring",
                              MONTH %in% c(6:8) ~ "Summer",
                              MONTH %in% c(9:11) ~ "Fall")) %>%
    subset(DATE>="1980-01-01" & DATE<="2009-12-31") %>%
    melt(id.vars = c("DATE","SEASON","YEAR","SEASON_YEAR","MONTH","MDAY"), na.rm = TRUE) %>%
    mutate(RUN = "Observed") %>%
    mutate(SET = "Observed")

all_flows <- rbind(all_flows,obs)


# assigning type of measurement
all_flows$TYPE <- "Observed"
all_flows$TYPE[grepl("MEAS",all_flows$variable)] <- "Measured"
all_flows$TYPE[grepl("SIM",all_flows$variable)] <- "Simulated"

a <- all_flows # for testing

# setting up time periods
all_flows$PERIOD[all_flows$YEAR %in% c(1950:1980)] <- paste0(all_flows$SET[all_flows$YEAR %in% c(1950:1980)]," (1950-1980)")
all_flows$PERIOD[all_flows$YEAR %in% c(1980:2010)] <- paste0(all_flows$SET[all_flows$YEAR %in% c(1980:2010)]," (1980-2010)")
all_flows$PERIOD[all_flows$YEAR %in% c(2010:2040)] <- paste0(all_flows$SET[all_flows$YEAR %in% c(2010:2040)]," (2010-2040)")
all_flows$PERIOD[all_flows$YEAR %in% c(2040:2070)] <- paste0(all_flows$SET[all_flows$YEAR %in% c(2040:2070)]," (2040-2070)")
all_flows$PERIOD[all_flows$YEAR %in% c(2070:2100)] <- paste0(all_flows$SET[all_flows$YEAR %in% c(2070:2100)]," (2070-2100)")
# all_flows$PERIOD[all_flows$YEAR %in% c(2020:2044)] <- paste0(all_flows$SET[all_flows$YEAR %in% c(2020:2044)]," (2020-2044)")
# all_flows$PERIOD[all_flows$YEAR %in% c(2045:2069)] <- paste0(all_flows$SET[all_flows$YEAR %in% c(2045:2069)]," (2045-2069)")
# all_flows$PERIOD[all_flows$YEAR %in% c(2070:2100)] <- paste0(all_flows$SET[all_flows$YEAR %in% c(2070:2100)]," (2070-2100)")

all_flows$PERIOD[all_flows$YEAR %in% c(1995:2010)] <- paste0(all_flows$SET[all_flows$YEAR %in% c(1995:2010)]," (1995-2010)")

# separated out because of time period difference
daily_ribbons <- subset(all_flows,select = c(SET,TYPE,variable,PERIOD,MONTH,MDAY,value)) %>%
    group_by(SET,TYPE,variable,PERIOD,MONTH,MDAY) %>% mutate(DAILY_AVG = mean(value)) %>%
    mutate(DAILY_MAX = max(value)) %>% mutate(DAILY_MIN = min(value)) %>% ungroup()


# min-max shaded for ensembles
# spaghetti plot same colour
# plus station observation
# 30-year time slices; 1980-2010, 2010-2040, 2040-2070, 2070-2100





for (station in 1:num_stations){
    # # plot obs vs WFDEI-GEM-CaPA vs CanRCM4-WFDEI-GEM-CaPA
    # # filename <- paste0("Souris_MESH_1980-2010_Streamflow_",station_list$station[station])
    # filename <- paste0("Souris_MESH_1995-2010_Streamflow_",station_list$station[station])
    # plotname <- paste0("Simulated vs Observed MESH Output Streamflow for Gauge ",
    #                    station_list$station[station])
    # 
    # plot_data <- subset(daily_ribbons,(variable==paste0("QOSIM",station) | variable==station_list$station[station])
    #                     # & grepl("1980-2010",PERIOD))
    #                     & grepl("1995-2010",PERIOD))
    # plot_data$DATE <- as.Date(paste0("1980-",plot_data$MONTH,"-",plot_data$MDAY))
    # plot_cwgc <- subset(plot_data,SET=="CanRCM4-WFDEI-GEM-CaPA" & TYPE=="Simulated")
    # plot_wgc <- subset(plot_data,SET=="WFDEI-GEM-CaPA" & TYPE=="Simulated")
    # plot_obs <- subset(plot_data,TYPE=="Observed")
    # 
    # ggplot() +
    #     geom_ribbon(data = plot_cwgc,aes(x = DATE, ymin = DAILY_MIN, ymax = DAILY_MAX, fill = PERIOD),
    #                 alpha = 0.3) +
    #     geom_ribbon(data = plot_wgc,aes(x = DATE, ymin = DAILY_MIN, ymax = DAILY_MAX, fill = PERIOD),
    #                 alpha = 0.3) +
    #     geom_ribbon(data = plot_obs,aes(x = DATE, ymin = DAILY_MIN, ymax = DAILY_MAX, fill = PERIOD),
    #                 alpha = 0.3) +
    #     geom_line(data = plot_data,aes(x = DATE, y = DAILY_AVG, colour = PERIOD)) +
    #     scale_x_date(name = "Date", date_labels = "%b",date_breaks = "1 month",
    #                  expand = expansion(0,0), label=scales::comma) +
    #     scale_y_continuous(name = expression("Streamflow"~(ft^{3}/s)), label=scales::comma,
    #                        sec.axis = sec_axis(~ . * 0.02831685, name = expression("Streamflow"~(m^{3}/s)))) +
    #     labs(title = plotname, colour = "Mean", fill = "Time Period Max/Min") +
    #     # scale_colour_manual(values=c("CanRCM4-WFDEI-GEM-CaPA (1980-2010)" = isrsb_red, "WFDEI-GEM-CaPA (1980-2010)" = isrsb_yellow,
    #     #                              "Observed (1980-2010)" = isrsb_blue)) +
    #     scale_colour_manual(values=c("CanRCM4-WFDEI-GEM-CaPA (1995-2010)" = isrsb_red, "WFDEI-GEM-CaPA (1995-2010)" = isrsb_yellow,
    #                                  "Observed (1995-2010)" = isrsb_blue)) +
    #     # scale_fill_manual(guide = FALSE, values=c("CanRCM4-WFDEI-GEM-CaPA (1980-2010)" = isrsb_red,
    #     #                                           "WFDEI-GEM-CaPA (1980-2010)" = isrsb_yellow, "Observed (1980-2010)" = isrsb_blue)) +
    #     scale_fill_manual(guide = FALSE, values=c("CanRCM4-WFDEI-GEM-CaPA (1995-2010)" = isrsb_red,
    #                                               "WFDEI-GEM-CaPA (1995-2010)" = isrsb_yellow, "Observed (1995-2010)" = isrsb_blue)) +
    #     theme_bw() + theme(legend.position = "bottom", legend.title.align = 0.5, legend.direction = "vertical",
    #                        plot.title = element_text(hjust = 0.5))
    # 
    # ggsave(paste0(output_dir,filename,".png"), width = 9, height = 6)
    
    
    
    # plot obs vs WFDEI
    filename <- paste0("Souris_MESH_WFDEI-GEM-CaPA_1980-2010_Streamflow_",station_list$station[station])
    plotname <- paste0("Simulated WFDEI-GEM-CaPA vs Observed MESH Output Streamflow for Gauge ",
                       station_list$station[station])

    plot_data <- subset(daily_ribbons,PERIOD=="WFDEI-GEM-CaPA (1980-2010)" & (variable==paste0("QOMEAS",station)
                                                               | variable==paste0("QOSIM",station)))
    plot_data$DATE <- as.Date(paste0("1980-",plot_data$MONTH,"-",plot_data$MDAY))
    plot_sim <- subset(plot_data,TYPE=="Simulated")
    plot_meas <- subset(plot_data,TYPE=="Measured")

    ggplot() +
        geom_ribbon(data = plot_sim,aes(x = DATE, ymin = DAILY_MIN, ymax = DAILY_MAX, fill = "Simulated"),
                    alpha = 0.5) +
        geom_ribbon(data = plot_meas,aes(x = DATE, ymin = DAILY_MIN, ymax = DAILY_MAX, fill = "Measured"),
                    alpha = 0.5) +
        geom_line(data = plot_data,aes(x = DATE, y = DAILY_AVG, colour = TYPE)) +
        scale_x_date(name = "Date", date_labels = "%b",date_breaks = "1 month",
                     expand = expansion(0,0), label=scales::comma) +
        scale_y_continuous(name = expression("Streamflow"~(ft^{3}/s)), label=scales::comma,
                           sec.axis = sec_axis(~ . * 0.02831685, name = expression("Streamflow"~(m^{3}/s)))) +
        labs(title = plotname, colour = "Time Period Averages", fill = "Time Period Max/Min") +
        scale_colour_manual(values=c("Simulated" = isrsb_green, "Measured" = isrsb_blue)) +
        scale_fill_manual(values=c("Simulated" = isrsb_green, "Measured" = isrsb_blue)) +
        theme_bw() + theme(legend.position = "bottom", legend.title.align = 0.5, legend.direction = "vertical",
                           plot.title = element_text(hjust = 0.5))

    ggsave(paste0(output_dir,filename,".png"), width = 9, height = 6)
    
    
    
    # # plot obs vs CanRCM4-WFDEI-GEM-CaPA
    # filename <- paste0("Souris_MESH_CanRCM4-WFDEI-GEM-CaPA_1980-2100_Sim_Streamflow_",station_list$station[station])
    # plotname <- paste0("Simulated CanRCM4-WFDEI-GEM-CaPA MESH Output Streamflow for Gauge ",
    #                    station_list$station[station])
    # 
    # plot_data <- subset(daily_ribbons,SET=="CanRCM4-WFDEI-GEM-CaPA" & PERIOD!="1950-1980"
    #                                   & (variable==paste0("QOSIM",station)
    #                                      | variable==paste0("QOMEAS",station)) )
    # plot_data$DATE <- as.Date(paste0("2020-",plot_data$MONTH,"-",plot_data$MDAY))
    # line_data <- subset(plot_data,TYPE=="Simulated")
    # # plotobs <- subset(plot_data,TYPE=="Measured" & PERIOD=="1980-2010")
    # plot80_10 <- subset(plot_data,TYPE=="Simulated" & PERIOD=="1980-2010")
    # plot10_40 <- subset(plot_data,TYPE=="Simulated" & PERIOD=="2010-2040")
    # plot40_70 <- subset(plot_data,TYPE=="Simulated" & PERIOD=="2040-2070")
    # plot70_00 <- subset(plot_data,TYPE=="Simulated" & PERIOD=="2070-2100")
    # 
    # ggplot() +
    #     geom_ribbon(data = plot80_10,aes(x = DATE, ymin = DAILY_MIN, ymax = DAILY_MAX, fill = "1980-2010"),
    #                 alpha = 0.3) +
    #     geom_ribbon(data = plot10_40,aes(x = DATE, ymin = DAILY_MIN, ymax = DAILY_MAX, fill = "2010-2040"),
    #                 alpha = 0.3) +
    #     geom_ribbon(data = plot40_70,aes(x = DATE, ymin = DAILY_MIN, ymax = DAILY_MAX, fill = "2040-2070"),
    #                 alpha = 0.3) +
    #     geom_ribbon(data = plot70_00,aes(x = DATE, ymin = DAILY_MIN, ymax = DAILY_MAX, fill = "2070-2100"),
    #                 alpha = 0.3) +
    #     geom_line(data = line_data,aes(x = DATE, y = DAILY_AVG, colour = PERIOD)) +
    #     # geom_line(data = plotobs,aes(x = DATE, y = DAILY_AVG, colour = "Observed")) +
    #     scale_x_date(name = "Date", date_labels = "%b",date_breaks = "1 month",
    #                  expand = expansion(0,0), label=scales::comma) +
    #     scale_y_continuous(name = expression("Streamflow"~(ft^{3}/s)), label=scales::comma,
    #                        sec.axis = sec_axis(~ . * 0.02831685, name = expression("Streamflow"~(m^{3}/s)))) +
    #     labs(title = plotname, colour = "Time Period Averages", fill = "Time Period Max/Min") +
    #     scale_colour_manual(values=c("1980-2010" = isrsb_blue, "2010-2040" = isrsb_green,
    #                            "2040-2070" = isrsb_orange, "2070-2100" = isrsb_grey#,
    #                            # "Observed" = isrsb_black
    #                         )) +
    #     scale_fill_manual(values=c("1980-2010" = isrsb_blue, "2010-2040" = isrsb_green,
    #                            "2040-2070" = isrsb_orange, "2070-2100" = isrsb_grey#,
    #                            # "Observed" = isrsb_black
    #                       )) +
    #     theme_bw() + theme(legend.position = "bottom", legend.title.align = 0.5, legend.direction = "vertical",
    #                        plot.title = element_text(hjust = 0.5))
    # 
    # ggsave(paste0(output_dir,filename,".png"), width = 9, height = 6)
    
    
    
    # # plot obs vs CanRCM4-WFDEI-GEM-CaPA (for 2020-2100 runs only)
    # filename <- paste0("Souris_MESH_CanRCM4-WFDEI-GEM-CaPA_2020-2100_Sim_Streamflow_",station_list$station[station])
    # plotname <- paste0("Simulated CanRCM4-WFDEI-GEM-CaPA MESH Output Streamflow for Gauge ",
    #                    station_list$station[station])
    # 
    # plot_data <- subset(daily_ribbons,(SET=="CanRCM4-WFDEI-GEM-CaPA" | is.na(SET)) & !is.na(PERIOD)
    #                                   & (variable==paste0("QOSIM",station)
    #                                      | variable==station_list$station[station]) )
    # plot_data$DATE <- as.Date(paste0("2020-",plot_data$MONTH,"-",plot_data$MDAY))
    # line_data <- subset(plot_data,TYPE=="Simulated")
    # plotobs <- subset(plot_data,TYPE=="Observed" & PERIOD=="1980-2010")
    # plot20_44 <- subset(plot_data,TYPE=="Simulated" & PERIOD=="2020-2044")
    # plot45_69 <- subset(plot_data,TYPE=="Simulated" & PERIOD=="2045-2069")
    # plot70_00 <- subset(plot_data,TYPE=="Simulated" & PERIOD=="2070-2100")
    # 
    # ggplot() +
    #     geom_ribbon(data = plotobs,aes(x = DATE, ymin = DAILY_MIN, ymax = DAILY_MAX, fill = "1980-2010"),
    #                 alpha = 0.5) +
    #     geom_ribbon(data = plot20_44,aes(x = DATE, ymin = DAILY_MIN, ymax = DAILY_MAX, fill = "2020-2044"),
    #                 alpha = 0.3) +
    #     geom_ribbon(data = plot45_69,aes(x = DATE, ymin = DAILY_MIN, ymax = DAILY_MAX, fill = "2045-2069"),
    #                 alpha = 0.3) +
    #     geom_ribbon(data = plot70_00,aes(x = DATE, ymin = DAILY_MIN, ymax = DAILY_MAX, fill = "2070-2100"),
    #                 alpha = 0.3) +
    #     geom_line(data = line_data,aes(x = DATE, y = DAILY_AVG, colour = PERIOD)) +
    #     geom_line(data = plotobs,aes(x = DATE, y = DAILY_AVG, colour = PERIOD)) +
    #     scale_x_date(name = "Date", date_labels = "%b",date_breaks = "1 month",
    #                  expand = expansion(0,0), label=scales::comma) +
    #     scale_y_continuous(name = expression("Streamflow"~(ft^{3}/s)), label=scales::comma,
    #                        sec.axis = sec_axis(~ . * 0.02831685, name = expression("Streamflow"~(m^{3}/s)))) +
    #     labs(title = plotname, colour = "Time Period Averages", fill = "Time Period Max/Min") +
    #     scale_colour_manual(values=c("1980-2010" = isrsb_blue, "2020-2044" = isrsb_green,
    #                                  "2045-2069" = isrsb_orange, "2070-2100" = isrsb_grey)) +
    #     scale_fill_manual(values=c("1980-2010" = isrsb_blue, "2020-2044" = isrsb_green,
    #                                "2045-2069" = isrsb_orange, "2070-2100" = isrsb_grey)) +
    #     theme_bw() + theme(legend.position = "bottom", legend.title.align = 0.5, legend.direction = "vertical",
    #                        plot.title = element_text(hjust = 0.5))
    # 
    # ggsave(paste0(output_dir,filename,".png"), width = 9, height = 6)
    
    
    
    # # plot obs vs CanRCM
    # filename <- paste0("Souris_MESH_CanRCM4_1950-2100_Streamflow_",station_list$station[station])
    # plotname <- paste0("Simulated CanRCM4 MESH Output Streamflow for Gauge ",
    #                    station_list$station[station])
    # 
    # plot_data <- subset(daily_ribbons,SET=="CanRCM4" & (variable==paste0("QOSIM",station)
    #                                                     | variable==paste0("QOMEAS",station)) )
    # plot_data$DATE <- as.Date(paste0("2020-",plot_data$MONTH,"-",plot_data$MDAY))
    # line_data <- subset(plot_data,TYPE=="Simulated")
    # plotobs <- subset(plot_data,TYPE=="Measured" & PERIOD=="1980-2010")
    # plot50_80 <- subset(plot_data,TYPE=="Simulated" & PERIOD=="1950-1980")
    # plot80_10 <- subset(plot_data,TYPE=="Simulated" & PERIOD=="1980-2010")
    # plot10_40 <- subset(plot_data,TYPE=="Simulated" & PERIOD=="2010-2040")
    # plot40_70 <- subset(plot_data,TYPE=="Simulated" & PERIOD=="2040-2070")
    # plot70_00 <- subset(plot_data,TYPE=="Simulated" & PERIOD=="2070-2100")
    # 
    # ggplot() +
    #     geom_ribbon(data = plotobs,aes(x = DATE, ymin = DAILY_MIN, ymax = DAILY_MAX, fill = "Observed"),
    #                 alpha = 0.5) +
    #     geom_ribbon(data = plot50_80,aes(x = DATE, ymin = DAILY_MIN, ymax = DAILY_MAX, fill = "1950-1980"),
    #                 alpha = 0.3) +
    #     geom_ribbon(data = plot80_10,aes(x = DATE, ymin = DAILY_MIN, ymax = DAILY_MAX, fill = "1980-2010"),
    #                 alpha = 0.3) +
    #     geom_ribbon(data = plot10_40,aes(x = DATE, ymin = DAILY_MIN, ymax = DAILY_MAX, fill = "2010-2040"),
    #                 alpha = 0.3) +
    #     geom_ribbon(data = plot40_70,aes(x = DATE, ymin = DAILY_MIN, ymax = DAILY_MAX, fill = "2040-2070"),
    #                 alpha = 0.3) +
    #     geom_ribbon(data = plot70_00,aes(x = DATE, ymin = DAILY_MIN, ymax = DAILY_MAX, fill = "2070-2100"),
    #                 alpha = 0.3) +
    #     geom_line(data = line_data,aes(x = DATE, y = DAILY_AVG, colour = PERIOD)) +
    #     geom_line(data = plotobs,aes(x = DATE, y = DAILY_AVG, colour = "Observed")) +
    #     scale_x_date(name = "Date", date_labels = "%b",date_breaks = "1 month",
    #                  expand = expansion(0,0), label=scales::comma) +
    #     scale_y_continuous(name = expression("Streamflow"~(ft^{3}/s)), label=scales::comma,
    #                        sec.axis = sec_axis(~ . * 0.02831685, name = expression("Streamflow"~(m^{3}/s)))) +
    #     labs(title = plotname, colour = "Time Period Averages", fill = "Time Period Max/Min") +
    #     scale_colour_manual(values=c("1950-1980" = isrsb_red, "1980-2010" = isrsb_blue,
    #                                  "2010-2040" = isrsb_green, "2040-2070" = isrsb_orange,
    #                                  "2070-2100" = isrsb_grey, "Observed" = isrsb_black)) +
    #     scale_fill_manual(values=c("1950-1980" = isrsb_red, "1980-2010" = isrsb_blue,
    #                                "2010-2040" = isrsb_green, "2040-2070" = isrsb_orange,
    #                                "2070-2100" = isrsb_grey, "Observed" = isrsb_black)) +
    #     theme_bw() + theme(legend.position = "bottom", legend.title.align = 0.5, legend.direction = "vertical",
    #                        plot.title = element_text(hjust = 0.5))
    # 
    # ggsave(paste0(output_dir,filename,".png"), width = 9, height = 6)
    
    
    
    # # plot obs vs CanRCM
    # filename <- paste0("Souris_MESH_CanRCM4_1950-2100_Streamflow_",station_list$station[station])
    # plotname <- paste0("Simulated CanRCM4 MESH Output Streamflow for Gauge ",
    #                    station_list$station[station])
    # 
    # plot_data <- subset(daily_ribbons,(grepl("CanRCM",SET) | is.na(SET)) & TYPE!="Observed"
    #                     & (variable==paste0("QOSIM",station)
    #                        | variable==paste0("QOMEAS",station)) )
    # plot_data$DATE <- as.Date(paste0("2020-",plot_data$MONTH,"-",plot_data$MDAY))
    # line_data <- subset(plot_data,TYPE=="Simulated")
    # plotobs <- subset(plot_data,TYPE=="Measured" & PERIOD=="1980-2010")
    # plot50_80 <- subset(plot_data,TYPE=="Simulated" & PERIOD=="1950-1980")
    # plot80_10 <- subset(plot_data,TYPE=="Simulated" & PERIOD=="1980-2010")
    # plot10_40 <- subset(plot_data,TYPE=="Simulated" & PERIOD=="2010-2040")
    # plot40_70 <- subset(plot_data,TYPE=="Simulated" & PERIOD=="2040-2070")
    # plot70_00 <- subset(plot_data,TYPE=="Simulated" & PERIOD=="2070-2100")
    # 
    # ggplot() +
    #     geom_ribbon(data = plotobs,aes(x = DATE, ymin = DAILY_MIN, ymax = DAILY_MAX, fill = "Observed"),
    #                 alpha = 0.5) +
    #     geom_ribbon(data = plot50_80,aes(x = DATE, ymin = DAILY_MIN, ymax = DAILY_MAX, fill = "1950-1980"),
    #                 alpha = 0.3) +
    #     geom_ribbon(data = plot80_10,aes(x = DATE, ymin = DAILY_MIN, ymax = DAILY_MAX, fill = "1980-2010"),
    #                 alpha = 0.3) +
    #     geom_ribbon(data = plot10_40,aes(x = DATE, ymin = DAILY_MIN, ymax = DAILY_MAX, fill = "2010-2040"),
    #                 alpha = 0.3) +
    #     geom_ribbon(data = plot40_70,aes(x = DATE, ymin = DAILY_MIN, ymax = DAILY_MAX, fill = "2040-2070"),
    #                 alpha = 0.3) +
    #     geom_ribbon(data = plot70_00,aes(x = DATE, ymin = DAILY_MIN, ymax = DAILY_MAX, fill = "2070-2100"),
    #                 alpha = 0.3) +
    #     geom_line(data = line_data,aes(x = DATE, y = DAILY_AVG, colour = PERIOD)) +
    #     geom_line(data = plotobs,aes(x = DATE, y = DAILY_AVG, colour = "Observed")) +
    #     scale_x_date(name = "Date", date_labels = "%b",date_breaks = "1 month",
    #                  expand = expansion(0,0), label=scales::comma) +
    #     scale_y_continuous(name = expression("Streamflow"~(ft^{3}/s)), label=scales::comma,
    #                        sec.axis = sec_axis(~ . * 0.02831685, name = expression("Streamflow"~(m^{3}/s)))) +
    #     labs(title = plotname, colour = "Time Period Averages", fill = "Time Period Max/Min") +
    #     scale_colour_manual(values=c("1950-1980" = isrsb_red, "1980-2010" = isrsb_blue,
    #                                  "2010-2040" = isrsb_green, "2040-2070" = isrsb_orange,
    #                                  "2070-2100" = isrsb_grey, "Observed" = isrsb_black)) +
    #     scale_fill_manual(values=c("1950-1980" = isrsb_red, "1980-2010" = isrsb_blue,
    #                                "2010-2040" = isrsb_green, "2040-2070" = isrsb_orange,
    #                                "2070-2100" = isrsb_grey, "Observed" = isrsb_black)) +
    #     theme_bw() + theme(legend.position = "bottom", legend.title.align = 0.5, legend.direction = "vertical",
    #                        plot.title = element_text(hjust = 0.5))
    # 
    # ggsave(paste0(output_dir,filename,".png"), width = 9, height = 6)
    
    
    
    # plot_label <- "Streamflow"
    # 
    # filename <- paste0("Souris_MESH_CanRCM-WFDEI-GEM-CaPA_2020-2100_Streamflow_",station)
    # plotname <- paste0("MESH Output ",plot_label)
    # 
    # 
    # plot_data <- subset(all_flows,select = c(DATE,paste0("QOSIM",station)))
    # 
    # ggplot(all_flows,aes(DATE,value)) + geom_line(aes(colour=SET)) +
    #     scale_x_date(name = "Date", date_labels = "%Y",date_breaks = "5 years") +
    #     scale_y_continuous(name = ycalc, label=scales::comma) +
    #     labs(title = plotname) +
    #     scale_colour_manual(values=c("CanRCM4" = isrsb_orange, "CanRCM4-WFDEI-GEM-CaPA" = isrsb_red)) +
    #     theme_bw() + theme(legend.position = "bottom", legend.title = element_blank(),
    #                        plot.title = element_text(hjust = 0.5))
    # 
    # ggsave(paste0(output_dir,filename,".png"), width = 7, height = 5)
}
