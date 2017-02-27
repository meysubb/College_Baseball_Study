### Advanced Sabermetrics 
### wOBA

Raw_sabr <- player_hitting %>% select(player_name,team_name,BA,AB,R,H,X1B,X2B,X3B,HR,BB,HBP,SF)
Raw_sabr <- subset(Raw_sabr,!is.na(AB))

Raw_sabr$wOBA <- with(Raw_sabr, (.69*BB + .72*HBP + .89*X1B + 1.27*X2B + 1.62*X3B + 2.1*HR)/(AB+BB+SF+HBP))

div_list <- unique(team_div_df$Division)
wOBA_conf_avg <- matrix(ncol=2,nrow=length(div_list))
wOBA_conf_avg[,1] <- div_list
wOBA_conf_avg <- data.frame(wOBA_conf_avg,stringsAsFactors = FALSE)
wOBA_conf_avg$X2 <- as.numeric(wOBA_conf_avg$X2)
for(j in 1:length(div_list)){
  teams <- subset(team_div_df,Division == div_list[j])
  Div_sabr <- Raw_sabr %>% select(player_name,team_name,BA,AB,wOBA) %>% 
    filter(team_name %in% teams$Team_Names) %>% 
    filter(AB > 10)
  wOBA_conf_avg[j,2] <- mean(Div_sabr$wOBA)
}

TAMU_sabr <- Raw_sabr %>% select(player_name,team_name,wOBA,AB,BA) %>%
  filter(team_name %in% c("Texas A&M")) %>% 
  filter(AB > 10)

Raw_Plot <- Raw_sabr %>% select(player_name,team_name,wOBA,AB) %>%
  filter(AB>10)

ggplot(data=Raw_Plot,aes(x=AB,y=wOBA,color = team_name == "Texas A&M",label=player_name)) + 
  geom_point(alpha=0.5) + 
  geom_hline(yintercept = mean(Raw_Plot$wOBA)) +
  theme(legend.position="none") +
  scale_colour_manual(values = setNames(c('maroon','grey'),c(T, F))) + 
  geom_text(data=subset(Raw_Plot,team_name == "Texas A&M" & AB < 150  & wOBA > 0.4),vjust=-1,hjust=0.25) +
  geom_text(data=subset(Raw_Plot,team_name == "Texas A&M" & AB > 200  & wOBA > 0.4),vjust=-1,hjust=0.25) + 
  geom_text(data=subset(Raw_Plot, AB > 200  & wOBA > 0.51),vjust=-1,hjust=1)

### Pie Chart 
### Compare Total vs. Texas A&M 

pit_data <- hitting_table %>% select(team_name,team_H,team_2B,team_3B,team_HR,team_SF,team_BB,team_HBP)
pit_data$team_1B <- with(pit_data,team_H - (team_2B + team_3B + team_HR))
## Calculate NCAA Average 
pit_data[301,1] <- "NCAA Average"
pit_data[301,2:9] <- apply(pit_data[2:9],2,function(X) mean(X,na.rm=TRUE))
## Calculate SEC Average
temp_SEC <- pit_data %>% filter(team_name %in% SEC$Team_Names)
temp_SEC[15,1] <- "SEC Average"
temp_SEC[15,2:9] <- apply(temp_SEC[2:9],2,function(X) mean(X,na.rm=TRUE))
pit_data[302,] <- temp_SEC[15,]
## Calculate Top 25 Average 
temp_T25 <- pit_data %>% filter(team_name %in% top_25_names$SCHOOL)
temp_T25[26,1] <- "Top 25 Average"
temp_T25[26,2:9] <- apply(temp_T25[2:9],2,function(X) mean(X,na.rm=TRUE))
pit_data[303,] <- temp_T25[26,]

pit_plot <- pit_data %>% select(team_name,team_1B,team_2B,team_3B,team_HR,team_BB,team_HBP) %>%
  filter(team_name == "Texas A&M" | team_name == "NCAA Average" | team_name == "Top 25 Average" | team_name == "SEC Average")
pit_plot$total <- apply(pit_plot[,2:7],1,sum) 
for(k in 1:nrow(pit_plot)){
  pit_plot[k,2:7] <- pit_plot[k,2:7] / pit_plot[k,8]
}
pit_plot <- pit_plot %>% select(team_name,team_1B,team_2B,team_3B,team_HR,team_BB,team_HBP) 
colnames(pit_plot) <- c("team_name","Single","Double","Triple","Home Run","Walks","Hit By Pitch")
pit_plot <- melt(pit_plot,id.vars="team_name")
pit_plot <- pit_plot %>% group_by(team_name) %>% mutate(pos=cumsum(value)-0.5*value)
pit_plot$value <- 100 * round(pit_plot$value,digits=3) 


ggplot(data=pit_plot,aes(x=factor(1),y=value,fill=factor(variable))) + 
  geom_bar(stat="identity",position="fill") + 
  facet_wrap(facets= ~ team_name) + 
  geom_text(aes(x = factor(1), y = pos, label = value)) +
  coord_polar(theta="y") + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank()) + 
  labs(x="",y="",fill = "Hit Type") 

#+geom_text(data=pit_plot,aes(y=value),label=value)

## BABIP

Raw_sabr_1 <- player_hitting %>% select(player_name,team_name,BA,AB,R,H,HR,SF,K,played,RBI)
Raw_sabr_1 <- subset(Raw_sabr_1,!is.na(AB))

Raw_sabr_1$BABIP <- with(Raw_sabr_1, (H-HR)/(AB-K-HR+SF))
Raw_sabr_1$RPG <- with(Raw_sabr_1,R/played)
Raw_sabr_1$RBIG <- with(Raw_sabr_1,RBI/played)

SEC_data <- Raw_sabr_1 %>% filter(team_name %in% SEC$Team_Names) %>% filter(AB >25)
T25_data <- Raw_sabr_1 %>% filter(team_name %in% top_25_names$SCHOOL) %>% filter(AB >25)
avg_BABIP <- mean(Raw_sabr_1$BABIP,na.rm=TRUE)
SEC_BABIP <- mean(SEC_data$BABIP)
T25_BABIP <- mean(T25_data$BABIP)
sd_BABIP <- sd(Raw_sabr_1$BABIP,na.rm=TRUE)
sd_T25_BABIP <- sd(T25_data$BABIP)

plot_data <- Raw_sabr_1 %>% select(player_name,team_name,BABIP,RBIG,AB) %>%
  filter(AB>10)

plot_data_mlb <- plot_data[which(plot_data$player_name %in% mlb_draft$Name),]

avg_plot_BABIP <- mean(plot_data_mlb$BABIP)
sd_plot_BABIP <- sd(plot_data_mlb$BABIP)

ggplot(data=plot_data_mlb,aes(x=AB,y=BABIP,color = team_name == "Texas A&M",label=player_name)) + 
  geom_point(alpha=0.5) + 
  theme(legend.position="none") +
  scale_colour_manual(values = setNames(c('maroon','grey'),c(T, F))) + 
  geom_hline(yintercept = avg_plot_BABIP) + 
  #geom_hline(yintercept = SEC_BABIP, color="blue") + 
  geom_hline(yintercept = avg_plot_BABIP + sd_plot_BABIP, linetype = "dashed") + 
  geom_hline(yintercept = avg_plot_BABIP - sd_plot_BABIP, linetype = "dashed") + 
  #geom_hline(yintercept = T25_BABIP, color = "orange") + 
  geom_text(data=subset(plot_data_mlb,BABIP > 0.45 & AB > 200 ),vjust=-1,hjust=0.25) +
  geom_text(data=subset(plot_data_mlb,AB>250 & BABIP>0.4),vjust=-1,hjust=0.25) +
  geom_text(data=subset(plot_data_mlb,AB>205 & BABIP>0.42),vjust=-1,hjust=0.25) +
  geom_text(data=subset(plot_data_mlb,team_name == "Texas A&M" & AB>250),vjust=-1,hjust=0.25)

avg_p_BABIP <- mean(plot_data$BABIP)
sd_p_BABIP <- sd(plot_data$BABIP)


ggplot(data=plot_data,aes(x=AB,y=BABIP,color = team_name == "Texas A&M",label=player_name)) + 
  geom_point(alpha=0.5) + 
  theme(legend.position="none") +
  scale_colour_manual(values = setNames(c('maroon','grey'),c(T, F))) +
  geom_hline(yintercept = avg_p_BABIP) + 
  #geom_hline(yintercept = SEC_BABIP, color="blue") + 
  geom_hline(yintercept = avg_p_BABIP + sd_p_BABIP, linetype = "dashed") + 
  geom_hline(yintercept = avg_p_BABIP - sd_p_BABIP, linetype = "dashed") +
  geom_text(data=subset(plot_data,team_name == "Texas A&M" & BABIP>0.39),vjust=-1,hjust=0.25)
  

### Team_SABR
### wRAA 
div_list <- as.data.frame(unique(team_div_df$Division),stringsAsFactors = FALSE)
colnames(div_list) <- "Division"
for(z in 1:nrow(div_list)) {
  TM <- subset(team_div_df,team_div_df$Division == div_list$Division[z])
  TM_subset <- subset(Raw_sabr,Raw_sabr$team_name %in% TM$Team_Names) %>% filter(AB>20)
  div_list$division_wOBA[z] <- mean(TM_subset$wOBA)
}
Raw_sabr <- Raw_sabr %>% filter(team_name %in% team_div_df$Team_Names)
rownames(Raw_sabr) <- seq(1:nrow(Raw_sabr))
Raw_sabr$Conference <- "Test"
for(x in 1:nrow(Raw_sabr)){
  index <- which(Raw_sabr$team_name[x] == team_div_df$Team_Names)
  Raw_sabr$Conference[x] <- team_div_df$Division[index]
}

wOBA_scale <- 1.227
Raw_sabr$wRAA <- 0
for(v in 1:nrow(Raw_sabr)){
  y <- div_list$division_wOBA[which(Raw_sabr$Conference[1]==div_list$Division)]
  Raw_sabr$wRAA[v] <- ((Raw_sabr$wOBA[v] - y)/wOBA_scale) * Raw_sabr$AB[v]
}

show_data <- Raw_sabr %>% filter(wRAA > 10 | wRAA < -10)
show_data <- show_data %>% select(player_name,team_name, AB,BA,wOBA,wRAA)

mlb_data <- show_data[which(show_data$player_name %in% mlb_draft$Name),]
mlb_order <- mlb_data[order(mlb_data$wRAA,decreasing = TRUE),]

top_half_data <- show_data %>% filter(wRAA > 25)
top_half_data <- top_half_data[order(top_half_data$wRAA,decreasing = TRUE),]
bottom_half_data <- show_data %>% filter(wRAA < -12)
bottom_half_data <- bottom_half_data[order(bottom_half_data$wRAA,decreasing = FALSE),]

mlb_order_top_half <- mlb_order %>% filter(wRAA > 20)
mlb_order_bottom_half <- mlb_order %>% filter(wRAA < 20)

library(xtable)
print(xtable(top_half_data),include.rownames = FALSE)
print(xtable(bottom_half_data),include.rownames = FALSE)
print(xtable(mlb_order_top_half),include.rownames=FALSE)
print(xtable(mlb_order_bottom_half),include.rownames=FALSE)
### Remove Data
rm(Raw_sabr,TAMU_sabr,Raw_Plot,pit_data,temp_SEC,temp_T25,pit_plot,Raw_sabr_1,SEC_data,T25_data,plot_data,wOBA_conf_avg,Div_sabr)
rm(div_list,TM,TM_subset)