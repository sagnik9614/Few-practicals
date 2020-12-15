theme_set(theme_minimal())

df<-read.csv("SpotifyFeatures.csv", header = T)
df<-as_tibble(df)
df<-distinct(df, track_id, .keep_all = T)
df$X<-seq.int(1, 19376, 1)
head(df)
df_reg<-df

features<-colnames(df)
sapply(df, class)

png(filename = "Figure 1.png", width = 800, height = 600)
grid.arrange(df%>%ggplot(aes(x=popularity))+geom_histogram(fill = "light blue", binwidth = 1, center = 0)+labs(title = "Histogram of popularity scores"), bottom = "Figure 1")
dev.off()

#There are too many songs which has 0 popularity score
#Changing the popularity score from continuous to 4 categories based on quartile values
for (i in 1:nrow(df))
{
  if (df$popularity[i]<=quantile(df$popularity, 0.25))
    df$pop_class[i]<-"unpopular"
  else if (df$popularity[i]<=quantile(df$popularity, 0.5) && df$popularity[i]>quantile(df$popularity>0.25))
    df$pop_class[i]<-"moderatepopular"
  else if (df$popularity[i]<=quantile(df$popularity, 0.75) && df$popularity[i]>quantile(df$popularity>0.5))
    df$pop_class[i]<-"popular"
  else if (df$popularity[i]>quantile(df$popularity, 0.75))
    df$pop_class[i]<-"verypopular"
}

rm(i)

df$pop_class<-factor(df$pop_class, levels = c("unpopular", "moderatepopular", "popular", "verypopular"))

genre.pop<-table(df$genre, df$pop_class)%>%as.data.frame()%>%as_tibble()
colnames(genre.pop)[1:2]<-c("Genre", "Popularity")
key.pop<-table(df$key, df$pop_class)%>%as.data.frame()%>%as_tibble()
colnames(key.pop)[1:2]<-c("Key", "Popularity")
time_sign.pop<-table(df$time_signature, df$pop_class)%>%as.data.frame()%>%as_tibble()
colnames(time_sign.pop)[1:2]<-c("Time_signature", "Popularity")
mode.pop<-table(df$mode, df$pop_class)%>%as.data.frame()%>%as_tibble()
colnames(mode.pop)[1:2]<-c("Mode", "Popularity")

keyvsmode<-chisq.test(df$mode, df$key)

lay<-matrix(c(1,2,3,4,5,NA), byrow = T, nrow = 2)

png(filename = "Figure 2.png", width = 1200, height = 600)
grid_arrange_shared_legend(df%>%ggplot()+geom_bar(aes(x=pop_class, fill = pop_class, col = pop_class))+ggtitle("Popularity Class plot")+labs(tag = "A"),
             genre.pop%>%ggplot(aes(x=Genre, y=Freq, fill = Popularity))+geom_col(position = "dodge")+
               ggtitle("Genre-wise popularity class plot")+theme(axis.text.x = element_blank())+labs(tag = "B"),
             key.pop%>%ggplot(aes(x=Key, y=Freq, fill = Popularity))+geom_col(position = "dodge")+ggtitle("Key-wise popularity class plot")+labs(tag = "C"),
             time_sign.pop%>%ggplot(aes(x=Time_signature, y=Freq, fill = Popularity))+geom_col(position = "dodge")+
               ggtitle("Time_signature-wise popularity class plot")+labs(tag = "D"),
             mode.pop%>%ggplot(aes(x=Mode, y=Freq, fill = Popularity))+geom_col(position = "dodge")+
             ggtitle("Mode-wise popularity class plot")+labs(tag = "E"), nrow=2, layout_matrix=lay, bottom = "Figure 2")
dev.off()

#checking the distribution of popularity score
lay<-matrix(c(1,2,3,4,5,6,7,8,NA,9,10,NA), nrow = 3, byrow = T)
png(filename = "Figure 3.png", width = 1600, height = 800)
grid_arrange_shared_legend(df%>%ggplot(aes(x=acousticness, col = pop_class, fill = pop_class))+geom_density(alpha = 0.09)+labs(tag="A"),
             df%>%ggplot(aes(x=danceability, col = pop_class, fill = pop_class))+geom_density(alpha = 0.09)+labs(tag="B"),
             df%>%ggplot(aes(x=duration_ms, col = pop_class, fill = pop_class))+geom_density(alpha = 0.09)+labs(tag="C"),
             df%>%ggplot(aes(x=energy, col = pop_class, fill =pop_class))+geom_density(alpha = 0.09)+labs(tag="D"),
             df%>%ggplot(aes(x=instrumentalness, col = pop_class, fill = pop_class))+geom_density(alpha = 0.09)+labs(tag="E"),
             df%>%ggplot(aes(x=liveness, col = pop_class, fill = pop_class))+geom_density(alpha = 0.09)+labs(tag="F"),
             df%>%ggplot(aes(x=loudness, col = pop_class, fill = pop_class))+geom_density(alpha = 0.09)+labs(tag="G"),
             df%>%ggplot(aes(x=speechiness, col = pop_class, fill = pop_class))+geom_density(alpha = 0.09)+labs(tag="H"),
             df%>%ggplot(aes(x=tempo, col = pop_class, fill = pop_class))+geom_density(alpha = 0.09)+labs(tag="I"),
             df%>%ggplot(aes(x=valence, col = pop_class, fill = pop_class))+geom_density(alpha = 0.09)+labs(tag="J"), nrow = 3, 
             top = "Density plots of continuous features grouped by popularity class", bottom = "Figure 3", 
             layout_matrix = lay)
dev.off()


cormat<-df[,features[c(7,8,9,10,11,13,14,16,17,19)]]%>%cor()%>%t()%>%as.table()%>%as.data.frame()%>%as_tibble()
png(filename = "Figure 4.png", width = 1000, height = 800)
grid.arrange(cormat%>%ggplot()+geom_tile(aes(x=Var1, y=Var2, fill = Freq))+scale_fill_gradient2(low = "blue", high = "red")+
               labs(title = "Correlation Plot", x = "Continuous features", y = "Continuous features", fill = "Correlation")+
               scale_x_discrete(limits = rev(levels(cormat$Var1)))+theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1)), bottom = "Figure 4")
dev.off()

opinion<-"There are a huge number of songs compared to the rest, which have popularity score zero 
so divided the popularity in 4 class based on quartiles .
Also mode, time_signature, instrumentalness, speechiness are not much contributing to determine popularity
as understood from the graphs, and contextual understanding.
All the songs are non-instrumental. Thus, these features are dropped.
Also there is high positive linear correlation between energy and loudness
and very naturally there is a high negative correlation between acousticness and energy or acousticness or loudness,
but as they are not perfectly correlated we keep them for now."

capture.output(print(opinion), file = "postEDAopinions.txt")

df<-df[,-c(1,3,4,5,6,11,12,15,18)]
features<-colnames(df)
capture.output(keyvsmode, file = "association_keyvsmode.txt")

rm(list=ls()[!(ls() %in% c("df", "features", "df_reg"))])

df$acousticness<-scale(df$acousticness, center = T, scale =T)
df$danceability<-scale(df$danceability, center = T, scale =T)
df$duration_ms<-scale(df$duration_ms, center = T, scale =T)
df$energy<-scale(df$energy, center = T, scale =T)
df$liveness<-scale(df$liveness, center = T, scale =T)
df$loudness<-scale(df$loudness, center = T, scale =T)
df$speechiness<-scale(df$speechiness, center = T, scale =T)
df$tempo<-scale(df$tempo, center = T, scale =T)
df$valence<-scale(df$valence, center = T, scale =T)
colnames(df)<-features
