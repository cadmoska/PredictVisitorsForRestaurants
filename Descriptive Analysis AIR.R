rm(list = objects())
graphics.off()




# Chargement des données 

library(tidyverse)
library(xts)
library(lubridate)



rpath <- "/Users/Nawel/Desktop/Statistiques/STA202/Projet/Data/"

air_visits <- read_csv(str_c(rpath,'air_visit_data.csv'), col_types = cols())
air_reserve <- read_csv(str_c(rpath,'air_reserve.csv'), col_types = cols())
air_store <- read_csv(str_c(rpath,'air_store_info.csv'), col_types = cols())


# Analyse descriptive 

# Dimensions

d <- list(air_visits, air_reserve, air_store)

data.frame(
  Dataframes = c('visits', 'air_res', 'dates', 'hpg_store', 'air_store', 'store_id_rel'),
  Nrows = sapply(d, nrow),
  Ncols = sapply(d, ncol),
  Size = sapply(d, function(x) { format(object.size(x), units = 'Mb')})
) 

summary(air_visits)
glimpse(air_visits)
head(air_visits)

air_visits %>% distinct(air_store_id) %>% nrow()


summary(air_reserve)
glimpse(air_reserve)
head(air_reserve)

air_reserve %>% distinct(air_store_id) %>% nrow()


summary(air_store)
glimpse(air_store)
head(air_store)

air_store %>% distinct(air_store_id) %>% nrow()




# Vérifications des données manquantes 

sum(is.na(air_visits))
sum(is.na(air_store))
sum(is.na(air_reserve))

# Aucune donnée manquante 

# Préparation des données 

# On décide de modifier les types de certaines données en factor afin de faciliter le travail 

air_store$air_area_name = as.factor(air_store$air_area_name)
air_store$air_genre_name = as.factor(air_store$air_genre_name)


# Visualisation des données 

# On s'interesse en premier temps aux visiteurs

# nombre total de visiteurs par date

x1 <- air_visits %>%
  group_by(visit_date) %>%
  summarise(visitors = sum(visitors))


x1ts <- xts(x1$visitors, order.by = x1$visit_date)
plot(x1ts)

x2 <- air_reserve %>%
  group_by(visit_datetime) %>%
  summarise(visitors = sum(reserve_visitors))

x2ts <- xts(x2$visitors, order.by = x2$visit_datetime)
plot(x2ts)


x3 <- air_reserve %>%
  group_by(reserve_datetime) %>%
  summarise(visitors = sum(reserve_visitors)) 

x3ts <- xts(x3$visitors, order.by = x3$reserve_datetime)
plot(x3ts)

y <- air_visits %>% ggplot(aes(visitors)) + geom_histogram(fill = 'pink', bins = 30)+scale_x_log10()
plot(y)

# nombre total de visiteurs par jour de la semaine

z <- air_visits %>% 
  mutate(wday = wday(visit_date, label = T, week_start = 1)) 
Days <- z$wday
DailyVisits <- tapply(z$visitors, Days, mean)
plot(DailyVisits, type = 'b', pch =20)  

# Sous forme d'histogramme pour plus de lisibilité

z <- air_visits %>%
  mutate(wday = wday(visit_date, label = TRUE, week_start = 1)) %>% group_by(wday) %>%
  summarise(visits = mean(visitors))

p <- ggplot(z, aes(x= z$wday, y=z$visits, fill = z$wday)) + geom_bar(stat = 'identity')  +
       labs(x = "Jours de la semaine ", y = "Moyenne des visiteurs par jour") + 
        theme_minimal() + theme(legend.position="none")
plot(p)


# nombre total de visiteur par mois 

w <- air_visits %>%
  mutate(month = month(visit_date, label = TRUE)) %>%
  group_by(month) %>%
  summarise(visits = mean(visitors))

p <-  ggplot(w, aes(x= w$month, y=w$visits, fill = w$month)) + geom_bar(stat = 'identity')  +
  labs(x = "Mois de l'années ", y = "Moyenne des visiteurs par mois") + 
  theme_minimal() + theme(legend.position="none")
plot(p)

# Nombre de visiteurs par type de restaurant

t <- air_store %>% 
  left_join(air_visits, c("air_store_id")) %>%
  group_by(air_genre_name) %>% 
  summarize(act_visitor_tol = sum(visitors))

p <-  ggplot(t,aes(x=reorder(air_genre_name,act_visitor_tol),
             y=act_visitor_tol)) +
  geom_bar( stat='identity', fill='steelBlue') + theme_minimal()+
  labs(x="genre",y="Nombre de visiteurs", title="Nombre de visiteurs total par type de restaurant") + coord_flip()

plot(p)

# Nombre de visiteurs par area 


l <- air_store %>% 
  left_join(air_visits, c("air_store_id")) %>%
  group_by(air_area_name) %>% 
  summarize(act_visitor_tol = sum(visitors)) %>% 
  top_n(10, air_area_name)

p <- ggplot(l,aes(x=reorder(air_area_name,act_visitor_tol),
                   y=act_visitor_tol)) +
  geom_bar( stat='identity', fill='steelBlue',) +
  labs(x="area",y="Nombre de visiteurs", title="Nombre de visiteurs total par area") + coord_flip()

plot(p)

# Autocorrélation 

air_visit_ts <- ts(air_visits[,"visitors"], start=c(2016-01-01), frequency=7) 
result_acf <- acf(air_visit_ts)

print(data.frame(result_acf$lag,result_acf$acf)[1:10,])


d.air_visit_ts <- diff(air_visit_ts)
d.result_acf <- acf(d.air_visit_ts)

print(data.frame(d.result_acf$lag,d.result_acf$acf)[1:10,])

# Autocorrélation partielle 

result_pacf <- pacf(air_visit_ts)

print(data.frame(result_pacf$lag,result_pacf$acf)[1:10,])

###################
## Saisonnalité ###
###################

x1ts <- xts(x1$visitors, order.by = x1$visit_date)
plot(x1ts)
K <- 7

MA <- stats::filter(x1ts, filter=array(1/K,dim=K),
                    method = c("convolution"), sides = 2, circular = FALSE)

plot(x1, type ='l')
lines(x1$visit_date,MA, col = 'red', lwd = 2)


############################
## Lissage exponentielle ###
############################

# Nombre de jours à prévoire
N = 39
max_date <- max(air_visits$visit_date)
# à quoi cela correspond ?
split_date <- max_date - N

visits_train <- air_visits %>% filter(visit_date <= split_date)
visits_valid <- air_visits %>% filter(visit_date > split_date)
test_visits <- visits_valid %>% group_by(visit_date)%>% summarise(visit = sum(visitors) )

hw.fit <- HoltWinters(ts(visits_train$visitors, frequency = 7))
hw_visits <- predict(hw.fit, n.ahead = N, prediction.interval = T, level = 0.95)
plot(hw_visits)


rmse <- function(p,a){
  n = length(y)
  return(sqrt(1/n *sum((log(p+1)-log(a+1))**2)))
}

length(hw_visits)
r <- rmse(hw_visits[1:39], test_visits$visit)

temp <- air_visits
temp$visitors[440 : 478] <- hw_visits[40:78]
s <- temp %>% group_by(visit_date) %>% summarise(visitors = sum(visitors) )
plot(s$visitors, type ='l')
abline(v=435, col="Red")

# 2eme plot 

plot(s$visitors[1:439], type ='l')
lines(c(440:478),s$visitors[440:478], col = 'Red')
abline(v=440, col="green")


# Prédiction sur le modèle lissé 
# Remarque : MA <=> air_visits %>% group_by(visit_date)%>% summarise(visit = sum(visitors) )$visitors

MA[1:3] <-3000
MA[476:478] <- 15450

x1$visitors <- MA

visits_train <- x1 %>% filter(visit_date <= split_date)
visits_valid <- x1 %>% filter(visit_date > split_date)
test_visits <- visits_valid %>% group_by(visit_date)%>% summarise(visit = sum(visitors) )


hw.fit <- HoltWinters(ts(visits_train$visitors, frequency = 7))
hw_visits <- predict(hw.fit, n.ahead = N, prediction.interval = T, level = 0.95)
plot(hw_visits)


temp <- x1
temp$visitors[440 : 478] <- hw_visits[1:39]
s <- temp %>% group_by(visit_date) %>% summarise(visitors = sum(visitors) )
plot(s$visitors, type ='l')
abline(v=435, col="Red")



