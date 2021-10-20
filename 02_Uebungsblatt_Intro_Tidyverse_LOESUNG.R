
library(tidyverse)


#c)
(olympics <- read_csv("data/olympics.csv")
)

#Datensatz im Viewer betrachten: entweder über point-and-click im Environment
#oder mit:
olympics %>% 
          View()
          
#d) Um die Dimensionen des Datensatzes zu erkennen, entweder 
#im Environment ablesen oder in der Konsole:
olympics 
#Der Datensatz hat 271.116 Beobachtungen und 15 Variablen.
#Jede Beobachtung steht für eine Sportlerin oder einen Sportler in einer Sportart
#bei einer der Olympischen Spiele. 

#e)
#Zwei Varianten für den Select Befehl

#Variante 1: Auswahl der zu behaltenden Variablen:
olympics_2016 <- olympics %>% 
          filter(games == "2016 Summer") %>% 
          select(c(id, name, sex, age, height, weight, team, sport, event, medal))
#Variante 2: Auswahl der nicht zu behaltenden Variablen mit '-':
olympics_2016 <- olympics %>% 
          filter(games == "2016 Summer") %>% 
          select(-c(games, year, season, city, noc))

#f)

          #i)              
#Größe der größten bzw. kleinsten AthletIn
olympics_2016 %>% 
          summarise(max_height = max(height, na.rm = TRUE),
                    min_height = min(height, na.rm = TRUE)) 
          
          #ii)
olympics_2016 %>% 
          summarise(max_height = max(height, na.rm = TRUE),
                    min_height = min(height, na.rm = TRUE)) %>% 
          mutate(height_diff_abs = max_height - min_height,
                 height_diff_percent = max_height / min_height - 1)


          
          #iii)
#Wettkampf der größten AthletIn:
olympics_2016 %>% 
          filter(height == max(height, na.rm = T))

#Wettkampf der kleinsten AthletIn:
olympics_2016 %>% 
          filter(height == min(height, na.rm = T))

#Oder Kombination aus größter und kleinster Athletin:
olympics_2016 %>% 
          filter(height == max(height, na.rm = T) | height == min(height, na.rm = T))

          #iv)
#Durchschnittliche Größe
#manche AthletInnen nehmen an mehreren Events teil, bleibt dies unberücksichtigt, so gilt:
olympics_2016 %>% 
          summarise(durschn_height = mean(height, na.rm = TRUE))

#wir können distinct() nutzen, um jeden Wettbewerber nur einmal zu berücksichtigen.
# wir nutzen id und nicht name, warum?
olympics_2016 %>% 
          distinct(id, .keep_all = T) %>% 
          summarise(durschn_height = mean(height, na.rm = TRUE))

          #v)
#durchschnittliche Größe nach Sportart:
olympics_2016 %>% 
          group_by(sport) %>% 
          distinct(id, .keep_all = T) %>% 
          summarise(durschn_height = mean(height, na.rm = TRUE)) %>% 
          ungroup() %>% 
          arrange(durschn_height)  #aufsteigend
          
          #arrange(desc(durschn_height)) #alternativ: absteigend


          #vi)
#durchschnittliche Größe nach Geschlecht:
olympics_2016 %>% 
          group_by(sex) %>% 
          distinct(id, .keep_all = T) %>% 
          summarise(durschn_height = mean(height, na.rm = TRUE))


          #vii)
#durchschnittliche Größe nach Sportart und Geschlecht:
olympics_2016 %>% 
          group_by(sport, sex) %>% 
          distinct(id, .keep_all = T) %>% 
          summarise(durschn_height = mean(height, na.rm = TRUE)) %>% 
          ungroup() 

          #viii)
olympics_2016 %>% 
          filter(sex == "M") %>% 
          count(height > 180) %>% 
          mutate(insgesamt = sum(n)) %>% 
          mutate(anteil = n / insgesamt)

#f) Formel für BMI: kg / m^2
olympics_2016 %>% 
          filter(sex == "F") %>% 
          filter(sport == "Volleyball") %>% 
          filter(age >= 20 & age <= 30) %>% #alternativ: filter(between(age, left = 20, right = 30)) 
          distinct(id, .keep_all = T) %>% 
          mutate(bmi = weight / (height/100)^2) %>% 
          summarise(durschn_bmi = mean(bmi, na.rm = T))

