# Uma_teoria_dinamica_do_apoio_-_democracia
Trabalho apresentado na ANPOCS 2022 no ST10: Comportamento político, opinião pública e cultura política.

# 1. Setup

```{r}
rm(list = ls())
gc()
```
# 2. Pacotes 

```{r}
library(tidyverse)
library(janitor)
library(rio)
library(Hmisc)
library(plm)
```

# 3. Banco de dados
[Banco de Dados](https://1drv.ms/u/s!AoFwlHJDBCURgUF51mTljW_wx6sC?e=l4uXbd)

Chamei meu banco de = q

# 3.1. Filtro [recortamos as ondas de 1 até 4] 

```{r}
q = q %>%
  filter(wave < 5)
```

# 4. Variáveis 

# 4.1. polyarchy_1_1 = Tolerância sobre manifetações legais 
#[1 = desaprova | 7 = aprova]

########## Outras variáveis possíveis neste bloco de perguntas ##########

polyarchy_1_2 = organização ou grupos comunitários  
polyarchy_1_3 = campanhas eleitorais  
polyarchy_1_4 = bloqueio de ruas ou rodovias  
polyarchy_1_5 = invasão de propriedade  

########## Outras variáveis possíveis neste bloco de perguntas ##########
```{r}
q = q %>%
  mutate(poli1 = polyarchy_1_1) # Selecionamos esta variável no artigo
```

# 4.2. ing4 = democracia é a melhor forma de governo 
# [1 = discorda muito | 7 = concorda muito]
```{r}
q = q %>%
  mutate(democracia = ing4)
```

# 4.3. conf_votecounting = o resultado reflete o voto depositado nas urnas
```{r}
q = q %>%
  mutate(eleições = conf_votecounting)
```

# 4.4. Dummy para identificar o período de tratamento 
```{r}
q = q %>%
  mutate(data_trat = ifelse(wave == 4, 1,0))
```

# 4.5. Dummy para identificar tratamento 1 [voto em bolsonaro] 
```{r}
q = q %>%
  mutate(votoBolsonaro = ifelse(cand_vote == 2, 1, 0))
```

# 4.6. Dummy para identificar tratamento 2 [voto em Haddad] 
```{r}
q = q %>%
  mutate(votoHaddad = ifelse(cand_vote == 1, 1, 0))
```

# 5. Regressões 

# 5.1. Apoio a democracia

Tratamento 1 - Voto em Bolsonaro
```{r}
trat1 = plm(democracia ~ votoBolsonaro + data_trat + votoBolsonaro*data_trat,
            weights = weight, model = "within", 
            index = c("idnumber", "wave"),
            data = q)

summary(trat1)

mean(fixef(trat1)) #Média dos interceptos

# calculo da diferença de médias (Diff-in-Diff)

c0 = 5.189077
t0 = 5.189077 +0.067726
c1 = 5.189077 +0.400405 
t1 = 5.189077 +0.509990 +0.067726 +0.400405

df_t = t1 - t0
df_c = c1 - c0
df_df = df_t - df_c

x = c(0,0,1,1)
y = c(5.189077,5.256803,5.589482,6.167198)#segundo valor (y),último valor (yend)
voto = c("não votou","votou","não votou","votou")
objeto = data.frame(x, y, voto)

objeto %>% ggplot(aes(x = x, y = y, 
                      color = voto)) +
  geom_point() +
  geom_line(aes(group = voto)) +
  annotate(geom = "segment", x = 0, xend = 1,
           y = 5.256803, yend = t1 - df_df,
           linetype = "dotted") +
  annotate(geom = "segment", x = 1, xend = 1,
           y = t1 - df_df, yend = 6.167198,
           color = "black") +
  scale_x_continuous(breaks = c(0,1)) +
  labs(
    y = "",
    x = "",
    color = "Voto em Bolsonaro \n1º turno 2018",
    caption = "Fonte: Democracy on the Ballot: Brazil 2018") +
  theme_bw()
```

Tratamento 2 - Voto em Haddad 
```{r}
trat2 = plm(democracia ~ votoHaddad + data_trat +
              votoHaddad*data_trat, weights = weight, model = "within", 
            index = c("idnumber", "wave"),
            data = q)

summary(trat2)

mean(fixef(trat2)) #Média dos interceptos

# calculo da diferença de médias (Diff-in-Diff)

c0 = 5.20458
t0 = 5.20458 +0.040446 
c1 = 5.20458 +0.705726    
t1 = 5.20458 -0.804497 +0.040446 +0.705726  

df_t = t1 - t0
df_c = c1 - c0
df_df = df_t - df_c

x = c(0,0,1,1)
y = c(5.20458,5.245026,5.910306,5.146255)#segundo valor (y),último valor (yend)
voto = c("não votou","votou","não votou","votou")
objeto = data.frame(x, y, voto)

objeto %>% ggplot(aes(x = x, y = y, 
                      color = voto)) +
  geom_point() +
  geom_line(aes(group = voto)) +
  annotate(geom = "segment", x = 0, xend = 1,
           y = 5.245026, yend = t1 - df_df,
           linetype = "dotted") +
  annotate(geom = "segment", x = 1, xend = 1,
           y = t1 - df_df, yend = 5.146255,
           color = "black") +
  scale_x_continuous(breaks = c(0,1)) +
  labs(
    y = "",
    x = "",
    color = "Voto em Hadadd \n1º turno 2018",
    caption = "Fonte: Democracy on the Ballot: Brazil 2018") +
  theme_bw()
```

# 5.2. Tolerancia com manifestações legais 

Tratamento 1 - Voto em Bolsonaro 
```{r}
trat1 = plm(poli1 ~ votoBolsonaro + data_trat + votoBolsonaro*data_trat,
            weights = weight, model = "within", 
            index = c("idnumber", "wave"),
            data = q)

summary(trat1)

mean(fixef(trat1)) #Média dos interceptos

# calculo da diferença de médias (Diff-in-Diff)

c0 = 5.356646
t0 = 5.356646 +0.541473
c1 = 5.356646 -0.119546 
t1 = 5.356646 +0.130695 +0.541473-0.119546

df_t = t1 - t0
df_c = c1 - c0
df_df = df_t - df_c

x = c(0,0,1,1)
y = c(5.356646,5.898119,5.2371,5.909268)#segundo valor (y),último valor (yend)
voto = c("não votou","votou","não votou","votou")
objeto = data.frame(x, y, voto)

objeto %>% ggplot(aes(x = x, y = y, 
                      color = voto)) +
  geom_point() +
  geom_line(aes(group = voto)) +
  annotate(geom = "segment", x = 0, xend = 1,
           y = 5.898119, yend = t1 - df_df,
           linetype = "dotted") +
  annotate(geom = "segment", x = 1, xend = 1,
           y = t1 - df_df, yend = 5.909268,
           color = "black") +
  scale_x_continuous(breaks = c(0,1)) +
  labs(
    y = "",
    x = "",
    color = "Voto em Bolsonaro \n1º turno 2018",
    caption = "Fonte: Democracy on the Ballot: Brazil 2018") +
  theme_bw()
```

Tratamento 2 - Voto em Haddad 
```{r}
trat2 = plm(poli1 ~ votoHaddad + data_trat +
              votoHaddad*data_trat, weights = weight, model = "within", 
            index = c("idnumber", "wave"),
            data = q)

summary(trat2)

mean(fixef(trat2)) #Média dos interceptos

# calculo da diferença de médias (Diff-in-Diff)

c0 = 5.635832
t0 = 5.635832 -0.855512 
c1 = 5.635832 -0.037202    
t1 = 5.635832 +0.103475-0.855512 -0.037202  

df_t = t1 - t0
df_c = c1 - c0
df_df = df_t - df_c

x = c(0,0,1,1)
y = c(5.635832,4.78032,5.59863,4.846593)#segundo valor (y),último valor (yend)
voto = c("não votou","votou","não votou","votou")
objeto = data.frame(x, y, voto)

objeto %>% ggplot(aes(x = x, y = y, 
                      color = voto)) +
  geom_point() +
  geom_line(aes(group = voto)) +
  annotate(geom = "segment", x = 0, xend = 1,
           y = 4.78032, yend = t1 - df_df,
           linetype = "dotted") +
  annotate(geom = "segment", x = 1, xend = 1,
           y = t1 - df_df, yend = 4.846593,
           color = "black") +
  scale_x_continuous(breaks = c(0,1)) +
  labs(
    y = "",
    x = "",
    color = "Voto em Hadadd \n1º turno 2018",
    caption = "Fonte: Democracy on the Ballot: Brazil 2018") +
  theme_bw()
```

# 5.3. Confiança [o resultado das urnas reflete o voto nas urnas] 

Tratamento 1 - Voto em Bolsonaro 
```{r}
trat1 = plm(eleições ~ votoBolsonaro + data_trat + votoBolsonaro*data_trat,
            weights = weight, model = "within", 
            index = c("idnumber", "wave"),
            data = q)

summary(trat1)

mean(fixef(trat1)) #Média dos interceptos

# calculo da diferença de médias (Diff-in-Diff)

c0 = 3.549046
t0 = 3.549046 +0.060306
c1 = 3.549046 +0.485984 
t1 = 3.549046 +0.864778 +0.060306 +0.485984

df_t = t1 - t0
df_c = c1 - c0
df_df = df_t - df_c

x = c(0,0,1,1)
y = c(3.549046,3.609352,4.03503,4.960114)#segundo valor (y),último valor (yend)
voto = c("não votou","votou","não votou","votou")
objeto = data.frame(x, y, voto)


objeto %>% ggplot(aes(x = x, y = y, 
                      color = voto)) +
  geom_point() +
  geom_line(aes(group = voto)) +
  annotate(geom = "segment", x = 0, xend = 1,
           y = 3.609352, yend = t1 - df_df,
           linetype = "dotted") +
  annotate(geom = "segment", x = 1, xend = 1,
           y = t1 - df_df, yend = 4.960114,
           color = "black") +
  scale_x_continuous(breaks = c(0,1)) +
  labs(
    y = "",
    x = "",
    color = "Voto em Bolsonaro \n1º turno 2018",
    caption = "Fonte: Democracy on the Ballot: Brazil 2018") +
  theme_bw()
```

Tratamento 2 - Voto em Haddad 
```{r}
trat2 = plm(eleições ~ votoHaddad + data_trat +
              votoHaddad*data_trat, weights = weight, model = "within", 
            index = c("idnumber", "wave"),
            data = q)

summary(trat2)

mean(fixef(trat2)) #Média dos interceptos

# calculo da diferença de médias (Diff-in-Diff)

c0 = 3.514687
t0 = 3.514687 +0.45773 
c1 = 3.514687 +0.92963    
t1 = 3.514687 -0.65285 +0.45773 +0.92963  

df_t = t1 - t0
df_c = c1 - c0
df_df = df_t - df_c

x = c(0,0,1,1)
y = c(3.514687,3.972417,4.444317,4.249197)#segundo valor (y),último valor (yend)
voto = c("não votou","votou","não votou","votou")
objeto = data.frame(x, y, voto)

objeto %>% ggplot(aes(x = x, y = y, 
                      color = voto)) +
  geom_point() +
  geom_line(aes(group = voto)) +
  annotate(geom = "segment", x = 0, xend = 1,
           y = 3.972417, yend = t1 - df_df,
           linetype = "dotted") +
  annotate(geom = "segment", x = 1, xend = 1,
           y = t1 - df_df, yend = 4.249197,
           color = "black") +
  scale_x_continuous(breaks = c(0,1)) +
  labs(
    y = "",
    x = "",
    color = "Voto em Hadadd \n1º turno 2018",
    caption = "Fonte: Democracy on the Ballot: Brazil 2018") +
  theme_bw()
  ```
  
# 6. Conclusões 

# As atitudes democráticas variam em um curto espaço de tempo
# O contexto eleitoral afeta as atitudes em relação a democracia
