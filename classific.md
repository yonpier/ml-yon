---
title: "Classification"
author: "Jean-Pierre Lopez"
date: "22/12/2020"
output:
  html_document:
    keep_md: true
---




# Introduction

Le but de ce projet est d'identifier si oui ou non le client d'une banque fait défaut sur la base de certaines informations. C'est un exercice qui relève de la classification supervisée car l'on essaye de prédire une variable qualitative prenant deux valeurs.

On va commencencer par un traitement des données suivie d'une analyse approfondie pour chaque variable via des graphiques et des tableaux. Ces analyses nous permettrons de déterminer dans un premier temps les variables ayant un bon potentiel pour expliquer la probabilité de défaut. Enfin, nous proposerons quelques classifieurs.

 

```r
library(readxl)
library(tidyverse)
library(caret)
library(glmnet) 
library(e1071) 
library(Matrix)

Credit <- read_xlsx("Credit_Cleaned.xlsx")
```

On a de l'information sur 5380 clients de la banque. Le nombre de variables explicatives est de 19.
 

```r
n = dim(Credit)[1]
dim(Credit)
```

```
## [1] 5380   19
```

On remarque l'absence de valeur manquante et la très grande présence de variables qualitatives.
On observe également la présence de variables de type POSIXct (date). On essayera d'extraire le plus d'information de ces variables afin de prédire Y.




```r
infodf = data.frame(type = t(data.frame(sapply(Credit, class)))[,1])
infodf$unique = sapply(Credit, function(x) length(unique(x)))
infodf$na = sapply(Credit, function(x) sum(is.na(x)))
infodf
```

```
##                          type unique na
## Y                   character      2  0
## Customer_Type       character      2  0
## BirthDate             POSIXct   4382  0
## Customer_Open_Date    POSIXct   1244  0
## P_Client            character      2  0
## Educational_Level   character      4  0
## Marital_Status      character      5  0
## Number_Of_Dependant   numeric     10  0
## Years_At_Residence    numeric     60  0
## Net_Annual_Income     numeric    522  0
## Years_At_Business     numeric     44  0
## Prod_Sub_Category   character      3  0
## Prod_Decision_Date    POSIXct    276  0
## Source              character      2  0
## Type_Of_Residence   character      5  0
## Nb_Of_Products        numeric      3  0
## Prod_Closed_Date      POSIXct    331  0
## Prod_Category       character     13  0
## Prod_Closed_Date_NA   logical      2  0
```

# Traitement des donnnées de type date (POSIXct)

## Date de naissance (BirthDate)

`BirthDate` est une des variables de type date. Il serait intéressant d'extraire l'âge des clients via l'année de naissance mais nous ne connaissons pas la date à laquelle ce jeu de donnée a été conçu à priori.


```r
Credit$BirthDate[1]
```

```
## [1] "1977-08-07 UTC"
```
Cependant on sait que dans la variable `Prod_Closed_Date` sont stockées les dates de cloture du prêt et que si la date est manquante alors elle est remplacée par la date la plus récente. `Prod_Closed_Date_Na` est la variable dummy qui indique si oui ou non la date de cloture est manquante. La date la plus récente est celle du 2 juin 2013, on soustrait à cette date le vecteur `BirthDate` pour obtenir l'âge des clients.


```r
head(as.data.frame(Credit[Credit$Prod_Closed_Date_NA==TRUE, "Prod_Closed_Date"]))
```

```
##   Prod_Closed_Date
## 1       2013-06-02
## 2       2013-06-02
## 3       2013-06-02
## 4       2013-06-02
## 5       2013-06-02
## 6       2013-06-02
```


```r
latest_date = max(Credit$Prod_Closed_Date)
diff = difftime(latest_date, Credit$BirthDate, unit = "days")
Credit$Age =  floor(as.numeric(diff)/365)
head(Credit$Age)
```

```
## [1] 35 38 39 30 59 30
```

## Date d'arrivée du client (Customer_Open_Date)

On extrait uniquement l'année et le mois d'arrivée du client, on ne pense pas qu'il soit possible que le jour du client puisse avoir un effet sur la probabilité de défaut.


```r
Credit$CustomerOpenDateYear = as.numeric(substring(Credit$Customer_Open_Date, 0,4))
Credit$CustomerOpenDateMonth = as.factor(substring(Credit$Customer_Open_Date, 6,7))
```


## Date d'ouverture et de fermeture du prêt financier (Prod_Decision_Date, Prod_Closed_Date)

On extrait la durée en mois du prêt afin d'examiner l'effet de celle-ci sur la probabilité de défaut. On sait que les prêts de longues dates ont des taux d'intérêts différents comparé aux prêts de courte période. On extrait également les années et les mois de ces deux variables.


```r
Credit$Credit_Duration = floor(as.numeric(Credit$Prod_Closed_Date - Credit$Prod_Decision_Date)/30)
Credit$Prod_Closed_DateYear= as.factor(substring(Credit$Prod_Closed_Date, 0,4))
Credit$Prod_Closed_DateMonth= as.factor(substring(Credit$Prod_Closed_Date, 6,7))
Credit$Prod_Decision_DateYear= as.factor(substring(Credit$Prod_Decision_Date, 0,4))
Credit$Prod_Decision_DateMonth= as.factor(substring(Credit$Prod_Decision_Date, 6,7))
```



```r
head(Credit$Credit_Duration)
```

```
## [1] 15 23 14 16 16 20
```



# Analyse des données

On introduit une fonction qu'on va réutiliser tout au long de notre analyse. Elle permet de récupérer la proportion de clients faisant défaut pour chaque catégorie d'une variable qualitative.


```r
TableProp = function(data, y, x){
  n <-  dim(data)[1]
  n_x <-  table(data[x])
  names_x <-  names(table(data[x]))
  df <-  data.frame(numeric(2))
    
  for(j in 1:length(n_x)){
    df[names_x[j]] <- table(data[data[x] == names_x[j], y])/n_x[j]
  }
  df = df[-1]
  rownames(df) <- names(table(data[y]))
    
return(df)
}
```



## Credit issue

On commence par observer que la proportion des clients ne faisant pas défault est très importante. On fait face à un jeu de donnée déséquilibré et il faudra donc prendre en compte cet aspect au moment de la validation croisée. En effet, seulement 7.3% des clients ont fait défaut.


```r
table(Credit$Y)/n
```

```
## 
##    DEFAULT NO_DEFAULT 
## 0.07304833 0.92695167
```


## Type de client (Customer_Type)

On remarque que la proportion de client ayant fait défault est la même quelque soit la catégorie dans laquelle appartient le client. On pourrait supposer qu'un nouveau client à la même probabilité de faire défault qu'un ancien. Cette variable ne parait pas très importante pour expliquer la probabililité de faire défaut.  
Le graphique nous montre également qu'il y a plus de nouveaux clients que d'anciens clients.


```r
TableProp(Credit, "Y", "Customer_Type")
```

```
##            Existing Client Non Existing Client
## DEFAULT         0.07458976          0.07212823
## NO_DEFAULT      0.92541024          0.92787177
```


![](classific_files/figure-html/unnamed-chunk-13-1.png)<!-- -->


## L'âge

On observe que les plus jeunes font défaut à un rythme plus élevé que les vieux clients. L'âge pourrait être une variable importante, on va créer des catégories d'âge pour avoir de meilleurs intérprétations.

![](classific_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

On observe un pattern intéressant: plus le client est jeune plus il est enclin à faire défaut. Parmis les clients âgés de plus de 60 ans, il y a seulement 5% d'entre eux qui ont fait défaut contre plus de 10% pour les clients les plus jeunes qui sont dans la vingtaine.  
On voit que la catégorie d'âge la plus représentée est celle des clients trentenaires.


```r
Credit$AgeCat <-  cut(Credit$Age, breaks = c(20, 30, 40 ,50,60,100), labels = c("20-30","31-40", "41-50", "51-60", "60+"))
TableProp(Credit, "Y", "AgeCat")
```

```
##                20-30      31-40      41-50      51-60        60+
## DEFAULT    0.1088083 0.07249712 0.05981595 0.05585392 0.04417671
## NO_DEFAULT 0.8911917 0.92750288 0.94018405 0.94414608 0.95582329
```


![](classific_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

## Date d'arrivée du client (Customer_Open_Date)

A partir des années 2003-2004 le nombre de nouveaux clients s'est mit à augmenter sensiblement et a poursuivi son ascension jusqu'à la dernière date disponible, soit 2012. 
On remarque qu' à partir de 2005 le nombre de clients faisant défaut a commencé à augmenter. Avant cette date, très peu de clients faisaient défaut. Cette variable est intéressante car elle montre potentiellement que le risque de défaut, toute chose égale par ailleurs, augmente au fur et à mesure du temps.


![](classific_files/figure-html/unnamed-chunk-17-1.png)<!-- -->



```r
Credit$CustomerOpenDateYear <-  cut(Credit$CustomerOpenDateYear, breaks =  c(0,2005,2010,2012), labels = c("<2005","2006-2010", ">2010"))
TableProp(Credit, "Y", "CustomerOpenDateYear")
```

```
##                 <2005  2006-2010      >2010
## DEFAULT    0.02083333 0.03398058 0.08744395
## NO_DEFAULT 0.97916667 0.96601942 0.91255605
```


## Catégorie de client (P_Client)

On voit que la catégorie du client parraît importante pour expliquer la probabilté de faire défaut: les clients de catégorie NP font plus défaut que les autres.  


```r
TableProp(Credit, "Y", "P_Client")
```

```
##             NP_Client   P_Client
## DEFAULT    0.07588567 0.03883495
## NO_DEFAULT 0.92411433 0.96116505
```
On observe que parmis les nouveaux clients il n' y a aucun client de catégorie P.

![](classific_files/figure-html/unnamed-chunk-20-1.png)<!-- -->

Ce sont les clients de catégorie NP arrivés après 2010 qui font le plus défaut.

![](classific_files/figure-html/unnamed-chunk-21-1.png)<!-- -->


## Niveau d'éducation (Educational_Level)

On remarque que parmis les clients ayant arrété leur éducation au lycée, sans le baccalauréat, aucun n'a fait défaut. L'effectif de ce groupe est également très petit (15).


```r
table(Credit$Educational_Level)
```

```
## 
##           Diploma        Master/PhD Secondary or Less        University 
##                58               522                15              4785
```

Afin d'éviter le surapprentissage, on regroupe au sein d'un même groupe les clients ayant eu le baccalauréat et les clients qui ont arrété avant.



```r
Credit$Educational_Level[Credit$Educational_Level == "Diploma"] = "Diploma or Less"
Credit$Educational_Level[Credit$Educational_Level == "Secondary or Less"] = "Diploma or Less"
TableProp(Credit, "Y", "Educational_Level")
```

```
##            Diploma or Less Master/PhD University
## DEFAULT         0.04109589 0.05555556  0.0754441
## NO_DEFAULT      0.95890411 0.94444444  0.9245559
```

La grande majorité des clients est allée à l'université et a obtenu un diplôme de licence. Ce sont ces mêmes clients qui ont le plus fait défaut.

![](classific_files/figure-html/unnamed-chunk-24-1.png)<!-- -->

La filliale fait face à de plus en plus de clients de catégorie NP ayant un diplôme de licence au cours du temps. Et ce sont eux même, qui après 2010 font le plus défaut.



![](classific_files/figure-html/unnamed-chunk-25-1.png)<!-- -->

## Situation de famille (Marital_Status)

Tous les clients qui sont séparés, divorcés ou veufs seront considérés comme célibataires.



```r
table(Credit$Marital_Status)
```

```
## 
##  Divorced   Married Separated    Single   Widowed 
##        63      4206         1      1046        64
```

```r
Credit$Marital_Status[Credit$Marital_Status == "Separated"] = "Single"
Credit$Marital_Status[Credit$Marital_Status == "Widowed"] = "Single"  
Credit$Marital_Status[Credit$Marital_Status == "Divorced"] = "Single" 
```


```r
TableProp(Credit, "Y", "Marital_Status")
```

```
##               Married     Single
## DEFAULT    0.06918688 0.08688245
## NO_DEFAULT 0.93081312 0.91311755
```

![](classific_files/figure-html/unnamed-chunk-28-1.png)<!-- -->

## Nombre de personnes à charge (Number_Of_Dependant)


```r
table(Credit$Number_Of_Dependant)
```

```
## 
##    0    1    2    3    4    5    6    7   12   20 
## 2891  485 1135  622  201   32    8    4    1    1
```

```r
Credit$Number_Of_DependantCat = cut(Credit$Number_Of_Dependant, c(-1,0,2,4, 20), labels = c("none", '1-2', "3-4", "5+"))
```

La proportion de défaut augmente avec le nombre de personnes à charge. Ce qui semble cohérent puisqu'ils ont plus de frais, ce qui diminue leur capacité de remboursement.


```r
TableProp(Credit, "Y", "Number_Of_DependantCat")
```

```
##                  none        1-2        3-4         5+
## DEFAULT    0.08094085 0.06604938 0.06196841 0.02173913
## NO_DEFAULT 0.91905915 0.93395062 0.93803159 0.97826087
```

## Nombre d'années vivant dans la propriété actuelle (Years_At_Residence)

Les clients vivant sous le même toit depuis plus de 40 ans ne font quasiment pas défaut.
Plus on vit longtemps sous un même toit plus la proportion de clients faisant défaut baisse.

![](classific_files/figure-html/unnamed-chunk-31-1.png)<!-- -->


```r
Credit$Years_At_ResidenceCat = cut(Credit$Years_At_Residence, breaks =  c(-1,10,20,30,40,100), labels = c("<10", "11-20", "21-30", "31-40", "40+"))
TableProp(Credit, "Y", "Years_At_ResidenceCat")  
```

```
##                   <10      11-20      21-30      31-40        40+
## DEFAULT    0.07924922 0.06710526 0.06629834 0.06862745 0.01818182
## NO_DEFAULT 0.92075078 0.93289474 0.93370166 0.93137255 0.98181818
```


## Nombre d'année dans le job actuel (Years_At_Business)

Les clients qui ont beaucoup d'expérience dans leur travail actuel font défaut à un rythme beaucoup moins élevé que ceux qui ont moins d'expérience. On remarque que certains clients travaillent depuis presque 100 ans dans leur job actuel, ce qui semble peu réaliste.


![](classific_files/figure-html/unnamed-chunk-33-1.png)<!-- -->

On examine ce groupe d'observations suspicieux et l'on confirme qu'il s'agit bien de valeurs aberrantes. En effet, on voit que ces personnes ont travaillé plus d'années que leur âge, ce qui est impossible.



```r
as.data.frame(Credit[Credit$Years_At_Business>40, c("BirthDate", "Years_At_Business", "Age")])
```

```
##     BirthDate Years_At_Business Age
## 1  1963-03-20                50  50
## 2  1956-03-02                50  57
## 3  1970-02-12                98  43
## 4  1975-11-12                98  37
## 5  1955-01-04                54  58
## 6  1979-07-09                97  33
## 7  1948-10-12                97  64
## 8  1962-06-03                98  51
## 9  1963-08-09                98  49
## 10 1972-07-01                98  40
## 11 1978-01-19                97  35
## 12 1977-11-18                98  35
## 13 1954-06-28                48  58
## 14 1982-07-01                98  30
```

On va filtrer les valeurs improbables puis les corriger. On suppose que les individus peuvent commencer à travailler uniquement à partir de 18 ans. Les clients qui travaillent depuis plus de leurs âge moins 18 verront leur valeur de `Years_At_Business` modifié par la médiane.


```r
Credit$Years_At_Business[Credit$Years_At_Business > Credit$Age - 18] = median(Credit$Years_At_Business)
```



## Revenu annuel net (Net_Annual_Income)

On suppose qu'il soit exprimé en millier d'euros ou millier de dollars.


```r
summary(Credit$Net_Annual_Income)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
##     0.004    20.000    36.000    61.172    36.000 10000.000
```



```r
Credit$Net_Annual_Income = Credit$Net_Annual_Income*1000
head(Credit$Net_Annual_Income)
```

```
## [1] 36000 18000 36000 36000 36000 60000
```

On remplace par la médiane les valeurs de `Net_Annual_Income` inférieures à 1000.


```r
as.data.frame(Credit[Credit$Net_Annual_Income<5000, "Net_Annual_Income"])
```

```
##   Net_Annual_Income
## 1              4200
## 2                 4
## 3                60
## 4              3120
## 5              2814
## 6              4200
```

```r
Credit$Net_Annual_Income[Credit$Net_Annual_Income<1000] = median(Credit$Net_Annual_Income)
```

On observe qu'aucun clients gagnant au-dessus de 1 million d'euros font défaut. On peut en conclure qu'au-delà de cette somme, le revenu paraît peu important, le client ne fera probablement pas défaut. On décide alors de remplacer par 1 million les valeurs au-dessus de ce seuil.



```r
as.data.frame(unique(Credit[Credit$Net_Annual_Income>=1000000, c("Y")]))
```

```
##            Y
## 1 NO_DEFAULT
```

```r
Credit$Net_Annual_Income[Credit$Net_Annual_Income>=1000000] = 1000000
```
On voit que la proportion de défaut baisse avec le revenu annuel net. Au-dessus de 60 000 euro net, la proportion de client faisant défaut est marginale.


![](classific_files/figure-html/unnamed-chunk-40-1.png)<!-- -->


# Analyse approfondie

On crée un nouveau jeu de données qui contient que les clients de niveau licence de catégorie NP et qui sont arrivés après 2010. Car ce sont eux qui ont le plus fait défaut. On va essayer d'identifier les variables qui expliquent la probabilité de défaut dans ce groupe.  
Maitenant on observe que les anciens clients de ce groupe font défaut à un rythme plus élevé que les nouveaux clients. 20% des anciens clients ont fait défaut contre 7% pour les nouveaux clients.


```r
Credit2 = Credit[Credit$Educational_Level=="University" & Credit$P_Client == "NP_Client" & Credit$CustomerOpenDateYear == ">2010",]
TableProp(Credit2, "Y", "Customer_Type")
```

```
##            Existing Client Non Existing Client
## DEFAULT          0.2013274          0.07287319
## NO_DEFAULT       0.7986726          0.92712681
```

Plus le client est jeune plus il est enclin à faire défaut.



```r
TableProp(Credit2, "Y", "AgeCat")
```

```
##                20-30      31-40      41-50      51-60        60+
## DEFAULT    0.1343284 0.09075342 0.07274827 0.06108202 0.03846154
## NO_DEFAULT 0.8656716 0.90924658 0.92725173 0.93891798 0.96153846
```

```r
TableProp(Credit2, "Y", "Marital_Status")
```

```
##               Married    Single
## DEFAULT    0.08214794 0.1152318
## NO_DEFAULT 0.91785206 0.8847682
```

## Catégorie du prêt (Prod_Category, Prod_Sub_Category)

On voit que la catégorie de produit la plus populaire est B. Dans cette catégorie, le produit le plus demandé est de sous-catégorie C. Les clients avec un prêt de catégorie L et de sous-catégorie C sont ceux qui en proportion font le plus défaut. En résumé, la grande majorité des défauts concerne des client ayant souscrit un prêt de sous-catégorie C.

![](classific_files/figure-html/unnamed-chunk-44-1.png)<!-- -->


## Source du financement et type de résidence (Source, Type_Of_Residence)

Très peu de clients vivent dans le domicile parental. La grande majorité des défauts se font auprès des clients propriétaires. Ceci peut être expliqué par le fait que ces clients payent encore leur bien immobillier, ce qui diminue leur capacité de remboursement.


![](classific_files/figure-html/unnamed-chunk-45-1.png)<!-- -->


## Durée du prêt (Credit_Duration)

On crèe de nouveau un jeu de donnée pour examiner l'effet de la durée de prêt. On prendra uniquement les clients propriétaires ayant souscrit un prêt de sous-catégorie C car ce sont eux qui font le plus défaut.


```r
Credit3 = Credit2[Credit2$Type_Of_Residence=="Owned" & Credit2$Prod_Sub_Category == "C",]
```

On remarque que les prêts de courte période sont ceux qui générent le plus de défaut chez les clients. Les prêts d'une durée supérieure à 16 mois sont à l'origine de très peu de défauts. Ce sont donc les crédits d'une durée inférieure à 1 an qui sont les plus à risque.

![](classific_files/figure-html/unnamed-chunk-47-1.png)<!-- -->
  

## Nombre de produits (Nb_Of_Products)

La majorité des clients ne cumule qu'un seul prêt. Les clients ayant à leur charge le moins de personnes sont ceux qui ont tendance à cumuler le plus de prêts. En effet, comme leurs besoins sont plus importants, ils s'endettent plus.


![](classific_files/figure-html/unnamed-chunk-48-1.png)<!-- -->

## Résumé

- La proportion de clients faisant défaut diminue avec l'âge. 
- La majorité des clients est arrivée après 2010, eux même faisant le plus de défaut.
- La majorité des défauts se produisent auprès de clients de type NP qui ont une licence et pas plus.
- Plus un client a un nombre important de personne à charge plus il est susceptible de faire défaut.
- Plus un individu vit depuis longtemps dans son logement actuel moins il est enclin à faire défaut.
- Au-dessus d'un certain seuil de salaire (600.000€), les clients ne font plus défaut.


# Classification supervisée

On se rappelle que le jeu de donnée est déséquilibré et ne pas prendre en compte ce fait pourrait endommager la qualité des classifieurs. Puisque le déséquilibre est très prononcé, on va utiliser le F1-Score comme critère pour selectionner le meilleur modèle lors de la validation croisée.
Le F1-Score est calculé à partir de la précision et du rappel (recall). Il s'agit enfait d'une moyenne harmonique de ces deux termes.

F1 = 2 * (PRE * REC) / (PRE + REC)

La précision est le nombre d'observations correctement identifié comme positif divisé par le nombre total de positifs prédits par le modèle. Autrement dit, dans notre cas, la précision mesure la proportion de clients correctement identifiée comme ayant fait défaut par le modèle sur le nombre total de clients prédits comme ayant fait défaut.

Le rappel nous donne la capacité du modèle à prédire correctement les clients faisant défaut. Il donne la proportion de clients prédit comme faisant défaut sur l'ensemble des clients qui ont vraiment fait défaut.

On se focalise sur le rappel car, si on se place du point de vue d'une banque, on veut que le taux de faux négatif soit le plus bas possible. En d'autres termes, on veut être capable d'identifier correctement le plus de clients susceptibles de faire défaut.

Les classifieurs devront avoir un taux de précision supérieur à 92.69%. Ce taux de précision correspond au classifieur qui prédit la classe majoritaire pour chaque observation.

On ne considére pas les modèles LDA et QDA puisque ces modèles reposent sur une hypothèse de normalité des variables explicatives.



```r
table(Credit$Y)/n
```

```
## 
##    DEFAULT NO_DEFAULT 
## 0.07304833 0.92695167
```


```r
set.seed(2020)
Credit <- Credit %>% mutate_if(is.character,as.factor)
Credit$Y = factor(Credit$Y ,levels(Credit$Y)[c(2,1)])
trainIndex <- createDataPartition(Credit$Y, p = .8, list = FALSE, times = 1)
CreditTrain <- Credit[ trainIndex,]
CreditTest  <- Credit[-trainIndex,]
```

Le train set et le test set conservent bien les proportions initiales de la variable à expliquer.


```r
table(CreditTrain$Y)/nrow(CreditTrain)
```

```
## 
## NO_DEFAULT    DEFAULT 
## 0.92682927 0.07317073
```

On réduit et centre les variables continues avant de les donner aux algorithmes de classification. Cela peut permettre à certains algorithmes de converger plus rapidement car ces variables auront le même ordre de grandeur.


```r
pp_data <- preProcess(CreditTrain[,-1], method = c("center", "scale"))
pp_CreditTrain <- predict(pp_data, newdata = CreditTrain)
pp_CreditTest <- predict(pp_data, newdata = CreditTest)
```

On enlève les variables de type date car nous les avons transformés en des variables quantitatives.


```r
formula = Y ~. -Customer_Open_Date -BirthDate -Prod_Decision_Date -Prod_Closed_Date -AgeCat -Number_Of_DependantCat -Years_At_ResidenceCat
```


## Régression logistique

La régression logistique est très utile car elle ne pose aucune hypothèse sur la loi des variables explicatives. Afin d'avoir des output Y compris entre 0 et 1, interprétable comme des probabilités, les predictions passent sous la fonction sigmoïde. Les paramètres théta assosiés aux variables explicatives sont obtenus en minimisant la fonction de coût de la régression logistique. On cherche donc les thétas tel que les prédictions soient les plus proches possible des valeurs actuelles de Y. Pour minimiser la fonction de coût on utilise l'algorithme de déscente de gradient.



```r
LogRegClf <- glm(formula, data= pp_CreditTrain, family = binomial)
LogRegClf_pred <- predict(LogRegClf, pp_CreditTest, type="response")
LogRegClf_class  <- ifelse(LogRegClf_pred>=0.5, "DEFAULT", "NO_DEFAULT")
```

On observe un taux de succès de 100% sur le train set, c'est-à-dire que le modèle prédit parfaitement la classe de chaque observation. C'est un signe de sur-apprentissage, on voudrait plutôt savoir si le pouvoir explicatif du modèle reste aussi bon lorsqu'on a affaire à des observations non présentes dans le jeu de données. 
Sur le test set, on observe un taux de succès de 95.9%.



```
## Accuracy score on train set  Accuracy score on test set 
##                   1.0000000                   0.9534884
```

On va stocker les coefficients dans un data frame pour les comparer aux coefficients pénalisés plus tard.


```r
CoeffDf = data.frame(LogReg= LogRegClf$coefficients)
```



## Régression logistique avec de la pénalisation (Lasso/Ridge/ElasticNet) 

La régréssion logistique est très sensible au sur-apprentissage car les thétas optimaux obtenus minimisent la fonction de coût du train set. Lorsque le modèle est victime de sur-apprentissage, les prédictions sur un autre jeu de données ne seront pas bonnes. La pénalisation ajoute un terme supplémentaire à la fonction de coût. Plus lambda est élevé plus la fonction de coût sera pénalisée et plus les coefficients seront contraints à diminuer vers 0. Ces méthodes permettent de réduire le sur-apprentissage et peuvent même aller jusqu'à effectuer une sélection de variable dans le cas de Lasso.

Pour chaque modèle, on va effectuer une validation croisée 5-fold. Et pour prendre en compte le déséquilibre entre les classes, à chaque itération on va tirer un échantillonnage stratifié pour conserver la même proportion que le jeu de donnée initial et pour éviter les tirages sans clients dans la catégorie défaut.


```r
folds = 5
cvIndex <- createFolds(pp_CreditTrain$Y, folds, returnTrain = T)

fitControl <- trainControl(index = cvIndex, method = "cv", number = folds, classProbs = TRUE, summaryFunction = prSummary)
```

On crée une fonction pour afficher le taux de succès sur le test set et le train set. Le taux de succès étant la proportion de clients correctement classifiée par le modèle.


```r
print_score <- function(model, trainData ,testData) {
testScore = sum(predict(model, newdata = testData)==testData$Y)/nrow(testData)
trainScore = sum(predict(RidgeClf, newdata = trainData)==trainData$Y)/nrow(trainData)
print(c("Accuracy score on train set:" = testScore, "Accuracy score on test set:" = trainScore))
}
```


### Lasso

La méthode Lasso peut ramener à zero les coefficients des variables explicatives non pertinentes. La pénalisation, de type l1, est présente dans la fonction de coût comme la somme de la valeur absolue des coefficients. Pour utiliser Lasso avec glmnet, on fixe alpha à 1. On va chercher dans une grille de valeurs le lambda optimal.


```r
gridLasso = expand.grid(alpha=1, lambda=seq(0, 0.5, by = 0.01))

set.seed(2020)
LassoClf <- train(formula, data = pp_CreditTrain, method = "glmnet", 
                trControl = fitControl,
                 verbose = FALSE,
                 tuneGrid= gridLasso,
                metric = "F",
                family = "binomial",
                standardize = FALSE)

plot(LassoClf)  
```

![](classific_files/figure-html/unnamed-chunk-59-1.png)<!-- -->

Lorsque le paramètre lambda augmente, le F1-Score moyen, calculé par validation croisée, diminue. Le F1-Score est maximisé lorsque le lambda est égal à 0 et il est égal à 0.976.
Cependant, on est plus intéressé par la valeur du rappel. Donc on prendra le lambda égal à 0.05 car pour ce lambda le modèle est capable d'identifier parfaitement les clients faisant défaut tout en conservant la meilleure précision sous cette contrainte.


```r
head(LassoClf$results, 10)[,2:6]
```

```
##    lambda       AUC Precision    Recall         F
## 1    0.00 0.9901979 0.9724768 0.9812030 0.9767895
## 2    0.01 0.9863370 0.9439932 0.9882206 0.9655923
## 3    0.02 0.9829311 0.9309203 0.9962406 0.9624691
## 4    0.03 0.9633162 0.9271754 0.9987469 0.9616309
## 5    0.04 0.9614870 0.9270278 0.9997494 0.9620162
## 6    0.05 0.9614870 0.9268293 1.0000000 0.9620253
## 7    0.06 0.9614870 0.9268293 1.0000000 0.9620253
## 8    0.07 0.9614870 0.9268293 1.0000000 0.9620253
## 9    0.08 0.9614870 0.9268293 1.0000000 0.9620253
## 10   0.09 0.9614870 0.9268293 1.0000000 0.9620253
```


```r
set.seed(2020)
LassoClfOpt <- train(formula, data = pp_CreditTrain, method = "glmnet", 
                trControl = fitControl,
                 verbose = FALSE,
                 tuneGrid= expand.grid(alpha=1, lambda=c(0.04, 0.05, 0.06)),
                metric = "F",
                family = "binomial",
                standardize=FALSE)
```



Le taux de succès sur le train set baisse par rapport au cas sans pénalisation, ce qui est naturel. Cependant les prédictions du modèle sur le test set sont moins bonnes. Le modèle classifie correctement 95% des observations sur le test set alors que la régréssion logistique à un taux de succès de 95.3%.


#print_score(LassoClfOpt, pp_CreditTrain, pp_CreditTest)




```r
CoeffDf["Lasso"] = as.data.frame(as.matrix(coef(LassoClfOpt$finalModel, LassoClfOpt$bestTune$lambda)))
```

### Ridge

Ridge est une autre méthode de contraction des coefficients, la pénalisation est de type l2.


```r
gridRidge = expand.grid(alpha=0, lambda=seq(0, 0.05, by = 0.001))

set.seed(2020)
RidgeClf <- train(formula, data = pp_CreditTrain, method = "glmnet", 
                trControl = fitControl,
                 verbose = FALSE,
                 tuneGrid= gridRidge,
                metric = "F",
                family = "binomial",
                standardize=FALSE)

plot(RidgeClf) 
```

![](classific_files/figure-html/unnamed-chunk-63-1.png)<!-- -->

Le modèle Lasso est meilleur selon le critère du F1-Score. Comme pour le modèle Lasso, on choisira le lambda qui maximise le rappel sous la contrainte d'une precision la plus élevée possible. On trouve un lambda égal à 0.020. Sous ce lambda, la précision est de 94.79% et le rappel de 99%.



```r
RidgeClf$results[,2:6]
```

```
##    lambda       AUC Precision    Recall         F
## 1   0.000 0.9881369 0.9579006 0.9862155 0.9718448
## 2   0.001 0.9881369 0.9579006 0.9862155 0.9718448
## 3   0.002 0.9881369 0.9579006 0.9862155 0.9718448
## 4   0.003 0.9881369 0.9579006 0.9862155 0.9718448
## 5   0.004 0.9881369 0.9579006 0.9862155 0.9718448
## 6   0.005 0.9881369 0.9579006 0.9862155 0.9718448
## 7   0.006 0.9881369 0.9579006 0.9862155 0.9718448
## 8   0.007 0.9881369 0.9579006 0.9862155 0.9718448
## 9   0.008 0.9881369 0.9579006 0.9862155 0.9718448
## 10  0.009 0.9881369 0.9579006 0.9862155 0.9718448
## 11  0.010 0.9881291 0.9579006 0.9862155 0.9718448
## 12  0.011 0.9880506 0.9579308 0.9869674 0.9722250
## 13  0.012 0.9879740 0.9574649 0.9869674 0.9719851
## 14  0.013 0.9879123 0.9553913 0.9874687 0.9711604
## 15  0.014 0.9878520 0.9544749 0.9877193 0.9708087
## 16  0.015 0.9878057 0.9531159 0.9882206 0.9703451
## 17  0.016 0.9877329 0.9517599 0.9887218 0.9698830
## 18  0.017 0.9877124 0.9513199 0.9892231 0.9698978
## 19  0.018 0.9876586 0.9511259 0.9899749 0.9701579
## 20  0.019 0.9876231 0.9495302 0.9899749 0.9693253
## 21  0.020 0.9875817 0.9479505 0.9902256 0.9686206
## 22  0.021 0.9875617 0.9475156 0.9907268 0.9686354
## 23  0.022 0.9875256 0.9462016 0.9917293 0.9684277
## 24  0.023 0.9874939 0.9453220 0.9922306 0.9682055
## 25  0.024 0.9874584 0.9444350 0.9924812 0.9678588
## 26  0.025 0.9874065 0.9439996 0.9927318 0.9677487
## 27  0.026 0.9873704 0.9433559 0.9932331 0.9676468
## 28  0.027 0.9873506 0.9424708 0.9934837 0.9673008
## 29  0.028 0.9873086 0.9424967 0.9939850 0.9675530
## 30  0.029 0.9872620 0.9415982 0.9939850 0.9670804
## 31  0.030 0.9872381 0.9404795 0.9939850 0.9664911
## 32  0.031 0.9872238 0.9395871 0.9939850 0.9660204
## 33  0.032 0.9871769 0.9382832 0.9944862 0.9655669
## 34  0.033 0.9871499 0.9370308 0.9957393 0.9654925
## 35  0.034 0.9871224 0.9363994 0.9962406 0.9653918
## 36  0.035 0.9871009 0.9359734 0.9964912 0.9652836
## 37  0.036 0.9870681 0.9357531 0.9964912 0.9651664
## 38  0.037 0.9870483 0.9353108 0.9964912 0.9649313
## 39  0.038 0.9870343 0.9348701 0.9964912 0.9646970
## 40  0.039 0.9870060 0.9348853 0.9967419 0.9648226
## 41  0.040 0.9869944 0.9342265 0.9967419 0.9644720
## 42  0.041 0.9869833 0.9335840 0.9969925 0.9642466
## 43  0.042 0.9869606 0.9329429 0.9972431 0.9640213
## 44  0.043 0.9869501 0.9325218 0.9974937 0.9639137
## 45  0.044 0.9869230 0.9323040 0.9974937 0.9637971
## 46  0.045 0.9868994 0.9320867 0.9974937 0.9636806
## 47  0.046 0.9868593 0.9312129 0.9974937 0.9632138
## 48  0.047 0.9868358 0.9301408 0.9977444 0.9627566
## 49  0.048 0.9868061 0.9299240 0.9977444 0.9626403
## 50  0.049 0.9868056 0.9299240 0.9977444 0.9626403
## 51  0.050 0.9867731 0.9295230 0.9982456 0.9626587
```



```r
set.seed(2020)
RidgeClfOpt <- train(formula, data = pp_CreditTrain, method = "glmnet", 
                trControl = fitControl,
                 verbose = FALSE,
                 tuneGrid= expand.grid(alpha=0, lambda=c(0.019, 0.020, 0.021)),
                metric = "F",
                family = "binomial",
                standardize=FALSE)
```


Le taux de succès sur le test set est le même que pour la méthode Lasso. Le taux de succès sur le train set est quand à lui plus élevé que celui de la méthode Lasso.
On préferera la méthode Lasso à la méthode Ridge, car Lasso atteint un même score sur le test set avec un nombre de variables explicatives inférieures. Le modèle Lasso est plus simple à priori.



```r
print_score(RidgeClfOpt, pp_CreditTrain, pp_CreditTest)
```

```
## Accuracy score on train set:  Accuracy score on test set: 
##                    0.9348837                    0.9500581
```

```r
CoeffDf["Ridge"] = as.data.frame(as.matrix(coef(RidgeClfOpt$finalModel, RidgeClfOpt$bestTune$lambda)))
```

### Elastic Net

Elastic Net est une méthode de contraction de coefficients qui combine les pénalisations de Lasso et Ridge. On a un hyperparamètre supplémentaire à optimiser, il s'agit du paramètre alpha compris entre 0 et 1. Lorsque alpha est proche de 0, la pénalisation l2 (Ridge) est plus importante et lorsque alpha est proche de 1, la pénalisation l1 devient plus importante. On laisse la fonction glmnet et la fonction train de caret nous trouver les paramètres lambda et alpha optimaux.



```r
set.seed(2020)
ENClf <- train(formula, data = pp_CreditTrain, method = "glmnet", 
                trControl = fitControl,
                 verbose = FALSE,
                metric = "F",
                family = "binomial",
               standardize=FALSE,
               tuneLength = 10)
```

Le choix des hyperparamétres lambda et alpha s'opère de la même manière que précédemment. On trouve un lambda égal à 0.05 et un alpha de 0.3.


```r
ENClf$results[,1:6][25:30,]
```

```
##    alpha      lambda       AUC Precision    Recall         F
## 25   0.3 0.001798379 0.9891957 0.9671724 0.9812030 0.9741193
## 26   0.3 0.004154490 0.9886270 0.9643263 0.9819549 0.9730472
## 27   0.3 0.009597410 0.9875111 0.9558316 0.9867168 0.9710202
## 28   0.3 0.022171262 0.9860432 0.9371619 0.9942356 0.9648534
## 29   0.3 0.051218493 0.9843122 0.9270448 1.0000000 0.9621414
## 30   0.3 0.118321365 0.9643913 0.9268293 1.0000000 0.9620253
```



```r
set.seed(2020)
ENClfOpt <- train(formula, data = pp_CreditTrain, method = "glmnet", 
                trControl = fitControl,
                 verbose = FALSE,
                 tuneGrid= expand.grid(alpha=0.3, lambda=c(0.05, 0.051)),
                metric = "F",
                family = "binomial",
                standardize=FALSE)
```

On trouve un taux de succès sur le test set identique au taux de succès de Lasso et de Rdige.



```r
print_score(ENClfOpt, pp_CreditTrain, pp_CreditTest)
```

```
## Accuracy score on train set:  Accuracy score on test set: 
##                    0.9255814                    0.9500581
```

```r
CoeffDf["ElasticNet"] = as.data.frame(as.matrix(coef(ENClf$finalModel, ENClf$bestTune$lambda)))
```

### Interprétation et sélection du meilleur modèle

Le nombre de variables explicatives est égal à 71.


```r
nrow(CoeffDf)
```

```
## [1] 71
```

Les méthodes Lasso et Elastic Net peuvent faire de la sélection de variables en réduisant le coefficients à 0 de certaines variables. On observe que le modèle Lasso ne comporte qu'une seule variable explicative et il est capable de prédire aussi bien que les autres modèles la probabilité de faire défaut. Le modèle Elastic Net mets à 0 les coefficients de 19 variables explicatives.



```r
c(NbrElasicNet= sum(abs(CoeffDf$ElasticNet)>0), NbrLasso= sum(abs(CoeffDf$Lasso)>0)) -1
```

```
## NbrElasicNet     NbrLasso 
##           52            1
```


On montre pour l'instant uniquement les coefficients de la régression logistique supérieur à 10 en valeur absolue.
Le coefficient de la régression logistique le plus élevé en valeur absolue est le coefficient associé à la variable `Prod_Closed_DateYear2013`. Il est de signe négatif. Cela signifie qu'avoir une date de cloture fixée en 2013 réduit la probabilité de faire défaut. On voit que ce coefficient reste négatif quelque soit la pénalisation.

Les coefficients assossiés au dates de cloture en mois sont relativement élevés en valeur absolue, ce qui peut être un signe de sur-apprentissage. Leurs coefficients sont réduit lorsqu'on utilise la méthode Ridge et Elastic Net.
La méthode Lasso et Elastic Net fixe à 0 certains coefficients comme `Type_Of_ResidenceOwned`.

On remarque que les coefficients du modèle Ridge, Lasso et Elastic Net sont bien plus petits que les coefficients de la régréssion logistique, ce qui est l'effet de la pénalisation.


```r
CoeffDf[abs(CoeffDf)>10, ][-1,]
```

```
##                              LogReg Lasso        Ridge  ElasticNet
## Prod_Sub_CategoryP        -10.91924     0 -0.020668230  0.00000000
## Type_Of_ResidenceNew rent  13.76374     0  0.031193257  0.06952058
## Type_Of_ResidenceOld rent  13.66698     0  0.013362553  0.00000000
## Type_Of_ResidenceOwned     13.75128     0 -0.038951094  0.00000000
## Type_Of_ResidenceParents   13.65828     0 -0.004768575  0.00000000
## Prod_CategoryJ            -13.85992     0 -0.032797702 -0.84393864
## Prod_Closed_DateYear2012  -17.69123     0  0.464365688  0.00000000
## Prod_Closed_DateYear2013  -36.87496     0 -0.467995612 -2.27182471
## Prod_Closed_DateMonth08   -11.35581     0 -0.022346009 -0.72178319
## Prod_Closed_DateMonth09   -11.83808     0  0.055288606  0.00000000
## Prod_Closed_DateMonth10   -13.75631     0  0.046404169 -0.35390739
## Prod_Closed_DateMonth11   -14.30293     0  0.201500155  0.32657440
## Prod_Closed_DateMonth12   -16.93985     0  0.004257502 -0.70298247
## Prod_Decision_DateMonth05 -10.78055     0  0.037488249 -0.32105884
## NA                               NA    NA           NA          NA
```

On observe que l'unique variable non égale à 0 dans le modèle Lasso est la durée du crédit. Plus la durée du crédit est longue moins le client sera enclin à faire défaut. 
Avec ce tableau, on voit bien que toutes les analyses dans la première partie sont vérifiées par le signe et la magnitude des coefficients.



```r
CoeffDf[abs(CoeffDf$ElasticNet)>0.15, ][-1,]
```

```
##                                      LogReg      Lasso        Ridge ElasticNet
## Educational_LevelMaster/PhD     0.589547174  0.0000000 -0.019205226  0.1653880
## Years_At_Residence             -0.141953430  0.0000000 -0.083026339 -0.1637475
## Prod_Sub_CategoryG             -0.369733663  0.0000000 -0.096350870 -0.2889062
## SourceSales                     0.628988832  0.0000000  0.123867545  0.4905196
## Nb_Of_Products                 -0.827468231  0.0000000 -0.467806861 -0.8445477
## Prod_CategoryC                  0.412164816  0.0000000 -0.077717624 -0.1697841
## Prod_CategoryH                  2.894660134  0.0000000  0.041980663  1.5569034
## Prod_CategoryJ                -13.859921174  0.0000000 -0.032797702 -0.8439386
## Prod_CategoryK                  0.952505809  0.0000000  0.013802060  0.3745399
## Prod_CategoryL                  2.687898649  0.0000000  0.346080588  1.9587459
## Prod_CategoryM                 -0.003081148  0.0000000 -0.014180319 -0.2779166
## Prod_Closed_Date_NATRUE        -1.813420408  0.0000000 -0.661053141 -2.6530904
## Age                            -0.205058575  0.0000000 -0.169944148 -0.1658901
## CustomerOpenDateYear2006-2010   0.015115771  0.0000000 -0.157199663 -0.1929818
## CustomerOpenDateYear>2010       0.708112454  0.0000000  0.209609442  0.5418052
## CustomerOpenDateMonth03        -1.025688636  0.0000000 -0.109403376 -0.6504321
## CustomerOpenDateMonth05         0.490091226  0.0000000  0.078876727  0.4069489
## CustomerOpenDateMonth06        -0.393560340  0.0000000 -0.019100780 -0.4873057
## CustomerOpenDateMonth08        -0.464127824  0.0000000 -0.037554242 -0.5513879
## CustomerOpenDateMonth10        -0.484091504  0.0000000 -0.004951432 -0.2724954
## CustomerOpenDateMonth11         0.619846753  0.0000000  0.046954674  0.5381087
## Credit_Duration                 5.785042541 -0.5493905 -0.658086241 -0.1652774
## Prod_Closed_DateYear2013      -36.874960684  0.0000000 -0.467995612 -2.2718247
## Prod_Closed_DateMonth02        -1.048788399  0.0000000  0.074918977  0.7591585
## Prod_Closed_DateMonth03        -2.277342174  0.0000000  0.092396334  0.8205228
## Prod_Closed_DateMonth04        -4.187629659  0.0000000  0.032101766  0.3475931
## Prod_Closed_DateMonth08       -11.355812085  0.0000000 -0.022346009 -0.7217832
## Prod_Closed_DateMonth10       -13.756313931  0.0000000  0.046404169 -0.3539074
## Prod_Closed_DateMonth11       -14.302934362  0.0000000  0.201500155  0.3265744
## Prod_Closed_DateMonth12       -16.939852732  0.0000000  0.004257502 -0.7029825
## Prod_Decision_DateYear2012      1.301575904  0.0000000 -0.133355613  0.4592961
## Prod_Decision_DateMonth02       1.350651300  0.0000000 -0.025021336 -0.1532039
## Prod_Decision_DateMonth04       4.595070050  0.0000000 -0.032038850  0.2473045
## Prod_Decision_DateMonth05     -10.780552653  0.0000000  0.037488249 -0.3210588
## Prod_Decision_DateMonth06      -9.331073942  0.0000000 -0.001421433 -0.1563386
## Prod_Decision_DateMonth08      -5.820899555  0.0000000  0.027850186  0.4980565
## Prod_Decision_DateMonth10      -2.299485346  0.0000000  0.055978595  0.7073769
## Prod_Decision_DateMonth12                NA  0.0000000  0.016080446  0.4254328
```

La méthode Lasso est capable de prédire la probabilité de défaut en utilisant que la durée de crédit, on ne prendra pas en compte ce modèle car il est jugé trop simpliste.

On pourrait considérer la méthode Elastic Net car même si la qualité de ses prédictions sur le test set est un peu plus bas que celle de la régréssion logistique, la méthode Elastic Net simplifie fortement le modèle car on passe de 71 variables à seulement à 52. Le taux de succès de cette méthode sur le test set est de 95%.

Cependant, on remarque que le modèle Ridge a un recall de 0.943%, ce qui est supérieur aux recall des autres modèles.
Etant donné son taux de succès de 95% et qu'il réduit certains coefficients vers 0, on le préferera aux autres modèles.


```r
recall(pp_CreditTest$Y, predict(ENClfOpt, newdata = pp_CreditTest)) 
```

```
## [1] 0.9273066
```

```r
recall(pp_CreditTest$Y, predict(RidgeClfOpt, newdata = pp_CreditTest)) 
```

```
## [1] 0.9435407
```

```r
recall(pp_CreditTest$Y, predict(LassoClfOpt, newdata = pp_CreditTest)) 
```

```
## [1] 0.9274419
```

## KNN

La méthode des k plus proches voisins est un algorithme basé sur la distance entre les observations. Ainsi, dans le cas de la classification, pour chaque observation que l'on cherchera à prédire, l'algorithme déterminera tous les voisins qui sont assez proches, selon le critère de distance choisi et ensuite il prédira la classe majoritaire.

Il est également important de signaler que l’on devra choisir un K ni trop élevé ni trop faible car dans un cas on aura un mauvais classifieur puisqu' il regroupe des individus qui ne se ressemblent pas et dans un autre cas on aura un classifieur qui ne joue plus la fonction de classifieur vu qu'il ne regrouperait que peu d’individus. Le choix de K est donc primordial et se décide par validation croisée.


```r
set.seed(2020)
KNNgrid = expand.grid(k = 1:50)
KNNClf <- train(formula, data = pp_CreditTrain, method = "knn", 
                trControl = fitControl,
                tuneGrid= KNNgrid,
                metric = "F")

plot(KNNClf)
```

![](classific_files/figure-html/unnamed-chunk-76-1.png)<!-- -->

On choisit k sous les mêmes conditions que précédemment. On trouve k = 50.


```r
KNNClf$results[,1:5]
```

```
##     k        AUC Precision    Recall         F
## 1   1 0.03242110 0.9613520 0.9656642 0.9634879
## 2   2 0.05692724 0.9650258 0.9666667 0.9658208
## 3   3 0.07095136 0.9663371 0.9764411 0.9713299
## 4   4 0.08510875 0.9663406 0.9774436 0.9718455
## 5   5 0.09449957 0.9639890 0.9784461 0.9711424
## 6   6 0.10885986 0.9630624 0.9794486 0.9711680
## 7   7 0.12299077 0.9642413 0.9786967 0.9713906
## 8   8 0.13282018 0.9640768 0.9799499 0.9719188
## 9   9 0.14413843 0.9645268 0.9796992 0.9720220
## 10 10 0.15797349 0.9635870 0.9802005 0.9717881
## 11 11 0.17207448 0.9640293 0.9796992 0.9717746
## 12 12 0.18393270 0.9628290 0.9794486 0.9710447
## 13 13 0.19874530 0.9637448 0.9784461 0.9710143
## 14 14 0.20859392 0.9633013 0.9791980 0.9711585
## 15 15 0.22116081 0.9633238 0.9799499 0.9715463
## 16 16 0.22914462 0.9635630 0.9804511 0.9719208
## 17 17 0.23784100 0.9630971 0.9807018 0.9718076
## 18 18 0.24921753 0.9631032 0.9809524 0.9719355
## 19 19 0.25796470 0.9628598 0.9809524 0.9718112
## 20 20 0.26662339 0.9621400 0.9807018 0.9713220
## 21 21 0.27629992 0.9619776 0.9824561 0.9720978
## 22 22 0.28296366 0.9617021 0.9814536 0.9714677
## 23 23 0.29264106 0.9617179 0.9817043 0.9715947
## 24 24 0.29956170 0.9621933 0.9817043 0.9718387
## 25 25 0.30734484 0.9624328 0.9817043 0.9719580
## 26 26 0.31477527 0.9626983 0.9822055 0.9723367
## 27 27 0.32171800 0.9624286 0.9814536 0.9718296
## 28 28 0.32991383 0.9631017 0.9807018 0.9718072
## 29 29 0.33810103 0.9614663 0.9814536 0.9713481
## 30 30 0.34608229 0.9619376 0.9812030 0.9714624
## 31 31 0.35639937 0.9624114 0.9814536 0.9718273
## 32 32 0.36361173 0.9614532 0.9807018 0.9709649
## 33 33 0.37364576 0.9619428 0.9814536 0.9715864
## 34 34 0.38079578 0.9631099 0.9812030 0.9720607
## 35 35 0.38730211 0.9614675 0.9817043 0.9714729
## 36 36 0.39745973 0.9619627 0.9822055 0.9719720
## 37 37 0.40516806 0.9617680 0.9832080 0.9723624
## 38 38 0.41239849 0.9612867 0.9829574 0.9719934
## 39 39 0.41900202 0.9617353 0.9822055 0.9718520
## 40 40 0.42386581 0.9612907 0.9824561 0.9717416
## 41 41 0.42984822 0.9608033 0.9822055 0.9713717
## 42 42 0.43724687 0.9610210 0.9819549 0.9713646
## 43 43 0.44438425 0.9603389 0.9822055 0.9711332
## 44 44 0.45038738 0.9598311 0.9814536 0.9705071
## 45 45 0.45829344 0.9600688 0.9817043 0.9707556
## 46 46 0.46374763 0.9600969 0.9824561 0.9711382
## 47 47 0.46872568 0.9605618 0.9822055 0.9712526
## 48 48 0.47760327 0.9603038 0.9817043 0.9708764
## 49 49 0.48415717 0.9593854 0.9822055 0.9706498
## 50 50 0.49111252 0.9591505 0.9822055 0.9705281
```


```r
set.seed(2020)
KNNgrid = expand.grid(k = 49:51)
KNNClfOpt <- train(formula, data = pp_CreditTrain, method = "knn", 
                trControl = fitControl,
                tuneGrid= KNNgrid,
                metric = "F")
```

On trouve un taux de succès de 95%.



```r
print_score(KNNClfOpt, pp_CreditTrain, pp_CreditTest)
```

```
## Accuracy score on train set:  Accuracy score on test set: 
##                    0.9423256                    0.9500581
```

Le recall est de 95.6%. Ce qui est supérieur aux modèles précédents. Ce modèle semble plus intéréssant.



```r
recall(pp_CreditTest$Y, predict(KNNClfOpt, newdata = pp_CreditTest)) 
```

```
## [1] 0.9560976
```


## SVM

### SVM linéaire

Les SVM peuvent être utiles pour résoudre des problèmes de discriminations. Ils sont connus pour être très performants car ils peuvent être linéaires ou non-linéaires. L’idée des SVM est de trouver une surface de séparation afin de pouvoir classifier les observations.
Pour les classifications on peut séparer les deux classes par un hyperplan. Pour trouver la qualité de cet hyperplan on peut mesurer la distance entre les données d'apprentissage et l’hyperplan de séparation. Ainsi les SVM linéaires cherchent à maximiser la marge qui n’est autre que la distance entre le plus proche individus et l’hyperplan de séparation.  



```r
SVMLinearClf <- train(formula, data = pp_CreditTrain,
             method="svmLinear",trControl=fitControl,
             tuneGrid=data.frame(C=seq(1,10,1)),
             metric = "F")

plot(SVMLinearClf)
```

![](classific_files/figure-html/unnamed-chunk-81-1.png)<!-- -->

On choisit un C égal à 7.


```r
SVMLinearClf$results[,1:5]
```

```
##     C       AUC Precision    Recall         F
## 1   1 0.9872008 0.9609917 0.9872180 0.9739148
## 2   2 0.9875927 0.9617203 0.9872180 0.9742778
## 3   3 0.9880560 0.9610808 0.9882206 0.9744284
## 4   4 0.9881165 0.9618197 0.9902256 0.9758007
## 5   5 0.9881241 0.9618116 0.9899749 0.9756737
## 6   6 0.9882952 0.9602052 0.9894737 0.9745829
## 7   7 0.9883345 0.9622289 0.9887218 0.9752827
## 8   8 0.9883597 0.9629809 0.9889724 0.9757748
## 9   9 0.9883944 0.9608704 0.9897243 0.9750651
## 10 10 0.9883542 0.9613070 0.9889724 0.9749266
```



```r
SVMLinearClfOpt <- train(formula, data = pp_CreditTrain,
             method="svmLinear",trControl=fitControl,
             tuneGrid=data.frame(C=c(6.9, 7.1)),
             metric = "F")
```



```r
print_score(SVMLinearClfOpt, pp_CreditTrain, pp_CreditTest)
```

```
## Accuracy score on train set:  Accuracy score on test set: 
##                    0.9534884                    0.9500581
```

Ayant un même taux de succès que les modèles précédents, on préferera pour l'instant ce modèle car il a le recall le plus élevé.


```r
recall(pp_CreditTest$Y, predict(SVMLinearClfOpt, newdata = pp_CreditTest)) 
```

```
## [1] 0.963761
```



## SVM non linéaire (gaussian)


```r
radialGrid = expand.grid(C = c(c(2,4,8,15)), sigma = c(0, 0.1, 0.5, 1, 5))

SVMradClf <- train(formula, data = pp_CreditTrain,
             method="svmRadial",trControl=fitControl,
             tuneGrid = radialGrid,
             metric = "F")

plot(SVMradClf)
```

![](classific_files/figure-html/unnamed-chunk-86-1.png)<!-- -->

On choisit comme hyperparamètre sigma égal à 0.1 et C égal à 2.


```r
SVMradClf$bestTune
```

```
##   sigma C
## 2   0.1 2
```



```r
print_score(SVMradClf, pp_CreditTrain, pp_CreditTest)
```

```
## Accuracy score on train set:  Accuracy score on test set: 
##                    0.9339535                    0.9500581
```

Le recall est moins bon que celui du modèle SVM linéaire.


```r
recall(pp_CreditTest$Y, predict(SVMradClf, newdata = pp_CreditTest)) 
```

```
## [1] 0.9392789
```

# Conclusion

Après une analyse graphique approfondie des données, nous avons essayé d'identifier les variables pouvant affecter la probabilité de défaut. 

Pour la sélection du modèle, nous nous basons sur le F1-Score et plus précisémment le rappel, qui doit être proche de 1, tout en ayant un taux de succès élevé.

Nous avons effectué de la validation croisée 5-fold, pour chaque modèle, afin de trouver les hyperparamètres qui maximisent le F1-Score sur le train set. Puis on a relevé la capacité de ces modèles à prédire des données non observées en examinant le taux de succès sur le test set.

Nous avons pu confirmer ensuite, la totalité des hypothèses émises de la partie analyse concernant le lien entre les variables explicatives et la probabilité de défaut. Nous avons vu que le modèle Lasso était capable d'expliquer correctement la probabilité de défaut avec uniquement une variable (durée du crédit). Ce modèle étant trop simpliste et éloigné de la réalité, nous l'avons rejeté.

Nous avons choisi le modèle SVM linéaire puisqu'il présente le plus haut rappel parmis tous les modèles étudiés, avec un taux de succès sur le test set identique aux autres.

Dans le contexte d'une banque, c'est le modèle SVM linéaire qui semble le plus efficace pour prédire la probabilité de défaut d'un client.

le Rappel du modèle SVM linéaire est égal à 0.963.


```r
recall(pp_CreditTest$Y, predict(SVMLinearClfOpt, newdata = pp_CreditTest)) 
```

```
## [1] 0.963761
```


```r
print_score(KNNClfOpt, pp_CreditTrain, pp_CreditTest)
```

```
## Accuracy score on train set:  Accuracy score on test set: 
##                    0.9423256                    0.9500581
```



