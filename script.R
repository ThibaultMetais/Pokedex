## Script pour l'analyse descriptive des donnees des pokemons et de leurs combats

##########################################################################################
### 0 ### Setup

# libraries
library(vioplot)
library(ggplot2)
library(gridExtra)
library(ggrepel)
library(scales)
library(tidyverse)
library(RColorBrewer)
library(plotly)

# chargement de la table des pokemons
tablePokemon <- read.csv("pokemon.csv", header = T, row.names = 1, na.strings = "")

# chargement de la table des combats
tableCombats <- read.csv("combats.csv", header = T, na.strings = "")

# vecteur de couleurs pour l'affichage par types
typeColors <- c("#B2DF8A", "#000000", "#B15928", "#FFFF33", "#0090ff", "#E31A1C", "#FF7F00",
                "#CAB2D6", "#ee00ff", "#057700", "#FDC086", "#A6CEE3", "#1B9E77", "#3fff00",
                "#F781BF", "#666666", "#6A3D9A", "#084da8", "#ffffff")
typeColorsList <- list(c(0, "#B2DF8A"), c(0.05882353, "#000000"), c(0.11764706 , "#B15928"), c(0.17647059, "#FFFF33"), 
                       c(0.23529412, "#0090ff"), c(0.29411765, "#E31A1C"), c(0.35294118, "#FF7F00"), c(0.41176471, "#CAB2D6"), 
                       c(0.47058824, "#ee00ff"), c(0.52941176, "#057700"), c(0.58823529, "#FDC086"), c(0.64705882, "#A6CEE3"), 
                       c(0.70588235, "#1B9E77"), c(0.76470588, "#3fff00"), c(0.82352941, "#F781BF"), c(0.88235294, "#666666"), 
                       c(0.94117647, "#6A3D9A"), c(1, "#084da8"), c(1, "#ffffff"))
show_col(typeColors)

##########################################################################################
### 1 ### Nettoyage des donnees 

## POKEMONS --------------------------------------------------------------
# casting des colonnes dans le bon type
tablePokemon$Name <- as.character(tablePokemon$Name)
tablePokemon$Legendary <- as.logical(tablePokemon$Legendary)
tablePokemon$Generation <- as.factor(tablePokemon$Generation)

# remplacement des NA dans la colonne Type.2 par des "None" pour les visualiser
levels(tablePokemon$Type.2) <- c(levels(tablePokemon$Type.2), "None")
tablePokemon[is.na(tablePokemon)] <- "None"

# separation des variables qualitatives et quantitatives
tablePokemonQualit <- tablePokemon[, c(1, 2, 3, 10, 11)]
tablePokemonQuantit <- tablePokemon[, 4:10]

## COMBATS -------------------------------------------------------------------------
# TOUT EST OK
for (poke in 1:nrow(tablePokemon)) {
  fought <- nrow(tableCombats[tableCombats$First_pokemon == poke | tableCombats$Second_pokemon == poke, ])
  won <- nrow(tableCombats[tableCombats$Winner == poke, ])
  if (fought == 0) {
    print(poke)
  }
  tablePokemon$victories[poke] <- won / fought
}

## COMBATS + POKEMONS --------------------------------------------------------------
# fusion des données des 2 tables 
# tableCombatsFull <- data.frame(matrix(ncol=16, nrow=0))
# 
# for (i in 1:nrow(tableCombats)) {
#   
#   print(i)
#   
#   if (tableCombats[i,]$Winner == tableCombats[i,]$First_pokemon) {
#     winner <- 1
#     loser <- 2
#   } else {
#     winner <- 2
#     loser <- 1
#   }
#   
#   quali <- cbind(tablePokemon[tableCombats[i, winner], c(1:3, 10, 11)], tablePokemon[tableCombats[i, loser], c(1:3, 10, 11)])
#   quanti <- tablePokemon[tableCombats[i, winner], 4:9] - tablePokemon[tableCombats[i, loser], 4:9]
#   
#   tableCombatsFull <- rbind(tableCombatsFull, cbind(quali, quanti))
#   
# }
# 
# colnames(tableCombatsFull) <- c("NameW", "Type1W", "Type2W", "GenerationW", "LegendaryW", 
#                                 "NameL","Type1L", "Type2L", "GenerationL", "LegendaryL",
#                                 "Diff.HP", "Diff.Attack", "Diff.Defence", "Diff.Sp.Atk", "Diff.Sp.Def", "Diff.Speed")
# 
# write.csv(tableCombatsFull, file="tableCombatsFull.csv")

tableCombatsFull <- read.csv("tableCombatsFull.csv")

##########################################################################################
### 2 ### Analyse descriptive

# summary
summary(tablePokemon)
summary(tablePokemon$Type.1)
summary(tablePokemon$Type.2)

# tests de correlation
plot(tablePokemon[,4:9])

shapiro.test(tablePokemon$HP)
shapiro.test(tablePokemon$Attack)
shapiro.test(tablePokemon$Defense)
shapiro.test(tablePokemon$Sp..Atk)
shapiro.test(tablePokemon$Sp..Def)
shapiro.test(tablePokemon$Speed)

cor(tablePokemon[,4:9], method = "kendall")

# comparaison des variables quantitatives selon les générations
p1 <- ggplot(tablePokemon, aes(x=Generation, y=HP, color=Generation)) + 
  geom_violin() +
  geom_boxplot(width=0.1)+
  theme_classic() +
  theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 18), axis.text.y = element_text(size = 18),
        legend.title=element_text(size=20), legend.text=element_text(size=18))

p2 <- ggplot(tablePokemon, aes(x=Generation, y=Attack, color=Generation)) + 
  geom_violin() +
  geom_boxplot(width=0.1)+
  theme_classic() +
  theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 18), axis.text.y = element_text(size = 18),
        legend.title=element_text(size=20), legend.text=element_text(size=18))

p3 <- ggplot(tablePokemon, aes(x=Generation, y=Defense, color=Generation)) + 
  geom_violin() +
  geom_boxplot(width=0.1)+
  theme_classic() +
  theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 18), axis.text.y = element_text(size = 18),
        legend.title=element_text(size=20), legend.text=element_text(size=18))

p4 <- ggplot(tablePokemon, aes(x=Generation, y=Sp..Atk, color=Generation)) + 
  geom_violin() +
  geom_boxplot(width=0.1)+
  theme_classic() +
  theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 18), axis.text.y = element_text(size = 18),
        legend.title=element_text(size=20), legend.text=element_text(size=18))

p5 <- ggplot(tablePokemon, aes(x=Generation, y=Sp..Def, color=Generation)) + 
  geom_violin() +
  geom_boxplot(width=0.1)+
  theme_classic() +
  theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 18), axis.text.y = element_text(size = 18),
        legend.title=element_text(size=20), legend.text=element_text(size=18))

p6 <- ggplot(tablePokemon, aes(x=Generation, y=Speed, color=Generation)) + 
  geom_violin() +
  geom_boxplot(width=0.1)+
  theme_classic() +
  theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 18), axis.text.y = element_text(size = 18),
        legend.title=element_text(size=20), legend.text=element_text(size=18))

pdf("repartition_selon_Génération.pdf", height = 20, width = 10)
grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 3)
dev.off()

## test de Kruskal Wallis
kruskal.test(HP ~ Generation, data = tablePokemon)
kruskal.test(Attack ~ Generation, data = tablePokemon)
kruskal.test(Defense ~ Generation, data = tablePokemon)
kruskal.test(Sp..Atk ~ Generation, data = tablePokemon)
kruskal.test(Sp..Def ~ Generation, data = tablePokemon)
kruskal.test(Speed ~ Generation, data = tablePokemon)

# - -------------------------------------------------------------------
# comparaison des variables quantitatives selon les types 1
p1 <- ggplot(tablePokemon, aes(x=Type.1, y=HP, color=Type.1)) + 
  geom_violin() +
  geom_boxplot(width=0.1) +
  scale_color_manual(values=typeColors)+
  theme_classic() +
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 14, angle = 60,  hjust=1), axis.text.y = element_text(size = 14),
        legend.title=element_text(size=16), legend.text=element_text(size=14))

p2 <- ggplot(tablePokemon, aes(x=Type.1, y=Attack, color=Type.1)) + 
  geom_violin() +
  geom_boxplot(width=0.1) +
  scale_color_manual(values=typeColors)+
  theme_classic() +
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 14, angle = 60,  hjust=1), axis.text.y = element_text(size = 14),
        legend.title=element_text(size=16), legend.text=element_text(size=14))

p3 <- ggplot(tablePokemon, aes(x=Type.1, y=Defense, color=Type.1)) + 
  geom_violin() +
  geom_boxplot(width=0.1) +
  scale_color_manual(values=typeColors)+
  theme_classic() +
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 14, angle = 60,  hjust=1), axis.text.y = element_text(size = 14),
        legend.title=element_text(size=16), legend.text=element_text(size=14))

p4 <- ggplot(tablePokemon, aes(x=Type.1, y=Sp..Atk, color=Type.1)) + 
  geom_violin() +
  geom_boxplot(width=0.1) +
  scale_color_manual(values=typeColors)+
  theme_classic() +
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 14, angle = 60,  hjust=1), axis.text.y = element_text(size = 14),
        legend.title=element_text(size=16), legend.text=element_text(size=14))

p5 <- ggplot(tablePokemon, aes(x=Type.1, y=Sp..Def, color=Type.1)) + 
  geom_violin() +
  geom_boxplot(width=0.1) +
  scale_color_manual(values=typeColors)+
  theme_classic() +
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 14, angle = 60,  hjust=1), axis.text.y = element_text(size = 14),
        legend.title=element_text(size=16), legend.text=element_text(size=14))

p6 <- ggplot(tablePokemon, aes(x=Type.1, y=Speed, color=Type.1)) + 
  geom_violin() +
  geom_boxplot(width=0.1) +
  scale_color_manual(values=typeColors)+
  theme_classic() +
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 14, angle = 60,  hjust=1), axis.text.y = element_text(size = 14),
        legend.title=element_text(size=16), legend.text=element_text(size=14))

pdf("repartition_selon_Type1_1.pdf", height = 25, width = 10)
grid.arrange(p1, p2, p3, nrow = 3)
dev.off()

pdf("repartition_selon_Type1_2.pdf", height = 25, width = 10)
grid.arrange(p4, p5, p6, nrow = 3)
dev.off()

## test de Kruskal Wallis
kruskal.test(HP ~ Type.1, data = tablePokemon)
kruskal.test(Attack ~ Type.1, data = tablePokemon)
kruskal.test(Defense ~ Type.1, data = tablePokemon)
kruskal.test(Sp..Atk ~ Type.1, data = tablePokemon)
kruskal.test(Sp..Def ~ Type.1, data = tablePokemon)
kruskal.test(Speed ~ Type.1, data = tablePokemon)

kruskal.test(HP ~ Type.2, data = tablePokemon)
kruskal.test(Attack ~ Type.2, data = tablePokemon)
kruskal.test(Defense ~ Type.2, data = tablePokemon)
kruskal.test(Sp..Atk ~ Type.2, data = tablePokemon)
kruskal.test(Sp..Def ~ Type.2, data = tablePokemon)
kruskal.test(Speed ~ Type.2, data = tablePokemon)

# on refait les tests en se restraignant aux types avec au moins 20 individus
subTable <- subset(tablePokemon, tablePokemon$Type.1 != "Fairy" & tablePokemon$Type.1 != "Flying")
kruskal.test(HP ~ Type.1, data = subTable)
kruskal.test(Attack ~ Type.1, data = subTable)
kruskal.test(Defense ~ Type.1, data = subTable)
kruskal.test(Sp..Atk ~ Type.1, data = subTable)
kruskal.test(Sp..Def ~ Type.1, data = subTable)
kruskal.test(Speed ~ Type.1, data = subTable)

kruskal.test(HP ~ Type.2, data = subTable)
kruskal.test(Attack ~ Type.2, data = subTable)
kruskal.test(Defense ~ Type.2, data = subTable)
kruskal.test(Sp..Atk ~ Type.2, data = subTable)
kruskal.test(Sp..Def ~ Type.2, data = subTable)
kruskal.test(Speed ~ Type.2, data = subTable)


# ----------------------------------------------------------------
# analyse des repartitions de légendaires
pdf('nombre_pokemons_type_principal.pdf', height=5)
ggplot(data=tablePokemon, aes(fct_infreq(Type.1), fill=factor(Legendary, levels = c(T, F)))) + 
  geom_bar() +
  scale_fill_manual(values=c("#E69F00", "#888888"), name="", labels=c("Legendary", "Normal")) +
  geom_text(aes(label=..count..),stat="count",position=position_stack(vjust = 0.5), size=5) +
  labs(title = "Nombre de Pokémons par Type 1") +
  xlab("Types de Pokémons") + 
  ylab("Effectifs") +
  theme_classic() +
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle=60, hjust=1),
        legend.position="bottom") 
dev.off()

pdf('nombre_pokemons_type_secondaire.pdf', height=5)
ggplot(data=tablePokemon, aes(fct_infreq(Type.2), fill=factor(Legendary, levels = c(T, F)))) + 
  geom_bar() +
  scale_fill_manual(values=c("#E69F00", "#888888"), name="", labels=c("Legendary", "Normal")) +
  geom_text(aes(label=..count..),stat="count",position=position_stack(vjust = 0), size=5) +
  labs(title = "Nombre de Pokémons par Type 2") +
  xlab("Types de Pokémons") + 
  ylab("Effectifs") +
  theme_classic() +
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle=60, hjust=1),
        legend.position="bottom")
dev.off()

# calcul proportion de légendaires pour Type 1 
areLegendary <- subset(tablePokemon, tablePokemon$Legendary == T)
arentLegendary <- subset(tablePokemon, tablePokemon$Legendary == F)
prop.Legendary <- data.frame(summary(areLegendary$Type.1), summary(arentLegendary$Type.1))
colnames(prop.Legendary) <- c("areLegendary", "arentLegendary")
prop.Legendary$total <- prop.Legendary$areLegendary + prop.Legendary$arentLegendary
prop.Legendary$prop.Leg <- prop.Legendary$areLegendary/prop.Legendary$total
prop.Legendary$prop.NonLeg <- prop.Legendary$arentLegendary/prop.Legendary$total

# plot proportion de légendaires
pdf("proportion_pokemons_legendaires_par_type_principal.pdf", height = 4)
ggplot(prop.Legendary, aes(x = rownames(prop.Legendary), y=prop.Leg)) + 
  geom_bar(stat = "identity", fill = "#E69F00") +
  scale_y_continuous(limits = c(0,1)) + 
  geom_text(aes(x = rownames(prop.Legendary), y = prop.Leg + 0.02, 
                label = paste0(round(prop.Leg*100, 2),"%")), size=4) +
  labs(title = "Proportion de Pokémons légendaires par type principal") +
  xlab("Types de Pokémons") + 
  ylab("Proportion de légendaires")
dev.off()

# test de prop
prop.test(prop.Legendary$areLegendary , prop.Legendary$total)$p.value 

# ----------------------------------------------------------------------------------------------
# calcul prop de légendaires pour Type 2
prop.Legendary2 <- data.frame(summary(areLegendary$Type.2), summary(arentLegendary$Type.2))
colnames(prop.Legendary2) <- c("areLegendary", "arentLegendary")
prop.Legendary2$total <- prop.Legendary2$areLegendary + prop.Legendary2$arentLegendary
prop.Legendary2$prop.Leg <- prop.Legendary2$areLegendary/prop.Legendary2$total
prop.Legendary2$prop.NonLeg <- prop.Legendary2$arentLegendary/prop.Legendary2$total

# plot proportion de légendaires 
pdf("proportion_pokemons_legendaires_par_type_secondaire.pdf", height = 4)
ggplot(prop.Legendary2, aes(x = rownames(prop.Legendary2), y=prop.Leg)) + 
  geom_bar(stat = "identity", fill = "#E69F00") +
  scale_y_continuous(limits = c(0,1)) + 
  geom_text(aes(x = rownames(prop.Legendary2), y = prop.Leg + 0.02, 
                label = paste0(round(prop.Leg*100, 2),"%")), size=4) +
  labs(title = "Proportion de Pokémons légendaires par type secondaire") +
  xlab("Types de Pokémons") + 
  ylab("Proportion de légendaires")
dev.off()

# certains classes ont plus de légendaires : tests pour voir si c'est significatif ou juste
# effet de petits nombres




##
## 
tableLegendByGen <- data.frame()
for (i in 1:6){
  tableLegendByGen <- rbind(tableLegendByGen, c(i, 
                                                nrow(tablePokemon[tablePokemon$Generation == i,]),
                                                nrow(tablePokemon[tablePokemon$Generation == i & tablePokemon$Legendary == T,])))
}

attach(tableLegendByGen)
prop.test(X6L, X166L)$p.value       # pas de différence significative à priori
detach(tableLegendByGen)

## -----------------------------------------------------------------------------
# caractéristiques

p <- tablePokemon %>%
  plot_ly(type = 'parcoords',
          line = list(color = ~as.numeric(Type.1),
                      colorscale = typeColorsList),
          dimensions = list(
            list(range = c(1, 6),
                 label = "Generation",
                 values = ~Generation),
            list(range = c(1, 18),
                 label = "Type1",
                 values = ~as.numeric(Type.1)),
            list(range = c(0, 260),
                 label = "HP",
                 values = ~HP),
            list(range = c(0, 260),
                 label = "Attack",
                 values = ~Attack),
            list(range = c(0, 260),
                 label = "Defense",
                 values = ~Defense),
            list(range = c(0, 260),
                 label = "Sp..Atk",
                 values = ~Sp..Atk),
            list(range = c(0, 260),
                 label = "Sp..Def",
                 values = ~Sp..Def),
            list(range = c(0, 260),
                 label = "Speed",
                 values = ~Speed)
          ))
p

# pas de tendance claire -> caractéristiques très hétérogènes dans un même type
# certains types font leur apparition et d'autres disparaissent suivant les générations


## LEGENDAIRES -----------------------------------------------------------------
##
tableLegend <- subset(tablePokemon, tablePokemon$Legendary == T)
tableNonLegend <- subset(tablePokemon, tablePokemon$Legendary == F)

library(reshape2)
dat.m <- melt(tablePokemon,id.vars='Name', measure.vars=c('HP','Attack','Defense','Sp..Atk','Sp..Def','Speed'))
dat.m <- cbind(dat.m, rep(tablePokemon$Legendary, 6))
colnames(dat.m) <- c("Name", "variable", "value", "Legendary")

pdf("legend_vs_nonLegend.pdf")
ggplot(dat.m) +
  geom_boxplot(aes(x=variable, y=value, fill=Legendary)) +
  scale_fill_manual(values=c("#888888", "#E69F00")) +
  theme_classic() +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        axis.text.x = element_text(angle=60, hjust=1),
        legend.text = element_text(size = 15),
        legend.position = "bottom") +
  xlab("") +
  ylab("")
dev.off()

# tests pour savoir si la distribution est similaire
pokeLegend <- tablePokemon[tablePokemon$Legendary == T, ]
pokeNonLegend <- tablePokemon[tablePokemon$Legendary == F, ]

wilcox.test(pokeLegend$HP, pokeNonLegend$HP, alternative = "greater")$p.value
wilcox.test(pokeLegend$Attack, pokeNonLegend$Attack, alternative = "greater")$p.value
wilcox.test(pokeLegend$Defense, pokeNonLegend$Defense, alternative = "greater")$p.value
wilcox.test(pokeLegend$Sp..Atk, pokeNonLegend$Sp..Atk, alternative = "greater")$p.value
wilcox.test(pokeLegend$Sp..Def, pokeNonLegend$Sp..Def, alternative = "greater")$p.value
wilcox.test(pokeLegend$Speed, pokeNonLegend$Speed, alternative = "greater")$p.value


pokeToRemove <- c(12, 33, 46, 66, 78, 90, 144, 183, 236, 322, 419, 479, 556, 618, 655, 782)

pdf("prop_victories_leg_vs_nonLeg.pdf", height = 5, width = 5)
ggplot(tablePokemon[-pokeToRemove,]) +
  geom_boxplot(aes(x=Legendary, y=victories, fill=Legendary)) +
  scale_fill_manual(values=c("#888888", "#E69F00")) +
  theme_classic() +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        legend.position="none") +
  xlab("") +
  scale_x_discrete(labels=c("FALSE" = "Non légendaire", "TRUE" = "Légendaire")) +
  ylab("")
dev.off()

wilcox.test(pokeLegend$victories, pokeNonLegend$victories, alternative = "greater")$p.value

# ------------------------------------------------------------------------------

ggplot(tablePokemon[-pokeToRemove,]) +
  geom_boxplot(aes(x=Type.1, y=victories, fill=Type.1)) +
  scale_fill_manual(values=c("#888888", "#E69F00")) +
  theme_classic() +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        legend.position="none") +
  xlab("") +
  scale_x_discrete(labels=c("FALSE" = "Non légendaire", "TRUE" = "Légendaire")) +
  ylab("")

# ------------------------------------------------------------------------------

## /!\ C'EST LONG

# dataWinnersVsLoosers <- data.frame(matrix(ncol=7, nrow=0))
# 
# for (i in 1:nrow(tableCombats)) {
# 
#   print(i)
# 
#   if (tableCombats[i,]$Winner == tableCombats[i,]$First_pokemon) {
#     winner <- 1
#     looser <- 2
#   } else {
#     winner <- 2
#     looser <- 1
#   }
# 
#   quantiW <- cbind(tablePokemon[tableCombats[i, winner], 4:9], "Winner")
#   quantiL <- as.vector(cbind(tablePokemon[tableCombats[i, looser], 4:9], "Loser"))
#   names(quantiL) <- names(quantiW)
# 
#   dataWinnersVsLoosers <- rbind(dataWinnersVsLoosers, quantiW)
#   dataWinnersVsLoosers <- rbind(dataWinnersVsLoosers, quantiL)
# 
# }

## test de la distribution des variables entre vainqueurs et perdants

# ... CODE ...
colnames(dataWinnersVsLoosers) <- c("HP", "Attack", "Defense", "Sp..Atk", "Sp..Def", "Speed", "Pokemon")

## test de la variance des variables entre vainqueurs et perdants
library(car)
leveneTest(HP ~ Pokemon, data = dataWinnersVsLoosers)

# ---------------------------------
dataWinnersVsLoosers$ID = rownames(dataWinnersVsLoosers)

dat.m2 <- melt(dataWinnersVsLoosers,id.vars='ID', measure.vars=c('HP','Attack','Defense','Sp..Atk','Sp..Def','Speed'))
dat.m2 <- cbind(dat.m2, rep(dataWinnersVsLoosers$Pokemon, 6))
colnames(dat.m2) <- c("Name", "variable", "value", "Pokemon")

pdf("winnerVsLoser.pdf")
ggplot(dat.m2) +
  geom_boxplot(aes(x=variable, y=value, fill=Pokemon)) +
  scale_fill_manual(values=c("green", "red")) +
  theme_classic() +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        axis.text.x = element_text(angle=60, hjust=1),
        legend.text = element_text(size = 15),
        legend.position = "bottom") +
  xlab("") +
  ylab("")
dev.off()

## PCA -------------------------------------------------------------------------
## on réalise une ACP sur les pokemons, en se basant uniquement sur les variables 
## quantitatives (HP, Atk, Def, Sp.Atk, Sp.Def, Speed)

do.ACP(tablePokemon[,4:9], typeColors)
## output : fichiers ACPpokemons_type1.pdf et ACPpokemons_type1_explainedVar.pdf

## Conclusions : 
# les PC1 et PC2 expliquent à eux seuls 63.41% de la variance totale
# La méthode du coude indique aussi des les PC1 et PC2 sont les plus explicatives
# difficile d'y voir clair : peut être trop de types
# beaucoup d'étalement des caractéristiques même intra-type

# hypothèse : le type 2 modifie les caractéristiques et étale artificiellement les caractéristiques
# ACP avec type1 et type=None --> pas de tendance claire : hypothèse fausse ?

# TODO : faire ACP en colorant par "type1-type2" considéré comme un seul type
# TODO : marquer les légendaires pour voir si ils apparaissent à part
# TODO : tenter un clustering multidimensionnel (4, 5, 6) ?

###################################################################################
######################## TEST PARALLEL PLOT #######################################
###################################################################################

## on plot l'ACP en coordonnées parallèles
## cf le script parallelACP.R
parallelACP(coordsACP, quant = 0.1)
## output : fichiers pokemonsACP_parallel_type1 et pokemonsACP_parallel_type2

parallelACP(coordsACP[coordsACP$Type2 == "None", ], quant = 0)

## Conclusions :
# il y a beaucoup de variabilité intra-types : l'axe PC1, bien qu'expliquant presque 50%
# de la variance totale, semble incapable de distinguer totalement le moindre type.
# certains types sont plus marqués que d'autres (ex : Flying et Fairy sont assez fortement polarisés
# sur PC2) mais les données sont peu nombreuses (flying et fairy sont les types les moins représentés)
# on pourrait donc regarder un artefact
# les types semblent donc assez équilibrés dans les caractéristiques, malgré des tendances
# peut être que les types n'apportent pas beaucoup d'information sur l'issue d'un combat


###################################################################################
###################################################################################
###################################################################################

###
# - Generation d une table qui combine combats et pokemon, les individues sont des combats. 
###
# tableCombat

#PCA pour cette table
# tableCombats.numeric <- tous les colonnes numerics
tableCombats.scale <- scale(tableCombats.numeric, center = TRUE, scale = TRUE)
tableCombats.cov <- cov.wt(tableCombats.scale, method = "ML")
tableCombats.acp <- eigen(tableCombats.cov)
vecteurPropre <- tableCombats.acp$vectors
combatsACP <- tableCOmbats.scale %*% vecteurPropre

### ACP THIBAULT

# Normalisation par centrage et réduction
Xetude = scale(tableEtude[,1:6],scale=TRUE)
covarMat = cov(Xetude)
Uetude = eigen(covarMat)$vectors

XetudeACP = Xetude %*% Uetude
head(XetudeACP)

summary(prcomp(Xetude))
acp<-prcomp(Xetude)
rot <- as.data.frame(acp$rotation)

barplot(c(0.452, 0.1848, 0.1286, 0.1182, 0.07131, 0.04511))

toPlot <- as.data.frame(cbind(XetudeACP, tableEtude$resultat))

png("ACPpokemons.png")
ggplot(data=toPlot, aes(x=V1, y=V2)) +
  geom_point(aes(color=factor(V7))) +
  geom_segment(data = rot, aes(x = 0, y = 0, xend = PC1*7, yend = PC2*7), arrow = arrow(length = unit(0.3,"cm"))) +      
  geom_text_repel(data = rot, aes(PC1*5, PC2*5, label = rownames(coordsVars)), size = 7) +
  theme_classic() +
  theme(legend.position="bottom", axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14),
        legend.title=element_text(size=16), legend.text=element_text(size=14)) +
  xlab("PC1") +
  ylab("PC2") +
  scale_color_manual(labels = c("Premier Pokemon gagnant", "Second Pokemon gagnant"), values=c("firebrick2", "skyblue2")) +
  labs(color = "")
dev.off()

###
# - Analyses 
###
# Remplissage 
################################################
# ATTENTION : EXTREMEMENT LONG ! #
################################################
# for (i in 1:length(tableCombats[,1])) {
#   tableEtude[i,]$types = which(tableTypesCombinaisons$type1 == tablePokemon[tableCombats[i,1],]$Type.1 & tableTypesCombinaisons$type2 == tablePokemon[tableCombats[i,2],]$Type.1) 
#   tableEtude[i,]$generations = which(tableGenerationCombinaisons$generation1 == tablePokemon[tableCombats[i,1],]$Generation & tableGenerationCombinaisons$generation2 == tablePokemon[tableCombats[i,2],]$Generation) 
#   tableEtude[i,]$legendary = which(tableLegendaireCombinaisons$legendaire1 == tablePokemon[tableCombats[i,1],]$Legendary & tableLegendaireCombinaisons$legendaire2 == tablePokemon[tableCombats[i,2],]$Legendary) 
#   tableEtude[i,]$diff_HP = tablePokemon[tableCombats[i,1],]$HP - tablePokemon[tableCombats[i,2],]$HP
#   tableEtude[i,]$diff_att = tablePokemon[tableCombats[i,1],]$Attack - tablePokemon[tableCombats[i,2],]$Attack 
#   tableEtude[i,]$diff_def = tablePokemon[tableCombats[i,1],]$Defense - tablePokemon[tableCombats[i,2],]$Defense
#   tableEtude[i,]$diff_spatt = tablePokemon[tableCombats[i,1],]$Sp..Atk - tablePokemon[tableCombats[i,2],]$Sp..Atk
#   tableEtude[i,]$diff_spdef = tablePokemon[tableCombats[i,1],]$Sp..Def - tablePokemon[tableCombats[i,2],]$Sp..Def
#   tableEtude[i,]$diff_speed = tablePokemon[tableCombats[i,1],]$Speed - tablePokemon[tableCombats[i,2],]$Speed
#   if (tableCombats[i,]$Second_pokemon == tableCombats[i,]$Winner) {
#     tableEtude[i,]$resultat = 2
#   } else {
#     tableEtude[i,]$resultat = 1
#   }
# }

# write.csv(tableEtude, "etude.csv")


tableEtude <- read.csv("etude.csv")
tableEtude = tableEtude[,-1]

# K means

k = 2
combatsClustering = kmeans(x = tableEtude[,1:6], centers = k, iter.max = 20)
# colors = brewer.pal(k, name = "Accent")
colors = c("#FF0000", "#00FF00")
plot(tableEtude[,1], 
     tableEtude[,2],
     col=colors[combatsClustering$cluster],
     pch = 20)
points(combatsClustering$centers, col ="black")

resultKmeans <- length(combatsClustering$cluster[combatsClustering$cluster == tableEtude[,7]])/length(tableEtude[,1])
# Pour obtnir la probabilité au dessus de 50%, peut-être il y a des moyens plus élégants
if (resultKmeans < 0.5 ) {
  resultKmeans <- 1 - resultKmeans
}
print(resultKmeans)
# 68% de réussite


################### Apprentissage automatique #########################
result_tab = cbind(tableEtude[,1:7])

for (k in 1:5) {
  # Génération d'un ensemble d'apprentissage et de test
  idx <- sample.int(length(result_tab[,1]),size=30000,replace=FALSE) 
  app_values <- result_tab[idx,]
  test_values <- result_tab[-idx,]
  
  # Differentes méthodes d'analyse discriminante
  param_adq = adq.app(Xapp = app_values[,1:6], zapp = app_values[,7])
  param_adl = adl.app(Xapp = app_values[,1:6], zapp = app_values[,7])
  param_nba = nba.app(Xapp = app_values[,1:6], zapp = app_values[,7])
  
  # Prédiction selon paramètres générés
  resultat_pred1 = ad.val(param_adq,test_values[,1:6])
  print(paste("Avec l'échantillon numéro ",k," et la méthode adq on obtient ",length(resultat_pred1$pred[resultat_pred1$pred == test_values[,7]])/(length(test_values[,7]))))
  resultat_pred2 = ad.val(param_adl,test_values[,1:6])
  print(paste("Avec l'échantillon numéro ",k," et la méthode adl on obtient ",length(resultat_pred2$pred[resultat_pred2$pred == test_values[,7]])/(length(test_values[,7]))))
  resultat_pred3 = ad.val(param_nba,test_values[,1:6])
  print(paste("Avec l'échantillon numéro ",k," et la méthode nba on obtient ",length(resultat_pred3$pred[resultat_pred3$pred == test_values[,7]])/(length(test_values[,7]))))
}
## pourquoi length(test_values[,7])-3000??
# On a environ 85% de réussite avec les deux premières méthodes et 77%
# L'ensemble d'apprentissage choisi est de longueur 3000.

# Seulement selon les deux premiers axes factoriels
param_adq_simple = adq.app(Xapp = app_values[,1:2], zapp = app_values[,7])
param_adl_simple = adl.app(Xapp = app_values[,1:2], zapp = app_values[,7])
param_nba_simple = nba.app(Xapp = app_values[,1:2], zapp = app_values[,7])

# Frontière de décision selon les deux premières variables
# ATTENTION : on limite aux 5000 premiers points pour ne pas alourdir le graphe
par(mfrow=c(1,1)) 
prob.ad(param_adq_simple,test_values[1:5000,1:2],test_values[1:5000,7],c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9))
prob.ad(param_adl_simple,test_values[1:5000,1:2],test_values[1:5000,7],c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9))
prob.ad(param_nba_simple,test_values[1:5000,1:2],test_values[1:5000,7],c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9))

# KPP algorithme:
library(class)
source("separ1.R")
kpp <- separ1(tableEtude[1:20000,1:6], tableEtude[1:20000,]$resultat)
library(class)
etudeTest.pred <- knn(train = kpp$Xapp, test = kpp$Xtst, cl = kpp$zapp, k = 10)
tableResultat <- table(kpp$ztst, etudeTest.pred)
tauxResultat <- sum(diag(tableResultat))/sum(tableResultat) 
result <- paste("Avec k = ",a,", le taux est ",tauxResultat)
print(result);
result2 <- confusionMatrix(as.factor(kpp$ztst), as.factor(etudeTest.pred))
print(result2);
## envrion 64% reussite avec les deux premières variables, varie avec differents K
## On a utilisé la fause table, il faut utilise XetudeACP au lieu de tableEtude, donc
## le resultat est environ 80%.

#### Régression linéaire

apprentissage_lin = log.app(app_values[,1:6], app_values[,7], intr = T, epsi = 10^(-5))
classement_lin = log.val(apprentissage_lin$beta,test_values[,1:6])
accuracy_lin = mean(classement_lin$pred == test_values[,7])

# Courbes de niveau
apprentissage_lin_simple1 = log.app(app_values[,1:2], app_values[,7], intr = T, epsi = 10^(-5))
par(mfrow=c(1,1))
prob.log(as.matrix(apprentissage_lin_simple1$beta), tableEtude[1:5000,1:2], tableEtude[1:5000,7], c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9))

# Trees
library(rpart.plot)
library(caret)
library(e1071)

par(mfrow=c(1,1)) 

# Génération d'un ensemble d'apprentissage et de test
idx_trees <- sample.int(length(tableEtude[,1]),size=30000,replace=FALSE) 
app_values_trees <- tableEtude[idx_trees,]
test_values_trees <- tableEtude[-idx_trees,]
app_values_trees[,7] = as.factor(app_values_trees[,7])
test_values_trees[,7] = as.factor(test_values_trees[,7])

# repeatedcv = on répète une validation croisée, ici le nombre de répétitions est repeats = 3
# On découpe le dataset en 5 sous parties à chaque répétition pour la validation croisée 
# On donne dans les paramètres le critère à adopter : ici le critère de gini
res.tree<-train(x = app_values_trees[,-7],y = app_values_trees[,7],method='rpart',
                trControl = trainControl(method = "repeatedcv",number = 5, repeats = 3))

# On prédit le test set
probs <- predict(res.tree, newdata=test_values_trees[,-7], type='prob')
# On déduit des probabilités la classe à affecter
probs2 <- c(rep(0,length(probs[,1])))
for (i in 1:length(probs[,1])) {
  if (probs[i,1] > probs[i,2]) {
    probs2[i] = 1
  }
  else {
    probs2[i] = 2
  }
}
probs <- data.frame(cbind(probs,resultat = as.factor(probs2)))

# On regarde les résultats vs les vraies données
confusionMatrix(probs[,3], test_values_trees[,7])
# 94% de réussite

# On représente l'arbre graphiquement

# Buguent sur RStudio Cloud
prp(res.tree$finalModel, box.palette = "Reds", tweak = 1.2)
rpart.plot(res.tree$finalModel)
# Le second graphique contient plus d'informations : le premier nombre correspond
# à la classe avec la probabilité la plus élevée à  cette étape.
# Le second nombre est la probabilité pour que le second pokemon gagne le combat.
# Le troisième représente le ratio (effectif au noeud choisi) / (effectif total) 

#Correlation entre variables et taux de victoire
var = c("HP", "Attack", "Defense", "Sp..Atk", "Sp..Def", "Speed")
for (i in 1:6) {
  result.P = cor.test(tablePokemonQuantit[,i], tablePokemon$victories, method = "pearson")
  result.S = cor.test(tablePokemonQuantit[,i], tablePokemon$victories, method = "spearman", exact = FALSE)
  print.P = paste("Correlation Pearson entre", var[i],"et victoire est ",result.P$estimate )
  print.S = paste("Correlation Spearman entre", var[i],"et victoire est ",result.S$estimate )
  print(print.P);
  print(print.S)
}

