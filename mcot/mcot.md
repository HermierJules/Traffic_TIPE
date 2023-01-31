# Traffic en ville: Modélisation et Prédiction de la congestion. <Modélisation et Prédiction du Traffic Routier>

Mon intérêt pour ce sujet est avant tout dû aux possibles applications de la simulation routière, notamment avec le développement des technologies GPS permettant une récolte d'information bien plus complète qu'il n'était possible auparavant.

## Professeur encadrant du candidat :
Marc De Falco (CIV)

## Positionement thématique

_INFORMATIQUE (Informatique pratique), MATHEMATIQUES (Mathématiques Appliquées)_

## Mots-clés 
**Mots-Clés** (en français) 

* Modèle microscopique
* Automates cellullaires
* Traffic
* Congestion
* Graphes
* Algorithmes de parcours

**Mots-Clés** (en anglais)


## Bibliographie commentée

La prédiction du traffic est une question omniprésente à la fois dans les études mathématiques et informatique. Le développement des voitures autonomes et techonologies GPS rendent la question encore plus pertinente.  
Pour y répondre de multiples modèles ont été développés[1]: des modèles microscopiques, macroscopiques et mésoscopiques. Les misoscopique simulant les voitures individuelles afin d'étudier le plus précisemment leur évolution, cependant le coût de la simulation est intensif. Les macroscopiques réprésentent le traffic sous la forme d'un fluide ou d'un flux, permettant de faire l'analogie entre le traffic dans un réseau routier et un gaz parcourant des tuyaux, ils sont particulièrement utile pour représenter le traffic sous forme purement mathématique. Finalement viennent les modèles mésoscopique, à la croisée des microscopiques et macroscopiques, ils permettent de faire des prédictions sur le traffic au cours du temps sur un volume très large pour un coût moins élevé ce que requiererait un modèle microscopique.  
rn 1992 Nagel et Schreckenberg[1] ont développé un modèle qui reste aujourd'hui majeur dans le domaine des modèles par sa simplicité. Un automate cellulaire à 4 paramètres rapide à implémenter mais illustrant néanmoins des phénomènes routiers très complexe. Bien qu'il atteigne ses limites très rapidement, n'étant conçu originellement que pour simuler uneseule route à une voie, il peut être étendu de nombreuses façons et est donc la base de nombreux autres modèles. Particulièrement, nous nous intéresseront à une évolution du modèle[4] prenont en compte des éléments plus poussés de la théorie du traffic[2].   
En 1955, James Lighthill et et Geral Beresford présentent un modèle mathématique permettant de prévoir l'évolution de la densité sur une route au travers d'une système d'équations différentielles [3]. Ce modèle peut être étendu sur un réseau en considérant chaque route comme une cellule et en considérant les entrées et sorties des véhicules comme des flux. Ainsi on peut considérer les carrefours et intersections avec la méthode de Newell-Daganzo [7] qui décrit une procédure pour rejoindre plusieurs flux rejoignant une même cellule. 
En 2022, Rôann CANTEL, grand scientifique, créa un modèle parfait, si réaliste que les experts du domaine se mirent à l'aduler. Son algorithme, le monte Karlo Ki Kasse (KKK) en O(1) est parfait.²
- nous nous concentrerons sur

- manières de modéliser le traffic
- manières de prévoir le traffic
- 

## Problématique retenue

## Objectifs du TIPE du candidat
Je me propose :

* d'étudier des algorithmes de prédiction de congestions
* Adapter des modèles théoriques existant pour comparer les résultats expérimentaux

## Abstract
(vide) 

## Références bibliographiques 
1. 2Fachrichtung Theoretische Physik, Universität des Saarlandes, Postfach 151150, 66041 Saarbrücken, Germany : _Empirical test for cellular automaton models of traffic flow_
2. Boris S. Kerner : _Introduction to Modern Traffic Flow Theory and Control_
3. Stephane Mollier : _Two-dimensional macroscopic models for large scale_
4. Kun Gao, Rui Jiang, Shou-Xin Hu, Bing-Hong Wang, Qing-Song Wu2: _Cellular-automaton model with velocity adaptation in the framework of Kerner’s
three-phase traffic theory_
5. Wikipedia page for the NaSch Model: _https://en.wikipedia.org/wiki/Nagel%E2%80%93Schreckenberg_model_
6. https://en.wikipedia.org/wiki/Newell%27s_car-following_model
7. https://en.wikipedia.org/wiki/Newell%E2%80%93Daganzo_merge_model
8. https://www.openstreetmap.org
## DOT

