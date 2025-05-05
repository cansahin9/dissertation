NBA Archetypes & Team Performance (2015–2024)
---

Overview
---
This project identifies NBA player archetypes via clustering and uses them to predict team Net Rating with machine learning. Built using 10 seasons of player/team data.

Highlights
---
5 Player Archetypes: Ball-Dominant Guards, 3PT Specialists, Versatile Wings, Stretch Bigs, Traditional Bigs

Models Used: GLMNet, Cubist, XGBoost, Random Forest, Stacked Ensemble

Best Model: Stacked (GLMNet + Cubist)

Explainability: SHAP highlights PER, OBPM, and Stretch Big share as key features

Repo Contents
clustering_analysis.R – Player archetype discovery

predicting_team_ntrtg.R – Team Net Rating prediction

ML_ws_bpm_cluster.R – Player BPM prediction

Other scripts for preprocessing, aggregation, and visualization
