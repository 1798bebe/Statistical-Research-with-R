## Statistical Research: Water Resource Analysis with R

Water is a fundamental resource essential for human survival, economic development, and environmental stability. Yet growing water stress concerns—driven by population dynamics, climate change, and economic activity—pose a major global challenge. A deep understanding of water‑use dynamics, and their links to economic and climate factors, is therefore critical for informed policymaking and sustainable management.

This study analyzes global and country‑level water‑use patterns by combining classical statistical techniques with machine learning workflows in R. Water-related variables were examined through a structured pipeline comprising exploratory data analysis, supervised learning(binary classification), unsupervised learning(PCA & clustering), and time‑series forecasting. A major emphasis was placed on rigorous validation and evaluation, such as constructing a reliable analytical workflow, optimizing model performance, critically reviewing assumptions and limitations, and examining generalizability of the models—to ensure that all findings are both reliable and applicable.

The final report is available here: [📘 Final Report (PDF, 70 pages)](https://github.com/1798bebe/Statistical-Research-with-R/blob/main/documents/final_report.pdf)

## Summary of Work

- Data collection and preprocessing completed, including filtering, label creation, new feature derivation, and regression-based imputation.
  - [📄 Report (PDF)](https://github.com/1798bebe/Statistical-Research-with-R/blob/main/preprocessing/report_data_preprocessing.pdf)
  - [🧠 R Script](https://github.com/1798bebe/Statistical-Research-with-R/blob/main/preprocessing/preprocessing.R) 

- Supervised learning applied to classify country population‑growth via nested CV (Random Forest, XGBoost, GLMNET, random‑baseline) with F1‑optimized random‑search tuning, pairwise Wilcoxon tests, GLMNET interpretability (odds ratios, variable importance, PDPs) and choropleth misclassification mapping.
  - [📄 Report (PDF)](https://github.com/1798bebe/Statistical-Research-with-R/blob/main/Supervised%20Learning%20%28Regression%2C%20Classification%29/report_supervised_learning.pdf) 
  - [🧠 R Script](https://github.com/1798bebe/Statistical-Research-with-R/blob/main/Supervised%20Learning%20%28Regression%2C%20Classification%29/binary%20classification.R) 

  
- Unsupervised learning applied to water-related indicators using PCA and clustering (K-means, DBSCAN, hierarchical), with exhaustive grid search across feature subsets and parameters; policy-relevant cluster profiles derived from the best configuration.
  - [📄 Report (PDF)](https://github.com/1798bebe/Statistical-Research-with-R/blob/main/unsupervised%20learning(PCA%2C%20clustering)/report_unsupervised_learning.pdf) 
  - [🧠 R Script](https://github.com/1798bebe/Statistical-Research-with-R/blob/main/unsupervised%20learning(PCA%2C%20clustering)/unsupervised_learning.R) 

- Time series forecasting completed using ARIMA and Prophet on Germany’s freshwater withdrawals, with scenario-based policy recommendations derived from potential trend drivers.
  - [📄 Report (PDF)](https://github.com/1798bebe/Statistical-Research-with-R/blob/main/time%20series%20forecasting/report_time_series_forecasting.pdf) 
  - [🧠 R Script](https://github.com/1798bebe/Statistical-Research-with-R/blob/main/time%20series%20forecasting/time_series_forecasting.R) 
  
- Exploratory Data Analysis (EDA) performed to support all three analytical streams: supervised learning, unsupervised learning, and time series forecasting.
  
## Appendix 

- Model overview: [📄 Appendix (PDF)](https://github.com/1798bebe/Statistical-Research-with-R/blob/main/documents/model_explanation.pdf) 

## Authors

- Seunghyun Kim (Group Leader)
  email: s4sekimm@uni-trier.de
  
- Niran Raj Pradhan
  
- Heejung Heo
