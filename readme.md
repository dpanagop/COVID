# COVID-19 related code

This code contains Python and R scripts for extracting and analysing COVID-19 related data.
Especially:
* [Scap_COVID_announcements.ipynb](https://github.com/dpanagop/COVID/blob/main/Scap_COVID_announcements.ipynb)
is a Python notebook for web scrapping COVID-related deaths and number of patients in ventilator from Greece's National Public Health Organisation (EODY). 
[Scap_COVID_announcements_verbose.ipynb](https://github.com/dpanagop/COVID/blob/main/Scap_COVID_announcements_verbose.ipynb) contains the same notebook but with several print commands that display outpout from the web scrapping process.
* [deaths_ventilator_20201114.xlsx](https://github.com/dpanagop/COVID/blob/main/deaths_ventilator_20201114.xlsx) is a result of the web scrapping notebook (executed on 2020/11/14). In [deaths_ventilator_20201120.xlsx](https://github.com/dpanagop/COVID/blob/main/deaths_ventilator_20201120.xlsx) some more data has been added manually.
* [deaths_ventilator_patients_modeling.R](https://github.com/dpanagop/COVID/blob/main/deaths_ventilator_patients_modeling.R) Is an R script that takes deaths_ventilator_20201120.xlsx as input and creates models for predicting COVID-related deaths based on number of patients in ventilator and previous deaths.
* The script above creates among other things two random forests. [rf_model_deaths_explained.html](https://github.com/dpanagop/COVID/blob/main/rf_model_deaths_explained.html) and [rf_model_ventilator_explained.html](https://github.com/dpanagop/COVID/blob/main/rf_model_ventilator_explained.html) are two HTML files with more information about them.
