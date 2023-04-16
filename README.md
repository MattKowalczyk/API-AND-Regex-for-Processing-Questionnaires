# API-AND-Regex-for-Processing-Questionnaires
Using API, and regex to automatically find specific questionnaires from a csv and then finally recode the values

The API would theoretically pull from an online database. I have not included a token or URL but instead have included sample code for the API. The rest of the script uses some regular expression to clean the titles of all the columns so that the code can be generalizable to any data import so long as the same type of data (e.g. same questionnaires are provided). The questionnaires are then grouped by patterns to create separate dataframes for each specific questionnaire. Lastly, the data values are then recoded for scoring purposes and are at last prepped for further analysis.
