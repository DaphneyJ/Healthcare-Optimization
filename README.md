# Predictive Analytics to Optimize Patient Care

## Team-149
Team 149's group project GitHub repository for MGT 6203 Fall of 2024 semester.
 
Members:
- Daphney Jacques
- Darren Wone
- Maria Martinez Cullas
- Nigele McCoy


### I. Project Objectives: 

The primary goal of this project is to develop predictive models that classify patients as either `"Healthy"`, `"At Risk"`, or `"Critical"` based on various health and lifestyle factors like medical history, demographics, and lifestyle habits. By identifying the variables most strongly linked to health outcomes, we created models that not only predict risk but also prescribe customized action plans for patients. These plans can guide healthcare providers on the best course of preventive care. Additionally, the insights are designed to inform businesses and partners about strategic marketing opportunities.

We developed a web application that integrates our predictive models. Healthcare providers and patients can enter their data and receive tailored recommendations and action plans aimed at improving their health. 


### II. Buisness Impact:

**From a Healthcare Perspective**:
Early identification of high-risk individuals can lead to more proactive preventative care. Hospitals and clinics can utilize our models to focus on reducing hospital visits, lowering costs, and improving patient health. This model gives providers a clear way to make data-driven decisions that can lead to better health outcomes for patients.

**From a Business Perspective**:
The insights our models generate can help health-related companies understand which patient segments benefit most from targeted interventions. For example:
 - Insurance Companies: Identify at-risk groups and implement targeted programs to reduce costs.
 - Health-Tech Firms: Develop subscription-based apps offering personalized, long-term care plans.
 - Healthcare Partnerships: Collaborate with gyms, meal-prep services, and wearable tech companies to create meaningful, health-focused marketing campaigns that resonate with different demographics.


### III. Expected Outcomes:

The anticipated outcomes of this project include:
- Predictive models including **Logistic Regression** and **Decision Tree Models** that classify patients based on their health profile and recommend action plans.
- Key insights into the most influential health factors like BMI, smoking status, or physical activity, that contributes the most to a patient's health outcome. This insight helps providers focus resources where they’ll have the greatest impact.
- Actionable recommendations for healthcare providers on how to use these insights to optimize patient care and prioritize resources for at-risk patients.
- A scalable and replicable approach that can be applied to other datasets to improve decision-making and patient outcomes.


### IV. Challenges and Mitigation:

Challenges:
- *Model Selection*:: The first challenge was model selection between binomial and multinomial logistic regression. Balancing model complexity and performance proved challenging.​
- **Imbalanaced Data**: The initial dataset had a skewed distribution of outcomes. This imbalance complicated our initial model training. Resampling was applied to balance the data for model training.
- *Bias and Variance trade off*: improving accuracy and F1 scores across the different categories of the multinomial model while maintaining model simplicity, required careful tuning.
- *Application Development*: Packaging our models into a functional tool for health care professionals and patients ​took time and effort.

Mitigation:
- Our Team decided to do a deep dive data analysis and use mutual information and LASSO regularization, to simplify and refine our models. 
- To handle the unbalanced data, we implemented stratified k-fold-Cross validation combined with over-sampling and under-sampling balancing technique to address the imbalance.
- Through parameter tunning, cross validation, and iterative model refinement, we minimized overfitting and improved model performance.
- Packaging the application took some research and understanding on how to implement a user-friendly design.   ​


### V. Insights
- **LASSO Multinomial Logistic Regression**: In order to best identify at-risk patients, our team had to augment the at-risk class, then implemented a multinomial logistic regression model with LASSO regularization. The LASSO outperformed the other models with a 71% accuracy rate on the rebalanced data. **Decision Trees**: Decision Tree models provided clear, interpretable pathways to predict patient outcomes based on various health features. They guided our understanding of which variables play the biggest role in patient risk classification.


### VI. Conclusion

`Application`:  
- We developed a web application that healthcare providers and patients can use to input their health data and the app generates customized treatment plans and recommendations to address identified risks. By detecting early signs in individuals who appear healthy, providers can intervene sooner, which could lead to drastic improvements in remission rates and overall health outcomes.​

Future Directions:
- Work to improve tree models (pruning , better feature selection, and better design of model)​
- We aim to add more features to our web application, improving user experience and the quality of insights delivered.
- Potential use of Support Vector Machine (SVM) or other advanced models to see if they can outperform our current approaches.


Thank you for taking the time to explore our project! 
​
