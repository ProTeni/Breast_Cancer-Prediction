![ALT](Image/pexels-photo-5072316.webp)

# Breast Cancer Classification — Recall-First Diagnosis with KNN

**A clinical classification model that predicts whether a breast tumour is malignant or benign,
deliberately tuned to minimise false negatives — because a missed cancer is the costliest error.**

> A K-Nearest Neighbours classifier on the Wisconsin Breast Cancer dataset, with the full workflow:
> scaling, evaluation, and hyperparameter tuning via GridSearchCV optimised for **recall** rather
> than raw accuracy — the right priority for a medical-screening context.

---

## Business / clinical problem
In cancer screening, not all errors are equal. A **false negative** (telling a patient they're
clear when they aren't) can be fatal; a **false positive** (a false alarm leading to further
tests) is far less harmful. So the model is optimised to catch as many true cancers as possible —
maximising **recall** — even at some cost to precision.

## Dataset
**Wisconsin Breast Cancer** dataset — cell-nucleus measurements (radius, texture, perimeter, area,
and more) labelled as malignant (M) or benign (B). Source: UCI / Kaggle.

## What I did
- **Preprocessing:** encoded the diagnosis label (M→1, B→0), checked for nulls, dropped an empty
  trailing column
- **EDA:** class-balance check and pairplots of the key nucleus measurements by diagnosis
- **Modelling:** train/test split, `StandardScaler` (fit on train only), KNN classifier
- **Evaluation:** confusion matrix and a full classification report, reading **recall** as the
  primary metric given the clinical context
- **Hyperparameter tuning, two ways:**
  - an error-rate elbow plot across k = 1–50 to find the stable low-error region
  - a `GridSearchCV` over a `Pipeline` (scaler + KNN) with a **custom recall scorer**, so tuning
    optimised the metric that actually matters here

## Result
Tuning the model for recall **reduced false negatives** — i.e. fewer missed cancers — which is the
outcome that matters most in a screening setting. The trade-off (slightly more false positives) is
clinically acceptable.

## Key takeaway
Metric choice is a clinical decision, not just a technical one. Optimising for accuracy alone would
have hidden the cost of missed diagnoses; optimising for recall aligns the model with how the error
actually affects patients.

## Tools
Python (pandas, NumPy, seaborn, matplotlib) · scikit-learn (KNeighborsClassifier, StandardScaler,
Pipeline, GridSearchCV)

---

## Related & planned work
- **Companion R version:** the same clinical problem solved in R as part of my MSc (CETM72),
  using KNN and a Decision Tree — to be consolidated here, showing the same problem across two
  languages and two skill levels.
- **Model comparison:** benchmark KNN against the Decision Tree in a single notebook.
- **Explainability:** add SHAP to show which measurements drove each prediction — essential for
  trust in any clinical model.
