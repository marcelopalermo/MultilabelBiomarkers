import sys

from iterstrat.ml_stratifiers import MultilabelStratifiedKFold
from scipy.stats import entropy

print(sys.version)
import numpy as np
import pandas as pd
from scipy.stats import entropy, wasserstein_distance
from sklearn.metrics import make_scorer, f1_score, hamming_loss, roc_auc_score, recall_score, precision_score, \
    jaccard_score
from keras.models import load_model
from collections import Counter
from scikeras.wrappers import KerasClassifier

entropy_scores = []
wasserstein_scores = []  # List to store Wasserstein distances

# Define custom scoring function for entropy
def entropy_scorer(y_true, y_pred_proba):
    return np.mean([entropy(pred) for pred in y_pred_proba])

# Define custom scoring function for Wasserstein distance
def wasserstein_scorer(y_true, y_pred_proba):
    return np.mean([wasserstein_distance(y_true, pred) for pred in y_pred_proba])


best_model = load_model(
    '/Users/marcelo_palermo/PycharmProjects/TeseDoutorado/Training/GridSearchCV/GridSearchResults/CV40/DeepLearning/Model_Proteins_1stRefinement/cv_best_model_DeepLearning_ALL_EarlyStopping2_Balanced_Adam')

print(best_model)

model = KerasClassifier(best_model)

df_train = pd.read_csv(
    '/Users/marcelo_palermo/PycharmProjects/TeseDoutorado/Data Cleansing/DataCleansingResults/df_train_each_with_zeroes_tissue.csv',
    sep=",")
df_train = df_train.drop(['Unnamed: 0'], axis=1)
print(df_train.head())

X = df_train.iloc[:, :4].values
print(X)
print(X.shape, type(X))

y = df_train.iloc[:, 4:].values
print(y)
print(y.shape, type(y))

# Initialize lists to store maximum entropy for each label
max_entropy_per_label = []


# Calculate maximum entropy for each label
for i in range(y.shape[1]):
    # Maximum entropy occurs when all predictions are equally likely
    num_classes = len(np.unique(y[:, i]))
    max_entropy = entropy(np.full(num_classes, 1 / num_classes))
    max_entropy_per_label.append(max_entropy)

# Print or use the maximum entropy values
print("Maximum Entropy for Each Label:", max_entropy_per_label)

# Initialize lists to store TP, TN, FP, and FN for each label
num_samples = df_train.shape[0]
num_features = X.shape[1]
num_labels = y.shape[1]

scoring = make_scorer(f1_score, average='macro', zero_division=True)
mskf = MultilabelStratifiedKFold(n_splits=55, shuffle=True, random_state=5)

roc_auc_scores = []
f1_scores = []
f1_scores_labels = []
hamming_loss_scores = []
contributing_roc_auc_labels = []
contributing_jaccard_labels = []
contributing_jaccard_label_names = []
precision_scores = []
recall_scores = []
jaccard_scores = []
hamming_scores = []
# Initialize lists to store summary statistics of Jaccard scores
jaccard_summary = []


for train_index, test_index in mskf.split(X, y):
    X_train, X_test = X[train_index], X[test_index]
    y_train, y_test = y[train_index], y[test_index]
    print(f"Train index = {train_index}, test_index = {test_index}")
    # Train the model on the current fold
    model.fit(X_train, y_train)

    # Make predictions on the test set
    y_pred = model.predict(X_test)
    y_pred_proba = model.predict_proba(X_test)



    entropy_per_label = [entropy(pred) for pred in y_pred_proba.T]
    entropy_scores.append(entropy_per_label)

    # Calculate Wasserstein distance for each label's predicted probability distribution
    wasserstein_per_label = [wasserstein_distance(y_test[:, i], y_pred_proba[:, i]) for i in
                             range(num_labels)]
    wasserstein_scores.append(wasserstein_per_label)

    f1_macro = f1_score(y_test, y_pred, average='macro', zero_division=True)

    # Store the F1 score for this fold
    f1_scores.append(f1_macro)
    print(f'F1 Macro: {f1_macro}')

    # Store Hamming Loss for this fold
    hamming_loss_scores.append(hamming_loss(y_test, y_pred))
    print(f'Hamming Loss: {hamming_loss(y_test, y_pred)}')




    # Calculate precision and recall for each label
    precision_per_label = []
    recall_per_label = []
    f1_per_label = []
    jaccard_per_label = []
    hamming_per_label = []
    roc_auc_label = []



    for i in range(num_labels):
        precision = precision_score(y_test[:, i], y_pred[:, i], zero_division=True)
        recall = recall_score(y_test[:, i], y_pred[:, i], zero_division=True)
        f1 = f1_score(y_test[:, i], y_pred[:, i], zero_division=True)
        jaccard = jaccard_score(y_test[:, i], y_pred[:, i], zero_division=True)
        jaccard_per_label.append(jaccard)
        if jaccard >= 0.9:
            contributing_jaccard_labels.append(i)
            contributing_jaccard_label_names.append(df_train.columns[i + 4])

        precision_per_label.append(precision)
        recall_per_label.append(recall)
        hamming = hamming_loss(y_test[:, i], y_pred[:, i])
        hamming_per_label.append(hamming)
        f1_per_label.append(f1)
        print(f'Precision for Label {i}: {precision}')
        print(f'Recall for Label {i}: {recall}')
        print(f'Jaccard for Label {i}: {jaccard}')
        print(f'Hamming Loss for Label {i}: {hamming}')
        # ONLY Labels with recall = 1 and precision = 1 are TRUE POSITIVES

        roc_auc = 0
        try:
            roc_auc = roc_auc_score(y_test[:, i], y_pred_proba[:, i])
        except ValueError:
            pass
        roc_auc_label.append(roc_auc)
        if roc_auc >= 0.9:
            contributing_roc_auc_labels.append(i)
        print(f'ROC-AUC for Label {i}: {roc_auc}')

    # Store the average precision and recall scores for this fold
    precision_scores.append(precision_per_label)
    recall_scores.append(recall_per_label)
    jaccard_scores.append(jaccard_per_label)
    hamming_scores.append(hamming_per_label)
    f1_scores_labels.append(f1_per_label)
    roc_auc_scores.append(roc_auc_label)

    # Append summary statistics
    jaccard_summary.append({
        'mean': np.mean(jaccard_per_label),
        'median': np.median(jaccard_per_label),
        'q1': np.percentile(jaccard_per_label, 25),
        'q3': np.percentile(jaccard_per_label, 75),
        'min': np.min(jaccard_per_label),
        'max': np.max(jaccard_per_label)
    })


# Calculate average precision and recall across all folds for each label
avg_precision_scores = np.mean(precision_scores, axis=0)
avg_recall_scores = np.mean(recall_scores, axis=0)
avg_jaccard_scores = np.mean(jaccard_scores, axis=0)
average_hamming_scores = np.mean(hamming_scores, axis=0)
average_f1_scores_label = np.mean(f1_scores_labels, axis=0)


average_hamming_loss = np.mean(hamming_loss_scores)
print(f'Average Hamming Loss Across Folds: {average_hamming_loss}')
print("Contributing Labels to ROC-AUC:", contributing_roc_auc_labels)

# Convert contributing_labels to a Counter object to count occurrences of each label
label_counter = Counter(contributing_roc_auc_labels)

# Rank the labels by occurrence
ranked_labels = label_counter.most_common()
print("ROC-AUC: Ranked Labels by Occurrence:")
for label, count in ranked_labels:
    print(f"Label {label}: Occurrences {count}")

print("Contributing Labels to Jaccard Score:", contributing_jaccard_labels)

# Convert contributing_labels to a Counter object to count occurrences of each label
label_counter = Counter(contributing_jaccard_labels)

# Rank the labels by occurrence
ranked_labels = label_counter.most_common()
print("Jaccard: Ranked Labels by Occurrence:")
for label, count in ranked_labels:
    print(f"Label {label}: Occurrences {count}")

import csv

with open('label_names_jaccard.txt', 'w', newline='') as csvfile:
    writer = csv.writer(csvfile)
    for string in contributing_jaccard_label_names:
        writer.writerow([string])

max_ranking = current_ranking = ranked_labels[0][1]
label_position = 0
limit = 0
ranked_label_names = []

while max_ranking == current_ranking:
    print("ranking", current_ranking)
    current_ranking = ranked_labels[label_position][1]
    print("Column", df_train.columns[4+ranked_labels[label_position][0]])
    ranked_label_names.append(df_train.columns[4+ranked_labels[label_position][0]])
    label_position += 1
    limit += 1
    if limit == 150:
        break

with open('label_names_potential_biomarkers.txt', 'w', newline='') as csvfile:
    writer = csv.writer(csvfile)
    for string in ranked_label_names:
        writer.writerow([string])


# Print or use the average precision and recall scores
print("Average Precision Scores for Each Label:", avg_precision_scores)
print("Average Recall Scores for Each Label:", avg_recall_scores)
print("Average Jaccard Scores for Each Label:", avg_jaccard_scores)
print(f'Average Hamming Loss Across Folds: {average_hamming_scores}')

np.savetxt("avg_recall_scores.csv", avg_recall_scores, delimiter=",")
np.savetxt("avg_precision_scores.csv", avg_precision_scores, delimiter=",")
np.savetxt("avg_jaccard_scores.csv", avg_jaccard_scores, delimiter=",")
np.savetxt("avg_hamming_loss_scores.csv", average_hamming_scores, delimiter=",")
np.savetxt("avg_f1_scores_label.csv", average_f1_scores_label, delimiter=",")

avg_entropy_scores = np.mean(entropy_scores, axis=0)

# Print or use the average entropy scores
print("Average Entropy Scores for Each Label:", avg_entropy_scores)
np.savetxt("avg_entropy_scores.csv", avg_entropy_scores, delimiter=",")

avg_roc_auc_scores = np.mean(roc_auc_scores, axis=0)

# Print or use the average ROC-AUC scores
print("Average ROC-AUC Scores for Each Label:", avg_roc_auc_scores)
np.savetxt("avg_roc_auc_scores.csv", avg_roc_auc_scores, delimiter=",")
np.savetxt("avg_roc_auc_scores_round.csv", np.round(avg_roc_auc_scores,3), delimiter=",")

# CERCAR AS LABELS CONTRUBUINTES ENTRE JACCARD E ROC-AUC ??? A INTERSECÇÃO SERIA O RESULTADO FINAL

# np.mean(avg_jaccard_scores)
# 0.849554013875124
# É IGUAL AO F1-macro estabelecido no GridSearchCV
# Duas vertentes: as métricas confirmando, e as proteinas biomarcadoras encontradas para Jaccard > 0.9


avg_wasserstein_scores = np.mean(wasserstein_scores, axis=0)
# Print or use the average Wasserstein distance scores
print("Average Wasserstein Distance Scores for Each Label:", avg_wasserstein_scores)
np.savetxt("avg_wasserstein_scores.csv", avg_wasserstein_scores, delimiter=",")