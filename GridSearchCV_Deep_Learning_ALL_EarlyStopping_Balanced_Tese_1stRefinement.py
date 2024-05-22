# GridSearchCV_Deep_Learning_Tese

import sys
import json

import tensorflow as tf
from iterstrat.ml_stratifiers import MultilabelStratifiedKFold
from sklearn.utils import compute_sample_weight

print(sys.version)
import numpy as np
import pandas as pd
from sklearn.metrics import make_scorer, f1_score

from sklearn.model_selection import GridSearchCV
from keras.layers import Dense, Dropout
from keras.models import Sequential, load_model

from scikeras.wrappers import KerasClassifier
from keras.callbacks import EarlyStopping

physical_devices = tf.config.list_physical_devices('GPU')
for device in physical_devices:
    tf.config.experimental.set_memory_growth(device, True)

Early_Stopping_Patience = 2

Suffix = "DeepLearning_ALL_EarlyStopping" + str(Early_Stopping_Patience) + "_Balanced_Adam"

df_train = pd.read_csv('../../Data Cleansing/DataCleansingResults/df_train_each_with_zeroes_tissue.csv', sep=",")
df_train = df_train.drop(['Unnamed: 0'], axis=1)
print(df_train.head())

X = df_train.iloc[:, :4].values
print(X)
print(X.shape, type(X))

y = df_train.iloc[:, 4:].values
print(y)
print(y.shape, type(y))

import joblib
from keras import backend as K

num_samples = df_train.shape[0]
num_features = X.shape[1]
num_labels = y.shape[1]

import gc

gc.enable()

early_stopping = EarlyStopping(monitor='loss', patience=Early_Stopping_Patience, restore_best_weights=True, mode='auto',
                               verbose=2)
# early_stopping = EarlyStopping(monitor='categorical_accuracy', patience=1, restore_best_weights=True, mode='auto', verbose=2)


scoring = make_scorer(f1_score, average='macro', zero_division=True)


# Define a function to create a neural network model
def create_model_training(hidden_units1=64, hidden_units2=36,
                          dropout_rate=0.2, activation='softmax'):
    # def create_model_training(optimizer='adam', activation='softmax', hidden_layer=128, hidden_units2=64, dropout_rate=0.1):
    K.clear_session()
    model = Sequential()
    model.add(Dense(hidden_units1, activation=activation, input_dim=num_features))
    model.add(Dropout(rate=dropout_rate))
    model.add(Dense(hidden_units2, activation=activation))
    model.add(Dropout(rate=dropout_rate))
    model.add(Dense(num_labels, activation=activation))
    #    model.compile(loss='binary_crossentropy', optimizer=optimizer, metrics=['accuracy'], sample_weight_mode="temporal")
    model.compile(loss='binary_crossentropy', metrics=['categorical_accuracy'],
                  sample_weight_mode="temporal", optimizer=tf.keras.optimizers.legacy.Adam())
    return model


# Create the model
model = KerasClassifier(activation='softmax', model=create_model_training, verbose=1, dropout_rate=0.2,
                        hidden_units1=64, hidden_units2=36, optimizer=tf.keras.optimizers.legacy.Adam(),
                        metrics=['categorical_accuracy'],
                        loss='binary_crossentropy', callbacks=[early_stopping])

#
# filepath = './GridSearchResults/CV40/DeepLearning/best_deeplearning_model' + Suffix + '.hdf5'
# checkpoint = ModelCheckpoint(filepath, monitor="loss",verbose=1,save_best_only=True,mode='auto')

tb_callback = tf.keras.callbacks.TensorBoard('./logs/' + Suffix, update_freq="epoch", histogram_freq=1)

print(model.get_params())
param_grid = {
    'hidden_units1': list(range(38, 43, 2)),
    'hidden_units2': list(range(14, 19, 2)),
    'activation': ['softmax'],
    'dropout_rate': [0.1],
    'epochs': [48,49,50,51,52],
    'batch_size': list(range(4,7,1))
}

# loocv = LeaveOneOut()
# Create the model

mskf = MultilabelStratifiedKFold(n_splits=55, shuffle=True, random_state=5)

grid_search = GridSearchCV(estimator=model, param_grid=param_grid, cv=mskf, scoring=scoring, verbose=4, n_jobs=-1,
                           refit=True)

params_grid_file = './GridSearchResults/CV40/DeepLearning/params_grid' + Suffix + '.txt'
with open(params_grid_file, 'w') as file:
    json.dump(grid_search.param_grid, file)
file.close()

# CLASS WEIGHT BALANCING
from sklearn.preprocessing import MultiLabelBinarizer

mlb = MultiLabelBinarizer()
sample_weights = compute_sample_weight(class_weight='balanced', y=mlb.fit_transform(y))
fit_params = {'sample_weight': sample_weights}

with tf.device('/device:GPU:0'):
    # with tf.device('CPU: 0'):
    grid_search.fit(X, y, **fit_params)
# Perform the grid search

print(grid_search.cv_results_)

# Print the best parameters and their corresponding score
print("Best Parameters: ", grid_search.best_params_)
print("Best Score: ", grid_search.best_score_)

with open("./GridSearchResults/CV40/DeepLearning/cv_results_" + Suffix + ".txt", 'w') as file:
    for key, value in grid_search.cv_results_.items():
        if isinstance(value, np.ndarray):
            # If the value is a NumPy array, convert it to a list for serialization
            value = value.tolist()
        file.write(f"{key}: {value}\n")
file.close()

with open("./GridSearchResults/CV40/DeepLearning/cv_best_param_" + Suffix + ".txt", 'w') as file:
    for key, value in grid_search.best_params_.items():
        file.write(f'{key}: {value}\n')
file.close()

print("Best F1_micro Score: {:.2f}".format(grid_search.best_score_))
# Write the best_score to the text file
with open("./GridSearchResults/CV40/DeepLearning/cv_best_score_" + Suffix + ".txt", 'w') as file:
    file.write(f'Best Score: {grid_search.best_score_}')
file.close()

# https://adriangb.com/scikeras/refs/heads/master/notebooks/Basic_Usage.html#4.-Saving-and-loading-a-model
best_keras_model = grid_search.best_estimator_
best_keras_model.model_.save('./GridSearchResults/CV40/DeepLearning/cv_best_model_' + Suffix)

best_loaded_model = load_model('./GridSearchResults/CV40/DeepLearning/cv_best_model_' + Suffix)
