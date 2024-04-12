# Pacakages 
import numpy as np 
import geopandas as gpd
import tensorflow as tf
import matplotlib.pyplot as plt
from tensorflow.keras.models import Model
from tensorflow.keras.layers import Dense, LSTM, Input, concatenate
from sklearn.preprocessing import MinMaxScaler


class PredictionModel():

    def __init__(self, feature_space, num_time_steps, num_features):
        self.feature_space  = feature_space
        self.num_time_steps = num_time_steps
        self.num_features   = num_features

    def train_test_id_extract(self):
        # Randomly select 80% of points in each built-up category for training
        train_set = (self.feature_space
                         .apply(lambda x: x.sample(frac = 0.8, random_state = np.random.RandomState(123))))
        train_ids = train_set.ID

        # Create the test set
        test_ids = np.asarray([test_id for test_id in self.feature_space.ID if test_id not in train_ids]) 

        train_ids = train_ids.values

        return(train_ids, test_ids)

    def train_test_extract(self, train_ids, test_ids):
        train_subset = self.feature_space.loc[train_ids - 1, ~self.feature_space.columns.isin(['ID'])]
        test_subset  = self.feature_space.loc[test_ids - 1, ~self.feature_space.columns.isin(['ID'])]
        
        return(train_subset, test_subset)

    def define(self):
        model_input       = Input(batch_shape = (256, self.num_time_steps, self.num_features))
        fst_lstm_output = LSTM(256, return_sequences = True)(model_input)
        snd_lstm_output = LSTM(256)(fst_lstm_output)

        # output = Dense(1)(snd_lstm_output)
        output = Dense(4)(snd_lstm_output)

        model = Model(inputs = model_input, outputs = output)

        return(model)

    # def model_fit(self, model, x_train, x_test, y_train, y_test):

    #     def custom_loss(y_true, y_pred):
    #         scaled_y = tf.clip_by_value(y_pred, clip_value_min=0, clip_value_max=1)
    #         # min_val  = tf.reduce_min(scaled_y)
    #         # max_val  = tf.reduce_max(scaled_y)

    #         # # Scale tensor between 0 and 1
    #         # scaled_y = (scaled_y - min_val) / (max_val - min_val)

    #         squared_diff = tf.square(y_true - scaled_y)
    #         loss = tf.reduce_mean(squared_diff, axis = -1)

    #         return(loss)


    #     x_train   = x_train.reshape((-1, self.num_time_steps, self.num_features))
    #     x_test    = x_test.reshape((-1, self.num_time_steps, self.num_features))

    #     model.compile(loss = custom_loss, optimizer = tf.keras.optimizers.Adam(learning_rate=0.001))

    #     history = model.fit(x_train, y_train, epochs = 40, verbose = True,
    #                         validation_data = (x_test, y_test))

    #     return(model, history) 
    
    def model_fit(self, model, x_train, x_test, y_train, y_test):


        x_train   = x_train.reshape((-1, self.num_time_steps, self.num_features))
        x_test    = x_test.reshape((-1, self.num_time_steps, self.num_features))

        model.compile(loss = 'MSE', optimizer = tf.keras.optimizers.Adam(learning_rate=0.001))

        history = model.fit(x_train, y_train, epochs = 40, verbose = True,
                            validation_data = (x_test, y_test))

        return(model, history) 

    def model_predict(self, model):

        prediction_features = self.feature_space.loc[:, ~self.feature_space.columns.isin(['ID'])].values

        prediction_features  = prediction_features.reshape((-1, self.num_time_steps, self.num_features))
        
        predictions = model.predict(prediction_features, batch_size = 256)

        return(predictions) 


def extract_feature_space(base_file, prediction_features, target_feature):
    all_features = prediction_features + [target_feature]
    features_df  = base_file.loc[:, ['ID'] + all_features] 

    return(features_df)
    
def plot_series(x, y, format = '-', start = 0, end = None, 
                title = None, xlabel = None, ylabel = None, legend = None):

    plt.figure(figsize = (10, 6))


    if type(y) is dict:
        dict_keys = y.keys()
        for dict_key in dict_keys:
            plt.plot(x[start:end], y.get(dict_key)[start:end], format)

    else:
        plt.plot(x[start:end], y[start:end], format)

    plt.xlabel(xlabel)
    plt.ylabel(ylabel)

    plt.xticks(ticks = list(range(1, end + 1)))

    if legend:
        plt.legend(legend)

    plt.title(title)
    plt.grid(True)
    plt.show()
    
# def generate_feature_space(base_file, current_characteristic, target_year):

#     feature_space = base_file
#     if (target_year == 2030):
#         pass
#     else:
#         feature_space['LL' + current_characteristic + str(target_year - 10)] = feature_space['LL' + current_characteristic + str(target_year - 20)]
#         feature_space['HH' + current_characteristic + str(target_year - 10)] = feature_space['HH' + current_characteristic + str(target_year - 20)]
#         columns_to_remove = [char + current_characteristic + str(target_year - 50) for char in ['LL', 'P', 'HH']]
#         feature_space     = feature_space.drop(columns = columns_to_remove) 


#     return(feature_space)


def generate_feature_space(base_file, target_year, characteristics, predictions):

    characteristics = ['ASO', 'HIS', 'NHB', 'NHW']
    feature_space = base_file

    for characteristic in characteristics:
        prediction_index = characteristics.index(characteristic) 
        feature_space['P'  + characteristic + str(target_year - 10)] = predictions[:, prediction_index]
        feature_space['HH' + characteristic + str(target_year - 10)] = feature_space['HH' + characteristic + str(target_year - 20)]
        feature_space['LL' + characteristic + str(target_year - 10)] = feature_space['LL' + characteristic + str(target_year - 20)]

    columns_to_remove = [i + char + str(target_year - 50) for char in characteristics for i in ['P', 'HH', 'LL']]
    feature_space     = feature_space.drop(columns = columns_to_remove) 


    return(feature_space)

