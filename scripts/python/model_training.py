# Header

# Putpose: This script is for creating a model to predict built-up areas through neighborhood characteristics
# Author : Hamidreza Zoraghein
# Date   : 4/24/2023


# Pacakages  
import sys
import geopandas as gpd
import pickle
import os
sys.path.append(os.path.join('.', 'scripts', 'python'))
import model_definition 


# Paths and Inputs
inputs_dir  = os.path.join('.', 'inputs', 'dem_decomposition')
outputs_dir = os.path.join('.', 'outputs', 'decomposition')
dem_ts_file = os.path.join(inputs_dir, 'char_ts.gpkg')

dem_char    = 'age'
dem_ts_file = dem_ts_file.replace('char', dem_char)

if dem_char == 'race':
    pred_years  = [str(year) for year in range(1980, 2020, 10)]
    target_year = str(int(pred_years[-1]) + 10)
    chars = ['ASO', 'HIS', 'NHB', 'NHW']
    prediction_features  = [i + char + year for year in pred_years for char in chars for i in ['P', 'HH', 'LL']]
    target_features      = ['P' + char + target_year for char in chars]

elif dem_char == 'age':
    pred_years  = [str(year) for year in range(1970, 2020, 10)]
    target_year = str(int(pred_years[-1]) + 10)
    chars = ['0', '20', '40', '60', '80']
    prediction_features  = [i + char + '_' + year for year in pred_years for char in chars for i in ['pop', 'HH', 'LL']]
    target_features      = ['pop' + char + '_' + target_year for char in chars]


# Main Code
# Extract the feature space and features for each characteristic
features_df          = gpd.read_file(dem_ts_file).drop(columns = 'geometry').loc[:, ['ID'] + prediction_features + target_features]
features_df.fillna(0, inplace = True)


# Extract the number of time steps and features per time step
num_time_steps = len(pred_years)
num_features   = int(len(prediction_features) / len(pred_years))
num_target_variables = len(target_features)


# Define models per characteristic
model_obj = model_definition.PredictionModel(features_df, num_time_steps, num_features, num_target_variables)


train_ids_pkl = os.path.join(outputs_dir, 'train_ids.pkl')  
test_ids_pkl  = os.path.join(outputs_dir, 'test_ids.pkl')  
if os.path.exists(train_ids_pkl) and os.path.exists(test_ids_pkl):
    print('Ids for train and test ids have already been created...')
    with open(train_ids_pkl, 'rb') as load_train_ids:
        train_ids = pickle.load(load_train_ids)
    with open(test_ids_pkl, 'rb') as load_test_ids:
        test_ids  = pickle.load(load_test_ids)
else:
    train_ids, test_ids = model_obj.train_test_id_extract()
    with open(train_ids_pkl, 'wb') as write_train_ids:
        pickle.dump(train_ids, write_train_ids)
    with open(test_ids_pkl, 'wb') as write_test_ids:
        pickle.dump(test_ids, write_test_ids)


# Extract train and test datasets
train_features, test_features = model_obj.train_test_extract(train_ids, test_ids)


cur_model = model_obj.define()


# Prepare the datasets for training
x_train, x_test = train_features.loc[:, prediction_features].values, test_features.loc[:, prediction_features].values
y_train, y_test = train_features.loc[:, target_features].values, test_features.loc[:, target_features].values

cur_model, history = model_obj.model_fit(cur_model, x_train, x_test, y_train, y_test)
loss = history.history



# Save the model and its history outputs
model_outputs_path  = os.path.join(outputs_dir, 'model_outputs')
model_loss_path     = os.path.join(model_outputs_path, '{}_model.pkl'.format(dem_char)) 
if os.path.exists(model_loss_path):
    print('The pickle file containing the history file of the trained model already exists')
else:
    cur_model.save(os.path.join(model_outputs_path, dem_char))
    with open(model_loss_path, 'wb') as write_file:
        pickle.dump(loss, write_file)
    


# Create and save the MSE plot
epochs    = [i + 1 for i in list(range(len(loss['loss'])))]
plot_path = os.path.join(model_outputs_path, '{}_plot.tif'.format(dem_char))  
model_definition.plot_series(x = epochs, y = loss, end = len(epochs), title = 'Mean Squared Error', xlabel = 'Epoch', ylabel = 'MSE',
                      legend = ['Train MSE', 'Validation MSE'])



