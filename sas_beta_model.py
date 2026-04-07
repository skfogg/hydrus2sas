#
# KBr from HYDRUS
#
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from mesas.sas.model import Model
import vis
from scipy.stats import beta
from scipy.stats import gamma
from scipy.stats import expon
from scipy.optimize import fmin

kbr_mesas_data = pd.read_csv('../sas_input_kbr.csv')

my_solute_parameters = {
  'C_in': {}
}

def make_model_from(params):
    a, b = params
    my_sas_specs = {'Q':
      {'soil column':
        {'scipy.stats': beta,
        'args': { 'a': a,
        'b': b,
        'scale': 116.0642,
        'loc': 0},
        'nsegment': 500}}}
    return Model(kbr_mesas_data, sas_specs = my_sas_specs, solute_parameters = my_solute_parameters, dt = 1, influx = 'J')

def minimize_me(params):
   model = make_model_from(params)
   model.run()
   obs = model.data_df['C_out']
   pred = model.data_df['C_in --> Q']
   RMSE = np.sqrt(np.mean((pred-obs)**2))
   print(f'RMSE = {RMSE} for params = {params}')
   return RMSE
 
 ## initial params
a = 2.0
b = 3.0
params_init = a, b
 
params = fmin(minimize_me, params_init)

my_model = make_model_from(params)
my_model.run()
my_model.data_df.to_csv('outputs_vari_beta.csv')
 

# my_sas_specs = {
#   'Q':{
#     'a uniform distribution over total storage':{
#       'ST': [0, 'S']
#     }
#   }
# }

# my_sas_specs = {
#     'Q':{
#         'a uniform distribution over total storage':{
#             'ST': [0,0.3]#,0.2, 0.31, 1.0] #, 0.39, 0.402357]
# # 'P': [0, 0.9, 1.0]
#             }
#         }
#     }

# my_sas_specs = {
#   'Q':{
#     'beta distribution':{'scipy.stats': beta,
#       'args': { 'a': 2.0,
#         'b': 2.5,
#         'scale': 116.0642,
#         'loc': 0},
#       'nsegment': 500}
#   }
# }

# my_sas_specs = {
#     'Q':{
#         'gamma distribution':{'scipy.stats': gamma,
#                            'args': { 'a': 0.999,
#                                  'scale': 0.0910744,
#                                  'loc': 0.0},
#                            'nsegment': 500}
#         }
#     }

# my_sas_specs = {
#     'Q':{
#         'EXPONENTIAL distribution':{'scipy.stats': expon,
#                            'args': {'scale': 1.5,
#                                  'loc': 0},
#                            'nsegment': 500}
#         }
#     }



#
# my_solute_parameters = {
#     'Cin': {'k1' : 1.1}
#     }

# my_options = {
#   'dt': 2
# }

# config = {
#   'sas_specs': my_sas_specs,
#   'solute_parameters': my_solute_parameters,
#   'options': my_options
# }
# 
# my_model = Model(data_df = kbr_mesas_data, config=config)
# 
# my_model.run()
# my_model.data_df.to_csv('outputs.csv')
print(my_model.data_df)


plt.plot(my_model.data_df.index, my_model.data_df['C_in'], label = "C_in", marker='o', linestyle='-')
plt.plot(my_model.data_df.index, my_model.data_df['C_in --> Q'], label = "C_out_sim", marker='o', linestyle='-')
plt.plot(my_model.data_df.index, my_model.data_df['C_out'], label = 'C_out_meas', marker='o', linestyle='-')
plt.legend()
plt.show()

ax1 = plt.subplot2grid((1,2), (0,0))
vis.plot_SAS_cumulative(my_model, 'Q', ax=ax1)
plt.show()


