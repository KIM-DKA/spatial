import pandas as pd 
import os 
import re 

os.getcwd()
os.listdir()
os.chdir('./ohiolung')

for x in os.listdir():
    print(x)

[x for x in os.listdir() if 'csv' in x]

ohio_wide = pd.read_csv('ohilung.txt',sep='\t')


ohio_wide.head()

pop_list = [x for x in ohio_wide.columns if 'POP' in x]
col_list = [x for x in ohio_wide.columns if (re.search('[0-9]+',x)) and 'POP' not in x]

pop_data = pd.melt(ohio_wide,id_vars= 'COUNTYID',value_vars=pop_list)
cas_data = pd.melt(ohio_wide,id_vars= 'COUNTYID',value_vars=col_list)

pop_data['variable'] = pop_data.variable.str.replace('POP','')
cas_data['variable'] = cas_data.variable.str.replace('L','')




# cas ['COUNTYID','CNT','YEAR','SEX','RACE']
# pop ['COUNTYID','YEAR','POP','SEX','RACE']

pop_data = pop_data[['COUNTYID','variable','value']]
cas_data = cas_data[['COUNTYID','value','variable']]

pop_data['YEAR'] = '19' + pop_data.variable.str[:-3:-1] 
pop_data['SEX'] = pop_data.variable.str[:1]
pop_data['RACE'] = pop_data.variable.apply(lambda x : re.sub('[MF0-9]','',x))

cas_data['YEAR'] = '19' + cas_data.variable.str[:-3:-1] 
cas_data['SEX'] = cas_data.variable.str[:1]
cas_data['RACE'] = cas_data.variable.apply(lambda x : re.sub('[MF0-9]','',x))


cas_data.groupby('RACE').count()
pop_data.groupby('RACE').count()

cas_data.head()
pop_data.head()

cas_data = cas_data.loc[cas_data['RACE'] != '',['COUNTYID','value','YEAR','SEX','RACE']]
pop_data = pop_data.loc[pop_data['RACE'] != '',['COUNTYID','YEAR','value','SEX','RACE']]

pd.to_csv('cas_data.csv',index=False,header=False)