import datetime  
import numpy as np  
import pandas as pd
import numpy.random as random
import statsmodels.api as sm
import statsmodels.tsa as tsa
import statsmodels.formula.api as smf
import pandas as pd
import numpy as np

# Create dataset

def create_dataset():
 
    data = pd.DataFrame()
    
    np.random.seed(0); np.random.seed(1) # Random seed needs to reset here for reproducibility

    data ['performance_metrics'] = [400,441,453,588,774,890,967,897,825,868,856,876,891,876,916,755,670 
    552,392,399,392,399,407,416,847,1040,1253,1310,1240,1254,1437,1584    
    1525,1552,1376,1384,1290,1401,1525,1695,1706,1601,1449,1284    
    1200,1215,1181,1307,1532,1811,1992,2191]

    data ['TV']=[0,0,0,37,35,35,35,10,0,
    0,0,10,10,10,10,10,0,0,0,0,0,
    0,0,0,10,70,40,40,40,0,10,40,35,
    10,0,0,10,35,35,37,10,0,0,0,0
    0,0,35,40,45,35,37]

    data ['Influencers'] = [1,1,1,1,1,5,5,5,1,
    0,0,1,1,1,1,1,1,1,1,1,1,
    0,0,1,1,1,1,1,1,1,1,1,1,
    0,0,1,1,1,1,1,1,1,1,1,1
    0,0,1,5,5,5,5]

    data ['Digital_Branding'] = [0,0,1,1,1,1,1,1,1,
    0,0,0,1,1,1,1,1,1,1,1,1,
    0,0,1,1,1,0,1,1,1,1,1,1,
    0,0,1,1,1,1,1,0,0,0,1,1
    0,0,0,0,2,2,2]

    data ['Radio'] = [0,0,0,0,1,1,0,1,1,
    0,0,1,1,1,1,1,0,0,0,1,1
    0,0,0,1,1,1,1,0,0,1,1,1,
    0,0,1,1,1,0,1,1,1,1,1,1,
    0,0,0,0,2,2,2]

    data ['OOH']=[
    0,0,0,0,0,0,1,1,1,0,0,0,
    0,0,0,0,0,0,0,1,1,0,0,0,
    0,0,0,0,0,0,0,0,1,1,1,1,
    0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0]
    
    return data

#create dataset and print first and last rows of data

data = create_dataset()  

### Incude adstock

ar_coeff = .5

TV_adstock = tsa.filters.filtertools.recursive_filter(data ['TV'], ar_coeff)

Influencers_adstock = tsa.filters.filtertools.recursive_filter(data ['Influencers'], ar_coeff)

DigitalBranding_adstock = tsa.filters.filtertools.recursive_filter(data['Digital Branding'], ar_coeff)

Radio_adstock = tsa.filters.filtertools.recursive_filter(data ['Radio'], ar_coeff)

OOH_adstock = tsa.filters.filtertools.recursive_filter(data ['OOH'], ar_coeff)

# Combine all the pandas series together

df_ad= pd.concat([data ['TV'],TV_adstock,data ['Incluencers'],Influencers_adstock,data ['Digital Branding'],DigitalBranding_adstock, 
       data ['Radio'],Radio_adstock,data ['OOH'],OOH_adstock],axis=1)

Performance_metrics=data ['performance metrics'] 

modelfit2 = smf.ols(formula='performance metrics ~ TV_adstock + Influencers_adstock + DigitalBranding_adstock + Radio_adstock + OOH_adstock',data=df_ad).fit()

print modelfit2.summary()
