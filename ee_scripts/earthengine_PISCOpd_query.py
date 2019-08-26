import ee
import pandas as pd
from ee.batch import Export
from datetime import datetime
from matplotlib import pyplot as plt
ee.Initialize()

''' 
PISCOp V2.1: Construction of a high-resolution gridded rainfall dataset for Peru from 1981 to the present day
Google Earth Engine assets:
  - PISCOpm: "users/csaybar/PISCOpm"
  - PISCOpd: "users/csaybar/PISCOpd"
Image properties: 'system:time_start' (Unix time)
ImageCollection properties: N/A
Scale : 0.01
More Information: https://tandfonline.com/doi/abs/10.1080/02626667.2019.1649411?journalCode=thsj20
'''

def my_reduce(pisco):
  params = {"collection": ROI, "reducer": ee.Reducer.mean(), "scale": 5000}
  image_value = pisco.divide(100).reduceRegions(**params).first().get('mean')
  image_date = pisco.get('system:time_start')
  ft = ee.Feature(None, {'system:time_start': image_date,
                         'date': ee.Date(image_date).format('Y-M-d'), 
                         'value': image_value})
  return ft

# ----- Parameters
ROI = ee.Geometry.Rectangle([-74, -16, -71, -14]) # Region of interest
start_date = "1981-01-01"
end_date = "1983-06-30"
seq_date = pd.date_range(start = start_date, end = end_date, freq = 'D').tolist()
# -----

PISCOpd = ee.ImageCollection("users/csaybar/PISCOpd")\
            .filterDate(start_date, end_date)\
            .map(my_reduce)
 
# Method - 01
# Limited - Use it just small queries  (NOT RECOMMENDED FOR QUERIES IN PISCOPd)
ts_pisco = PISCOpd.getInfo()
pisco_values = [x['properties']['value'] for x in ts_pisco['features']]
pisco_pd = pd.DataFrame(data=pisco_values,index=seq_date[0:len(seq_date)-1])
plt.plot(pisco_pd)
plt.show()

# Method - 02
# Save results in Google Drive
pisco_task = Export.table.toDrive(**{'collection': PISCOpd,
                                     'selectors': 'date, value',
                                     'fileNamePrefix':'pisco_ts'})
pisco_task.start()

pisco_ts = pd.read_csv('../pisco_ts.csv')[['value']]
pisco_ts.index = seq_date
plt.plot(pisco_pd)
plt.show()
