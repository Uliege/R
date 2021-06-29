# -*- coding: utf-8 -*-
"""
Created on Tue Jun 22 11:02:50 2021

@author: GMoncayo
"""
#######
#Lectura del archivo R#
import pyreadr
import pandas as pd
import geopandas as gpd
import movingpandas as mpd
import datetime as dt
from datetime import datetime, timedelta
import skmob
from skmob.measures.individual import home_location
from skmob.preprocessing import filtering
from skmob.preprocessing import compression
from skmob.measures.individual import distance_straight_line
from skmob.preprocessing import detection
from skmob.preprocessing import clustering
import os, platform, logging
from pathlib import Path

#Data Project
urlDefault = 'D:\\G\\GitHub\\Uliege\\R\\TimeLineAnalysis'
#Use this when script is in the same folder than other folders to export results
#urlProject = os.getcwd()
urlProject = urlDefault 
urlRawData = urlProject + '\\rawdata\\'
urlExcelData = urlProject + '\\exceldata\\'
urlLogs = urlProject + '\\logs\\'

#Name for log file
nomFileLog = 'py-Clean.log'

#Create and configure log file
if platform.platform().startswith('Windows'):
    fichero_log = os.path.join(urlLogs,nomFileLog)
else:
    fichero_log = os.path.join(os.getenv('HOME'), nomFileLog)

logging.basicConfig(level=logging.DEBUG,
                    format='%(asctime)s : %(levelname)s : %(message)s',
                    filename = fichero_log,
                    filemode = 'w',)

logging.debug('Inicio del proceso')

#Especifica el nombre de los archivos raw
rawName = "tld"

#Constantes
#Filter
MAX_SPEED_KMH = 200 #200 km/h
#Compression
MIN_SPATIAL_RADIUS_KM = 0.05 #50 meters
#Trajectories
MIN_LENGTH_TRAJ_MTR = 200 #200 meters 
MIN_DIFF_BETWEEN_ROWS_INSEC = 30 #30 seconds
#Trips
MIN_TIME_GAP_THRESHOLD_INMIN = 30 #30 minutes
MIN_LENGTH_TRIP_MTR = 100 #100 meters
#Stops - Activity Location
MIN_MINUTES_FOR_A_STOP = 5 #5 Minutes
MIN_STOP_RADIUS_FACTOR = 0.1
MIN_SPATIAL_RADIUS_KM_STOP = 0.2
# MIN_STOP_RADIUS_FACTOR * MIN_SPATIAL_RADIUS_KM_STOP = 0.02 - 20 meters
#Clusters
MIN_SAMPLES_CLUSTER=1


df_total_stops = pd.DataFrame()
df_total_homes = pd.DataFrame()
df_total_summary = pd.DataFrame()
#Proceso repetitivo
#for k in range(0, 270):
for k in range(174, 270):    
  #Change here to process individual files  
  #k = 174
  idFile = rawName + str(k) + '.RData'
  urlFile = urlRawData + idFile
  fileObj = Path(urlFile)
  if(fileObj.is_file()):
      r_file= pyreadr.read_r(urlFile)
      df_ini = pd.DataFrame(r_file['dataQuito'])
      del r_file
      #df_ini.columns
      df = df_ini[['dateTimeLine', 'longitude', 'latitude']]
      
      if(len(df.columns) == 3):
          #df.dtypes
          df['maps'] = df.latitude.astype(str)+","+df.longitude.astype(str)
          df['dateTimeLine'] = df['dateTimeLine'] - dt.timedelta(hours=5)
          df= df.rename(columns = {'dateTimeLine': 'datetime', 'longitude':'lon', 'latitude':'lat'})
          df['year'] =  df.datetime.dt.year
          df['month'] =  df.datetime.dt.month
          df['day'] =  df.datetime.dt.day
          df['week'] = df.datetime.dt.isocalendar().week
          df['week'] = df.apply(lambda x: 1 if (x['month']==1 and x['week']>48) else x['week'], axis=1)
          df['idWeek']  = (df.year.astype(str) + df.month.astype(str) + df.week.astype(str)).astype(int)
          df['idFile'] = rawName + str(k)
          msg = idFile 
          print(msg)
          logging.debug(msg)
          msg = "Inicial dataset contains {} records ".format(len(df))
          print(msg)
          logging.info(msg)
          
          #Validate the file contains at least 10 observations
          if len(df)>100:
              #Limpieza con scikit-mobility
              df_skmob = skmob.TrajDataFrame(df, datetime='datetime', longitude='lon', latitude='lat', user_id='idFile')
              
              #Home location (approximately)
              df_h = home_location(df_skmob, show_progress=False)   
              df_h['maps'] = df_h.lat.astype(str)+","+df_h.lng.astype(str)
              
              # filter out all points with a speed (in km/h) from the previous point higher than 200 km/h
              df_filter = filtering.filter(df_skmob, max_speed_kmh=MAX_SPEED_KMH, include_loops=True)
              del df_skmob
              
              #Reduce the number of points in a trajectory for each individual in a TrajDataFrame. 
              #All points within a radius of `spatial_radius_km` kilometers from a given initial point are 
              #compressed into a single point that has the median coordinates of all points and 
              #the time of the initial point. - 50 metres 
              df_compress = compression.compress(df_filter, spatial_radius_km=MIN_SPATIAL_RADIUS_KM)
              df = df_compress
              del df_compress
              del df_filter
              
              df= df.rename(columns = {'datetime': 'timestamp'})
              df['t'] = pd.to_datetime(df['timestamp'])
              
              #Index table
              df = df.set_index('t').tz_localize(None)
              
              df['idWeek'] = df.index.to_period('W')
               
              df = gpd.GeoDataFrame(df, geometry=gpd.points_from_xy(df.lng, df.lat))
              df = df.set_crs(epsg=4326)
              original_crs1 = df.crs
              msg = "Filter dataset contains {} records ".format(len(df))
              print(msg)
              logging.info(msg)
              msg = "Dataset covers the time between {} and {}.".format(df.index.min(), df.index.max())
              print(msg)
              logging.info(msg)
              msg = "That's {}".format(df.index.max() - df.index.min())
              print(msg)
              logging.info(msg)      
        
              #Trajectories
              # Desired minimum length of trajectories 200 metros
              df_traj_collection = mpd.TrajectoryCollection(df, traj_id_col='idWeek', min_length=MIN_LENGTH_TRAJ_MTR)
              msg = "Dataset contain {} trajectories".format(len(df_traj_collection))
              print(msg)
              logging.info(msg)
              msg = 'Number of weeks: ' + str(len(df['idWeek'].unique()))
              print(msg)
              logging.info(msg)
              
              #Desired minimum time difference between consecutive rows - 30 sec
              df_traj_collection = mpd.MinTimeDeltaGeneralizer(df_traj_collection).generalize(tolerance=timedelta(seconds=MIN_DIFF_BETWEEN_ROWS_INSEC))
              
              #Trips
              #Time gap threshold - 30 minutes
              # minimun distance for trips 100 mtr
              df_traj_trips = pd.DataFrame()
              for i in range(0,len(df_traj_collection)):
                  #i=1
                  df_traj = df_traj_collection.trajectories[i]
                  df_traj_df = df_traj.df
                  df_traj_df['numWeek'] = i
                  df_trips_traj = mpd.ObservationGapSplitter(df_traj).split(gap=timedelta(minutes=MIN_TIME_GAP_THRESHOLD_INMIN), min_length=MIN_LENGTH_TRIP_MTR)              
                  df_trip_total = pd.DataFrame()
                  for j in range(0,len(df_trips_traj)):
                      #j=1
                      df_trip = df_trips_traj.trajectories[j]
                      df_trip_df = df_trip.df 
                      df_trip_df['idTrip'] = j
                      df_trip_total = df_trip_total.append(df_trip_df)
                  df_traj_trips = df_traj_trips.append(df_trip_total)
              
              df_traj_trips = df_traj_trips.sort_index()   
              arrayWeeks = df_traj_trips.idWeek.unique()
              
              #Trips Summary
              df_traj_trips_summary = pd.DataFrame(columns=['idFile', 'idWeek', 'numWeek', 'idTrip', 'tripTimeMin', 'tripLenKm', 'tripPoints'])
              for numWeek in arrayWeeks:
                  #print(numWeek)
                  #numWeek = '2019-06-24/2019-06-30'
                  df_consolidado = df_traj_trips[df_traj_trips['idWeek'] == numWeek]
                  arrayTrips = df_consolidado.idTrip.unique()
                  for numTrip in arrayTrips:
                      #numTrip = 0
                      df_each_trip = df_consolidado[df_consolidado['idTrip'] == numTrip]
                      t = df_each_trip.reset_index().t
                      df_each_trip = df_each_trip.assign(delta_t=t.diff().values)
                      df_each_trip['delta_t_m'] = df_each_trip['delta_t'].dt.total_seconds()/60
                      df_each_trip_tmp = skmob.TrajDataFrame(df_each_trip, latitude='lat', longitude='lng', datetime='timestamp', user_id='uid')
                      df_each_trip_len = distance_straight_line(df_each_trip_tmp, show_progress=False)
                      df_each_trip=df_each_trip.reset_index()
                      trip_file = df_each_trip.at[0,'uid']
                      trip_idWeek = df_each_trip.loc[0]['idWeek']
                      trip_numWeek = df_each_trip.loc[0]['numWeek']
                      trip_idTrip = df_each_trip.loc[0]['idTrip']
                      trip_time_min = df_each_trip['delta_t_m'].sum()
                      trip_len_km = df_each_trip_len.at[0,'distance_straight_line']
                      trip_points = df_each_trip['idTrip'].count()
                      trip_serie = {'idFile':trip_file, 'idWeek':trip_idWeek, 'numWeek':trip_numWeek, 'idTrip':trip_idTrip, 'tripTimeMin':trip_time_min, 'tripLenKm':trip_len_km, 'tripPoints':trip_points}
                      df_traj_trips_summary = df_traj_trips_summary.append(trip_serie,ignore_index=True) 
                  
              #Stops - Activity Location
              #Detect the stops for each individual in a TrajDataFrame (each Trajectory). 
              #A stop is detected when the individual spends at least `minutes_for_a_stop` minutes (5 minutes)
              #within a distance `stop_radius_factor * spatial_radius` km from a given trajectory point. 
              #The stop's coordinates are the median latitude and longitude values of the points found 
              #within the specified distance
              df_traj_trips_stops = pd.DataFrame()
              for numWeek in arrayWeeks:
                  df_traj_trips_tmp = df_traj_trips[df_traj_trips['idWeek'] == numWeek]
                  df_traj_trips_tmp = skmob.TrajDataFrame(df_traj_trips_tmp, latitude='lat', longitude='long', datetime='timestamp', user_id='idFile')
                  #df_traj_total_tmp = detection.stops(df_traj_total_tmp, stop_radius_factor=0.1, minutes_for_a_stop=5.0, spatial_radius_km=0.2, leaving_time=True)
                  df_traj_trips_tmp = detection.stops(df_traj_trips_tmp, minutes_for_a_stop=MIN_MINUTES_FOR_A_STOP, stop_radius_factor=MIN_STOP_RADIUS_FACTOR, spatial_radius_km=MIN_SPATIAL_RADIUS_KM_STOP, leaving_time=False)
                  if(len(df_traj_trips_tmp) > 0):
                      df_traj_trips_stops = df_traj_trips_stops.append(df_traj_trips_tmp)
              
              try:
                  #Clusters  
                  df_traj_trips_stops = skmob.TrajDataFrame(df_traj_trips_stops, latitude='lat', longitude='long', datetime='datetime', user_id='uid')
                  df_traj_trips_stops = clustering.cluster(df_traj_trips_stops, cluster_radius_km=MIN_SPATIAL_RADIUS_KM, min_samples=MIN_SAMPLES_CLUSTER)       
              except:
                  msg = 'Archivo ' + str(k) + ' Imposible calcular clusters'
                  print(msg)
                  logging.warning(msg)    
              
              
              #Exportar los datos de Stops, Home y Resumen de viajes del archivo k
              excelFile = rawName + str(k) + '.csv'
              df_traj_trips_stops.to_csv(urlExcelData + excelFile, header=True)
              
              excelFile = 'Home-' + rawName + str(k) + '.csv'
              df_h.to_csv(urlExcelData + excelFile, header=True)
              
              excelFile = 'Trips-' + rawName + str(k) + '.csv'
              df_traj_trips_summary.to_csv(urlExcelData + excelFile, header=True)
              
              #Unir todo en un solo DataFrame
              df_total_stops = df_total_stops.append(df_traj_trips_stops)
              df_total_homes = df_total_homes.append(df_h)
              df_total_summary = df_total_summary.append(df_traj_trips_summary)
              msg = 'Archivo ' + str(k) + ' OK!!'
              print(msg)
              logging.debug(msg)
          else:
            msg = 'Archivo ' + str(k) + ' Contiene pocas observaciones'
            print(msg)
            logging.warning(msg)    
      else:
          msg = 'Archivo ' + str(k) + ' Contiene menos columnas de las requeridas'
          print(msg)
          logging.warning(msg)
  else:
       msg = 'Archivo ' + str(k) + ' No existe'
       print(msg)
       logging.warning(msg)

#Exportar todos los datos
df_total_stops.to_csv(urlExcelData + 'tldTotal.csv')
df_total_homes.to_csv(urlExcelData + 'Home-tldTotal.csv')
df_total_summary.to_csv(urlExcelData + 'Trips-tldTotal.csv')

msg = "Fin del proceso.........."
print(msg)
logging.debug(msg)
