# -*- coding: utf-8 -*-
"""
Created on Wed Apr 14 08:11:50 2021

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
from skmob.preprocessing import filtering
from skmob.preprocessing import compression
from skmob.preprocessing import detection
import os 

#Data Project
urlProject = os.getcwd()
urlRawData = urlProject + '/rawdata/'
urlExcelData = urlProject + '/exceldata/'

#Especifica el nombre de los archivos raw
rawName = "tld"

#Constantes
MAX_SPEED_KMH = 200
MAX_SPATIAL_RADIUS_KM = 0.01
MIN_LENGTH_TRAJ_MTR = 200 # meters 
MIN_DIFF_BETWEEN_ROWS_INSEC = 30
MIN_TIME_GAP_THRESHOLD_INMIN = 30
MIN_LENGTH_TRIP_MTR = 100 

df_total_stops = pd.DataFrame()

#Proceso repetitivo
for k in range(0, 161):
  #Change here to process individual files  
  #k = 0
  idFile = rawName + str(k) + '.RData'
  urlFile = urlRawData + idFile
  r_file= pyreadr.read_r(urlFile)
  df = pd.DataFrame(r_file['dataQuito'])
  
  if(len(df.columns) == 21):
      df0 = df      
      df = df.drop(columns=['verticalAccuracy','altitude','heading','dateActivity','activity1','confidence1','activity2','confidence2','Y', 'M', 'D', 'Wd', 'W', 'Ho', 'Mi', 'Se',], axis=1)
      df['dateTimeLine'] = df['dateTimeLine'] - dt.timedelta(hours=5)
      df= df.rename(columns = {'dateTimeLine': 'timestamp', 'latitude':'lat', 'longitude':'long'})
      df['idDiaNum'] = (df.timestamp.dt.year.astype(str) + df.timestamp.dt.month.astype(str) + df.timestamp.dt.day.astype(str)).astype(int)
      df['file'] = rawName + str(k)
      print("This dataset contains {} records ".format(len(df)))
      
      #Limpieza con scikit-mobility
      df_skmob = skmob.TrajDataFrame(df, latitude='lat', longitude='long', datetime='timestamp', user_id='file')
      
      # filter out all points with a speed (in km/h) from the previous point higher than 200 km/h
      df_filter = filtering.filter(df_skmob, max_speed_kmh=MAX_SPEED_KMH)
      
      #Reduce the number of points in a trajectory for each individual in a TrajDataFrame. 
      #All points within a radius of `spatial_radius_km` kilometers from a given initial point are 
      #compressed into a single point that has the median coordinates of all points and 
      #the time of the initial point. - 10 metres 
      df_compress = compression.compress(df_filter, spatial_radius_km=MAX_SPATIAL_RADIUS_KM)
      
      df = df_compress
      
      df= df.rename(columns = {'datetime': 'timestamp'})
      df['t'] = pd.to_datetime(df['timestamp'])
      #Index table
      df = df.set_index('t').tz_localize(None)
      df['idDia'] = df.index.to_period('D')
      df = gpd.GeoDataFrame(df, geometry=gpd.points_from_xy(df.lng, df.lat))
      df = df.set_crs(epsg=4326)
      original_crs1 = df.crs
      print("This dataset contains {} records ".format(len(df)))
      print("The dataset covers the time between {} and {}.".format(df.index.min(), df.index.max()))
      print("That's {}".format(df.index.max() - df.index.min()))

      t = df.reset_index().t
      df = df.assign(delta_t=t.diff().values)
      df['delta_t_m'] = df['delta_t'].dt.total_seconds()/60

      #Trajectories
      # Desired minimum length of trajectories 200 metros
      df_traj_collection = mpd.TrajectoryCollection(df, traj_id_col='idDia', min_length=MIN_LENGTH_TRAJ_MTR)
      print("Finished creating {} trajectories".format(len(df_traj_collection)))
      print('Número de días: ', len(df['idDia'].unique()))

      #Desired minimum time difference between consecutive rows - 30 sec
      df_traj_collection = mpd.MinTimeDeltaGeneralizer(df_traj_collection).generalize(tolerance=timedelta(seconds=MIN_DIFF_BETWEEN_ROWS_INSEC))

      #Trips
      #Time gap threshold - 30 minutes
      # minimun distance for trips 100 mtr
      
      df_traj_total = pd.DataFrame()
      for i in range(0,len(df_traj_collection)):
          df_traj = df_traj_collection.trajectories[i]
          df_traj_df = df_traj.df
          df_traj_df['idTraj'] = i
          df_trips_traj = mpd.ObservationGapSplitter(df_traj).split(gap=timedelta(minutes=MIN_TIME_GAP_THRESHOLD_INMIN), min_length=MIN_LENGTH_TRIP_MTR)
          df_trip_total = pd.DataFrame()
          
          for j in range(0,len(df_trips_traj)):
              df_trip = df_trips_traj.trajectories[j]
              df_trip_df = df_trip.df 
              df_trip_df['idTrip'] = j
              df_trip_total = df_trip_total.append(df_trip_df)
        
          df_traj_total = df_traj_total.append(df_trip_total)
      
      df_traj_total = df_traj_total.sort_index()    
      arrayDias = df_traj_total.idDia.unique()
      
      #Detect the stops for each individual in a TrajDataFrame (each Trajectory). 
      #A stop is detected when the individual spends at least `minutes_for_a_stop` minutes (5 minutes)
      #within a distance `stop_radius_factor * spatial_radius` km from a given trajectory point. 
      #The stop's coordinates are the median latitude and longitude values of the points found 
      #within the specified distance
      
      df_traj_total_stops = pd.DataFrame()
      for dia in arrayDias:
          df_traj_total_tmp = df_traj_total[df_traj_total['idDia'] == dia]
          df_traj_total_tmp = skmob.TrajDataFrame(df_traj_total_tmp, latitude='lat', longitude='long', datetime='timestamp', user_id='file')
          df_traj_total_tmp = detection.stops(df_traj_total_tmp, stop_radius_factor=0.1, minutes_for_a_stop=5.0, spatial_radius_km=0.2, leaving_time=True)
          if(len(df_traj_total_tmp) > 0):
              df_traj_total_stops = df_traj_total_stops.append(df_traj_total_tmp)

      #Exportar los datos del archivo k
      excelFile = rawName + str(k) + '.csv'
      df_traj_total_stops.to_csv(urlExcelData + excelFile)
      
      #Unir todo en un solo DataFrame
      df_total_stops = df_total_stops.append(df_traj_total_stops)
      print('Archivo ' + str(k) + ' OK!!')
  else:
      print('Archivo ' + str(k) + ' No se procesa')

#Exportar todos los datos
df_total_stops.to_csv(urlExcelData + 'tldTotal.csv')

print("Fin del proceso..........")