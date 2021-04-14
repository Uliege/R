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

#Especifica el nombre de los archivos raw
rawName = "tld"
urlRawData = 'D:/G/GitHub/Uliege/R/TimeLineAnalysis/rawdata/'
urlExcelData = 'D:/G/GitHub/Uliege/R/TimeLineAnalysis/exceldata/'

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
#for k in range(0, 15):
  #k = 100
  idFile = rawName + str(k) + '.RData'
  urlFile = urlRawData + idFile
  r_file= pyreadr.read_r(urlFile)
  df1 = pd.DataFrame(r_file['dataQuito'])
  
  if(len(df1.columns) == 21):
            
      df1 = df1.drop(columns=['verticalAccuracy','altitude','heading','dateActivity','activity1','confidence1','activity2','confidence2','Y', 'M', 'D', 'Wd', 'W', 'Ho', 'Mi', 'Se',], axis=1)
      df1['dateTimeLine'] = df1['dateTimeLine'] - dt.timedelta(hours=5)
      df1= df1.rename(columns = {'dateTimeLine': 'timestamp', 'latitude':'lat', 'longitude':'long'})
      df1['idDiaNum'] = (df1.timestamp.dt.year.astype(str) + df1.timestamp.dt.month.astype(str) + df1.timestamp.dt.day.astype(str)).astype(int)
      df1['file'] = rawName + str(k)
      print("This dataset contains {} records ".format(len(df1)))
      #df1_0 = df1
      
      #Limpieza con scikit-mobility
      df1_skmob = skmob.TrajDataFrame(df1, latitude='lat', longitude='long', datetime='timestamp', user_id='file')
      #df1_1 = df1_skmob
      
      # filter out all points with a speed (in km/h) from the previous point higher than 200 km/h
      df1_filter = filtering.filter(df1_skmob, max_speed_kmh=MAX_SPEED_KMH)
      
      #Reduce the number of points in a trajectory for each individual in a TrajDataFrame. 
      #All points within a radius of `spatial_radius_km` kilometers from a given initial point are 
      #compressed into a single point that has the median coordinates of all points and 
      #the time of the initial point. - 10 metres 
      df1_compress = compression.compress(df1_filter, spatial_radius_km=MAX_SPATIAL_RADIUS_KM)
      
      df1 = df1_compress
      
      df1= df1.rename(columns = {'datetime': 'timestamp'})
      df1['t'] = pd.to_datetime(df1['timestamp'])
      #Index table
      df1 = df1.set_index('t').tz_localize(None)
      df1['idDia'] = df1.index.to_period('D')
      df1 = gpd.GeoDataFrame(df1, geometry=gpd.points_from_xy(df1.lng, df1.lat))
      df1 = df1.set_crs(epsg=4326)
      original_crs1 = df1.crs
      print("This dataset contains {} records ".format(len(df1)))
      print("The dataset covers the time between {} and {}.".format(df1.index.min(), df1.index.max()))
      print("That's {}".format(df1.index.max() - df1.index.min()))

      t = df1.reset_index().t
      df1 = df1.assign(delta_t=t.diff().values)
      df1['delta_t_m'] = df1['delta_t'].dt.total_seconds()/60

      #Trajectories
      # Desired minimum length of trajectories 200 metros
      df1_traj_collection = mpd.TrajectoryCollection(df1, traj_id_col='idDia', min_length=MIN_LENGTH_TRAJ_MTR)
      print("Finished creating {} trajectories".format(len(df1_traj_collection)))
      print('Número de días: ', len(df1['idDia'].unique()))

      #Desired minimum time difference between consecutive rows - 30 sec
      df1_traj_collection = mpd.MinTimeDeltaGeneralizer(df1_traj_collection).generalize(tolerance=timedelta(seconds=MIN_DIFF_BETWEEN_ROWS_INSEC))

      #Trips
      #Time gap threshold - 30 minutes
      # minimun distance for trips 100 mtr
      
      df1_traj_total = pd.DataFrame()
      for i in range(0,len(df1_traj_collection)):
          df1_traj = df1_traj_collection.trajectories[i]
          df1_traj_df = df1_traj.df
          df1_traj_df['idTraj'] = i
          df1_trips_traj = mpd.ObservationGapSplitter(df1_traj).split(gap=timedelta(minutes=MIN_TIME_GAP_THRESHOLD_INMIN), min_length=MIN_LENGTH_TRIP_MTR)
          df1_trip_total = pd.DataFrame()
          
          for j in range(0,len(df1_trips_traj)):
              df1_trip = df1_trips_traj.trajectories[j]
              df1_trip_df = df1_trip.df 
              df1_trip_df['idTrip'] = j
              df1_trip_total = df1_trip_total.append(df1_trip_df)
        
          df1_traj_total = df1_traj_total.append(df1_trip_total)
      
      df1_traj_total = df1_traj_total.sort_index()    
      arrayDias = df1_traj_total.idDia.unique()
      
      #Detect the stops for each individual in a TrajDataFrame (each Trajectory). 
      #A stop is detected when the individual spends at least `minutes_for_a_stop` minutes (5 minutes)
      #within a distance `stop_radius_factor * spatial_radius` km from a given trajectory point. 
      #The stop's coordinates are the median latitude and longitude values of the points found 
      #within the specified distance
      
      df1_traj_total_stops = pd.DataFrame()
      for dia in arrayDias:
          df1_traj_total_tmp = df1_traj_total[df1_traj_total['idDia'] == dia]
          df1_traj_total_tmp = skmob.TrajDataFrame(df1_traj_total_tmp, latitude='lat', longitude='long', datetime='timestamp', user_id='file')
          df1_traj_total_tmp = detection.stops(df1_traj_total_tmp, stop_radius_factor=0.1, minutes_for_a_stop=5.0, spatial_radius_km=0.2, leaving_time=True)
          if(len(df1_traj_total_tmp) > 0):
              df1_traj_total_stops = df1_traj_total_stops.append(df1_traj_total_tmp)

      #Exportar los datos del archivo k
      excelFile = rawName + str(k) + '.csv'
      df1_traj_total_stops.to_csv(urlExcelData + excelFile)
      
      #Unir todo en un solo DataFrame
      df_total_stops = df_total_stops.append(df1_traj_total_stops)
      print('Archivo ' + str(k) + ' OK!!')
  else:
      print('Archivo ' + str(k) + ' No se procesa')

#Exportar todos los datos
df_total_stops.to_csv(urlExcelData + 'tldTotal.csv')

print("Fin del proceso..........")