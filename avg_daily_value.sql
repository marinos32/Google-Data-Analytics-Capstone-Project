SELECT  
  pollutant_id,
  AVG(pollutant_value) as avg_station_value,
  station_code,
  EXTRACT(DAY FROM date_time) AS day,
  EXTRACT(MONTH FROM date_time) AS month,
  EXTRACT(YEAR FROM date_time) AS year

FROM
  `capstone-420118.Pollution.2023`

WHERE 
  pollutant_id = 2 OR # Nitrogen Dioxide(NO2)
  pollutant_id = 4 OR # Sulphur Dioxide (SO2)
  pollutant_id = 5 # Ozone (O3)

GROUP BY
  pollutant_id,
  station_code,
  day,
  month,
  year

ORDER BY
  day,
  month,
  station_code,
  pollutant_id