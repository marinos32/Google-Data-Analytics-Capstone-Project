SELECT  
  id.station_name_en,
  CONCAT(tr.pollutant_name_en, " ",tr.pollutant_code) as pollutant_name,
  CONCAT(tr.avg_station_value," " ,tr.unit_of_measurement_en) AS avg_station_value,
  tr.month,
  tr.year
FROM
  `capstone-420118.Pollution.pollutant_25_PM10` AS tr

JOIN
  `capstone-420118.Pollution.station_id` AS id

ON  
tr.station_code = id.station_code