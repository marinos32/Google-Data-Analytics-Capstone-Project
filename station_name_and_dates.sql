SELECT
  CONCAT(dl.day,"/",dl.month,"/",dl.year) AS date,
  id.station_name_en AS station_name,
  dl.pollutant_id,
  dl.avg_daily_station_value

FROM
  `capstone-420118.Pollution.all_daily_data` AS dl
JOIN
  `capstone-420118.Pollution.station_id` AS id
ON
  dl.station_code = id.station_code