WITH dt AS (
  SELECT
    CONCAT(day,"/",month,"/",year) AS date,
    station_code,
    pollutant_id,
    avg_station_value
  FROM
    `capstone-420118.Pollution.daily_avg_2023`
)

SELECT
  dt.date,
  sid.station_name_en,
  dt.pollutant_id,
  dt.avg_station_value

FROM dt
JOIN
  `capstone-420118.Pollution.station_id` AS sid
ON
  dt.station_code = sid.station_code
