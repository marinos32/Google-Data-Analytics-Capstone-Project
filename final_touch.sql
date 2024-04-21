SELECT
  dal.date,
  dal.station_name,
  CONCAT(id.pollutant_name_en, " (",id.pollutant_code, ")") AS pollutant,
  dal.avg_daily_station_value
FROM
  `capstone-420118.Pollution.daily_almost` as dal
JOIN
  `capstone-420118.Pollution.pollutant_id` as id
ON
  dal.pollutant_id = id.pollutant_id