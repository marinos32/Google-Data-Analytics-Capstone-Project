SELECT
  *
FROM
  `capstone-420118.Pollution.daily_avg_2018`
UNION ALL
SELECT
  *
FROM
  `capstone-420118.Pollution.daily_avg_2019`
UNION ALL
SELECT
  *
FROM
  `capstone-420118.Pollution.daily_avg_2020`
UNION ALL
SELECT
  *
FROM
  `capstone-420118.Pollution.daily_avg_2021`
UNION ALL
SELECT
  *
FROM
  `capstone-420118.Pollution.daily_avg_2022`
UNION ALL
SELECT
  *
FROM
  `capstone-420118.Pollution.daily_avg_2023`

ORDER BY
  year,
  month,
  station_code,
  pollutant_id
  
