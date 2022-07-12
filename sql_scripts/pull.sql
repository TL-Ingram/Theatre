SELECT *
FROM dbo.reporting_theatres
WHERE surgery_date_dt >= '2022-01-01 00:00:00.000' 
AND surgery_date_dt <= '2022-07-11 00:00:00.000'
AND site_description = 'Wrightington Hospital'

SELECT * 
FROM