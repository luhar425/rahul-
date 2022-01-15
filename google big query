
#Total hospitalized patients by region
Most affected
SELECT 
region_name,
SUM(total_hospitalized_patients) AS total
FROM
  `bigquery-public-data.covid19_italy.data_by_region`

GROUP BY 
region_name
ORDER BY 
total desc limit 10 ;

least affected
SELECT 
region_name,
SUM(total_hospitalized_patients) AS total
FROM
  `bigquery-public-data.covid19_italy.data_by_region`
GROUP BY 
region_name
ORDER BY 
total asc  limit 10 ;

#confirmed cases by provinces
SELECT 
name,
SUM(confirmed_cases) AS total
FROM
  `bigquery-public-data.covid19_italy.data_by_province`
GROUP BY 
name
ORDER BY 
total asc  limit 10 ;

SELECT 
name,
SUM(confirmed_cases) AS total
FROM
  `bigquery-public-data.covid19_italy.data_by_province`
GROUP BY 
name
ORDER BY 
total desc  limit 10 ;

#total  confirmed cases
SELECT 
SUM(confirmed_cases) AS total_confirmed_cases
FROM
  `bigquery-public-data.covid19_italy.data_by_province`

#fatality ratio 
SELECT 
SUM(total_hospitalized_patients) AS hospitalized_patients,
SUM(hospitalized_patients_intensive_care) AS total_inteinsive_care_patients,
SUM(hospitalized_patients_intensive_care)/SUM(total_hospitalized_patients)*100 AS fatality_ratio
FROM
  `bigquery-public-data.covid19_italy.data_by_region`

#total data
SELECT 
region_name AS reg,
SUM(hospitalized_patients_symptoms) as total_hospitalized_patients_symptoms,
SUM(hospitalized_patients_intensive_care) AS total_hospitalized_patients_intensive_care,
SUM(total_hospitalized_patients) AS total_total_hospitalized_patients,
SUM(home_confinement_cases) AS total_home_confinement_cases,
SUM(recovered) AS total_recovered,
SUM(deaths) AS total_deaths,
SUM(total_confirmed_cases) as total_confirmed_cases,
SUM(tests_performed) AS total_test_performed
FROM
  `bigquery-public-data.covid19_italy.data_by_region`
GROUP BY 
reg
ORDER BY
reg limit 10

SELECT 
EXTRACT(MONTH from date) as month,
EXTRACT(YEAR FROM date) AS year,
SUM(confirmed_cases) AS total_confirmed_cases
FROM
  `bigquery-public-data.covid19_italy.data_by_province`
where 
date between '2020-2-15' and '2020-11-11'
group by 
1,2 

WITH cases_by_date AS (
SELECT
region_name,
cast(date as date) as dat,
SUM(total_confirmed_cases) AS cases
FROM
    `bigquery-public-data.covid19_italy.data_by_region`
where 
cast(date as date) between date_SUB(CURRENT_date() ,INTERVAL 1 month) and CURRENT_date()
GROUP BY 
1,2
ORDER BY
1,2 ASC 
)
, previous_day_comparison AS 
(SELECT
region_name,
  dat,
  cases,
  LAG(cases) OVER(ORDER BY dat) AS previous_day,
  cases - LAG(cases) OVER(ORDER BY dat) AS net_new_cases,
  (cases - LAG(cases) OVER(ORDER BY dat))*100/LAG(cases) OVER(ORDER BY dat) AS percentage_increase
FROM cases_by_date
)
SELECT 
region_name,
Dat, 
cases as Confirmed_Cases_On_Day, 
previous_day as Confirmed_Cases_Previous_Day, 
percentage_increase as Percentage_Increase_In_Cases
FROM previous_day_comparison
WHERE percentage_increase > 10
