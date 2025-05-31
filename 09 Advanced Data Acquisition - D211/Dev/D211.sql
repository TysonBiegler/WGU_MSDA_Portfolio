-- D211 Advanced Data Acquisition
-- Student ID: 012170282

-- Creating the table for the external CSV file
CREATE TABLE us_pop_by_state (
    STATE INT,
    NAME VARCHAR(255),
    Name_ID VARCHAR(2) PRIMARY KEY,
    ESTIMATESBASE2020 INT,
    POPESTIMATE2020 INT,
    POPESTIMATE2021 INT,
    POPESTIMATE2022 INT,
    POPESTIMATE2023 INT
);

COPY us_pop_by_state (STATE, NAME, Name_ID, ESTIMATESBASE2020, POPESTIMATE2020, POPESTIMATE2021, POPESTIMATE2022, POPESTIMATE2023)
FROM 'C:\Program Files\PostgreSQL\16\NST-EST2023-POPCHG2020_2023_CLEANED.csv'
DELIMITER ','
CSV HEADER;






-- <<<<<<<< NO NEED TO RUN THE FOLLOWING QUERIES. THESE ARE FOR DEMONSTRATION ONLY >>>>>>

-- this query joins the customer table to the location table 
SELECT "customer"."age" AS "age",
  "customer"."bandwidth_gp_year" AS "bandwidth_gp_year",
  "customer"."children" AS "children",
  CAST("customer"."churn" AS TEXT) AS "churn",
  CAST("location1"."city" AS TEXT) AS "city (location1)",
  "customer"."contacts" AS "contacts",
  "customer"."contract_id" AS "contract_id",
  CAST("location1"."county" AS TEXT) AS "county (location1)",
  CAST("customer"."customer_id" AS TEXT) AS "customer_id",
  "customer"."email" AS "email",
  CAST("customer"."gender" AS TEXT) AS "gender",
  "customer"."income" AS "income",
  "customer"."job_id" AS "job_id",
  "customer"."lat" AS "lat",
  "customer"."lng" AS "lng",
  "location1"."location_id" AS "location_id (location1)",
  "customer"."location_id" AS "location_id",
  CAST("customer"."marital" AS TEXT) AS "marital",
  "customer"."monthly_charge" AS "monthly_charge",
  "customer"."outage_sec_week" AS "outage_sec_week",
  "customer"."payment_id" AS "payment_id",
  "customer"."population" AS "population",
  CAST("customer"."port_modem" AS TEXT) AS "port_modem",
  CAST("location1"."state" AS TEXT) AS "state (location1)",
  CAST("customer"."tablet" AS TEXT) AS "tablet",
  CAST("customer"."techie" AS TEXT) AS "techie",
  "customer"."tenure" AS "tenure",
  "customer"."yearly_equip_faiure" AS "yearly_equip_faiure",
  "location1"."zip" AS "zip (location1)"
FROM "public"."customer" "customer"
  INNER JOIN "public"."location" "location1" ON ("customer"."location_id" = "location1"."location_id")

-- this query joins the location table to the aditional csv census data table named “us_pop_by_state”
SELECT CAST("location"."city" AS TEXT) AS "city",
  CAST("location"."county" AS TEXT) AS "county",
  "us_pop_by_state1"."estimatesbase2020" AS "estimatesbase2020 (us_pop_by_state1)",
  "location"."location_id" AS "location_id (location)",
  "us_pop_by_state1"."name" AS "name (us_pop_by_state1)",
  "us_pop_by_state1"."name_id" AS "name_id (us_pop_by_state1)",
  "us_pop_by_state1"."popestimate2020" AS "popestimate2020 (us_pop_by_state1)",
  "us_pop_by_state1"."popestimate2021" AS "popestimate2021 (us_pop_by_state1)",
  "us_pop_by_state1"."popestimate2022" AS "popestimate2022 (us_pop_by_state1)",
  "us_pop_by_state1"."popestimate2023" AS "popestimate2023 (us_pop_by_state1)",
  "us_pop_by_state1"."state" AS "state (us_pop_by_state1)",
  CAST("location"."state" AS TEXT) AS "state",
  "location"."zip" AS "zip"
FROM "public"."location" "location"
  INNER JOIN "public"."us_pop_by_state" "us_pop_by_state1" ON (CAST("location"."state" AS TEXT) = "us_pop_by_state1"."name_id")
