-- Creating the table for the external CSV file
CREATE TABLE us_pop_by_state (
    STATE INT PRIMARY KEY,
    NAME VARCHAR(255),
    Name_ID VARCHAR(2),
    ESTIMATESBASE2020 INT,
    POPESTIMATE2020 INT,
    POPESTIMATE2021 INT,
    POPESTIMATE2022 INT,
    POPESTIMATE2023 INT
);

select * From us_pop_by_state

-- -- Adding a column to calculate the population change percentage
-- ALTER TABLE us_pop_by_state
-- ADD COLUMN Population_Change_Percent DECIMAL(10, 2);

-- -- Calculating the population change
-- UPDATE us_pop_by_state
-- SET Population_Change_Percent = 
--     ((CAST(popestimate2023 AS DECIMAL) - CAST(popestimate2022 AS DECIMAL)) / CAST(popestimate2022 AS DECIMAL)) * 100;


