-- converting Churn to binary
UPDATE customer
SET Churn = CASE
    WHEN Churn = 'Yes' THEN 1
    WHEN Churn = 'No' THEN 0
END;

-- converting churn to integer
ALTER TABLE customer
ALTER COLUMN Churn TYPE INTEGER USING Churn::integer;


-- Checking if any null values were introduced
SELECT COUNT(*)
FROM customer
WHERE Churn IS NULL;
