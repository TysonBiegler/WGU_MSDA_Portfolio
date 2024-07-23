CREATE TABLE public.services (
-- The customer_id column in the services table is the primary key of the table
customer_id VARCHAR(20) NOT NULL PRIMARY KEY, internetService VARCHAR(20) NOT NULL,
phone VARCHAR(3) NOT NULL,
multiple VARCHAR(3) NOT NULL,
onlineSecurity VARCHAR(3) NOT NULL,
onlineBackup VARCHAR(3) NOT NULL,
deviceProtection VARCHAR(3) NOT NULL,
techSupport VARCHAR(3) NOT NULL,
-- The ‘customer_id’ column in the ‘services’ table is also the foreign key that references the primary key ‘customer_id’ in the ‘customer’ table.
CONSTRAINT services_customer_id FOREIGN KEY (customer_id)  
REFERENCES public.customer (customer_id) ON DELETE CASCADE 
);

ALTER TABLE public.services
OWNER to postgres;

 
COPY services (
customer_id,
internetService,
phone,
multiple,
onlineSecurity,
onlineBackup,
deviceProtection,
techSupport
)
FROM 'C:\LabFiles\Services.csv'
DELIMITER ','
CSV HEADER;


SELECT
CASE 
WHEN s.techsupport = 'No' THEN 'Not subscribed' 
WHEN s.techsupport = 'Yes' THEN 'Subscribed' 
END AS tech_support, 
COUNT(*) AS churned_customers
FROM 
services s
JOIN 
customer c ON s.customer_id = c.customer_id
WHERE 
c.churn = 'Yes'
GROUP BY
tech_support
ORDER BY
tech_support;
