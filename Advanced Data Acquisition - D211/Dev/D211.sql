create table us_pop (
	rank float,
	state varchar(50),
	state_code varchar(50),
	"2020_census" bigint,
	percent_of_total float
);

select * FROM us_pop
select * from customer

-- removing the row total
delete from us_pop
where ctid = (
	select ctid
	from us_pop
	order by ctid
	limit 1 offset 51
)

-- converting churn to binary
ALTER TABLE customer ADD COLUMN Churn_Binary INTEGER;
UPDATE customer
SET Churn_Binary = CASE 
    WHEN Churn = 'Yes' THEN 1
    WHEN Churn = 'No' THEN 0
    ELSE NULL
END;

