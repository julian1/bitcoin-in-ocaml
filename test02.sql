

select * from

(SELECT 
    id,
    ROW_NUMBER() OVER(PARTITION BY r ) AS Row,
    r
    
    -- ROW_NUMBER() OVER(PARTITION BY r order by id ) AS Row
    FROM signature ) as s2

    where Row > 1;

