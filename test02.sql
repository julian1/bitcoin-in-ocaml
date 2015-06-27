
-- drop table test;
--create table test( id integer, input_id integer, r bytea, row integer );

truncate test;

insert into test
select * from
(SELECT 
    id,
    input_id,
    r,
    ROW_NUMBER() OVER(PARTITION BY r ) AS Row
    
    -- ROW_NUMBER() OVER(PARTITION BY r order by id ) AS Row
    FROM signature ) as s2

    where Row =  2
;

