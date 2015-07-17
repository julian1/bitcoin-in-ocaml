
-- todo change this to a materialized view 


-- drop table test;
-- create table test( id integer, input_id integer, r bytea, row integer );

-- truncate test;
-- insert into test

begin;

CREATE MATERIALIZED VIEW _test as 

select * from
(SELECT 
    id,
    input_id,
    r,
    ROW_NUMBER() OVER(PARTITION BY r ) AS Row
    
    -- ROW_NUMBER() OVER(PARTITION BY r order by id ) AS Row
    FROM signature ) as s2

    -- actually want >= so can see last date of change.
    where Row =  2
;

-- create index on test( input_id);
-- create index on test( r); 

commit;

