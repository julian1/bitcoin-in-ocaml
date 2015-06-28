
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

    -- actually want >= so can see last date of change.
    where Row =  2
;


select 
    t.r,  
    a.hash, 
    format_amount( received( a.hash) ) as received, 
    format_amount( unspent( a.hash) ) as unspent 
from test t 
join input i on i.id = t.input_id
join output o on o.id = i.output_id
join output_address oa on oa.output_id = o.id

join address a on a.id = oa.address_id 



