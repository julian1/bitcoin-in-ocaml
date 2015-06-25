
-- VERY IMPORTANT - constrain with a function?????
-- eg. lookup a balance for a particular address... and 
-- it can do the group by efficiently. 

-- ok, even when we select into a table the join with x is really
-- heavy... 

-- it's basically the same cost 46 million ... as evaluating all 
-- of the x balance view


--create table test2( r bytea, address bytea );
--insert into test2

--explain select * from 

select t.r,  a.hash, received1( a.hash) 
from test t 
join input i on i.id = t.input_id
join output o on o.id = i.output_id
join output_address oa on oa.output_id = o.id

join address a on a.id = oa.address_id 


-- join address a on a.id = oa.address_id 
-- ok, maybe its not the join order. but the lack of  
-- group by....
-- we're trying to join it against address. it has to scan 
-- to group the addresses. therefore it cals all addressesou
-- lets join it to get all outputs for the address
