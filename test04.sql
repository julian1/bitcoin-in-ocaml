
-- need to try a vacuum analyze on large data.
-- the query with a limit of 10 should be easy

drop view x;
drop view unspent;
drop view received;


-- null means unknown address or nothing unspent.

create or replace view unspent as 
select a.hash, count(o.amount), sum(o.amount) as unspent
from address a
join output_address oa on oa.address_id = a.id 
join output o on o.id = oa.output_id 
left join input i on i.output_id = o.id
where i.id is null -- ie unspent
group by a.hash
; 

create or replace view received as
select a.hash, count(o.amount), sum(o.amount) as received
from address a
join output_address oa on (oa.address_id = a.id)
join output o on o.id = oa.output_id
group by a.hash
; 

-- create or replace view x as 
-- select a.hash, r.received, u.unspent  from  
-- address a
-- left join unspent u on u.hash = a.hash
-- left join received r on r.hash = a.hash 
-- ; 



CREATE OR REPLACE FUNCTION received1( hash1 bytea )
RETURNS bigint AS $total$
declare
    total bigint ;
BEGIN
   SELECT received into total FROM received where hash = hash1;
   RETURN total;
END;
$total$ LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION unspent1( hash1 bytea )
RETURNS bigint AS $total$
declare
    total bigint ;
BEGIN
   SELECT unspent into total FROM unspent where hash = hash1;
   RETURN total;
END;
$total$ LANGUAGE plpgsql;

-- should have a count outputs function as well... 

-- the function constrains the query, we could even build another
-- view from this if we wanted.  


