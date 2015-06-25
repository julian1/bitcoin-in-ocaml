
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


create or replace view x as 
select a.hash, r.received, u.unspent  from  
address a
left join unspent u on u.hash = a.hash
left join received r on r.hash = a.hash 

; 


