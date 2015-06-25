
-- ok, we want to join it with the 
--- we could try to put this in a table... and then join to x.
--- it's weird why it's slow?

select t.r, o.amount, a.hash --, x.received, x.unspent
from test t 
join input i on i.id = t.input_id
join output o on o.id = i.output_id
join output_address oa on oa.output_id = o.id
join address a on a.id = oa.address_id 
-- join x on x.hash = a.hash 
--join received on received.hash = a.hash 

-- left join address a on a.id =  

; 

