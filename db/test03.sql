
-- eg. select * from outputs where address = '\x119b098e2e980a229e139a9ed01a469e518e6f26' 

-- should create a view 
drop view if exists both_ ;
drop view if exists outputs;

-- CHange name to _outputs  and _inputs

create view outputs as
select 
  b.time as time,
  tx.hash as tx,
  tx.id as tx_id,
  o.index as index,
  o.amount as amount,
  a.hash as address 
  
from output o
join tx on tx.id = o.tx_id  
join block b on b.id = tx.block_id

join output_address oa on oa.output_id = o.id
join address a on a.id = oa.address_id 

order by b.time desc, tx.id, o.index  
;


drop view if exists inputs;

create view inputs as
select 
  -- b.time,
  tx.hash as tx,
  tx.id as tx_id,
  -- i.* 
  -- o.index as index,
  o.amount as amount,
  a.hash as address 
   
from input i
join tx on tx.id = i.tx_id  
join block b on b.id = tx.block_id

-- perhaps we should just join on the outputs view ...
join output o on o.id = i.output_id
join output_address oa on oa.output_id = o.id
join address a on a.id = oa.address_id 
;

-- actually we probably want a union
create view both_ as
select 
  i.*
from outputs o
-- left join inputs i on i.tx_id = o.tx_id   -- wrong should be txid 
left join inputs i on i.tx = o.tx -- wrong should be txid 

;



