

prepare insert_block as 


 with s as ( select b.id from block b where hash = $2)


insert into block(hash,previous_id,time) 
 
select $1, (select b.id from block b where hash = $2) , to_timestamp($3) at time zone 'UTC'
from s
where s.id is not null

returning id 


