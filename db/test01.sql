
begin;

drop view if exists _tx ;
-- drop materialized view if exists _main ;
drop view if exists _main ;

-- OK, a way to do tx output verification 
-- on receipt of block set the head to the block. this will make it available in main chain view 
-- then call refresh materialized view on the _main 
-- then tx's can refer to tx's in the just received block 


-- create index on _main( id );


create view _tx as
select 
  tx.*, 
  tx_block.block_id, 
  -- substring( block_data.data, tx_block.pos + 1, tx_block.length)
  substring( block_data.data, tx_block.pos + 1, 10)
from tx 
join tx_block on tx_block.tx_id = tx.id 
join _main on _main.id = tx_block.block_id 
left join block_data on block_data.block_id = tx_block.block_id 
;

-- it does appear to work
-- select * from _tx order by hash  limit 50 ;
commit;
