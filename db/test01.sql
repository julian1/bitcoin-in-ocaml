
drop view if exists _main ;

create view _main as
with recursive t( id ) AS (
    select block.id 
    from block 
    where hash = '\x0000000000000000147011a429bd8c2e30475ad61eaa544c3b971e99bb7bc307'  -- chain tip
  UNION ALL
  SELECT 
    block_previous_id
  FROM block
  join previous on previous.block_previous_id = block.id 
  join t on t.id = previous.block_id
)
select block.* 
FROM t join block on block.id = t.id;



