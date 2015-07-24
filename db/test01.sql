
drop view if exists _main ;

create view _main as
with recursive t( id, depth ) AS (
  select (
    -- tree root
    select block.id 
    from block 
    where hash = '\x0000000000000000147011a429bd8c2e30475ad61eaa544c3b971e99bb7bc307'  -- chain tip
  ), 0
  UNION ALL
  SELECT 
    block_previous_id, t.depth + 1
  FROM block
  join previous on previous.block_previous_id = block.id 
  join t on t.id = previous.block_id
)
select t.depth, block.* 
FROM t join block on block.id = t.id;



