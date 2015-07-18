
begin;

-- important chainstate projection to tx ought to be easy, we just
-- create a view of tx's that are the mainchain only, and use that
-- for all the received, unspent etc.

-- can use materialized views for height etc if . may not be bad...
------------
drop view if exists  _leaves2; 
drop view if exists  _leaves; 
drop view if exists  _height; 

create view _leaves as
select
  b.id as id,
  b.hash as hash
  -- p.*
from block b
left join previous p on p.block_previous_id = b.id 
where p.id is null
;


create view _height as
with recursive t( id, height ) AS (
  select (
    -- tree root
    select block.id 
    from block 
    left join previous on previous.block_id = block.id 
    where previous.id is null
  ), 0
  UNION ALL
  SELECT 
    block_id, t.height + 1
  FROM block
  join previous on previous.block_id = block.id 
  join t on t.id = previous.block_previous_id
)
select t.height, block.* 
FROM t join block on block.id = t.id;



create view _leaves2 as
select 
  now() - time as when, 
  time, 
  _height.height,
  _leaves.hash,
  _leaves.id as block_id
from _leaves
join _height on _height.id = _leaves.id 
order by _height.height desc
;

commit;

