
begin;

-- important chainstate projection to tx ought to be easy, we just
-- create a view of tx's that are the mainchain only, and use that
-- for all the received, unspent etc.

-- can use materialized views for height etc if . may not be bad...
------------

create or replace view _leaves as
select
  b.hash as b,
  pb.hash as pb,
  pb.id
from block b
-- should reorder to left join...
right join block pb on pb.id = b.previous_id
where b.id is null
;

-- a view of the block table including height
-- drop view if exists _leaves2 ; 
-- drop view if exists _block ; 

create or replace view _block as
with recursive t( id, height ) AS (
  select (select id from block where previous_id is null), 0
  UNION ALL
  SELECT block.id, t.height + 1
  FROM block
  join t on t.id = block.previous_id
)
select t.height, block.* 
FROM t join block on block.id = t.id;


-- could return more than one entry...
-- ** query to get longest chain tip **
-- select * from _block where
-- height = (SELECT max(height) FROM _block ) ; 

create or replace view _leaves2 as
select 
  now() - time as when, 
  time, 
  height,
  hash,
  b.id as _blockid
from _leaves l 
join _block b on b.id = l.id 
order by height desc;

commit;

