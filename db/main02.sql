
begin;

-- important chainstate projection to tx ought to be easy, we just
-- create a view of tx's that are the mainchain only, and use that
-- for all the received, unspent etc.

-- can use materialized views for height etc if . may not be bad...
------------

drop view if exists _locator_hashes2 ; 
drop view if exists _locator_hashes ; 
drop view if exists  _main ;
drop view if exists  _longest;

drop view if exists  _leaves2_dynamic;
drop view if exists  _leaves2;
drop view if exists  _leaves;
drop view if exists _height_dynamic ;
drop view if exists _genesis ;



create view _leaves as
  select
    b.id as block_id
  from block b
  left join previous p on p.block_previous_id = b.id
  where p.id is null
;


-- always just one 

create view _genesis as
  select
    block.id as block_id
  from block
  left join previous on previous.block_id = block.id
  where previous.id is null
;

-- max height
-- always just one...

create view _longest as
  select
    id as block_id,
    height 
  from block
  order by height desc
  limit 1
;




create view _height_dynamic as
  with recursive t( id, height_ ) AS (
    select (
      -- genesis - only selects one of them...
      select block_id from _genesis
    ), 0
    UNION ALL
    SELECT
      block_id, t.height_ + 1
    FROM block
    join previous on previous.block_id = block.id
    join t on t.id = previous.block_previous_id
  )
  select t.height_, block.*
  FROM t
  join block on block.id = t.id
;


create view _leaves2_dynamic as
  select
    now() - block.time as when,
    block.time,
    _height_dynamic.height,
    block.hash,
    _leaves.block_id
  from _leaves
  join _height_dynamic on _height_dynamic.id = _leaves.block_id
  join block on block.id = _leaves.block_id
  -- order by _height.height desc
;

create view _leaves2 as
  select
    now() - block.time as when,
    block.time,
    block.height,
    block.hash,
    block.id as block_id
  from _leaves
  join block on block.id = _leaves.block_id
  -- order by _height.height desc
;


-- create materialized view _main as
create view _main as
  with recursive t( id, depth ) AS (
    select (
      -- tree tip
      select block_id from _longest
    ), 0
    UNION ALL
    SELECT
      block_previous_id, t.depth + 1
    FROM block
    join previous on previous.block_previous_id = block.id
    join t on t.id = previous.block_id
  )
  select 
    t.depth, 
    block.*
  FROM t join block on block.id = t.id
;

-- ok, it takes 2 secs, to get _main, this means, i wonder how long it will take
-- to get all utxos to verfiy a block?  it may be a lot less than this.  

-- and... ughhh... the block kind of has to be inserted... txs that self-referential txs in the same block.
-- damnnn.

-- TODO change name/function longest to best?
-- change name hashes to height

create view _locator_hashes as (
  with recursive t( height, start_, step ) AS (
    -- tree leaf
    select (select height from _longest), 1, 1
    UNION ALL
    SELECT
        t.height - t.step,
        t.start_ + 1,
        CASE WHEN t.start_ >= 10 THEN t.step * 2
        ELSE t.step
    END
    FROM t
    where t.height > 0
  )
  select height 
  FROM t where t.height > 0
  )
  union all
  select 0
;


create view _locator_hashes2 as 
  select _main.* 
  from _main 
  join _locator_hashes on _locator_hashes.height = _main.height 
;

commit;

