
begin;
-- so we should'nt deal with hashes, just numbers


--- note that this thing is very fast to calculate - < 1 ms, because of the step size increment. 
--- we want to pass in the height explicitly

drop view if exists _locator_hashes ;

create view _locator_hashes as 
  (
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




drop function if exists flocator_height( int );

CREATE FUNCTION flocator_height(arg_height int) 
RETURNS TABLE(height int)
AS $$ 
begin 
  return query select * from ( 
    (
      with recursive t( height_, start_, step ) AS (
        -- non recursive
        select arg_height, 1, 1
        UNION ALL
        -- recursive
        SELECT
            t.height_ - t.step,
            t.start_ + 1,
            CASE WHEN t.start_ >= 10 THEN t.step * 2
            ELSE t.step
        END
        FROM t
        where t.height_ > 0
      )
      select height_ 
      FROM t where t.height_ > 0
      )
      union all
      select 0
  ) as x; 
end;
$$ LANGUAGE plpgsql volatile ;


-------------------------
-- calculate chain, this function is expensive and could use a memoizing table for height, depth...

drop function if exists fmain( int );

CREATE FUNCTION fmain(arg_block_id int) 
RETURNS TABLE(block_id int, height int, depth int)
AS $$ 
begin 
  return query select * from ( 

    with recursive t( id, depth ) AS (
      select arg_block_id, 0
      UNION ALL
      SELECT
        block_previous_id, t.depth + 1
      FROM block
      join previous on previous.block_previous_id = block.id
      join t on t.id = previous.block_id
    )
    select 
      block.id,
      block.height,
      t.depth
      -- block.*
    FROM t join block on block.id = t.id

  ) as x; 
end;
$$ LANGUAGE plpgsql volatile ;


--- so now we want to join them...

-- change name to locator_block_id
-- should we add the hash. yes because that's what we send

drop function if exists fx( int );

CREATE FUNCTION fx(arg_block_id int) 
RETURNS TABLE(block_id int, height int, depth int, hash bytea)
AS $$ 
begin 
  return query 
    select 
      m.block_id, 
      m.height, 
      m.depth, 
      b.hash
    from fmain( arg_block_id ) m 
    join flocator_height(( select b.height from block b where b.id = arg_block_id) ) l on l.height = m.height  
    join block b on b.id = m.block_id
    order by m.height desc 
; 
end;
$$ LANGUAGE plpgsql volatile ;






commit;
