
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




drop function if exists flocator_hashes( int );

CREATE FUNCTION flocator_hashes(arg int) 
RETURNS TABLE(height int)
AS $$ 
begin 
  return query select * from ( 
    (
      with recursive t( height, start_, step ) AS (
        -- non recursive
        select arg, 1, 1
        UNION ALL
        -- recursive
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
  ) as x; 
end;
$$ LANGUAGE plpgsql volatile ;


drop function if exists fmain( int );

CREATE FUNCTION fmain(arg int) 
RETURNS TABLE(depth int, block_id int)
AS $$ 
begin 
  return query select * from ( 

    with recursive t( id, depth ) AS (
      select (
        -- tree tip
        select arg
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
      -- block.*
      block.id 
    FROM t join block on block.id = t.id


  ) as x; 
end;
$$ LANGUAGE plpgsql volatile ;







--    LANGUAGE SQL;

commit;
