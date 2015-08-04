
begin;
-- so we should'nt deal with hashes, just numbers


--- note that this thing is very fast to calculate - < 1 ms, because of the step size increment. 
--- we want to pass in the height explicitly



-----------------------------------------
-- calulate the height series for getdata block requests 
-- ref, https://en.bitcoin.it/wiki/Protocol_documentation#getblocks

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
-- calculate ids of blocks in chain
-- function is expensive and could use a memoizing table for height, depth...

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


-----------------------------------
-- create locator hashes by combining main chain ids, with locator height 

drop function if exists flocator_hashes( int );

CREATE FUNCTION flocator_hashes(arg_block_id int) 
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

------------------------------------------
-- longest chain's block id - better name? 

drop function if exists flongest();

CREATE FUNCTION flongest() 
RETURNS int
AS $$ 
    select
      id -- as block_id,
      -- height 
    from block
    order by height desc
    limit 1
;
$$ LANGUAGE sql volatile ;


-- select hash from flocator_hashes( flongest() ) ; 




commit;
