
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



drop function if exists dup( int );

CREATE FUNCTION dup(arg int) 
RETURNS TABLE(f1 int)
AS $$ 
-- declare
--  total integer ;
begin 
 return query select (
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
end
$$ LANGUAGE plpgsql;



--    LANGUAGE SQL;

commit;
