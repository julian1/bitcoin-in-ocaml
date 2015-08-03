
begin;
-- so we should'nt deal with hashes, just numbers
drop view locator_hashes;

-- ok, we can't just use height... because of forks, instead we have to walk back from a specific block id
-- which means we need the recursive query... uggh.

-- eg. choose id.
-- recursive query following previous_id back
-- select using the locator hashes

-- how do we get the initial starting tip condition in there?

-- ughhh. jjjjjjjjj

create view locator_hashes as

(
with recursive t( i, start_, step ) AS (
  -- tree root
  -- we can presumably insert the first value...
  select 300000, 1, 1
  UNION ALL
  SELECT
      t.i - t.step,
      t.start_ + 1,
      CASE WHEN t.start_ >= 10 THEN t.step * 2
      ELSE t.step
  END
  FROM t
  where t.i > 0 
)
select i
FROM t where t.i > 0 
)

union all
select 0
;

commit;
