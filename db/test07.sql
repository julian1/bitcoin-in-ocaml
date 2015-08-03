
begin;
-- so we should'nt deal with hashes, just numbers 
drop view locator_hashes; 


create view locator_hashes as

with recursive t( i, start_, step ) AS (
    -- tree root
    select 100, 1, 1
  UNION ALL
  SELECT 
      t.i - t.step, t.start_ + 1, 
      CASE
      WHEN t.start_ >= 10 THEN t.step * 2 
      ELSE t.step
  END  
  FROM t 
  where t.i > 0 
)

select * 
FROM t; 

commit;
