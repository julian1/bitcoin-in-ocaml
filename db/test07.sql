
begin;
-- so we should'nt deal with hashes, just numbers 
drop view locator_hashes; 


create view locator_hashes as

with recursive t( id, height ) AS (
    -- tree root
    select 100, 0
  UNION ALL
  SELECT 
    t.id, t.height + 1
  FROM t where t.height < 100 
)

select * 
FROM t; 

commit;
