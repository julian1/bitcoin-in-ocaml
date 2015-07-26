

begin;

-- has to be changed to only take sigs from the tx in the 'main' chain, not orphaned blocks.
-- or do we need to do this only for the _dups2 ? .

-- actually no - tx (and outputs, sigs) will only ever be recorded once
-- it's the raw blockdata that's an issue
-- still might be good to exclude txs not on the main chain


-- note this selects duplicate r, but s could repeat 

CREATE MATERIALIZED VIEW _dups as

select * from
  (SELECT
      id,
      r,
      ROW_NUMBER() OVER(PARTITION BY r ) AS Row
  FROM signature ) as s2

-- no we just want row = 2, then we'll do a later join for all entries
-- else using >= 2 we miss where Row = 1
where Row = 2
;

-- shouldn't 
--create index on _dups( id);
--create index on _dups( r);

commit;

