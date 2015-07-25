
-- we're getting tx's recorded twice, because they're in an orphaned block
-- prod=> select * from tx join block b on b.id = tx.block_id where tx.hash = '\xb6f414c9c21d7d0c6c7c2b79781d44b05f737454a38784f8a38ed477ff8909eb' ;

-- ahhh, how easy would it be to ignore
--- actually could be easy....
-- just link output, tx, sig back to block, and have a boolean
-- can do it at the end, or the beginning.

-- actually only need tx view with limit and non left join and it will work,

begin;

drop MATERIALIZED VIEW if exists _dups2 ;

CREATE MATERIALIZED VIEW _dups2 as

--explain
select
  substr( s.r, 0, 10 ),
  tx.hash as tx,
  o.index,

  a.hash as address,
  -- format_amount( unspent( a.hash) ) as unspent
  unspent( a.hash)  as unspent

-- from _test t
from _dups t
join signature s on s.r = t.r
join input i on i.id = s.input_id
join tx on tx.id = i.tx_id

join output o on o.id = i.output_id  -- the output we're unlocking
join output_address oa on oa.output_id = o.id
join address a on a.id = oa.address_id

limit 100
;
-- shouldn't do the order here
--order by s.r;

commit;
