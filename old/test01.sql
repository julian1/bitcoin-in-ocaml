
-- should be address focused? yes tx's that spend --

-- we're not linking up inputs very well...
-- we want the block so that we can order them...
-- so we have one of the inputs.

-- think we have to select txes...

select
    --t.hash as tx,
    ia.hash as inputaddress,
    round( CAST(io.amount / 100000000. as numeric), 5),
    c.id is not null as coinbase,
    a.hash as address,
    round( CAST(o.amount / 100000000. as numeric), 5)
from address a
join output o on o.id = a.output_id
join tx t on t.id = o.tx_id

-- this isn't right because there might be lots of inputs...
left join coinbase c on c.tx_id = t.id
left join input i on i.tx_id = t.id  -- has to be left otherwise
left join output io on io.id = i.output_id
left join address ia on ia.output_id = io.id

where a.hash = '\x119b098e2e980a229e139a9ed01a469e518e6f26'

-- order by a.hash
-- where tx.hash = 'xxxx' and output.index=0;

-- ok, we're



