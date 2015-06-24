

--    round( CAST(io.amount / 100000000. as numeric), 5),

CREATE FUNCTION format_value(in v numeric, out numeric )
    AS $$ 
        select round( CAST(v / 100000000. as numeric), 5)
    $$
    LANGUAGE SQL; 


select format_value( cast( 123 as numeric) );

select 
    -- * 
    amount,
    hash,
    i.id is null as unspent

from output o
join address a on a.output_id = o.id 
left join input i on i.output_id = o.id
where a.hash = '\x119b098e2e980a229e139a9ed01a469e518e6f26'
;

select sum(amount) as received
from output o
join address a on a.output_id = o.id 
and a.hash = '\x119b098e2e980a229e139a9ed01a469e518e6f26'
;


select hash,count(amount),format_value(sum(amount)) as balance
from address a
join output o on a.output_id = o.id 
-- where a.hash = '\x119b098e2e980a229e139a9ed01a469e518e6f26'
group by a.hash
limit 1000


--- hmmmm address is not normalized, so we can't just scan down...

-- can we create an address,received,balance view.... 
-- we can then join it with the signature stuff....
