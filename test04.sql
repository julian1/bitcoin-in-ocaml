
-- need to try a vacuum analyze on large data.
-- the query with a limit of 10 should be easy


-- null means unknown address or nothing unspent.

-- IMPORTANT we should be using the address id here.
-- not the actual address


-- there's something not right...
-- coinbase? 
-- 11b366edfc0a8b66feebae5c2e25a7b6a5d1cf31 
-- count is 6 but there's more...

CREATE OR REPLACE FUNCTION format_amount( v bigint )
RETURNS numeric AS $total$
declare
    total numeric ;
BEGIN
    select round( CAST(v / 100000000. as numeric), 5) into total; 
   -- SELECT received into total FROM received where hash = hash1;
   RETURN total;
END;
$total$ LANGUAGE plpgsql;



CREATE OR REPLACE FUNCTION count( hash1 bytea )
RETURNS bigint AS $$
declare
    total bigint ;
BEGIN
    select count(o.amount) into total 
    from address a
    join output_address oa on oa.address_id = a.id
    join output o on o.id = oa.output_id
    where a.hash = hash1; 
   RETURN total;
END;
$$ LANGUAGE plpgsql;



CREATE OR REPLACE FUNCTION received( hash1 bytea )
RETURNS bigint AS $$
declare
    total bigint ;
BEGIN
    select sum(o.amount) into total 
    from address a
    join output_address oa on (oa.address_id = a.id)
    join output o on o.id = oa.output_id
    where a.hash = hash1; 
   RETURN total;
END;
$$ LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION unspent( hash1 bytea )
RETURNS bigint AS $$
declare
    total bigint ;
BEGIN
    -- create or replace view unspent as 
    select sum(o.amount) into total 
    from address a
    join output_address oa on oa.address_id = a.id 
    join output o on o.id = oa.output_id 
    left join input i on i.output_id = o.id
    where i.id is null -- ie unspent
    and a.hash = hash1
    -- group by a.hash
    ; 
   RETURN total;
END;
$$ LANGUAGE plpgsql;

    select o.*, i.tx_id as spending_tx_id
    from address a
    join output_address oa on oa.address_id = a.id 
    join output o on o.id = oa.output_id 
    left join input i on i.output_id = o.id
    -- where i.id is null -- ie unspent
    where a.hash = '\x11b366edfc0a8b66feebae5c2e25a7b6a5d1cf31'; 

-- ok, there's a problem in the addition where the tx spends 
-- which we will ahve to fix...



-- should have a count outputs function as well... 
-- the function constrains the query, we could even build another
-- view from this if we wanted.  

-- IMPORTANT
-- should also try the (subset offset 0) trick 
-- and we want transactions organized around blocks.

