
-- always a join b on 

explain select * --, x.received, x.unspent
from test 
t 
join 
    (input 
    join (output 
          join ( output_address 
                join address on address.id = output_address.address_id )


          on output_address.output_id = output.id)
    on output.id = input.output_id )
on input.id = t.input_id
;

-- join output_address oa on oa.output_id = o.id


