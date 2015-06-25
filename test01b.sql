
-- always a join b on 

select * --, x.received, x.unspent
from 
(test join 
    (input join 
        (output join 
            (output_address join 
                address  
            on address.id = output_address.address_id )
        on output_address.output_id = output.id)
    on output.id = input.output_id )
on input.id = test.input_id)
;


-- join x on x.hash = address.hash )

-- join output_address oa on oa.output_id = o.id


