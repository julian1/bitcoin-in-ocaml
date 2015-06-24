

select a.hash, sum(o.amount)
from address a

join output_address oa on oa.address_id = a.id 
join output o on o.id = oa.output_id 

left join input i on i.output_id = o.id
where i.id is null



-- where a.hash = '\x00084af65c146c6ca4bc9939f2ab22d4e1fdd01f'
group by a.hash

-- 008f1dd93b4d22cabb40b04666a9759d0241c3cf


