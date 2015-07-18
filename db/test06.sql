
select 
  output.* ,
  tx.hash,
  tx_block.pos + output.pos,
  substring( block_data.data, tx_block.pos + output.pos + 1, output.length )
  
from output
join tx on tx.id = output.tx_id 
join tx_block on tx_block.tx_id = tx.id
join block on block.id = tx_block.block_id
join block_data on block_data.id = block.block_data_id

where tx.id = 1;

