begin;

-- '\x0000000000000000000000000000000000000000000000000000000000000000' 

-- litecoin insert into block(hash,time) select '\x12a765e31ffd4059bada1e25190f6e98c99d9714d334efa41a195a7e7e04bfe2',   to_timestamp(0) at time zone 'UTC' ;

-- dogecoin 
insert into block(hash,time, height) select '\x1a91e3dace36e2be3bf030a65679fe821aa1d6ef92e7c9902eb318182c355691',   to_timestamp(0) at time zone 'UTC', 0 ;

-- bitcoin
-- insert into block(hash,time) select E'\\x000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f', to_timestamp(0) at time zone 'UTC' ;

commit; 

