begin;

-- '\x0000000000000000000000000000000000000000000000000000000000000000'

-- bitcoin
-- insert into block(hash,time) select E'\\x000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f', to_timestamp(0) at time zone 'UTC' ;

-- litecoin insert into block(hash,time) select '\x12a765e31ffd4059bada1e25190f6e98c99d9714d334efa41a195a7e7e04bfe2',   to_timestamp(0) at time zone 'UTC' ;

-- dogecoin
insert into block(hash,time, height) select '\x1a91e3dace36e2be3bf030a65679fe821aa1d6ef92e7c9902eb318182c355691',   to_timestamp(0) at time zone 'UTC', 0 ;



-- TODO should dump as sql and get all peers.
-- also separate files for bitcoin, litecoin, dogecoin
-- dogecoin

-- eg. sudo -u postgres pg_dump dogecoin -t peer --inserts | less

insert into peer(addr, port) values ( '128.199.78.238', 22556 );
insert into peer(addr, port) values ( '216.155.138.34', 22556 ) ;
insert into peer(addr, port) values ( '162.243.251.36', 22556 ) ;
insert into peer(addr, port) values ( '69.61.29.79', 22556 ) ;
insert into peer(addr, port) values ( '190.123.36.160', 22556 ) ;
insert into peer(addr, port) values ( '115.231.98.98', 22556 ) ;
insert into peer(addr, port) values ( '70.121.131.203', 22556 ) ;
insert into peer(addr, port) values ( '50.140.183.191', 22556 ) ;
insert into peer(addr, port) values ( '49.116.148.241', 22556 ) ;
insert into peer(addr, port) values ( '37.49.9.64', 22556 ) ;
insert into peer(addr, port) values ( '173.53.4.185', 22556 ) ;
insert into peer(addr, port) values ( '50.149.31.242', 22556 ) ;
insert into peer(addr, port) values ( '201.50.197.138', 22556 ) ;
insert into peer(addr, port) values ( '190.73.227.252', 22556 ) ;
insert into peer(addr, port) values ( '86.170.27.246', 22556 ) ;


commit;

