
begin;

-- drop view if exists leaves; 
-- drop view if exists block_; 
-- drop table if exists signature;
-- drop table if exists coinbase;
-- drop table if exists output_address;
-- drop table if exists address;
-- drop table if exists input;
-- drop table if exists output;
-- drop table if exists tx;
-- drop table if exists block;
 
--  need pos of block in .dat file. 
create table block(id serial primary key, hash bytea unique not null, previous_id integer, time timestamptz );
create index on block(hash);
create index on block(previous_id); --  not sure if needed 

create table tx(id serial primary key, block_id integer references block(id), hash bytea);
create index on tx(block_id);
create index on tx(hash);

create table output(id serial primary key, tx_id integer references tx(id), index int, amount bigint);
create index on output(tx_id);

create table input(id serial primary key, tx_id integer references tx(id), output_id integer references output(id) unique );
create index on input(tx_id);
create index on input(output_id);

create table address(id serial primary key, hash bytea unique);
  --  "create index on address(output_id)" 
create index on address(hash);

create table output_address(id serial primary key, output_id integer references output(id), address_id integer references address(id));
create index on output_address(output_id);
create index on output_address(address_id);

create table coinbase(id serial primary key, tx_id integer references tx(id));
create index on coinbase(tx_id);

create table signature(id serial primary key, input_id integer references input(id), r bytea, s bytea );
create index on signature(input_id);
create index on signature(r);

commit;

