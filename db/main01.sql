
begin;

-- having a separate block_data means we can insist on not null attribute
-- and may make scanning block table faster


-- maybe change name to header
create table block(
  id serial primary key, 
  hash bytea unique not null, 
  time timestamptz not null,
  height integer not null
);
create index on block(hash);
create index on block(height); -- for leaves max...


create table previous (
  id serial primary key, 
  block_id integer references block(id) not null,
  block_previous_id integer references block(id) not null
);
create index on previous(block_id);
create index on previous(block_previous_id);



create table block_data(
  id serial primary key, 
  block_id integer references block(id) not null,
  data bytea not null 
);
create index on block_data(block_id);


create table tx(
  id serial primary key, 
  -- block_id integer references block(id) not null, 
  hash bytea unique not null
  
  -- data might be in the block, or the mempool so can't have pos,length here
);
create index on tx(hash);


-- VERY IMPORTANT we need a mapping between tx and block, to allow the same tx to appear
-- in different fork blocks or mempool.
--create table output_address(id serial primary key, output_id integer references output(id), address_id integer references address(id));
create table tx_block(
  id serial primary key, 
  tx_id integer references tx(id) not null, 
  block_id integer references block(id) not null,

  pos integer not null, 
  length integer not null 
);

create index on tx_block(tx_id);
create index on tx_block(block_id);


-- needs pos, len, index 
-- VERY IMPORTANT - should probably be offset by start of tx, rather than block
-- so that we can find if in mempool and not yet in a block
create table output(
  id serial primary key, 
  tx_id integer references tx(id) not null, 
  index int not null, 
  amount bigint not null,

  pos integer not null, 
  length integer not null 
);
create index on output(tx_id);


create table input(
  id serial primary key, 
  tx_id integer references tx(id) not null, 
  -- output_id cannot be unique, since different tx in different forked blocks could spend the same output
  output_id integer references output(id) not null, 

  pos integer not null, 
  length integer not null 
);

create index on input(tx_id);
create index on input(output_id);


create table address(
  id serial primary key, 
  hash bytea not null, 
  script text not null
); -- the tuple(bytea,script) is unique
  --  "create index on address(output_id)" 
create index on address(hash);


create table output_address(
  id serial primary key, 
  output_id integer references output(id) not null, 
  address_id integer references address(id) not null
);
create index on output_address(output_id);
create index on output_address(address_id);


create table coinbase(
  id serial primary key, 
  tx_id integer references tx(id) not null
);
create index on coinbase(tx_id);

-- unique as a tuple, duplicates don't need to be normalized at insertion
create table signature(
  id serial primary key, 
  input_id integer references input(id) not null, 
  r bytea not null, 
  s bytea not null,
  sig_type integer not null
);

create index on signature(input_id);
create index on signature(r);
create index on signature(sig_type);

commit;

