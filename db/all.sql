
-- eg. psql -h 127.0.0.1 -U meteo -d test -f db/all.sql  

begin;
\i db/main01.sql
\i db/main02.sql
\i db/main03.sql
\i db/main04.sql
\i db/main05.sql

commit ;
