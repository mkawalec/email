CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TABLE email_address (
  id uuid primary key default uuid_generate_v4(),
  label text,
  address text not null unique
);

CREATE TABLE references (
  id uuid primary key default uuid_generate_v4(),
  messsage_id uuid references message(id),
  references text
);

CREATE TABLE message (
  id uuid primary key default uuid_generate_v4(),
  uid bigint not null,
  from uuid refereces email_address(id),
  sent_date datetime,
  reply_to uuid references email_address(id),
  message_id text,
  in_reply_to text,
  subject text,
  message text
);
