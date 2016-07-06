CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TABLE email_address (
  id uuid primary key default uuid_generate_v4(),
  label text,
  address text not null unique
);

CREATE TABLE message (
  id uuid primary key default uuid_generate_v4(),
  uid bigint not null,
  from_addr uuid references email_address(id),
  sent_date timestamp,
  reply_to uuid references email_address(id),
  message_id text,
  in_reply_to text,
  subject text,
  message text
);

CREATE TYPE email_relation AS ENUM ('To', 'CC', 'BCC');
CREATE TABLE message_emails (
  id uuid primary key default uuid_generate_v4(),
  message_id uuid references message(id) ON DELETE CASCADE,
  email_id uuid references email_address(id) ON DELETE CASCADE,
  relation_type email_relation not null
);

CREATE TABLE message_references (
  id uuid primary key default uuid_generate_v4(),
  message_id uuid references message(id) ON DELETE CASCADE,
  references_id text not null
);
