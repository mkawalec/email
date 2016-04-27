CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TABLE message (
  id uuid primary key default uuid_generate_v4(),
  uid bigint not null,
  message text
);
