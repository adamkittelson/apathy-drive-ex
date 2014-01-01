defmodule Repo.Migrations.CreateAccounts do
  use Ecto.Migration

  def up do
    "CREATE TABLE IF NOT EXISTS accounts(id serial primary key,
                                         email     varchar(255) NOT NULL,
                                         password  varchar(255) NOT NULL,
                                         salt      varchar(255) NOT NULL,
                                         entity_id integer)"
  end

  def down do
    "DROP TABLE accounts"
  end
end
