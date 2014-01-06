defmodule ApathyDrive.Account do
  use Ecto.Entity
  use Ecto.Model
  use Ecto.Query

  field :email,      :string
  field :password,   :string
  field :salt,       :string
  field :characters, {:list, :integer}

  queryable "accounts" do
    field :email,      :string
    field :password,   :string
    field :salt,       :string
    field :characters, {:list, :integer}
  end

  def find(email_address) do
    query = from a in ApathyDrive.Account,
            where: a.email == "#{email_address}",
            select: a

    accounts = Repo.all(query)
    Enum.first(accounts)
  end

  def find(email_address, password) do
    account = find(email_address)
    if account do
      account_password = '#{account.password}'
      salt             = '#{account.salt}'
      {:ok, password}  = :bcrypt.hashpw(password, salt)
      if password != account_password do
        account = nil
      end
    end
    account
  end

end
