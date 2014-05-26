defmodule Account do
  use Ecto.Model
  import Ecto.Query, only: [from: 2]

  schema "accounts" do
    field :email,      :string
    field :password,   :string
    field :salt,       :string
    field :characters, {:array, :integer}
  end

  def find(email_address) do
    query = from a in Account,
            where: a.email == "#{email_address}",
            select: a

    accounts = Repo.all(query)
    List.first(accounts)
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
