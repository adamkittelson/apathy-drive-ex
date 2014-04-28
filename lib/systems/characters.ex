defmodule Systems.Characters do

  def for_account(account) do
    Components.all(Components.IndexAsCharacter)
    |> Components.find_all_by(Components.AccountID, account.id)
  end

  def find_by_account_and_name(account, name) do
    for_account(account) |> Enum.find fn(character) ->
      String.downcase(Components.Name.get_name(character)) == String.downcase(name)
    end
  end

  def name_taken?(name) do
    Enum.any?(Components.all(Components.IndexAsCharacter), fn(character) ->
      String.downcase(Components.Name.get_name(character)) == String.downcase(name)
    end)
  end

  def online do
    Components.all(Components.IndexAsCharacter)
    |> Enum.filter fn(character) -> Components.Online.value(character) == true end
  end

end