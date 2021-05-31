defmodule ApathyDrive.Currency do
  def copper_value(:copper), do: 1
  def copper_value(:silver), do: 10
  def copper_value(:gold), do: 100
  def copper_value(:platinum), do: 10000
  def copper_value(:runic), do: 1_000_000
  def copper_value(currency), do: currency |> currency_from_name() |> copper_value()

  def currency_from_name("copper farthing"), do: :copper
  def currency_from_name("silver noble"), do: :silver
  def currency_from_name("gold crown"), do: :gold
  def currency_from_name("platinum piece"), do: :platinum
  def currency_from_name("runic coin"), do: :runic

  def name_from_currency(:copper), do: "copper farthing"
  def name_from_currency(:silver), do: "silver noble"
  def name_from_currency(:gold), do: "gold crown"
  def name_from_currency(:platinum), do: "platinum piece"
  def name_from_currency(:runic), do: "runic coin"

  @currencies [:runic, :platinum, :gold, :silver, :copper]

  @default %{copper: 0, silver: 0, gold: 0, platinum: 0, runic: 0}

  def currencies, do: @currencies

  def set_value(currency_holder \\ @default, copper) do
    {currency_holder, _} =
      Enum.reduce(@currencies, {currency_holder, copper}, fn currency,
                                                             {currency_holder, copper} ->
        currency_holder = Map.put(currency_holder, currency, div(copper, copper_value(currency)))
        copper = Integer.mod(copper, copper_value(currency))
        {currency_holder, copper}
      end)

    currency_holder
  end

  def matches do
    [
      %{name: "copper farthing", currency: :copper},
      %{name: "silver noble", currency: :silver},
      %{name: "gold crown", currency: :gold},
      %{name: "platinum piece", currency: :platinum},
      %{name: "runic coin", currency: :runic}
    ]
  end

  def add(currency_holder, copper) do
    set_value(currency_holder, wealth(currency_holder) + copper)
  end

  def subtract(currency_holder, copper) do
    set_value(currency_holder, wealth(currency_holder) - copper)
  end

  def wealth(currency_holder) do
    Enum.reduce(@currencies, 0, fn currency, total ->
      total + Map.get(currency_holder, currency) * copper_value(currency)
    end)
  end

  def to_list(currency_holder) do
    Enum.map(@currencies, fn currency ->
      amount = Map.get(currency_holder, currency)

      cond do
        amount > 1 ->
          "#{amount} #{name_from_currency(currency)}s"

        amount == 1 ->
          "#{amount} #{name_from_currency(currency)}"

        :else ->
          nil
      end
    end)
    |> Enum.reject(&is_nil/1)
  end

  def to_string(currency_holder, length \\ :long)

  def to_string(currency_holder, :long) do
    currency_holder
    |> to_list
    |> ApathyDrive.Commands.Inventory.to_sentence()
    |> case do
      "" ->
        "FREE"

      other ->
        other
    end
  end

  def to_string(currency_holder, :short) do
    currency_holder
    |> to_list
    |> Enum.take(2)
    |> ApathyDrive.Commands.Inventory.to_sentence()
    |> case do
      "" ->
        "FREE"

      other ->
        other
    end
  end
end
