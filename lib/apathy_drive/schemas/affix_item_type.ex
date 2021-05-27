defmodule ApathyDrive.AffixItemType do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{Affix, ItemType}
  use GenServer, restart: :transient

  schema "affixes_items_types" do
    belongs_to(:affix, Affix)
    belongs_to(:item_type, ItemType)

    field(:allowed, :boolean)
  end

  def start_link(_arg) do
    GenServer.start_link(__MODULE__, %{}, name: __MODULE__)
  end

  def not_allowed?(%Affix{id: id} = _affix, item_types) do
    Enum.map(item_types, fn item_type ->
      key = {id, item_type}

      case :ets.lookup(:affix_item_types, key) do
        [{^key, allowed}] ->
          allowed

        _ ->
          nil
      end
    end)
    |> Enum.reject(&is_nil/1)
    |> Enum.any?(&(&1 == false))
  end

  def init(state) do
    :ets.new(:affix_item_types, [
      :named_table,
      :set,
      read_concurrency: true,
      write_concurrency: true
    ])

    {:ok, state, {:continue, :populate_affix_item_types}}
  end

  def handle_continue(:populate_affix_item_types, state) do
    __MODULE__
    |> Repo.all()
    |> Enum.each(fn affix_item_type ->
      :ets.insert(
        :affix_item_types,
        {{affix_item_type.affix_id, affix_item_type.item_type_id}, affix_item_type.allowed}
      )
    end)

    {:noreply, state}
  end
end
