defmodule ApathyDrive.ShopItem do
  use ApathyDriveWeb, :model

  alias ApathyDrive.{ChannelHistory, ItemInstance, Room, ShopItem}

  schema "shop_items" do
    field(:stock, :integer)
    field(:restock_frequency_in_minutes, :integer)
    field(:restock_chance, :integer)
    field(:restock_amount, :integer)
    field(:next_restock_at, :utc_datetime)
    field(:count, :integer, virtual: true, default: 0)
    field(:name, :string, virtual: true)

    belongs_to(:shop, ApathyDrive.Shop)
    belongs_to(:item, ApathyDrive.Item)
  end

  @required_fields ~w(room_id item_id)
  @optional_fields ~w()

  def changeset(model, params \\ %{}) do
    model
    |> cast(params, @required_fields, @optional_fields)
    |> unique_constraint(:item_id, name: :shop_items_room_id_item_id_index)
    |> foreign_key_constraint(:item_id)
    |> foreign_key_constraint(:room_id)
  end

  def all do
    __MODULE__
    # |> Ecto.Query.where([si], si.room_id == ^id)
    |> Repo.all()
  end

  def restock!(%Room{} = room, %ShopItem{} = shop_item) do
    if !is_nil(shop_item.restock_frequency_in_minutes) and
         (is_nil(shop_item.next_restock_at) or
            DateTime.compare(shop_item.next_restock_at, DateTime.utc_now()) == :lt) do
      if shop_item.count < shop_item.stock and :rand.uniform(100) <= shop_item.restock_chance do
        restock_amount = min(shop_item.stock - shop_item.count, shop_item.restock_amount)

        message =
          "<p>[<span class='yellow'>announce</span> : Apotheosis] #{room.name} just received a shipment of: #{
            shop_item.item.name
          } (#{restock_amount})</p>"

        Repo.insert!(%ChannelHistory{
          character_name: "Apotheosis",
          channel_name: "announce",
          message: message
        })

        ApathyDriveWeb.Endpoint.broadcast!("chat:gossip", "chat", %{
          html: message
        })

        Enum.each(1..restock_amount, fn _ ->
          %ItemInstance{item_id: shop_item.item_id, shop_id: shop_item.shop_id}
          |> Repo.insert!()
        end)
      end

      shop_item
      |> Ecto.Changeset.change(
        next_restock_at:
          Timex.shift(DateTime.utc_now(), minutes: shop_item.restock_frequency_in_minutes)
      )
      |> Repo.update!()
    end
  end
end
