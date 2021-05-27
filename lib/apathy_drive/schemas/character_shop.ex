defmodule ApathyDrive.CharacterShop do
  use ApathyDriveWeb, :model

  alias ApathyDrive.{
    Character,
    CharacterShop,
    Item,
    ItemInstance,
    Monster,
    Room,
    Shop,
    ShopItemType
  }

  require Logger

  schema "character_shops" do
    field :last_restocked_at, :utc_datetime_usec

    belongs_to :character, ApathyDrive.Character
    belongs_to :shop, ApathyDrive.Shop
    has_many :items, ApathyDrive.ItemInstance
  end

  def find_or_create_for(%Shop{} = shop, %Character{} = character) do
    __MODULE__
    |> Repo.get_by(shop_id: shop.id, character_id: character.id)
    |> case do
      nil ->
        %CharacterShop{character_id: character.id, shop_id: shop.id}
        |> Repo.insert!()

      %CharacterShop{} = cs ->
        cs
    end
  end

  def items(%Shop{} = shop, %Character{} = character) do
    cs = find_or_create_for(shop, character)

    cs
    |> Ecto.assoc(:items)
    |> Ecto.Query.preload([:item])
    |> Repo.all()
    |> Enum.map(&Item.from_assoc/1)
  end

  def restock!(%Room{} = room, %Character{} = character) do
    if cs = restock?(room, character) do
      IO.puts("restocking!")

      # delete existing items
      cs
      |> Ecto.Changeset.change(%{last_restocked_at: DateTime.utc_now()})
      |> Repo.update!()
      |> Ecto.assoc(:items)
      |> Repo.delete_all()

      room.shop
      |> Ecto.assoc(:shop_item_types)
      |> Repo.all()
      |> Enum.each(fn %ShopItemType{} = sit ->
        Enum.each(1..sit.amount, fn _n ->
          item_types = Item.child_item_types(sit.item_type_id)

          item_level = (div(min(character.level, room.area.level), 3) + 1) * 3
          affix_level = (div(character.level, 3) + 1) * 3

          items = Item.for_shop(item_level, item_types)

          item =
            items
            |> Enum.random()
            |> Item.load_item_types()

          Logger.info("Stocking item##{item.id} for #{character.name} in Room##{room.id}")

          quality = sit.quality

          ac = Monster.ac_for_item(item, quality)

          item_instance =
            %ItemInstance{
              item_id: item.id,
              room_id: nil,
              character_id: nil,
              character_shop_id: cs.id,
              equipped: false,
              hidden: false,
              ac: ac,
              name: item.name,
              quality: quality,
              level: item_level
            }
            |> Repo.insert!()
            |> Repo.preload(:item)
            |> update_in([Access.key!(:item)], &Item.load_item_types/1)

          {prefixes, suffixes} = Monster.item_affixes(item_instance, affix_level)

          prefixes = Enum.reject(prefixes, &is_nil/1)
          suffixes = Enum.reject(suffixes, &is_nil/1)

          name = Monster.item_name(item_instance, prefixes, suffixes)

          item_instance
          |> Ecto.Changeset.change(%{name: name})
          |> Repo.update!()
        end)
      end)
    end
  end

  defp restock?(%Room{} = room, %Character{} = character) do
    cs = find_or_create_for(room.shop, character)

    if cs.last_restocked_at do
      one_hour_ago = DateTime.add(DateTime.utc_now(), -:timer.hours(1), :millisecond)

      if :gt == DateTime.compare(one_hour_ago, cs.last_restocked_at) do
        cs
      end
    else
      cs
    end
  end
end
