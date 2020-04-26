defmodule ApathyDrive.CraftingRecipe do
  use ApathyDriveWeb, :model

  alias ApathyDrive.{
    Character,
    CraftingRecipe,
    Item,
    ItemInstance,
    ItemTrait,
    Material,
    Mobile,
    Room,
    Skill
  }

  schema "crafting_recipes" do
    field(:level, :integer)
    field(:material_amount, :integer)
    field(:type, :string)
    field(:armour_type, :string)
    field(:worn_on, :string)
    field(:weapon_type, :string)
    field(:damage, :integer)

    belongs_to(:material, Material)
    belongs_to(:skill, Skill)
  end

  def types_for_skill(skill_id) do
    Ecto.Adapters.SQL.query!(
      Repo,
      "select distinct type, armour_type, weapon_type, worn_on from crafting_recipes where skill_id = $1",
      [skill_id]
    )
    |> case do
      %Postgrex.Result{rows: rows} ->
        rows
    end
  end

  def for_item(%Item{level: nil}), do: nil

  def for_item(%Item{type: "Armour"} = item) do
    __MODULE__
    |> where([r], r.type == "Armour")
    |> where([r], r.level == ^item.level)
    |> where([r], r.armour_type == ^item.armour_type)
    |> where([r], r.worn_on == ^item.worn_on)
    |> select([r], r)
    |> Repo.one()
  end

  def for_item(%Item{type: "Weapon"} = item) do
    __MODULE__
    |> where([r], r.type == "Weapon")
    |> where([r], r.level == ^item.level)
    |> where([r], r.weapon_type == ^item.weapon_type)
    |> where([r], r.worn_on == ^item.worn_on)
    |> select([r], r)
    |> Repo.one()
  end

  def for_item(%Item{}), do: nil

  def random_level(min, max) do
    min..min(50, max)
    |> Enum.flat_map(&List.duplicate(&1, &1))
    |> Enum.random()
  end

  def item_with_traits(%CraftingRecipe{} = recipe, %Item{} = item) do
    item_traits = ItemTrait.load_traits(item.id)

    item =
      item
      |> Map.put(:traits, item_traits)

    case item do
      %Item{type: "Weapon"} = item ->
        damage = Character.weapon_damage(item.speed, recipe.damage, item.level)

        damage = %{
          min_damage: (item.min_damage + damage.min_damage) / 2,
          max_damage: (item.max_damage + damage.max_damage) / 2
        }

        Map.merge(item, damage)

      item ->
        item
    end
  end

  def drop_loot_for_character(%Room{} = room, %Character{level: level} = character) do
    rarity =
      case :rand.uniform(100) do
        n when n > 99 ->
          "rare"

        n when n >= 75 ->
          "common"

        _ ->
          nil
      end

    if !is_nil(rarity) do
      min_level = max(room.area.level - 10, 1)
      max_level = min(room.area.level + 5, level)

      level = random_level(min_level, max_level)

      count =
        __MODULE__
        |> where([mi], mi.level == ^level)
        |> select([mi], count(mi.id))
        |> Repo.one()

      {recipe, item} =
        __MODULE__
        |> where([mi], mi.level == ^level)
        |> offset(fragment("floor(random()*?) LIMIT 1", ^count))
        |> select([mi], mi)
        |> Repo.one()
        |> case do
          %CraftingRecipe{type: "Armour"} = recipe ->
            count =
              Item
              |> where([i], i.type == ^recipe.type)
              |> where([i], i.armour_type == ^recipe.armour_type)
              |> where([i], i.worn_on == ^recipe.worn_on)
              |> where([i], i.global_drop_rarity == ^rarity)
              |> select([i], count(i.id))
              |> Repo.one()

            item =
              Item
              |> where([i], i.type == ^recipe.type)
              |> where([i], i.armour_type == ^recipe.armour_type)
              |> where([i], i.worn_on == ^recipe.worn_on)
              |> where([i], i.global_drop_rarity == ^rarity)
              |> offset(fragment("floor(random()*?) LIMIT 1", ^count))
              |> select([i], i)
              |> Repo.one()
              |> Map.put(:level, recipe.level)

            {recipe, item}

          %CraftingRecipe{type: "Weapon"} = recipe ->
            count =
              Item
              |> where([i], i.type == ^recipe.type)
              |> where([i], i.weapon_type == ^recipe.weapon_type)
              |> where([i], i.worn_on == ^recipe.worn_on)
              |> where([i], i.global_drop_rarity == ^rarity)
              |> select([i], count(i.id))
              |> Repo.one()

            item =
              Item
              |> where([i], i.type == ^recipe.type)
              |> where([i], i.weapon_type == ^recipe.weapon_type)
              |> where([i], i.worn_on == ^recipe.worn_on)
              |> where([i], i.global_drop_rarity == ^rarity)
              |> offset(fragment("floor(random()*?) LIMIT 1", ^count))
              |> select([i], i)
              |> Repo.one()
              |> Map.put(:level, recipe.level)

            {recipe, item}
        end

      item = item_with_traits(recipe, item)

      if item.traits["MinLevel"] && item.traits["MinLevel"] > level do
        drop_loot_for_character(room, character)
      else
        Mobile.send_scroll(
          character,
          "<p>A #{Item.colored_name(item, character: character)} drops to the floor.</p>"
        )

        %ItemInstance{
          item_id: item.id,
          room_id: room.id,
          level: item.level,
          character_id: nil,
          dropped_for_character_id: character.id,
          equipped: false,
          hidden: false,
          delete_at: Timex.shift(DateTime.utc_now(), hours: 1)
        }
        |> Repo.insert!()
      end
    end

    room
  end
end
