defmodule ApathyDrive.CraftingRecipe do
  use ApathyDriveWeb, :model

  alias ApathyDrive.{
    Character,
    CraftingRecipe,
    CraftingRecipeTrait,
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
    field(:weight, :integer)
    field(:cost_value, :integer)
    field(:cost_currency, :string)
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

  def random_level(level) do
    1..min(50, level)
    |> Enum.flat_map(&List.duplicate(&1, &1))
    |> Enum.random()
  end

  def item_with_traits(%CraftingRecipe{} = recipe, %Item{} = item) do
    item_traits = ItemTrait.load_traits(item.id)

    recipe_traits = CraftingRecipeTrait.load_traits(recipe.id)

    traits =
      item_traits
      |> Map.merge(recipe_traits)
      |> Map.put_new("MinLevel", item.level)

    item =
      item
      |> Map.put(:traits, traits)
      |> Map.put(:weight, recipe.weight)
      |> Map.put(:cost_value, recipe.cost_value)
      |> Map.put(:cost_currency, recipe.cost_currency)

    case item do
      %Item{type: "Weapon"} = item ->
        damage = Character.weapon_damage(item.speed, recipe.damage, item.level)
        Map.merge(item, damage)

      item ->
        item
    end
  end

  def drop_loot_for_character(%Room{} = room, %Character{level: level} = character) do
    level = random_level(level)

    rarity =
      case :rand.uniform(100) do
        n when n > 95 ->
          "rare"

        n when n > 50 ->
          "common"

        _ ->
          nil
      end

    if !is_nil(rarity) do
      count =
        __MODULE__
        |> where([mi], mi.level == ^level)
        |> select([mi], count(mi.id))
        |> Repo.one()

      item =
        __MODULE__
        |> where([mi], mi.level == ^level)
        |> offset(fragment("floor(random()*?) LIMIT 1", ^count))
        |> select([mi], mi)
        |> Repo.one()
        |> case do
          %CraftingRecipe{type: "Armour"} = mi ->
            count =
              Item
              |> where([i], i.type == ^mi.type)
              |> where([i], i.armour_type == ^mi.armour_type)
              |> where([i], i.worn_on == ^mi.worn_on)
              |> where([i], i.global_drop_rarity == ^rarity)
              |> select([i], count(i.id))
              |> Repo.one()

            Item
            |> where([i], i.type == ^mi.type)
            |> where([i], i.armour_type == ^mi.armour_type)
            |> where([i], i.worn_on == ^mi.worn_on)
            |> where([i], i.global_drop_rarity == ^rarity)
            |> offset(fragment("floor(random()*?) LIMIT 1", ^count))
            |> select([i], i)
            |> Repo.one()
            |> Map.put(:level, mi.level)

          %CraftingRecipe{type: "Weapon"} = mi ->
            count =
              Item
              |> where([i], i.type == ^mi.type)
              |> where([i], i.weapon_type == ^mi.weapon_type)
              |> where([i], i.worn_on == ^mi.worn_on)
              |> where([i], i.global_drop_rarity == ^rarity)
              |> select([i], count(i.id))
              |> Repo.one()

            Item
            |> where([i], i.type == ^mi.type)
            |> where([i], i.weapon_type == ^mi.weapon_type)
            |> where([i], i.worn_on == ^mi.worn_on)
            |> where([i], i.global_drop_rarity == ^rarity)
            |> offset(fragment("floor(random()*?) LIMIT 1", ^count))
            |> select([i], i)
            |> Repo.one()
            |> Map.put(:level, mi.level)
        end

      item = Map.put(item, :traits, ItemTrait.load_traits(item.id))

      if item.traits["MinLevel"] && Enum.sum(item.traits["MinLevel"]) > level do
        drop_loot_for_character(room, character)
      else
        Mobile.send_scroll(character, "<p>A #{Item.colored_name(item)} drops to the floor.</p>")

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
