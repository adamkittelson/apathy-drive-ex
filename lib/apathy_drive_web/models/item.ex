defmodule ApathyDrive.Item do
  use ApathyDrive.Web, :model
  alias ApathyDrive.{Character, EntityItem, Item, Skill}
  require Logger
  require Ecto.Query

  schema "items" do
    field :name, :string
    field :description, :string
    field :worn_on, :string
    field :grade, :string
    field :global_drop, :boolean
    field :game_limit, :integer
    field :rarity, :string
    field :hit_verbs, ApathyDrive.JSONB
    field :miss_verbs, ApathyDrive.JSONB
    field :attacks_per_round, :integer
    field :physical_resistance, :integer
    field :magical_resistance, :integer
    field :abilities, :map, virtual: true, default: %{}
    field :level, :integer, virtual: true
    field :equipped, :boolean, virtual: true, default: false
    field :strength, :integer, virtual: true
    field :agility, :integer, virtual: true
    field :intellect, :integer, virtual: true
    field :willpower, :integer, virtual: true
    field :health, :integer, virtual: true
    field :charm, :integer, virtual: true
    field :entities_items_id, :integer, virtual: true

    has_many :shop_items, ApathyDrive.ShopItem
    has_many :shops, through: [:shop_items, :room]

    has_many :characters_items, ApathyDrive.EntityItem
    has_many :characters, through: [:characters_items, :character]

    has_many :items_abilities, ApathyDrive.ItemAbility

    timestamps()
  end

  @required_fields ~w(name description worn_on level grade)a
  @optional_fields ~w(abilities global_drop)a
  @rarities %{
    "common" => %{
      cost_multiplier: 1,
      color: "teal",
      attributes: 3
    },
    "uncommon" => %{
      cost_multiplier: 2,
      color: "chartreuse",
      attributes: 6
    },
    "rare" => %{
      cost_multiplier: 3,
      color: "blue",
      attributes: 9
    },
    "epic" => %{
      cost_multiplier: 4,
      color: "darkmagenta",
      attributes: 15
    },
    "legendary" => %{
      cost_multiplier: :infinity,
      color: "red",
      attributes: 24
    }
  }

  @doc """
  Creates a changeset based on the `model` and `params`.

  If no params are provided, an invalid changeset is returned
  with no validation performed.
  """
  def changeset(model, params \\ %{}) do
    model
    |> cast(params, @required_fields ++ @optional_fields)
    |> validate_required(@required_fields)
  end

  def from_assoc(%EntityItem{id: id, item: item} = ei) do
    values =
      Map.take(ei, [:level, :equipped, :strength, :agility,
                    :intellect, :willpower, :health, :charm])

    Map.merge(item, values)
    |> Map.put(:entities_items_id, id)
  end

  def attribute_at_level(%Item{} = item, level, attribute) do
    level = min(item.level, level)
    average = @rarities[item.rarity].attributes

    growth =
      [:strength, :agility, :intellect, :willpower, :health, :charm]
      |> Enum.reduce(0, & &2 + (Map.get(item, &1) || average))
      |> div(6)

    base = Map.get(item, attribute) || average

    base + ((growth / 10) * (level - 1))
  end

  def power_at_level(%Item{} = item, level) do
    base = @rarities[item.rarity].attributes * 6
    base + ((base / 10) * (level - 1))
  end
  def power_at_level(rarity, level) do
    power_at_level(%Item{rarity: rarity}, level)
  end

  def slots do
    ["Arms", "Back", "Ears", "Feet", "Finger", "Finger", "Hands", "Head", "Two Handed",
     "Legs", "Neck", "Off-Hand", "Torso", "Waist", "Weapon Hand", "Wrist", "Wrist"]
  end

  def grade_for_character(%Character{skills: skills}, slot) do
    grades = grades_for_slot(slot)

    skills
    |> Enum.filter(fn {skill_name, _value} -> skill_name in grades end)
    |> Enum.max_by(fn {_skill_name, value} -> value end, fn -> nil end)
    |> case do
      {skill_name, _value} ->
        skill_name
      nil ->
        if "accessory" in grades do
          "accessory"
        else
          nil
        end
    end
  end

  def grades_for_slot(slot) do
    Item
    |> Ecto.Query.where(worn_on: ^slot)
    |> Ecto.Query.distinct(true)
    |> Ecto.Query.select([:grade])
    |> Repo.all
    |> Enum.map(& &1.grade)
  end

  def random_item_id_for_slot_and_rarity(%Character{} = character, slot, rarity) do
    grade = grade_for_character(character, slot)

    if grade do
      random_item_id_for_grade_and_slot_and_rarity(grade, slot, rarity)
    end
  end

  def random_item_id_for_grade_and_slot_and_rarity(grade, slot, rarity) do
    Logger.info "finding random #{inspect rarity} item with grade: #{inspect grade} for slot: #{inspect slot}"
    count =
      __MODULE__
      |> grade(grade)
      |> worn_on(slot)
      |> rarity(rarity)
      |> select([item], count(item.id))
      |> Repo.one

    if count > 0 do
      __MODULE__
      |> grade(grade)
      |> worn_on(slot)
      |> rarity(rarity)
      |> offset(fragment("floor(random()*?) LIMIT 1", ^count))
      |> select([item], item.id)
      |> Repo.one
    else
      Logger.error("Tried to spawn item for #{grade} #{slot} #{rarity}, but none exist.")
    end
  end

  def worn_on(query, slot) do
    query |> where([item], item.worn_on == ^slot)
  end

  def grade(query, grade) do
    query |> where([item], item.grade == ^grade)
  end

  def rarity(query, rarity) do
    query |> where([item], item.rarity == ^rarity)
  end

  def global_drops(query) do
    query |> where([item], item.global_drop == true)
  end

  def below_level(query, level) do
    query |> where([item], item.level <= ^level )
  end

  def datalist do
    __MODULE__
    |> Repo.all
    |> Enum.map(fn(item) ->
         "#{item.name} - #{item.id}"
       end)
  end

  def all do
    __MODULE__
    |> Repo.all
  end

  def generate_item_attributes(rarity) do
    %{strength: @rarities[rarity].attributes,
      agility: @rarities[rarity].attributes,
      intellect: @rarities[rarity].attributes,
      willpower: @rarities[rarity].attributes,
      health: @rarities[rarity].attributes,
      charm: @rarities[rarity].attributes
    }
  end

 def generate_for_character!(%Item{rarity: rarity} = item, %Character{} = character, source) do
    ei =
      %EntityItem{
        assoc_table: "characters",
        assoc_id: character.id,
        item_id: item.id,
        level: (if rarity == "legendary", do: :infinity, else: generated_item_level(character, item.grade))
      }
      |> Map.merge(generate_item_attributes(rarity))

    item =
      ei
      |> Map.put(:item, item)
      |> from_assoc()

    if source == :loot and !upgrade_for_character?(item, character) do
      item
    else
      ei = Repo.insert!(ei)
      Map.put(item, :entities_items_id, ei.id)
    end
  end


  def upgrade_for_character?(%Item{worn_on: slot, rarity: rarity, level: level}, %Character{equipment: equipment}) do
    item_power = Item.power_at_level(rarity, level)

    worn_item =
      Enum.find(equipment, fn(worn_item) ->
        cond do
          slot in ["Off-Hand", "Weapon Hand"] ->
            worn_item.worn_on == slot or worn_item.worn_on == "Two Handed"
          slot == "Two Handed" ->
            worn_item.worn_on == slot or
            worn_item.worn_on == "Weapon Hand" or
            worn_item.worn_on == "Off-Hand"
          :else ->
            worn_item.worn_on == slot
        end
      end)

    slot_power =
      if worn_item do
        Item.power_at_level(worn_item.rarity, worn_item.level)
      else
        0
      end
    item_power > slot_power
  end

  # for accessories use the highest level of any skill
  def generated_item_level(%Character{} = character, "accessory") do
    {_skill_name, skill} =
      character.skills
      |> Enum.max_by(fn {_skill_name, %Skill{experience: exp}} -> exp end, fn -> {"accessory", %Skill{level: 1}} end)

    skill.level
  end
  def generated_item_level(%Character{} = character, grade) do
    if skill_info = Enum.find(character.skills, fn {skill_name, _exp} -> skill_name == grade end) do
      {_skill_name, skill} = skill_info
      skill.level
    else
      1
    end
  end

  def price(%Item{rarity: "legendary"}), do: "priceless"
  def price(%Item{rarity: rarity, level: level} = item) do
    attributes =
      [:strength, :agility, :intellect, :willpower, :health, :charm]
      |> Enum.reduce(0, fn attribute, total ->
           total + attribute_at_level(item, level, attribute)
         end)

    trunc((attributes * @rarities[rarity].cost_multiplier))
  end

  def color(%Item{rarity: rarity}) do
    @rarities[rarity].color
  end

  def colored_name(%Item{name: name} = item, opts \\ []) do
    name =
      name
      |> String.pad_trailing(opts[:pad_trailing] || 0)
      |> String.pad_leading(opts[:pad_leading] || 0)

    "<span style='color: #{color(item)}'>#{name}</span>"
  end

end
