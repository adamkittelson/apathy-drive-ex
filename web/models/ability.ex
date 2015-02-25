defmodule Ability do
  use Ecto.Model
  import Systems.Text
  alias ApathyDrive.Repo
  alias Phoenix.PubSub

  schema "abilities" do
    field :name,            :string
    field :command,         :string
    field :kind,            :string
    field :description,     :string
    field :required_skills, ApathyDrive.JSONB
    field :properties,      ApathyDrive.JSONB
    field :keywords,        {:array, :string}, virtual: true

    timestamps
  end

  after_load :set_keywords

  def set_keywords(%Ability{name: name} = ability) do
    Map.put(ability, :keywords, String.split(name))
  end

  def trainable do
    query = from a in Ability, where: not is_nil(a.required_skills), select: a
    Repo.all(query)
  end

  def color(%Ability{kind: "attack"}),      do: "red"
  def color(%Ability{kind: "curse"}),       do: "red"
  def color(%Ability{kind: "area attack"}), do: "red"
  def color(%Ability{kind: "area curse"}),  do: "red"
  def color(%Ability{kind: _}), do: "blue"

  def prep_message(message, %Ability{} = ability, %Monster{} = user, %Monster{} = target) do
    message = message
              |> interpolate(%{"user" => user, "target" => target})
              |> capitalize_first
    "<p><span class='#{color(ability)}'>#{message}</span></p>"
  end

  def cast_messages(%Ability{} = ability, %Monster{} = user, %Monster{} = target) do
    %{
      "user"      => prep_message(ability.properties["cast_message"]["user"],      ability, user, target),
      "target"    => prep_message(ability.properties["cast_message"]["target"],    ability, user, target),
      "spectator" => prep_message(ability.properties["cast_message"]["spectator"], ability, user, target)
    }
  end

  def execute(%Monster{mana: mana} = monster,
              %Ability{properties: %{"mana_cost" => cost}}, _) when cost > mana do
    monster
    |> Monster.send_scroll("<p><span class='red'>You do not have enough mana to use that ability.</span></p>")
  end

  def execute(%Monster{} = monster, %Ability{} = ability, "") do
    execute(monster, ability, monster)
  end

  def execute(%Monster{} = monster, %Ability{} = ability, %Monster{} = target) do
    PubSub.broadcast("rooms:#{monster.room_id}", {:cast_message,
                                                   messages: cast_messages(ability,
                                                                           monster,
                                                                           target),
                                                   user: monster,
                                                   target: target})

    send(target.pid, {:ability_target, ability})

    monster
    |> Map.put(:mana, monster.mana - ability.properties["mana_cost"])
    |> Systems.Prompt.update
  end

end
