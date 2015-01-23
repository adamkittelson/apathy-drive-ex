defmodule Monster do
  use Ecto.Model
  use GenServer
  use Systems.Reload
  alias ApathyDrive.Repo

  schema "monsters" do
    field :name,                :string
    field :skills,              :string #json
    field :limbs,               :string #json
    field :monster_template_id, :integer
    field :room_id,             :integer
    field :experience,          :integer, default: 0
    field :level,               :integer, default: 1
    field :alignment,           :decimal
    field :hp,                  :integer, virtual: true
    field :mana,                :integer, virtual: true
    field :hunting,             :any,     virtual: true, default: []
    field :combat,              :any,     virtual: true, default: %{"break_at" => 0}
    field :flags,               :any,     virtual: true, default: %{}
    field :effects,             :any,     virtual: true, default: %{}
    field :disposition,         :string,  virtual: true
    field :description,         :string,  virtual: true
    field :death_message,       :string,  virtual: true
    field :enter_message,       :string,  virtual: true
    field :exit_message,        :string,  virtual: true
    field :abilities,           :any,     virtual: true, default: []
    field :greeting,            :string,  virtual: true
    field :gender,              :string,  virtual: true
    field :strength,            :integer, virtual: true
    field :agility,             :integer, virtual: true
    field :intelligence,        :integer, virtual: true
    field :health,              :integer, virtual: true
    field :hit_verbs,           :any,     virtual: true
    field :chance_to_follow,    :integer, virtual: true
    field :damage,              :any,     virtual: true
    field :possession_level,    :integer, virtual: true
    field :questions,           :any,     virtual: true
  end

  # Generate functions from Ecto schema

  fields = Keyword.keys(@assign_fields)

  Enum.each(fields, fn(field) ->
    def unquote(field)(monster_template) do
      GenServer.call(monster_template, unquote(field))
    end
  end)

  Enum.each(fields, fn(field) ->
    def handle_call(unquote(field), _from, monster_template) do
      {:reply, Map.get(monster_template, unquote(field)), monster_template}
    end
  end)

end
