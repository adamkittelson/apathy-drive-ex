defmodule MonsterTemplate do
  use Ecto.Model
  use Systems.Reload
  alias ApathyDrive.Repo

  schema "monster_templates" do
    field :name,              :string
    field :description,       :string
    field :death_message,     :string
    field :enter_message,     :string
    field :exit_message,      :string
    field :abilities,         {:array, :integer}, default: []
    field :greeting,          :string
    field :gender,            :string
    field :game_limit,        :integer
    field :adjectives,        {:array, :string}, default: []
    field :strength,          :integer
    field :agility,           :integer
    field :intelligence,      :integer
    field :health,            :integer
    field :skills,            :string #json
    field :hit_verbs,         {:array, :string}, default: ["attack", "assault", "strike"]
    field :limbs,             :string #json
    field :chance_to_follow,  :integer
    field :damage,            :string #json
    field :disposition,       :string
    field :alignment,         :string
    field :possession_level,  :integer
    field :questions,         :string #json
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
