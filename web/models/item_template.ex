defmodule ItemTemplate do
  use Ecto.Model
  use Systems.Reload
  alias ApathyDrive.Repo

  schema "item_templates" do
    field :name,                  :string
    field :keywords,              {:array, :string}, default: []
    field :description,           :string
    field :slot,                  :string
    field :worn_on,               :string #json
    field :hit_verbs,             {:array, :string}, default: []
    field :damage,                :string #json
    field :required_skills,       :string #json
    field :speed,                 :decimal
    field :accuracy_skill,        :string
    field :ac,                    :integer
    field :uses,                  :integer
    field :destruct_message,      :string
    field :room_destruct_message, :string
    field :can_pick_up,           :boolean
    field :value,                 :integer
    field :light,                 :integer
    field :light_duration,        :integer
    field :always_lit,            :boolean
  end


  # Generate functions from Ecto schema

  fields = Keyword.keys(@assign_fields)

  Enum.each(fields, fn(field) ->
    def unquote(field)(item_template) do
      GenServer.call(item_template, unquote(field))
    end
  end)

  Enum.each(fields, fn(field) ->
    def handle_call(unquote(field), _from, item_template) do
      {:reply, Map.get(item_template, unquote(field)), item_template}
    end
  end)
end
