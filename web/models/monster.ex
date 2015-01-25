defmodule Monster do
  use Ecto.Model
  use GenServer
  use Systems.Reload
  import Systems.Text
  alias ApathyDrive.Repo
  alias Phoenix.PubSub

  schema "monsters" do
    field :name,                :string
    field :skills,              :string #json
    field :limbs,               :string #json
    field :monster_template_id, :integer
    field :room_id,             :integer
    field :experience,          :integer, default: 0
    field :level,               :integer, default: 1
    field :alignment,           :float
    field :lair_id,             :integer, virtual: true
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
    field :spirit,              :any,     virtual: true
  end

  def init(monster) do
    if monster.room_id do
      PubSub.subscribe(self, "rooms:#{monster.room_id}:monsters")
    end

    {:ok, monster}
  end

  def look_name(%Monster{} = monster) do
    cond do
      evil?(monster) ->
        "<span class='magenta'>#{monster.name}</span>"
      good?(monster) ->
        "<span class='grey'>#{monster.name}</span>"
      neutral?(monster) ->
        "<span class='dark-cyan'>#{monster.name}</span>"
    end
  end

  def good?(%Monster{alignment: alignment}) when alignment < -50, do: true
  def evil?(%Monster{alignment: alignment}) when alignment < -50, do: true
  def neutral?(%Monster{}), do: true

  def display_enter_message(%Room{} = room, monster) do
    display_enter_message(room, monster, Room.random_direction(room))
  end

  def display_enter_message(%Room{} = room, monster, direction) do
    message = monster
              |> enter_message
              |> interpolate(%{
                   "name" => name(monster),
                   "direction" => Room.enter_direction(direction)
                 })
              |> capitalize_first

    Phoenix.Channel.broadcast "rooms:#{room.id}", "scroll", %{:html => "<p><span class='dark-green'>#{message}</span></p>"}
  end

  # Generate functions from Ecto schema
  fields = Keyword.keys(@assign_fields)

  Enum.each(fields, fn(field) ->
    def unquote(field)(pid) do
      GenServer.call(pid, unquote(field))
    end

    def unquote(field)(pid, new_value) do
      GenServer.call(pid, {unquote(field), new_value})
    end
  end)

  Enum.each(fields, fn(field) ->
    def handle_call(unquote(field), _from, state) do
      {:reply, Map.get(state, unquote(field)), state}
    end

    def handle_call({unquote(field), new_value}, _from, state) do
      {:reply, new_value, Map.put(state, unquote(field), new_value)}
    end
  end)


end
