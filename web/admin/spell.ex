defmodule ApathyDrive.ExAdmin.Spell do
  use ExAdmin.Register
  alias ApathyDrive.{EntityAbility, Repo}

  register_resource ApathyDrive.Spell do

    index do
      selectable_column

      column :id
      column :command
      column :name
      column :targets
      column :kind
      column :duration_in_ms
      column :cooldown_in_ms

      actions
    end

    show spell do
      attributes_table do
        row :id
        row :command
        row :name
        row :description
        row :targets
        row :kind
        row :mana
        row :duration_in_ms
        row :cooldown_in_ms
        row :user_message
        row :target_message
        row :spectator_message
      end

      panel "Abilities" do
        markup_contents do
          rows =
            "spells"
            |> EntityAbility.load_abilities(spell.id)
            |> Enum.map(fn {ability, value} ->
                 ~s(
                   <tr>
                     <th>#{ability}</th>
                     <td>#{inspect(value)}</td>
                   </tr>
                 )
               end)

          ~s(
            <table border="0" cellspacing="0" cellpadding="0" class="table">
              <thead>
                <tr>
                  <th style="width: 150px;">Ability</th>
                  <th class="th-value">Value</th>
                </tr>
              </thead>
              <tbody>
                #{rows}
              </tbody>
            </table>
          )
        end

      end

      panel "Casters" do
        markup_contents do
          rows =
            spell
            |> Ecto.assoc(:entity_spells)
            |> Repo.all
            |> Enum.map(fn entity_spell ->
                 %{name: name} =
                   Ecto.Query.from(es in entity_spell.assoc_table)
                   |> Ecto.Query.where([c], c.id == ^entity_spell.assoc_id)
                   |> Ecto.Query.select([:name])
                   |> ApathyDrive.Repo.one

                 ~s(
                   <tr>
                     <th>#{entity_spell.assoc_table}</th>
                     <td>#{name}</td>
                   </tr>
                 )
               end)

          ~s(
            <table border="0" cellspacing="0" cellpadding="0" class="table">
              <thead>
                <tr>
                  <th style="width: 150px;">Table</th>
                  <th class="th-value">Name</th>
                </tr>
              </thead>
              <tbody>
                #{rows}
              </tbody>
            </table>
          )
        end

      end

      # panel "Shop" do
      #   table_for room.items_for_sales do
      #     column :id, link: true
      #     column :name, link: true
      #   end
      # end
      #
      # panel "Lair" do
      #   table_for room.lair_monsters do
      #     column :id, link: true
      #     column :name, link: true
      #   end
      # end
    end

  end
end
