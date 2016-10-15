defmodule ApathyDrive.ExAdmin.Race do
  use ExAdmin.Register

  register_resource ApathyDrive.Race do

    index do
      selectable_column

      column :id
      column :name
      column :strength
      column :agility
      column :intellect
      column :willpower
      column :health
      column :charm
      column "Total", fn(race) ->
        [race.strength, race.agility, race.intellect, race.willpower, race.health, race.charm]
        |> Enum.sum
        |> to_string()
      end
      actions
    end

    form race do
      inputs do
        input race, :name
        content do
          ~s(
          <div id="race_description_input" class="form-group">
            <label class="col-sm-2 control-label" for="race_description">
              Desription<abbr class="required" title="required">*</abbr>
            </label>
            <div class="col-sm-10">
              <textarea id="race_description" name="race[description]">#{race.description}</textarea>
            </div>
          </div>
          )
        end
        input race, :strength
        input race, :agility
        input race, :intellect
        input race, :willpower
        input race, :health
        input race, :charm
      end
    end

  end
end
