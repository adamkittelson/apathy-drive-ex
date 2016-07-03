defmodule ApathyDrive.ExAdmin.Ability do
  use ExAdmin.Register

  def display_name(ability) do
    "##{ability.id}"
  end


  register_resource ApathyDrive.Ability do

    form ability do

      inputs do
        content do
          ~s(
          <div id="ability_properties_input" class="form-group">
            <label class="col-sm-2 control-label" for="ability_properties">
              Properties<abbr class="required" title="required">*</abbr>
            </label>
            <div class="col-sm-10">
              <textarea id="ability_properties" name="ability[properties]" class="json">#{Poison.encode!(ability.properties)}</textarea>
            </div>
          </div>
          )
        end

      end
    end

  end
end
