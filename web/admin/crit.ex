defmodule ApathyDrive.ExAdmin.Crit do
  use ExAdmin.Register

  register_resource ApathyDrive.Crit do

    form crit do

      inputs do
        input crit, :crit_table
        input crit, :letter

        content do
          ~s(
          <div id="crit_abilities_input" class="form-group">
            <label class="col-sm-2 control-label" for="crit_abilities">
              Abilities<abbr class="required" title="required">*</abbr>
            </label>
            <div class="col-sm-10">
              <textarea id="crit_abilities" name="crit[abilities]" class="json">#{Poison.encode!(crit.abilities)}</textarea>
            </div>
          </div>
          )
        end

      end
    end

  end
end
