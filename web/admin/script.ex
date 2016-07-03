defmodule ApathyDrive.ExAdmin.Script do
  use ExAdmin.Register

  def display_name(script) do
    "##{script.id}"
  end

  register_resource ApathyDrive.Script do

    form script do

      inputs do
        content do
          ~s(
          <div id="script_instructions_input" class="form-group">
            <label class="col-sm-2 control-label" for="script_instructions">
              Instructions<abbr class="required" title="required">*</abbr>
            </label>
            <div class="col-sm-10">
              <textarea id="script_instructions" name="script[instructions]" class="json">#{Poison.encode!(script.instructions)}</textarea>
            </div>
          </div>
          )
        end

      end
    end

  end
end
