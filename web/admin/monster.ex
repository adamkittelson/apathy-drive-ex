defmodule ApathyDrive.ExAdmin.Monster do
  use ExAdmin.Register

  register_resource ApathyDrive.Monster do

    form monster do
      inputs "Spells" do
        content do
          ~s(
          <div class="form-group">
            <label class="col-sm-2 control-label" for="room_items_for_sale_ids">Spells</label>
            <div class="col-sm-10">
              <input name="monster[entities_spells_ids][]" type="hidden" value="">
              <div class="checkbox">
                <label>
                  <input type="checkbox" name="monster[entities_spells_ids][13]">minor healing
                </label>
              </div>
            </div>
          </div>
          )
        end
      end
    end
  end
end
