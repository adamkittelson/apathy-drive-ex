defmodule ApathyDriveWeb.Admin.RaceView do
  use ApathyDriveWeb, :view
  alias ApathyDrive.{Race, RaceTrait}

  def link_to_trait_fields do
    changeset = Race.changeset(%Race{races_traits: [%RaceTrait{}]})
    form = Phoenix.HTML.FormData.to_form(changeset, [])
    fields = render_to_string(__MODULE__, "trait_fields.html", f: form)

    link(
      "Add Trait",
      to: "#",
      "data-template": fields,
      id: "add_trait",
      class: "button button-primary"
    )
  end
end
