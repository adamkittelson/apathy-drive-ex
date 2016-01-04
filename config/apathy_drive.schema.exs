@moduledoc """
A schema is a keyword list which represents how to map, transform, and validate
configuration values parsed from the .conf file. The following is an explanation of
each key in the schema definition in order of appearance, and how to use them.

## Import

A list of application names (as atoms), which represent apps to load modules from
which you can then reference in your schema definition. This is how you import your
own custom Validator/Transform modules, or general utility modules for use in
validator/transform functions in the schema. For example, if you have an application
`:foo` which contains a custom Transform module, you would add it to your schema like so:

`[ import: [:foo], ..., transforms: ["myapp.some.setting": MyApp.SomeTransform]]`

## Extends

A list of application names (as atoms), which contain schemas that you want to extend
with this schema. By extending a schema, you effectively re-use definitions in the
extended schema. You may also override definitions from the extended schema by redefining them
in the extending schema. You use `:extends` like so:

`[ extends: [:foo], ... ]`

## Mappings

Mappings define how to interpret settings in the .conf when they are translated to
runtime configuration. They also define how the .conf will be generated, things like
documention, @see references, example values, etc.

See the moduledoc for `Conform.Schema.Mapping` for more details.

## Transforms

Transforms are custom functions which are executed to build the value which will be
stored at the path defined by the key. Transforms have access to the current config
state via the `Conform.Conf` module, and can use that to build complex configuration
from a combination of other config values.

See the moduledoc for `Conform.Schema.Transform` for more details and examples.

## Validators

Validators are simple functions which take two arguments, the value to be validated,
and arguments provided to the validator (used only by custom validators). A validator
checks the value, and returns `:ok` if it is valid, `{:warn, message}` if it is valid,
but should be brought to the users attention, or `{:error, message}` if it is invalid.

See the moduledoc for `Conform.Schema.Validator` for more details and examples.
"""
[
  extends: [],
  import: [],
  mappings: [
    "logger.console.format": [
      commented: false,
      datatype: :binary,
      default: """
      $time $metadata[$level] $message
      """,
      doc: "Provide documentation for logger.console.format here.",
      hidden: true,
      to: "logger.console.format"
    ],
    "logger.console.metadata": [
      commented: false,
      datatype: [
        list: :atom
      ],
      default: [
        :request_id
      ],
      doc: "Provide documentation for logger.console.metadata here.",
      hidden: true,
      to: "logger.console.metadata"
    ],
    "logger.console.level": [
      commented: false,
      datatype: :atom,
      default: :info,
      doc: "Provide documentation for logger.console.level here.",
      hidden: true,
      to: "logger.console.level"
    ],
    "logger.level": [
      commented: false,
      datatype: :atom,
      default: :info,
      doc: "Provide documentation for logger.level here.",
      hidden: true,
      to: "logger.level"
    ],
    "apathy_drive.Elixir.ApathyDrive.Endpoint.render_errors.accepts": [
      commented: false,
      datatype: [
        list: :binary
      ],
      default: [
        "html",
        "json"
      ],
      doc: "Provide documentation for apathy_drive.Elixir.ApathyDrive.Endpoint.render_errors.accepts here.",
      hidden: true,
      to: "apathy_drive.Elixir.ApathyDrive.Endpoint.render_errors.accepts"
    ],
    "apathy_drive.Elixir.ApathyDrive.Endpoint.debug_errors": [
      commented: false,
      datatype: :atom,
      default: false,
      doc: "Provide documentation for apathy_drive.Elixir.ApathyDrive.Endpoint.debug_errors here.",
      hidden: true,
      to: "apathy_drive.Elixir.ApathyDrive.Endpoint.debug_errors"
    ],
    "apathy_drive.Elixir.ApathyDrive.Endpoint.pubsub.name": [
      commented: false,
      datatype: :atom,
      default: :pub_sub,
      doc: "Provide documentation for apathy_drive.Elixir.ApathyDrive.Endpoint.pubsub.name here.",
      hidden: true,
      to: "apathy_drive.Elixir.ApathyDrive.Endpoint.pubsub.name"
    ],
    "apathy_drive.Elixir.ApathyDrive.Endpoint.pubsub.adapter": [
      commented: false,
      datatype: :atom,
      default: Phoenix.PubSub.PG2,
      doc: "Provide documentation for apathy_drive.Elixir.ApathyDrive.Endpoint.pubsub.adapter here.",
      hidden: true,
      to: "apathy_drive.Elixir.ApathyDrive.Endpoint.pubsub.adapter"
    ],
    "apathy_drive.Elixir.ApathyDrive.Endpoint.root": [
      commented: false,
      datatype: :binary,
      default: "/app",
      doc: "Provide documentation for apathy_drive.Elixir.ApathyDrive.Endpoint.root here.",
      hidden: true,
      to: "apathy_drive.Elixir.ApathyDrive.Endpoint.root"
    ],
    "apathy_drive.Elixir.ApathyDrive.Endpoint.http.port": [
      commented: false,
      datatype: :integer,
      default: 4000,
      doc: "Provide documentation for apathy_drive.Elixir.ApathyDrive.Endpoint.http.port here.",
      hidden: false,
      to: "apathy_drive.Elixir.ApathyDrive.Endpoint.http.port"
    ],
    "apathy_drive.Elixir.ApathyDrive.Endpoint.url.host": [
      commented: false,
      datatype: :binary,
      default: "apotheos.is",
      doc: "Provide documentation for apathy_drive.Elixir.ApathyDrive.Endpoint.url.host here.",
      hidden: false,
      to: "apathy_drive.Elixir.ApathyDrive.Endpoint.url.host"
    ],
    "apathy_drive.Elixir.ApathyDrive.Endpoint.url.port": [
      commented: false,
      datatype: :integer,
      default: 4000,
      doc: "Provide documentation for apathy_drive.Elixir.ApathyDrive.Endpoint.url.port here.",
      hidden: false,
      to: "apathy_drive.Elixir.ApathyDrive.Endpoint.url.port"
    ],
    "apathy_drive.Elixir.ApathyDrive.Endpoint.cache_static_manifest": [
      commented: false,
      datatype: :binary,
      default: "priv/static/manifest.json",
      doc: "Provide documentation for apathy_drive.Elixir.ApathyDrive.Endpoint.cache_static_manifest here.",
      hidden: false,
      to: "apathy_drive.Elixir.ApathyDrive.Endpoint.cache_static_manifest"
    ],
    "apathy_drive.Elixir.ApathyDrive.Endpoint.server": [
      commented: false,
      datatype: :atom,
      default: true,
      doc: "Provide documentation for apathy_drive.Elixir.ApathyDrive.Endpoint.server here.",
      hidden: true,
      to: "apathy_drive.Elixir.ApathyDrive.Endpoint.server"
    ],
    "apathy_drive.Elixir.ApathyDrive.Endpoint.secret_key_base": [
      commented: false,
      datatype: :binary,
      doc: "Provide documentation for apathy_drive.Elixir.ApathyDrive.Endpoint.secret_key_base here.",
      hidden: false,
      to: "apathy_drive.Elixir.ApathyDrive.Endpoint.secret_key_base"
    ],
    "apathy_drive.Elixir.Facebook.client_id": [
      commented: false,
      datatype: :binary,
      doc: "Provide documentation for apathy_drive.Elixir.Facebook.client_id here.",
      hidden: false,
      to: "apathy_drive.Elixir.Facebook.client_id"
    ],
    "apathy_drive.Elixir.Facebook.client_secret": [
      commented: false,
      datatype: :binary,
      doc: "Provide documentation for apathy_drive.Elixir.Facebook.client_secret here.",
      hidden: false,
      to: "apathy_drive.Elixir.Facebook.client_secret"
    ],
    "apathy_drive.Elixir.Facebook.redirect_uri": [
      commented: false,
      datatype: :binary,
      default: "http://apotheos.is/auth/callback",
      doc: "Provide documentation for apathy_drive.Elixir.Facebook.redirect_uri here.",
      hidden: false,
      to: "apathy_drive.Elixir.Facebook.redirect_uri"
    ],
    "apathy_drive.Elixir.ApathyDrive.Repo.hostname": [
      commented: false,
      datatype: :binary,
      default: "apotheos.is",
      doc: "Provide documentation for apathy_drive.Elixir.ApathyDrive.Repo.hostname here.",
      hidden: false,
      to: "apathy_drive.Elixir.ApathyDrive.Repo.hostname"
    ],
    "apathy_drive.Elixir.ApathyDrive.Repo.database": [
      commented: false,
      datatype: :binary,
      default: "apathy_drive",
      doc: "Provide documentation for apathy_drive.Elixir.ApathyDrive.Repo.database here.",
      hidden: false,
      to: "apathy_drive.Elixir.ApathyDrive.Repo.database"
    ],
    "apathy_drive.Elixir.ApathyDrive.Repo.username": [
      commented: false,
      datatype: :binary,
      default: "apathy_drive",
      doc: "Provide documentation for apathy_drive.Elixir.ApathyDrive.Repo.username here.",
      hidden: false,
      to: "apathy_drive.Elixir.ApathyDrive.Repo.username"
    ],
    "apathy_drive.Elixir.ApathyDrive.Repo.password": [
      commented: false,
      datatype: :binary,
      doc: "Provide documentation for apathy_drive.Elixir.ApathyDrive.Repo.password here.",
      hidden: false,
      to: "apathy_drive.Elixir.ApathyDrive.Repo.password"
    ],
    "apathy_drive.Elixir.ApathyDrive.Repo.adapter": [
      commented: false,
      datatype: :atom,
      default: Ecto.Adapters.Postgres,
      doc: "Provide documentation for apathy_drive.Elixir.ApathyDrive.Repo.adapter here.",
      hidden: true,
      to: "apathy_drive.Elixir.ApathyDrive.Repo.adapter"
    ],
    "apathy_drive.Elixir.ApathyDrive.Repo.extensions.Elixir.Extensions.JSON.library": [
      commented: false,
      datatype: :atom,
      default: Poison,
      doc: "Provide documentation for apathy_drive.Elixir.ApathyDrive.Repo.extensions.Elixir.Extensions.JSON.library here.",
      hidden: true,
      to: "apathy_drive.Elixir.ApathyDrive.Repo.extensions.Elixir.Extensions.JSON.library"
    ],
    "apathy_drive.Elixir.ApathyDrive.Repo.size": [
      commented: false,
      datatype: :integer,
      default: 10,
      doc: "Provide documentation for apathy_drive.Elixir.ApathyDrive.Repo.size here.",
      hidden: false,
      to: "apathy_drive.Elixir.ApathyDrive.Repo.size"
    ],
    "apathy_drive.Elixir.ApathyDrive.Repo.lazy": [
      commented: false,
      datatype: :atom,
      default: false,
      doc: "Provide documentation for apathy_drive.Elixir.ApathyDrive.Repo.lazy here.",
      hidden: true,
      to: "apathy_drive.Elixir.ApathyDrive.Repo.lazy"
    ],
    "apathy_drive.Elixir.ApathyDrive.Repo.max_overflow": [
      commented: false,
      datatype: :integer,
      default: 0,
      doc: "Provide documentation for apathy_drive.Elixir.ApathyDrive.Repo.max_overflow here.",
      hidden: true,
      to: "apathy_drive.Elixir.ApathyDrive.Repo.max_overflow"
    ]
  ],
  transforms: [],
  validators: []
]