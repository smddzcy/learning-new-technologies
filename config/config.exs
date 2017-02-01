# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.
use Mix.Config

# General application configuration
config :awesomeproject_backend,
  ecto_repos: [AwesomeprojectBackend.Repo]

# Configures the endpoint
config :awesomeproject_backend, AwesomeprojectBackend.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "TemTImyoPe/KKsXz4DfFaJKaeDIrIWygNuijWI9IYg0H56cArAkSa72qXm32tAKA",
  render_errors: [view: AwesomeprojectBackend.ErrorView, accepts: ~w(html json)],
  pubsub: [name: AwesomeprojectBackend.PubSub,
           adapter: Phoenix.PubSub.PG2]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env}.exs"
