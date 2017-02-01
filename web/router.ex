defmodule AwesomeprojectBackend.Router do
  use AwesomeprojectBackend.Web, :router

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/", AwesomeprojectBackend do
    pipe_through :api # Use the api

    resources "/posts", PostController
  end

  # Other scopes may use custom stacks.
  # scope "/api", AwesomeprojectBackend do
  #   pipe_through :api
  # end
end
