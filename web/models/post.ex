defmodule AwesomeprojectBackend.Post do
  use AwesomeprojectBackend.Web, :model

  schema "posts" do
    field :title, :string
    field :content, :string

    timestamps()
  end

  @doc """
  Builds a changeset based on the `struct` and `params`.
  """
  def changeset(struct, params \\ %{}) do
    struct
    |> cast(params, [:title, :content])
    |> validate_required([:title, :content])
  end

  @doc """
  Returns the JSON representation of a Post
  """
  def to_json(post) do
    %{
      id: post.id,
      title: post.title,
      content: post.title
    }
  end
end
