defmodule AwesomeprojectBackend.PostController do
  use AwesomeprojectBackend.Web, :controller

  alias AwesomeprojectBackend.Post

  plug :scrub_params, "post" when action in [:create, :update]

  def index(conn, _params) do
    posts = Repo.all(Post)
    render(conn, "index.json", posts: posts)
  end

  def create(conn, %{"post" => post_params}) do
    changeset = Post.changeset(%Post{}, post_params)

    case Repo.insert(changeset) do
      {:ok, post} ->
        conn
        |> put_status(:created)
        |> put_resp_header("location", post_path(conn, :show, post))
        |> render("show.json", post: post)
      {:error, changeset} ->
        conn
        |> put_status(:unprocessable_entity)
        |> render(AwesomeprojectBackend.ChangesetView, "error.json", changeset: changeset)
    end
  end

  def show(conn, %{"id" => id}) do
    post = Repo.get(Post, id)

    if post do
      render(conn, "show.json", post: post)
    else
      send_resp(conn, :not_found, "")
    end
  end

  def update(conn, %{"id" => id, "post" => post_params}) do
    post = Repo.get(Post, id)

    if post do
      changeset = Post.changeset(post, post_params)

      case Repo.update(changeset) do
        {:ok, post} ->
          render(conn, "show.json", post: post)
        {:error, changeset} ->
          conn
          |> put_status(:unprocessable_entity)
          |> render(AwesomeprojectBackend.ChangesetView, "error.json", changeset: changeset)
      end
    else
      send_resp(conn, :not_found, "")
    end
  end

  def delete(conn, %{"id" => id}) do
    post = Repo.get(Post, id)

    if post do
      Repo.delete!(post)
      send_resp(conn, :no_content, "")
    else
      send_resp(conn, :not_found, "")
    end
  end
end
