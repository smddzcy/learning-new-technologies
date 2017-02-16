# Docker

Docker packages your app with its dependencies, freeing you from worrying about your system configuration, and making your app more portable.

## Basics

- See running containers: `docker ps`

- See all containers: `docker ps -a`

- Remove a container (and stop if it's running with `-f`): `docker rm -f <container>` (This removes the container, but not its image.)

- See images: `docker images`

- Remove an image: `docker rmi <image-id-or-name>`

## Creating an Image

- An example `Dockerfile` to create an image:
```
# base image
FROM alpine:3.5

# install python and pip
RUN apk add --update py2-pip

# install Python modules needed by the Python app
COPY requirements.txt /usr/src/app/
RUN pip install --no-cache-dir -r /usr/src/app/requirements.txt

# copy files required for the app to run
COPY app.py /usr/src/app/
COPY templates/index.html /usr/src/app/templates/

# tell the port number the container should expose
EXPOSE 5000

# run the application
CMD ["python", "/usr/src/app/app.py"]
```

- Build the image: `docker build -t <username>/<image-name> <dir-contains-dockerfile>`

- Push the image to Docker Hub: `docker push <username>/<image-name>`

## Examples

- Start a Dockerized web server: `docker run -d -p 8080:80 --name webserver nginx`
  - `-d`: Detach run process from terminal
  - `-p 8080:80`: Bind `8080` to `80` port
  - `--name <container-name>`: Name of the new container

## Dockerfile

- `FROM` starts the Dockerfile. It is a requirement that the Dockerfile must start with the `FROM` command. Images are created in layers, which means you can use another image as the base image for your own. The `FROM` command defines your base layer. As arguments, it takes the name of the image. Optionally, you can add the Docker Hub username of the maintainer and image version, in the format `username/imagename:version`.

- `RUN` is used to build up the Image you're creating. For each `RUN` command, Docker will run the command then create a new layer of the image. This way you can roll back your image to previous states easily. The syntax for a `RUN` instruction is to place the full text of the shell command after the `RUN` (e.g., `RUN mkdir /user/local/foo`). This will automatically run in a `/bin/sh` shell. You can define a different shell like this: `RUN /bin/bash -c 'mkdir /user/local/foo'`

- `COPY` copies local files into the container.

- `CMD` defines the commands that will run on the Image at start-up. Unlike a `RUN`, this does not create a new layer for the Image, but simply runs the command. There can only be one `CMD` per a Dockerfile/Image. If you need to run multiple commands, the best way to do that is to have the `CMD` run a script. `CMD` requires that you tell it where to run the command, unlike `RUN`. So example `CMD` commands would be:
```
CMD ["python", "./app.py"]
CMD ["/bin/bash", "echo", "Hello World"]
```

- `EXPOSE` opens ports in your image to allow communication to the outside world when it runs in a container.

- `PUSH` pushes your image to Docker Hub, or alternately to another saved registery.

- More: [Best practices for writing Dockerfiles](https://docs.docker.com/engine/userguide/eng-image/dockerfile_best-practices/).
