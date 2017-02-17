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

## Swarm

- Swarm mode is the cluster management and orchestration features embedded in the Docker. Swarm allows you to run your containers on more than one machine.

- Create a Swarm: `docker swarm init`

- Create a Docker Compose file (`docker-stack.yml`):
```yml
# version 3 is important.
# docker stack deploy won't support use of earlier versions.
version: "3"

services:
  redis:
    image: redis:alpine
    ports:
      - "6379"
    networks:
      - frontend
    deploy:
      replicas: 2
      update_config:
        parallelism: 2
        delay: 10s
      restart_policy:
        condition: on-failure
  
  db:
    image: postgres:9.4
    volumes:
      - db-data:/var/lib/postgresql/data
    networks:
      - backend
    deploy:
      placement:
        constraints: [node.role == manager]
  
  vote:
    image: dockersamples/examplevotingapp_vote:before
    ports:
      - 5000:80
    networks:
      - frontend
    depends_on:
      - redis
    deploy:
      replicas: 2
      update_config:
        parallelism: 2
      restart_policy:
        condition: on-failure

  result:
    image: dockersamples/examplevotingapp_result:before
    ports:
      - 5001:80
    networks:
      - backend
    depends_on:
      - db
    deploy:
      replicas: 2
      update_config:
        parallelism: 2
        delay: 10s
      restart_policy:
        condition: on-failure

  worker:
    image: dockersamples/examplevotingapp_worker
    networks:
      - frontend
      - backend
    deploy:
      mode: replicated
      replicas: 1
      labels: [APP=VOTING]
      restart_policy:
        condition: on-failure
        delay: 10s
        max_attempts: 3
        window: 120s

  visualizer:
    image: manomarks/visualizer
    ports:
      - "8080:8080"
    stop_grace_period: 1m30s
    volumes:
      - "/var/run/docker.sock:/var/run/docker.sock"

networks:
  frontend:
  backend:

volumes:
  db-data:
```

- Deploy the stack: `docker stack deploy --compose-file docker-stack.yml <app-name>`

- Verify: `docker stack services <app-name>` Output should be sth like this:
```
ID            NAME         MODE        REPLICAS  IMAGE
25wo6p7fltyn  vote_db      replicated  1/1       postgres:9.4
2ot4sz0cgvw3  vote_worker  replicated  1/1       dockersamples/examplevotingapp_worker:latest
9faz4wbvxpck  vote_redis   replicated  2/2       redis:alpine
ocm8x2ijtt88  vote_vote    replicated  2/2       dockersamples/examplevotingapp_vote:latest
p1dcwi0fkcbb  vote_result  replicated  2/2       dockersamples/examplevotingapp_result:latest
```

- The Compose file also defines two networks, front-tier and back-tier. Each container is placed on one or two networks. Once on those networks, they can access other services on that network in code just by using the name of the service.

- Services are isolated on their network. Services are only able to discover each other by name if they are on the same network.

