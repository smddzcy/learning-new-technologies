# Docker

Docker packages your app with its dependencies, freeing you from worrying about your system configuration, and making your app more portable.

## Examples

- See running containers: `docker ps`

- See all containers: `docker ps -a`

- Remove a container (and stop if it's running): `docker rm -f <container>` (This removes the container, but not its image.)

- See images: `docker images`

- Remove an image: `docker rmi <image-id-or-name>`

- Start a Dockerized web server: `docker run -d -p 80:80 --name webserver nginx`


