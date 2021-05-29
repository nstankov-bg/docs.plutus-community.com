FROM node:lts-alpine3.13

COPY ./www /home/node/app
COPY ./book /home/node/app/book
COPY ./docker/entrypoint/docker-entrypoint.sh /usr/local/bin/

WORKDIR /home/node/app
ENTRYPOINT ["docker-entrypoint.sh"]

CMD [ "node" ]