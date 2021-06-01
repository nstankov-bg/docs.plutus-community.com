FROM node:lts-alpine3.13

COPY ./www /home/node/app

COPY ./docker/entrypoint/docker-entrypoint.sh /usr/local/bin/
COPY ./docker/entrypoint/docker-entrypoint-watch.sh /usr/local/bin/
COPY ./docker/entrypoint/docker-entrypoint-export.sh /usr/local/bin/

WORKDIR /home/node/app

#RUN yarn install
#RUN yarn build

ENTRYPOINT ["docker-entrypoint.sh"]

CMD [ "yarn start" ]