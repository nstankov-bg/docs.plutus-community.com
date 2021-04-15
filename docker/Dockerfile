FROM node:10.22.0-alpine

RUN mkdir /app/

RUN npm install graceful-fs --save-dev \
 && npm install gitbook-cli -g

EXPOSE 4000

WORKDIR '/app'
CMD [ "gitbook ./" ]
