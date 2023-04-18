# FROM --platform=linux/amd64 node:18 AS frontend
FROM  node:18.15.0 AS frontend
ARG CURRENT_COMMIT_ARG
ENV CURRENT_COMMIT=$CURRENT_COMMIT_ARG
WORKDIR /app
COPY ./src ./src
COPY package.json     package.json
COPY packages.dhall   packages.dhall
COPY spago.dhall      spago.dhall
COPY webpack.config.js webpack.config.js
# COPY yarn.lock        yarn.lock

RUN npm install -g purescript@0.15.7
RUN npm install -g yarn@1.22.19 --force
RUN yarn install
RUN mkdir ./target
RUN yarn spago --jobs 10 build --purs-args '--codegen js,sourcemaps -o ./target/output.purescript' -v
RUN ls ./src/main/js && yarn package

FROM sbtscala/scala-sbt:eclipse-temurin-17.0.4_1.7.1_3.2.0 AS backend
WORKDIR /app
COPY --from=frontend /app/spago.dhall ./spago.dhall
COPY ./ ./
# remove option to remove tests when everything else works
RUN sbt 'set test in assembly := {}' clean assembly

FROM openjdk:jre-alpine
COPY --from=frontend /app/target/output.webpack ./target/output.webpack
COPY --from=backend '/app/target/*/clipperz.jar' /app/target/clipperz.jar
CMD [ "java", "-jar", "/app/target/clipperz.jar", "/archive/blob", "/archive/user", "8080"]
