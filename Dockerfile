FROM --platform=linux/amd64 node:18 AS frontend
WORKDIR /app
COPY ./src ./src
COPY package.json package.json
COPY packages.dhall packages.dhall
COPY spago.dhall spago.dhall
RUN npm install -g purescript
RUN npm install -g yarn@1.22.18 --force
RUN yarn install
RUN mkdir ./target
RUN yarn spago --jobs 10 build --purs-args '--codegen js,sourcemaps -o ./target/output.purescript' -v
RUN ls ./src/main/js && yarn package

FROM sbtscala/scala-sbt:eclipse-temurin-19.0.1_10_1.8.0_3.2.1 AS backend
WORKDIR /app
COPY --from=frontend /app/spago.dhall ./spago.dhall
COPY ./ ./
RUN sbt compile && sbt package

FROM openjdk:jre-alpine
COPY --from=frontend /app/target/output.parcel ./target/output.parcel
COPY --from=backend /app/target/scala-3.2.0/clipperz-backend_3-0.1.0-SNAPSHOT.jar /app/target/scala-3.2.0/clipperz-backend_3-0.1.0-SNAPSHOT.jar
CMD [ "java", "-jar", "/app/target/scala-3.2.0/clipperz-backend_3-0.1.0-SNAPSHOT.jar"]
