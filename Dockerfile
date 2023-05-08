# FROM --platform=linux/amd64 node:18 AS frontend
FROM  node:18.15.0 AS frontend
ARG CURRENT_COMMIT_ARG
ENV CURRENT_COMMIT=$CURRENT_COMMIT_ARG
WORKDIR /app
COPY ./src ./src
COPY package.json     	package.json
COPY packages.dhall   	packages.dhall
COPY spago.dhall      	spago.dhall
COPY webpack.config.js	webpack.config.js
COPY package-lock.json	package-lock.json
COPY --chown=root ./.git ./.git

# RUN npm install -g purescript-installer@0.3.3
RUN npm ci
RUN mkdir ./target
RUN npm run build
# RUN yarn spago --jobs 10 build --purs-args '--codegen js,sourcemaps -o ./target/output.purescript' -v
RUN ls ./src/main/js && npm run package -- --env production
# RUN ls ./src/main/js && yarn package

FROM sbtscala/scala-sbt:eclipse-temurin-17.0.4_1.7.1_3.2.0 AS backend
WORKDIR /app
COPY --from=frontend /app/spago.dhall ./spago.dhall
COPY ./ ./
# remove option to remove tests when everything else works
RUN sbt 'set test in assembly := {}' clean assembly

FROM eclipse-temurin:17.0.6_10-jre-alpine
WORKDIR /app
COPY --from=frontend /app/target/output.webpack ./target/output.webpack
COPY --from=backend '/app/target/*/*.jar' ./target/clipperz.jar 
RUN chmod -R 755 ./ && \
    addgroup --system clipperz && adduser --system clipperz --ingroup clipperz && \
    chown -R clipperz: ./
USER clipperz
# CMD [ "java", "-jar", "/app/target/clipperz.jar", "/archive/blob", "/archive/user", "/archive/one_time_share", "8080"]
ENTRYPOINT [ "java", "-jar", "/app/target/clipperz.jar"]
