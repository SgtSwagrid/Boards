# boards-server

The server subproject is responsible for authentication, persistence, and the handling of game sessions.

### Libraries

boards-server uses the following libraries, which will be installed automatically by sbt:

* The [Play Framework](https://www.playframework.com/) for handling web requests on the server.
* [Slick](https://scala-slick.org/) for database access by [functional relational mapping](https://scala-slick.org/talks/2014-06-09_33rd_Degree/Functional%20Relational%20Mapping%20with%20Slick.pdf).
* [H2](https://www.h2database.com/html/main.html) for running an embedded database.
* [Apache Pekko](https://pekko.apache.org/), an [actor](https://pekko.apache.org/docs/pekko/current/general/actors.html) framework, used here for managing server-side state in relation to web socket sessions.
* [Circe](https://circe.github.io/circe/) for automatic serialisation/deserialisation to/from JSON.

### Directory Structure

* [scala/controllers/](./src/main/public/scala/controllers) handlers for HTTP and websocket requests.
* [scala/models/](./src/main/public/scala/models) database actions for queryig or updating persistent state.
* [scala/schema/](./src/main/public/scala/schema) database table schema, used to represent database tables and rows therein directly in the language.
* [public/](./src/main/public) contains all assets (audio and textures) used by the website and games.
* [resources/](./src/main/resources) contains configuration/routes for the Play framework, as well as database migration scripts.
* [twirl](./src/main/twirl) contains webpage templates for server-side rendering.

### Architecture

#### Models
For most parts of the application, we use standard HTTP GET/POST requests to communicate with the client. The major exception is for the game view itself, where a websocket is the more natural choice. In the controllers directory, classes ending with "___Controller.scala" are for handling GET/POST requests, whereas classes ending with "___Actor.scala" are for handling websocket messages.

#### Database
We use an embdedded H2 database. No special setup is required. All data is stored locally in the repository for testing.

#### Rendering
Boards only uses client-side rendering. Thus, the twirl directory is almost empty - we need only a single template.