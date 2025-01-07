# boards-client

The client subproject is responsible for rendering and handling user input.

### Libraries

boards-client uses the following libraries, which will be installed automatically by sbt:

* [Laminar](https://laminar.dev/) for client-side rendering.
* [Airstream](https://github.com/raquo/Airstream), which is required by Laminar, is a library for [functional reactive programming](https://en.wikipedia.org/wiki/Functional_reactive_programming).
* [Circe](https://circe.github.io/circe/) for automatic serialisation/deserialisation to/from JSON.

### Directory Structure

* [views/](./src/main/scala/boards/views) for top-level views each corresponding to an entire page of the website.
* [components/](./src/main/scala/boards/views) for reusable web components created using Laminar.
* [util/](./src/main/scala/boards/util) for assorted helper functions.
* [imports/](./src/main/scala/boards/imports) for collections of standard import groups to avoid repeating them in every file.

Perhaps the most interesting view is that for the [game](./src/main/scala/boards/views/GameView.scala) itself, along with its constituent [components](./src/main/scala/boards/components/game).

### Architecture

This subproject is compiled exclusively to Javascript using [Scala.js](https://www.scala-js.org/), and is served to the client's browser. A single .js file is produced for the entire application. All pages use the same [template](../serversrc/main/twirl/views/PageTemplate.scala.html), which does nothing but delegate to the appropriate client-side view. All rendering is done on the client, the server only provides code and data.