# boards-common

The common subproject contains code which is common to both the client and server subprojects. This primarily consists of data formats for communication therebetween.

### Libraries

* [Circe](https://circe.github.io/circe/) for automatic serialisation/deserialisation to/from JSON.

### Directory Structure

* [protocol/](./src/main/scala/boards/imports) data formats for messages sent between client and server.
* [graphics/](./src/main/scala/boards/imports) data formats for things the client should render.
* [util/](./src/main/scala/boards/util) for assorted helper functions, and custom JSON serialisation.
* [imports/](./src/main/scala/boards/imports) for collections of standard import groups to avoid repeating them in every file.

### Architecture

This code is available to both the client and server at compile-time. It is compiled into both subprojects separately. In particular, this means that no JVM or JS specific functionality may be used.

Also note that while the game and dsl code is _also_ available to both client and server, these are not part of the common subproject, and instead occupy their own subprojects.
