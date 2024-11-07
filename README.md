# Boards 4.0

This is an ongoing project featuring:
1. _BoardLang_, a [DSL](https://en.wikipedia.org/wiki/Domain-specific_language) for easily creating [combinatorial](https://brilliant.org/wiki/combinatorial-games-definition/) games in [Scala](https://www.scala-lang.org/).
2. A collection of simple games implemented using _BoardLang_. (WIP)
3. [Minimax](https://en.wikipedia.org/wiki/Minimax) and [RL](https://en.wikipedia.org/wiki/Reinforcement_learning) based solvers to play (2), implemented in a game-independent manner. (COMING SOON)
4. A web interface for playing (2) against (3) or other human players. (WIP)

Featured games (this list will continually expand):
1. [Chess](https://en.wikipedia.org/wiki/Chess)
2. [Chaturanga](https://en.wikipedia.org/wiki/Chaturanga)

This is a continuation from a long lineage of similar projects ([1.0](https://github.com/SgtSwagrid/boards-1.0), [2.0](https://github.com/SgtSwagrid/boards-2.0), [3.0](https://github.com/SgtSwagrid/boards-3.0)).

### Information for Developers

#### Project Structure

Boards is composed of 4 subprojects, which you will find in the top-level directories of the same names:
* `server` code which is compiled to JVM bytecode for use on a web server. Handles user requests, authentication and persistence.
* `client` code which is transpiled to JS and served to the user's web browser. Handles rendering and user input.
* `common` code which is shared by both the `server` and `client`, and compiled into both projects. Primarily contains data formats for communication therebetween.
* `games` contains both the implementation of _BoardLang_ as well as the games written therein.

#### Libraries

Boards relies on the following open-source libraries:

* The [Play Framework](https://www.playframework.com/) for handling web requests on the server.
* [Slick](https://scala-slick.org/) for database access by [functional relational mapping](https://scala-slick.org/talks/2014-06-09_33rd_Degree/Functional%20Relational%20Mapping%20with%20Slick.pdf).
* [Apache Pekko](https://pekko.apache.org/), an [actor](https://www.oreilly.com/library/view/applied-akka-patterns/9781491934876/ch01.html) framework, used here for managing server-side state in relation to web socket sessions.
* [Circe](https://github.com/circe/circe) for automatic serialisation/deserialisation to/from JSON.
* [Laminar](https://laminar.dev/) for client-side rendering.
* [Airstream](https://github.com/raquo/Airstream), which is required by Laminar, is a library for [functional reactive programming](https://en.wikipedia.org/wiki/Functional_reactive_programming).
