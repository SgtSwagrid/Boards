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

#### Requirements

Boards is written entirely in [Scala](https://www.scala-lang.org/), and in order to work on this project you will need the following:
* An up-to-date [JDK](https://www.oracle.com/java/technologies/downloads/) for development in a JVM-based language.
* [Scala 3.5.2](https://www.scala-lang.org/) itself.
* [sbt](https://www.scala-sbt.org/), the pre-eminent build tool for Scala projects.
* [Git](https://git-scm.com/) for version control.

#### Libraries

Boards relies on the following open-source libraries, this installation of which should be handled automatically by sbt:

* The [Play Framework](https://www.playframework.com/) for handling web requests on the server.
* [Slick](https://scala-slick.org/) for database access by [functional relational mapping](https://scala-slick.org/talks/2014-06-09_33rd_Degree/Functional%20Relational%20Mapping%20with%20Slick.pdf).
* [H2](https://www.h2database.com/html/main.html) for running an embedded database.
* [Apache Pekko](https://pekko.apache.org/), an [actor](https://www.oreilly.com/library/view/applied-akka-patterns/9781491934876/ch01.html) framework, used here for managing server-side state in relation to web socket sessions.
* [Circe](https://github.com/circe/circe) for automatic serialisation/deserialisation to/from JSON.
* [Laminar](https://laminar.dev/) for client-side rendering.
* [Airstream](https://github.com/raquo/Airstream), which is required by Laminar, is a library for [functional reactive programming](https://en.wikipedia.org/wiki/Functional_reactive_programming).

#### Local Execution

To download the project into a subdirectory named `Boards`, run:
```
git clone https://github.com/SgtSwagrid/Boards.git
```

To run a local development server, navigate to the `Boards` directory and run:
```
sbt "project server" "~run"
```
You should then be able to access the website at `localhost:9000` in your browser.

#### Development

For development purposes, it is recommended that you use [IntelliJ IDEA](https://www.jetbrains.com/idea/) with the [Scala plugin](https://plugins.jetbrains.com/plugin/1347-scala). IntelliJ configuration files are deliberately included in the project to offer a uniform developer experience with consistent formatting rules, code highlighting and build configurations. If you _are_ using IntelliJ, the `Boards Development Server` run option is equivalent to the command shown above.

In any case, the project is configured to automatically detect code changes while the server is running, so that changes are reflected immediately. Note however that this unfortunately isn't foolproof and if something isn't working, a full server restart is the safest option.

#### Games

In the `games` subproject, the implementation of the `BoardLang` DSL can be found in `games/src/main/scala/boards/algebra`, which in turn makes use of mathematical structures from `games/src/main/scala/boards/math`. The games themselves can be found in `games/src/main/scala/boards/games`, which is where you should start if you're looking for an example. Please put any new games here too. Currently, no games will be loaded unless they are also referenced in `games/src/main/scala/boards/Games.scala`. In the future, it should be possible to load games dynamically at runtime from user submissions, but this is not yet supported.

#### _BoardLang_ Architecture

_BoardLang_ uses a functional style and all states are immutable. The current "physical" state of the game is represented by an `InstantaneousState` instance. From a state, a `Rule` instance creates a set of transitions to possible future states, with each transition triggered by some specific user input. However, the transitions are not Markovian in the `InstantaneousState`, which is to say the transition possibilities can depend arbitrarily on the state _history_. The full state, which includes the entire history, is represented by a `GameState`, which encloses a chain of `InstantenousState` instances.
