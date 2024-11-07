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

Boards is composed of 6 subprojects, which you will find in the top-level directories of the same names:
* `dsl` contains the implementation of the _BoardLang_ DSL.
* `games` contains the rule specifications for the games themselves, written using `BoardLang`.
* `bots` contains general and game-specific strategies for playing these games.
* `server` code which is compiled to JVM bytecode for use on a web server. Handles user requests, authentication and persistence.
* `client` code which is transpiled to JS and served to the user's web browser. Handles rendering and user input.
* `common` code which is shared by both the `server` and `client`, and compiled into both projects. Primarily contains data formats for communication therebetween.

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

### The _BoardLang_ DSL

A key commponent of Boards is _BoardLang_, an [embedded domain specific language](https://en.wikipedia.org/wiki/Domain-specific_language) (eDSL) for creating turn-based board games in Scala.
* You will find the implementation of _BoardLang_ in `dsl/src/main/scala/boards`.
* You will find examples of _BoardLang_ in use in `games/src/main/scala/boards`.

#### Philosophy

_BoardLang_ uses a [functional](https://en.wikipedia.org/wiki/Functional_programming) style and all objects are [immutable](https://en.wikipedia.org/wiki/Immutable_object). Fundamentally, a game is built by defining some number of `PieceTypes`s and composing `Rule`s to precisely specify what the player can and can't do with these pieces. Each `Action` the player takes causes a transition to a new `GameState` in accordance with the current `Rule`.

#### Important Types

* `Game`: A precise specification of the rules for a game (e.g. Chess, Connect Four, etc).

For the state of the game:
* `InstantaneousState`: The current "physical" state of the game. Contains the `Board` and current `PieceSet`, and tracks the `activePlayerId`.
* `GameState`: The total state of the game. Contains the entire game history, including all past `InstantaneousState`s and user `Action`s.
* `Rule`: A mechanism for, given some current `GameState`, enumerating possible player `Action`s and corresponding successor `GameState`s, as well as performing `Action` legality testing.

For the game board and pieces:
* `Board`: The topology of the game, describing which positions are in bounds and the connectivity therebetween.
* `Piece`: A specific piece that is on the board at a specific time.
* `PieceType`: A property held by a `Piece` used to distinguish different kinds of pieces (e.g. Rook, Knight, etc).
* `PieceSet`: A set of `Pieces`, used in particular by an `InstantaneousState`, with specialised functionality for filtering, `Action` querying and modification.

Mathematical types:
* `VecI`: An integer vector representing a position on the `Board`.
* `Ker`: A set of vectors describing a region in space.
* `Dir`: A direction or set thereof, used for describing _relative_ positions.
* `Ray`: A specific kind of `Ker` formed by making up to some number of steps in some `Dir`.
