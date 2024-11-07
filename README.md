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

## Information for Developers

### Project Structure

Boards is composed of 6 subprojects, which you will find in the top-level directories of the same names:
* `dsl` contains the implementation of the _BoardLang_ DSL.
* `games` contains the rule specifications for the games themselves, written using `BoardLang`.
* `bots` contains general and game-specific strategies for playing these games.
* `server` code which is compiled to JVM bytecode for use on a web server. Handles user requests, authentication and persistence.
* `client` code which is transpiled to JS and served to the user's web browser. Handles rendering and user input.
* `common` code which is shared by both the `server` and `client`, and compiled into both projects. Primarily contains data formats for communication therebetween.

### Requirements

Boards is written entirely in [Scala](https://www.scala-lang.org/), and in order to work on this project you will need the following:
* An up-to-date [JDK](https://www.oracle.com/java/technologies/downloads/) for development in a [JVM](https://en.wikipedia.org/wiki/Java_virtual_machine)-based language.
* [Scala 3.5.2](https://www.scala-lang.org/) itself.
* [sbt](https://www.scala-sbt.org/), the pre-eminent build tool for Scala projects.
* [Git](https://git-scm.com/) for version control.

### Libraries

Boards relies on the following open-source libraries, this installation of which should be handled automatically by sbt:

* The [Play Framework](https://www.playframework.com/) for handling web requests on the server.
* [Slick](https://scala-slick.org/) for database access by [functional relational mapping](https://pekko.apache.org/docs/pekko/current/general/actors.html).
* [H2](https://www.h2database.com/html/main.html) for running an embedded database.
* [Apache Pekko](https://pekko.apache.org/), an [actor](https://pekko.apache.org/docs/pekko/current/general/actors.html) framework, used here for managing server-side state in relation to web socket sessions.
* [Circe](https://circe.github.io/circe/) for automatic serialisation/deserialisation to/from JSON.
* [Laminar](https://laminar.dev/) for client-side rendering.
* [Airstream](https://github.com/raquo/Airstream), which is required by Laminar, is a library for [functional reactive programming](https://en.wikipedia.org/wiki/Functional_reactive_programming).

### Local Execution

To download the project into a subdirectory named `Boards`, run:
```
git clone https://github.com/SgtSwagrid/Boards.git
```

To run a local development server, navigate to the `Boards` directory and run:
```
sbt "project server" "~run"
```
You should then be able to access the website at `localhost:9000` in your browser.

### Development

For development purposes, it is recommended that you use [IntelliJ IDEA](https://www.jetbrains.com/idea/) with the [Scala plugin](https://plugins.jetbrains.com/plugin/1347-scala). IntelliJ configuration files are deliberately included in the project to offer a uniform developer experience with consistent formatting rules, code highlighting and build configurations. If you _are_ using IntelliJ, the `Boards Development Server` run option is equivalent to the command shown above.

In any case, the project is configured to automatically detect code changes while the server is running, so that changes are reflected immediately. Note however that this unfortunately isn't foolproof and if something isn't working, a full server restart is the safest option.

## Architecture of the _BoardLang_ DSL

A key commponent of Boards is _BoardLang_, an [embedded domain-specific language](https://en.wikipedia.org/wiki/Domain-specific_language) (eDSL) for creating turn-based board games in Scala.
* You will find the implementation of _BoardLang_ in `dsl/src/main/scala/boards`.
* You will find examples of games created using _BoardLang_ in `games/src/main/scala/boards`.

### Philosophy

_BoardLang_ uses a [functional](https://en.wikipedia.org/wiki/Functional_programming) style and all objects are [immutable](https://en.wikipedia.org/wiki/Immutable_object). Fundamentally, a game is built by defining some number of `PieceTypes`s and composing `Rule`s to precisely specify what the player can and can't do with these pieces. Each `Action` the player takes causes a transition to a new `GameState` in accordance with the current `Rule`.

### Important Abstractions

* `Game`: A precise specification of the rules for a game (e.g. Chess, Connect Four, etc).

For the state of the game:
* `InstantaneousState`: The current "physical" state of the game. Contains the `Board` and current `PieceSet`, and tracks the `activePlayerId`.
* `GameState`: The total state of the game. Contains the entire game history, including all past `InstantaneousState`s and user `Action`s.
* `Rule`: A mechanism for, given some current `GameState`, enumerating possible player `Action`s and corresponding successor `GameState`s, as well as performing `Action` legality testing.

For the game board and pieces:
* `Board`: The topology of the game, describing which positions are in bounds and the connectivity therebetween.
* `Piece`: A specific piece that is on the board at a specific time in a specific place.
* `PieceType`: A property held by a `Piece` used to distinguish different kinds of pieces (e.g. rook, knight, etc).
* `PieceSet`: A set of `Pieces`, used in particular by an `InstantaneousState`, with specialised functionality for filtering, `Action` querying and modification.

Mathematical types:
* `VecI`: An integer vector representing a position on the `Board`.
* `Ker`: A set of vectors describing a region in space.
* `Dir`: A direction or set thereof, used for describing _relative_ positions.
* `Ray`: A specific kind of `Ker` formed by making up to some number of steps in some `Dir`.

### Rule Semantics

There are two important things to note about `Rule`s in _BoardLang_:

1. A `Rule` is **not** [Markovian](https://en.wikipedia.org/wiki/Markov_property) in the current `InstantaneousState`, which is to say that the state transitions can depend arbitarily on the full state _history_ in `GameState`. To see why, consider chess: [en passant](https://www.chess.com/terms/en-passant) is legal _only_ on the turn directly following the initial double pawn move. The account for this, the `Rule` must be able to see when and how the pawn being captured arrived in its current position.

2. The `Rule` itself is a property of the `GameState`, not of the entire `Game`. In particular, this means that the `Rule` is _dynamic_ rather than _static_, meaning it can (and typically does) change over time. The `Game` specifies the _initial_ `Rule`, and thereafter each successor `GameState` is infused with a _new_ `Rule` upon creation. When a `Rule` generates successor `GameState`s, it is also responsible for determining which `Rule` should apply thereafter from that state. The reason is that this makes it much easier to reason about _sequences_ of `Action`s, and implicitly provides support for two kinds of situation which arise very frequently: **turn phases** and **game phases**.

#### Turn Phases

In many games, the turn is divided up into multiple phases. For instance, maybe you first have to roll, then trade, and finally build. To implement this behaviour, one can simply create a separate `Rule` for each phase, and sequence them together, whereby each phase knows that when it is done, it should replace the `Rule` with the one corresponding to the next phase.

For an example, consider chess again: after a pawn moves to the final rank, as a separate action it must then [promote](https://www.chess.com/terms/pawn-promotion). With a _static_ `Rule`, the state would need some kind of a global flag indicating the need for promotion, to override the default behaviour on the next action. This moves the promotion logic outside of the pawn `PieceType` where it belongs. Instead, with a _dynamic_ `Rule`, the pawn can simply infuse the successor `GameState` with a special, one-time promotion rule.

#### Game Phases

In some games, there are even multiple global game phases. For instance, it is common to have a separate _setup_ phase, which still requires input from the players, but with completely different rules than the main phase (example: [Catan](https://www.catan.com/)). Again, with a _dynamic_ `Rule`, this is easy to achieve without any global flags by creating a separate `Rule` for each phase and sequencing them in the same way as before.

### Types of Rule

Any `Rule` is actually a [tree](https://en.wikipedia.org/wiki/Tree_(abstract_data_type)) of `Rule`s, of the following basic kinds:
* `Generator`: The leaves of the `Rule` tree. A `Generator` simply enumerates legal `Action`s and the direct consequences thereof. For example, a king in chess might provide a `Generator` which produces one `Move` action for each [octagonal](https://www.researchgate.net/figure/A-2-d-octagon-and-its-four-Octagonal-directions-Observe-that-octagonal-directions-within_fig1_332078957) direction.
* `Effect`: A passive effect which is only indirectly caused by the `Action` of the `Player`. For instance, when we [castle](https://www.chess.com/terms/castling-chess) in chess, a `Generator` allows the king to move, but then a susequent `Effect` ensures that the rook moves too as a result.
* `Combinator`: A composition of multiple simpler `Rule`s, for reasoning about `Action` sequencing. For example, in chess, we need to take the _union_ of `Rule`s from individual `Piece`s to indicate that the `Player` can choose which `Piece` to move, then _sequence_ this with `Effect.endTurn`, then _repeat_ indefinitely.

## Using the _BoardLang_ DSL

To use the _BoardLang_ DSL, the following import is always required:
```scala
import boards.imports.games.{*, given}
```

Furthermore, any game must inherit from the base class `Game`:
```scala
class Chess extends Game
```

We can't have chess without a chessboard!
```scala
// A chessboard is an 8x8 grid.
override val Board = Kernel.box(8, 8)
  // The following is only required for aesthetic reasons:
  .paint(Pattern.Checkered(Colour.Chess.Dark, Colour.Chess.Light))
```

We may want to define some types of pieces, otherwise there won't be much to do in our game:
```scala
object Pawn extends PieceType.WithTexture(Texture.WhitePawn, Texture.BlackPawn)
```

The abstract class `Game` only requires us to implement a single member,  `rule`:
```scala
def rule: Rule = ???
```
This defines the _initial_ `Rule` for our `Game`. In other words, from the start of the game, what happens and what are the players allowed to do?

Often, the first thing that should happen is some setup. For example, if we're making chess, we might want to start with this `Rule` to insert a row of white pawns:
```scala
Effect.insert(/* The owner of the pieces */ PlayerId(0))(Pawn -> Board.row(1))
```


### Important Operators

_BoardLang_ provides a number of important operators for combining and modifying `Rule`s. The most important ones are:
* `|`: An infix _union_ operator for specifying that the `Player` may choose which of two `Rule`s to follow. Also available in function notation as `Rule.union` for use with any number of alternative `Rule`s.
* `|>`: An infix _sequence_ operator for specifying that the `Player` must execute both `Rule`s in the given order. Also available in function notation as `Rule.sequence` for user with any number of chained `Rule`s.
* `_.optional`: For specifying that the `Player` can decide whether or not to execute this `Rule`.
* `_.repeatXXX`: There are various methods of this kind for performing a `Rule` multiple times. See `Rule.scala` for all variants.
