package boards.imports

object games:
  
  export boards.algebra.{
    Action, InstantaneousState, Game, GameState, Generator, Piece, PieceSet, Rule
  }
  export boards.algebra.Action.*
  
  export boards.algebra.Game.{WithMetadata => _, *}
  export boards.algebra.GameState.{
    InitialState, InterimState, FinalState, NonInitialState, NonFinalState,
    Outcome, /*~>, |>,*/ Following
  }
  export boards.algebra.GameState.Outcome.{Winner, Draw}
  export boards.algebra.Generator.given
  export boards.algebra.Piece.PieceType
  
  export boards.graphics.{Colour, Pattern, Texture, Shape}
  
  export boards.util.extensions.FunctionOps.*
  export boards.util.extensions.Conversions.given
  
  export boards.imports.math.{*, given}