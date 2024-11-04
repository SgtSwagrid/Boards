package boards.imports

object games:
  
  export boards.algebra.{Action, Game}
  //export boards.algebra.rules.{Rule, Generator, Transformer}
  //export boards.algebra.rules.Rule.given
  //export boards.algebra.rules.Transformer.given
  
  export boards.algebra.rules.{Generator, Effect, Rule, Capability, Combinator}
  
  export boards.algebra.state.{
    InstantaneousState, GameState, Piece, PieceSet
  }
  
  export boards.algebra.state.Piece.PieceType
  
  export boards.algebra.Action.{Place, Move, Destroy, Skip}
  
  export boards.algebra.Game.{PlayerId, *, given}
  export GameState.{
    InitialState, InterimState, FinalState, NonInitialState, NonFinalState,
    Outcome, Winner, Draw, /*~>, |>,*/ Following
  }
  
  export Generator.given
  
  export boards.algebra.state.InstantaneousState.given
  
  export boards.graphics.{Colour, Pattern, Texture, Shape}
  
  export boards.util.extensions.FunctionOps.*
  export boards.util.extensions.Conversions.given