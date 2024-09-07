package boards

object GameImports:
  
  export boards.algebra.{
    Action, InstantaneousState, Game, GameState, Generator, Piece, PieceSet, Rule
  }
  export boards.algebra.Action.*
  export boards.algebra.InstantaneousState.given
  export boards.algebra.Game.*
  export boards.algebra.GameState.{
    InitialState, InterimState, FinalState, NonInitialState, NonFinalState,
    Outcome, /*~>, |>,*/ Following, given
  }
  export boards.algebra.GameState.Outcome.{Winner, Draw}
  export boards.algebra.Generator.given
  export boards.algebra.Piece.{PieceType, given}
  export boards.algebra.PieceSet.given
  export boards.algebra.Rule.given
  
  export boards.graphics.{Colour, Pattern, Texture}
  
  export util.math.{Metric, Vec}
  export util.math.kernel.{Align, Dir, Kernel, Ray}
  export util.math.Vec.{VecI, VecF, given}
  export util.math.kernel.Align.given
  export util.math.kernel.Dir.given
  export util.math.kernel.Kernel.given
  
  export util.extensions.FunctionOps.*
  export util.extensions.Conversions.given