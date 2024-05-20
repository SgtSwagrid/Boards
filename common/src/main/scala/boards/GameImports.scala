package boards

object GameImports:
  
  export boards.algebra.{
    Action, BoardState, Game, GameState, Generator, Piece, PieceSet, Rule
  }
  export boards.algebra.Action.*
  export boards.algebra.BoardState.given
  export boards.algebra.Game.*
  export boards.algebra.GameState.{
    InitialState, InterimState, FinalState, NonInitialState, NonFinalState, Outcome, ~>, |>, given
  }
  export boards.algebra.GameState.Outcome.{Winner, Draw}
  export boards.algebra.Generator.given
  export boards.algebra.Piece.{PieceType, given}
  export boards.algebra.PieceSet.given
  export boards.algebra.Rule.given
  
  export boards.graphics.{Colour, Pattern}
  
  export util.math.{Metric, Pos, Vec}
  export util.math.kernel.{Align, Dir, Kernel, Ray}
  export util.math.Pos.{*, given}
  export util.math.Vec.given
  export util.math.kernel.Align.given
  export util.math.kernel.Dir.given
  export util.math.kernel.Kernel.given
  
  export util.extensions.FunctionOps.*
  export util.extensions.Conversions.given