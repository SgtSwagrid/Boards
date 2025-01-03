package boards.imports

object games:
  
  export boards.dsl.meta.Game
  export Game.{
    Board,
    GameConfig,
    Property,
  }
  
  export boards.dsl.meta.TurnId
  export boards.dsl.meta.TurnId.{TurnId, previous, next, toInt, given}
  
  export boards.dsl.meta.PlayerRef.{PlayerId, PlayerRef, Player, previous, next, given}
  
  export boards.dsl.rules.{
    Rule,
    Cause,
    Effect,
    Control,
    Input,
  }
  
  export boards.dsl.states.{
    InstantaneousState,
    HistoryState,
    GameState,
  }
  
  export HistoryState.{
    InitialState,
    SuccessorState,
  }
  
  export GameState.{
    ActiveState,
    FinalState,
    Outcome,
  }
  
  export GameState.Outcome.{
    Winner,
    Draw,
  }
  
  export boards.dsl.pieces.{
    PieceFilter,
    PieceSet,
    PieceView,
    PieceState,
    Piece,
    PieceType,
    PieceRef,
    PieceUpdate,
  }
  
  export boards.dsl.pieces.PieceRef.PieceId
  export boards.dsl.pieces.PieceView.Pieces
  export boards.dsl.pieces.PieceState.Version
  
  export boards.dsl.pieces.PieceType.{
    TexturedPiece,
    DynamicPiece,
    MoveablePiece,
    StaticPiece,
  }
  
  export boards.graphics.{
    Colour,
    Pattern,
    Texture,
    Shape,
  }
  
  export boards.util.extensions.FunctionOps.*