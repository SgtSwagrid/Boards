package boards.dsl.rules

import boards.dsl.pieces.Piece
import boards.imports.math.{*, given}
import boards.imports.games.{*, given}
import boards.dsl.Shortcuts.{*, given}
import boards.math.region.Region.HasRegionI
import boards.math.region.Vec.HasVecI
import boards.dsl.Shortcuts.State

object Control:
  
  def place (
    owner: HistoryState ?=> PlayerId,
    region: HistoryState ?=> HasRegionI,
    pieceTypes: PieceType*,
  ): Rule = Rule.union:
    pieceTypes.map: pieceType =>
      Cause.click(region) |> Effect.create (
        owner,
        State.latestInput.get.asInstanceOf[Input.Click].region,
        pieceType,
      )
      
  def placeMine (
    region: HistoryState ?=> HasRegionI,
    pieceTypes: PieceType*,
  ) (using owner: PlayerId): Rule =
    place(owner, region, pieceTypes*)
    
  def fill (
    owner: HistoryState ?=> PlayerId,
    region: HistoryState ?=> HasRegionI,
    pieceTypes: PieceType*,
  ): Rule =
    Cause.clickRegion(region) |> Effect.create(owner, region, pieceTypes*)
    
  def fillMine (
    region: HistoryState ?=> HasRegionI,
    pieceTypes: PieceType*,
  ) (using owner: PlayerId): Rule =
    fill(owner, region, pieceTypes*)
    
  def move (
    pieces: HistoryState ?=> PieceFilter,
    region: (HistoryState, Piece) ?=> HasRegionI,
  ): Rule =
    Cause.dragPiece(pieces, region) |> Effect.slide (
      State.latestInput.get.asInstanceOf[Input.Drag].from,
      State.latestInput.get.asInstanceOf[Input.Drag].to.asVec.get,
    )
    
  def moveThis (
    region: (HistoryState, Piece) ?=> HasRegionI,
  ) (using piece: PieceRef): Rule = Rule.union:
    piece.now.map: piece =>
      move(piece, region)
    
  def capture (
    pieces: HistoryState ?=> PieceFilter,
  ): Rule =
    Cause.clickPiece(pieces) |> Effect.clear(State.latestInput.get.asInstanceOf[Input.Click].region)
    
  def promote (
    pieces: HistoryState ?=> PieceFilter,
    pieceTypes: PieceType*,
  ): Rule = Rule.union:
    for
      piece <- pieces.now.pieces
      pieceType <- pieceTypes
      if !(piece is PieceType)
    yield Cause.clickPiece(piece) |> Effect.create(piece.owner, piece.position, pieceType)
    
  object PieceControl:
    
    def move (
      region: (HistoryState, Piece) ?=> HasRegionI
    ): Piece ?=> Rule = Rule.union:
      piece.now.map: piece =>
        Control.move(summon[Piece], region)