package boards.dsl.rules

import boards.imports.games.{*, given}
import boards.imports.math.{*, given}
import boards.dsl.Shortcuts.{State, Piece}
import boards.math.region.Region.HasRegionI
import Control.*

import scala.annotation.targetName
import scala.collection.mutable

/** A [[Control]] is a shorthand for the composition of a [[Cause]] with an immediately following [[Effect]].
  *
  * Almost always, a [[Cause]] should have a direct and immediate corresponding [[Effect]].
  * For instance, when a [[Piece]] is dragged to some other position, the immediate [[Effect]] should
  * be that the [[Piece]] is actually moved to the new position.
  * It would be tedious to have to explicitly sequence these two steps every time they are needed.
  * Hence, the [[Control]].
  *
  * Since the link between [[Cause]] and [[Effect]] isn't universally so obvious or 1-to-1,
  * it remains possible to use [[Cause]]s and [[Effect]]s individually too.
  * However, the [[Control]] should be favoured where it is applicable, for sake of brevity.
  */
object Control:
  
  def place (
    owner: HistoryState ?=> PlayerRef,
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
  ) (using owner: PlayerRef): Rule =
    place(owner, region, pieceTypes*)
    
  def fill (
    owner: HistoryState ?=> PlayerRef,
    region: HistoryState ?=> HasRegionI,
    pieceTypes: PieceType*,
  ): Rule =
    Cause.clickRegion(region) |> Effect.create(owner, region, pieceTypes*)
    
  def fillMine (
    region: HistoryState ?=> HasRegionI,
    pieceTypes: PieceType*,
  ) (using owner: PlayerRef): Rule =
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
      Piece.now.map: piece =>
        Control.move(summon[Piece], region)