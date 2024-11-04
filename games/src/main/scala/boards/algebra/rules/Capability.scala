package boards.algebra.rules

import boards.imports.games.{*, given}
import boards.imports.math.{*, given}
import boards.algebra.rules.Generator
import boards.algebra.rules.Generator.{PlaceGenerator, MoveGenerator, DestroyGenerator}
import boards.algebra.rules.Rule.{Query, SkipRule}
import boards.algebra.state.GameState
import boards.algebra.state.GameState.NonInitialState
import boards.algebra.Game.PlayerId
import boards.algebra.state.Piece.PieceType

/**
 * A helper for determining, with a given ruleset and from a given state,
 * which kinds of action are possible.
 * 
 * Note that the rule instance currently attached to the state is ignored.
 * Here, we ask a hypothetical question; if the rule were instead this, what would be possible?
 * 
 * @param rule the hypothetical ruleset to consider
 * @param state the current game state from which to query
 */
class Capability (rule: Rule, state: GameState):
  
  def next: Iterator[GameState] = next(g => g)
  def next(query: Query): Iterator[GameState] = state match
    case state: NonFinalState => rule
      .next(state, query)
      .distinctBy(_.actionOption)
      .map(_.flattenFutureSkips)
    case _ => Iterator.empty
  
  def actions: Iterator[Action] = actions(g => g)
  def actions(query: Query): Iterator[Action] = state match
    case state: NonFinalState => rule
      .actions(state, query)
      .distinct
    case _ => Iterator.empty
  
  def can(query: Query): Boolean = state match
    case state: NonFinalState => rule.actions(state, query).exists(_ != Action.skip)
    case _ => false
  
  def canAct: Boolean = can(g => g)
  
  def canPlace: Boolean = can:
    case g: PlaceGenerator => g
    
  def canPlaceAt(region: Kernel[?]): Boolean = can:
    case PlaceGenerator(o, p, r) => PlaceGenerator(o, p, r & region)
    
  def canPlaceType(pieces: PieceType*): Boolean = can:
    case PlaceGenerator(o, p, r) => PlaceGenerator(o, p.filter(pieces.contains), r)
    
  def canMove: Boolean = can:
    case g: MoveGenerator => g
    
  def canMoveFrom(region: Kernel[?]): Boolean = can:
    case MoveGenerator(f, t) => MoveGenerator(f & region, t)
    
  def canMoveType(pieces: PieceType*): Boolean =
    canMoveFrom(state.now.pieces.ofType(pieces*).positions)
    
  def canMoveTo(region: Kernel[?]): Boolean = can:
    case MoveGenerator(f, t) => MoveGenerator(f, t & region)
    
  def canCaptureType(pieces: PieceType*): Boolean =
    canMoveTo(state.now.pieces.ofType(pieces*).positions)
    
  def canCapturePlayer(players: PlayerId*): Boolean =
    canMoveTo(state.now.pieces.ofPlayer(players*).positions)
    
  def canMoveBetween(from: Kernel[?], to: Kernel[?]): Boolean = can:
    case MoveGenerator(f, t) => MoveGenerator(f & from, t & to)
    
  def canDestroy: Boolean = can:
    case g: DestroyGenerator => g
    
  def canDestroyAt(region: Kernel[?]): Boolean = can:
    case DestroyGenerator(r) => DestroyGenerator(r & region)
    
  def canDestroyType(pieces: PieceType*): Boolean =
    canDestroyAt(state.now.pieces.ofType(pieces*).positions)
    
  def canDestroyPlayer(players: PlayerId*): Boolean =
    canDestroyAt(state.now.pieces.ofPlayer(players*).positions)