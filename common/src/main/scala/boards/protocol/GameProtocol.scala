package boards.protocol

import boards.algebra.Game
import boards.Games

object GameProtocol:
  
  enum GameRequest:
    case TakeAction(actionHash: Int)
    case InviteToRoom(userId: Int)
    case RemovePlayer(userId: Int)
    case ReorderPlayer(userId: Int)
    case PromotePlayer(userId: Int)
    case ChangeGame(gameId: Int)
    case JoinRoom, StartGame, CancelRoom
  
  case class CreateRoomRequest(gameId: String)
  case class CreateRoomResponse(roomId: String)
  
  sealed trait Participant:
    
    def isActivePlayer(activePlayer: Int): Boolean = this match
      case Player(_, _, position, _, _) => position == activePlayer
      case _ => false
      
    def userIdOption: Option[Int] = this match
      case Player(userId, _, _, _, _) => Some(userId)
      case Spectator(userId, _) => Some(userId)
      case Unregistered => None
      
    def isPlayer: Boolean = this match
      case _: Player => true
      case _ => false
      
    def isRegistered: Boolean = this match
      case Unregistered => false
      case _ => true
  
  case class Player (
    userId: Int,
    roomId: String,
    position: Int,
    isOwner: Boolean,
    username: String,
  ) extends Participant
  
  case class Spectator (
    userId: Int,
    username: String,
  ) extends Participant
  
  case object Unregistered extends Participant
  
  case class Room (
    id: String,
    gameId: String,
    status: Status,
  ):
    lazy val game: Game = Games.byName.getOrElse(gameId, Game.none)
    
  object Room:
    def empty: Room = Room("00000", "", Status.Pending)
  
  enum Status(val isPending: Boolean, val isActive: Boolean, val isComplete: Boolean):
    case Pending extends Status(true, false, false)
    case Active extends Status(false, true, false)
    case Complete extends Status(false, false, true)