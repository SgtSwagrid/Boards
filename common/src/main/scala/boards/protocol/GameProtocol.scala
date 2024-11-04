package boards.protocol

import boards.Games
import boards.imports.games.{*, given}
import boards.protocol.UserProtocol.User

object GameProtocol:
  
  enum GameRequest:
    case TakeAction(actionHash: String)
    case InviteToRoom(userId: Int)
    case RemovePlayers(playerId: PlayerId*)
    case ReorderPlayer(playerId: PlayerId)
    case PromotePlayer(userId: Int)
    case ChangeGame(gameId: Int)
    case JoinRoom, StartGame, CancelRoom
  
  case class CreateRoomRequest(gameId: String)
  case class CreateRoomResponse(roomId: String)
  
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
  
  case class Player (
    userId: Int,
    roomId: String,
    position: PlayerId,
    isOwner: Boolean,
    username: String,
  )
  
  sealed trait Participant:
    
    def isPlayer(playerId: PlayerId): Boolean = this match
      case participant: RegisteredParticipant => participant.positions.contains(playerId)
      case UnregisteredParticipant => false
    
    def isPlayerExclusively(playerId: PlayerId): Boolean =
      isPlayer(playerId) && isPlayingExclusively
      
    def isPlaying: Boolean = this match
      case participant: RegisteredParticipant => participant.positions.nonEmpty
      case UnregisteredParticipant => false
      
    def isPlayingExclusively: Boolean = this match
      case participant: RegisteredParticipant => participant.positions.sizeIs == 1
      case UnregisteredParticipant => false
      
    def userIdOption: Option[Int] = this match
      case participant: RegisteredParticipant => Some(participant.userId)
      case UnregisteredParticipant => None
  
  case class RegisteredParticipant (
    roomId: String,
    userId: Int,
    username: String,
    positions: Seq[PlayerId],
  ) extends Participant
  
  case object UnregisteredParticipant extends Participant