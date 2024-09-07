package boards.protocol

object GameProtocol:
  
  enum GameRequest:
    case TakeAction(actionHash: Int)
    case InviteToRoom(userId: Int)
    case RemovePlayer(userId: Int)
    case ReorderPlayer(userId: Int, position: Int)
    case PromotePlayer(userId: Int)
    case ChangeGame(gameId: Int)
    case JoinRoom, StartGame, CancelRoom
  
  case class CreateRoomRequest(gameId: Int)
  case class CreateRoomResponse(roomId: String)
  
  case class Player (
    userId: Int,
    roomId: String,
    position: Int,
    isOwner: Boolean,
    username: String,
  )