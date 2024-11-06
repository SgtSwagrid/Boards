package boards.protocol

import boards.imports.games.{*, given}

/** Messages sent between the client and server regarding a game or room. */
object GameProtocol:
  
  /** A request sent by the user to the server by websocket while participating in a game room. */
  enum GameRequest:
    /**
     * Take an action, which is uniquely identifiable by its hash.
     * Will fail if the game is inactive or if it is not this user's turn.
     */
    case TakeAction(actionHash: String)
    /** Invite a user to join this room. */
    case InviteToRoom(userId: Int)
    /** Remove the players in the given positions from the game. */
    case RemovePlayers(positions: PlayerId*)
    /** Swap the order of two players in the given positions. */
    case SwapPlayers(left: PlayerId, right: PlayerId)
    /** Make this player the group leader. */
    case PromotePlayer(userId: Int)
    /** Change this room to a different game. */
    case ChangeGame(gameId: Int)
    /** Request to the join the room; automatically accepted if there is still space. */
    case JoinRoom
    /**
     * Start the game.
     * Will fail if the game has already started or if there are not enough players.
     * Once the game has started, no further changes to the settings or players are permitted.
     */
    case StartGame
    /** Remove all players and delete this room. */
    case CancelRoom
    /** Resign the game, marking the opponent as the winner. */
    case Resign(resign: Boolean, positions: PlayerId*)
    /** Offer a draw or accept an offer for a draw. */
    case OfferDraw(draw: Boolean, positions: PlayerId*)
  
  /** A request sent by the user to create a new room. */
  case class CreateRoomRequest(gameId: String)
  /** A response sent by the server upon receiving a CreateRoomRequest. */
  case class CreateRoomResponse(roomId: String)