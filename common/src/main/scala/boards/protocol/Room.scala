package boards.protocol

import boards.imports.games.{*, given}
import boards.protocol.UserProtocol.User
import boards.util.extensions.IntOps.*
import Room.*
import boards.Catalogue
import boards.dsl.meta.Game

/**
 * A game room, representing a specific instance of a game being played.
 *
 * @param id The unique ID of this room.
 * @param gameId The ID of the game which is being played in this room.
 * @param status The status of this room (i.e. pending/active/complete).
 */
case class Room (
  id: String,
  gameId: String,
  status: Status,
):
  /** The game which is being played in this room. */
  lazy val game: Game = Catalogue.byName.getOrElse(gameId, Game.none)
  
  /** The permissible options for number of players according to the game. */
  lazy val requiredNumPlayers: Seq[Int] = game.numPlayers
  /** The minimum number of players according to the game. */
  lazy val minPlayers: Int = requiredNumPlayers.min
  /** The maximum number of players according to the game. */
  lazy val maxPlayers: Int = requiredNumPlayers.max
  
  export status.{isPending, isActive, isComplete}
  
  /** Augment a room with direct knowledge of its participants. */
  def withPlayers(players: Seq[Player] = Seq.empty): RichRoom = RichRoom(this, players)
  
object Room:
  
  /** An empty room with no game or players. */
  def empty: RichRoom = Room("00000", "", Status.Pending).withPlayers()
  
  /**
   * A game room, representing a specific instance of a game being played.
   *
   * This class is a version of 'Room' which is augmented with some additional information.
   *
   * @param baseRoom The basic information about this room.
   * @param simplePlayers The basic information about the players in this room.
   */
  case class RichRoom (
    baseRoom: Room,
    simplePlayers: Seq[Player],
  ):
    
    /** The players who are participating in this room, in turn order. */
    lazy val players: Seq[RichPlayer] = simplePlayers.map: player =>
      RichPlayer (
        player = player,
        name = game.playerNames(player.position.toInt),
        colour = game.playerColours(player.position.toInt),
        isHotseat = simplePlayers.filter(_.userId == player.userId).sizeIs > 1,
        hotseatOrder = simplePlayers.filter(_.userId == player.userId).count(_.position.toInt < player.position.toInt),
      )
      
    def player(playerId: PlayerId): RichPlayer = players(playerId.toInt)
      
    /** The players who are participating in this room, grouped by the device they are playing on. */
    lazy val playersByUser: Map[Int, Seq[RichPlayer]] =
      players.groupBy(_.userId)
    /** Find all the players who are controlled by the current user. */
    def playersOf(userId: Int): Seq[RichPlayer] =
      playersByUser.get(userId).toSeq.flatten
    /** The users who are participating in this game. */
    lazy val userIds: Seq[Int] = players.map(_.userId).distinct
    /** Whether the given user is participating in this room. */
    def isParticipating(userId: Int): Boolean = userIds.contains(userId)
      
    /** The number of players currently participating in this room. */
    lazy val numPlayers: Int = simplePlayers.size
    /** The number of distinct users currently participating in this room. */
    lazy val numUsers: Int = playersByUser.keys.size
    
    /** Whether the game can currently be started. */
    lazy val canStart: Boolean = isPending && game.numPlayers.contains(numPlayers)
    /** Whether all players slots are already filled up. */
    lazy val isFull: Boolean = numPlayers >= maxPlayers
    
    /** Whether some device has multiple players. */
    lazy val isHotseat: Boolean = numUsers < numPlayers
    /** Whether all players are on the same device. */
    lazy val isExclusivelyHotseat: Boolean = numUsers == 1
    
    /** Whether players on multiple devices are involved. */
    lazy val isOnline: Boolean = numUsers > 1
    /** Whether no device has multiple players. */
    lazy val isExclusivelyOnline: Boolean = numUsers == numPlayers
    
    /** The single player who hasn't resigned, if all other players have. */
    lazy val winnerByResignation: Option[RichPlayer] =
      val nonResigned = players.filter(!_.hasResigned)
      if !isPending && nonResigned.sizeIs == 1 then
        nonResigned.headOption
      else None
      
    lazy val drawByAgreement: Boolean =
      !isPending && players.forall(_.hasOfferedDraw)
      
    lazy val agreedOutcome: Option[Outcome] =
      winnerByResignation.map(winner => Winner(winner.position))
        .orElse(Option.when(drawByAgreement)(Draw))
    
    /** Produces a version of this room with some given status. */
    def withStatus(status: Status): RichRoom =
      baseRoom.copy(status = status).withPlayers(simplePlayers)
    
    export baseRoom.*
  
  /**
   * The status of a room (i.e. pending/active/complete).
   *
   * @param isPending True if the game hasn't yet started.
   * @param isActive True if the game has started and hasn't yet finished.
   * @param isComplete True if the game is already finished.
   */
  enum Status (
    val isPending: Boolean,
    val isActive: Boolean,
    val isComplete: Boolean,
  ):
    /** The game hasn't yet started. */
    case Pending extends Status(true, false, false)
    /** The game has started and hasn't yet finished. */
    case Active extends Status(false, true, false)
    /** The game is already finished. */
    case Complete extends Status(false, false, true)
  
  /**
   * A human player who is participating in a game room.
   * A separate player instance exists for each (room, player position)-tuple.
   * In hotseat games, multiple players exist for the same user.
   *
   * @param userId The ID of the user who controls this player.
   * @param username The name of the user who controls this player.
   * @param roomId The ID of the room this player belongs to.
   * @param position The turn order of this player.
   * @param isOwner Whether this player is the owner of the game.
   * @param hasResigned Whether this player has decided to resign the game.
   * @param hasOfferedDraw Whether this player has decided to offer a draw.
   */
  case class Player (
    userId: Int,
    username: String,
    roomId: String,
    position: PlayerId,
    isOwner: Boolean,
    hasResigned: Boolean = false,
    hasOfferedDraw: Boolean = false,
  )
  
  /**
   * A human player who is participating in a game room.
   * A separate player instance exists for each (room, player position)-tuple.
   * In hotseat games, multiple players exist for the same user.
   *
   * This class is a version of 'Player' which is augmented with some additional information.
   *
   * @param player The basic information about this player.
   * @param name The name of this player position according to the game.
   * @param colour The colour of this player position according to the game.
   * @param isHotseat Whether the user who controls this player also controls other players.
   * @param hotseatOrder The turn order of this player, only among those players who are using the same device.
   */
  case class RichPlayer (
    player: Player,
    name: String,
    colour: Colour,
    isHotseat: Boolean,
    hotseatOrder: Int,
  ):
    
    /** The display name is the username, with a number added when the same user appears multiple times. */
    lazy val displayName: String = username + suffix
    /** A roman numeral to indicate hotseat order, only if this is a hotseat player. */
    lazy val suffix: String =
      if isHotseat then " " + (hotseatOrder + 1).toRomanNumeral else ""
      
    export player.*