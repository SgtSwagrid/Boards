package boards.protocol

import boards.protocol.UserProtocol.User
import boards.util.extensions.IntOps.*
import Room.*
import boards.GameCatalogue
import boards.dsl.meta.Game
import boards.dsl.meta.PlayerRef.PlayerId
import boards.dsl.meta.TurnId.TurnId
import boards.dsl.states.GameState.Outcome
import boards.dsl.states.GameState.Outcome.{Draw, Winner}
import boards.graphics.Colour
import io.circe.Codec
export io.circe.generic.auto.*
import boards.util.Codecs.{*, given}

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
  properties: Map[String, Int] = Map.empty,
  seed: Long = 0L,
  forkedFrom: Option[String] = None,
  forkedTurn: Option[TurnId] = None,
  rematchOf: Option[String] = None,
  rematch: Option[String] = None,
) derives Codec.AsObject:
  /** The game which is being played in this room. */
  lazy val game: Game = GameCatalogue.byName.getOrElse(gameId, Game.none)
  
  /** The permissible options for number of players according to the game. */
  lazy val possiblePlayerCounts: Seq[Int] = game.numPlayers
  /** The minimum number of players according to the game. */
  lazy val minPlayers: Int = possiblePlayerCounts.min
  /** The maximum number of players according to the game. */
  lazy val maxPlayers: Int = possiblePlayerCounts.max
  
  export status.{isPending, isActive, isComplete}
  
  def property(name: String): Int =
    properties.getOrElse(name, game.properties.find(_.name == name).map(_.default).get)
  
object Room:
  
  /** An empty room with no game or players. */
  def empty: RichRoom = RichRoom(RoomWithPlayers(Room("00000", "", Status.Pending), Seq.empty))
  
  case class RoomWithPlayers (
    baseRoom: Room,
    private val simplePlayers: Seq[Player],
  ) derives Codec.AsObject:
    
    /** The players who are participating in this room, in turn order. */
    lazy val players: Seq[RichPlayer] = simplePlayers.map: player =>
      RichPlayer (
        player = player,
        name = game.players(player.position.toInt).name,
        colour = game.players(player.position.toInt).colour,
        isHotseat = simplePlayers.count(_.belongsToUser(player.userIdOpt)) > 1,
        hotseatOrder = simplePlayers.filter(_.belongsToUser(player.userIdOpt))
          .count(_.position.toInt < player.position.toInt),
      )
    
    def player (playerId: PlayerId): RichPlayer = players(playerId.toInt)
    
    /** The players who are participating in this room, grouped by the device they are playing on. */
    lazy val playersByUser: Map[Int, Seq[RichPlayer]] =
      players.groupBy(_.userIdOpt.getOrElse(-1))
      
    def userIsPlaying (userId: Int): Boolean =
      playersByUser.contains(userId)
    
    /** The IDs of all users who are participating in this game with at least one player. */
    lazy val users: Seq[Int] = playersByUser.keys.toSeq
    
    /** Find all the players who are controlled by the current user. */
    def playersOf (userId: Int): Seq[RichPlayer] =
      playersByUser.get(userId).toSeq.flatten
      
    def playersOf (identity: PlayerIdentity): Seq[RichPlayer] =
      identity.userIdOpt.map(playersOf).getOrElse(Seq.empty)
      
    /** The users who are participating in this game. */
    lazy val userIds: Seq[Int] = players.flatMap(_.userIdOpt).distinct
    /** Whether the given user is participating in this room. */
    def isParticipating(userId: Int): Boolean = userIds.contains(userId)
    
    /** The number of players currently participating in this room. */
    def numPlayers: Int = simplePlayers.size
    /** The number of distinct users currently participating in this room. */
    def numUsers: Int = playersByUser.keys.size
    
    /** Whether some device has multiple players. */
    def isHotseat: Boolean = numUsers < numPlayers
    /** Whether all players are on the same device. */
    def isExclusivelyHotseat: Boolean = numUsers == 1
    
    /** Whether players on multiple devices are involved. */
    def isOnline: Boolean = numUsers > 1
    /** Whether no device has multiple players. */
    def isExclusivelyOnline: Boolean = numUsers == numPlayers
    
    def humanPlayers: Seq[RichPlayer] = players.filter(_.isHuman)
    def botPlayers: Seq[RichPlayer] = players.filter(_.isBot)
    
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
    
    export baseRoom.*
  
  /**
   * A game room, representing a specific instance of a game being played.
   *
   * This class is a version of 'Room' which is augmented with some additional information.
   *
   * @param baseRoom The basic information about this room.
   * @param simplePlayers The basic information about the players in this room.
   * @param numPlayersInPrevious If this game is a fork, the number of players who played in the original game.
   */
  case class RichRoom (
    baseRoom: RoomWithPlayers,
    forkedFrom: Option[RoomWithPlayers] = None,
    rematchOf: Option[RoomWithPlayers] = None,
    rematch: Option[RoomWithPlayers] = None,
  ) derives Codec.AsObject:
    
    export baseRoom.{baseRoom => _, forkedFrom => _, rematchOf => _, rematch => _, *}
    
    lazy val permittedPlayerCounts: Seq[Int] =
      forkedFrom.map(_.numPlayers) match
        case Some(numPlayers) => Seq(numPlayers)
        case None => possiblePlayerCounts
    
    /** Whether the game can currently be started. */
    lazy val canStart: Boolean = isPending && permittedPlayerCounts.contains(numPlayers)
    
    /** Whether all players slots are already filled up. */
    lazy val isFull: Boolean = numPlayers >= permittedPlayerCounts.max
    
    /** Produces a version of this room with some given status. */
    def withStatus(status: Status): RichRoom =
      copy(baseRoom = baseRoom.copy(baseRoom = baseRoom.baseRoom.copy(status = status)))
  
  /** The status of a room (i.e. pending/active/complete). */
  enum Status derives Codec.AsObject:
    
    /** The game hasn't yet started. */
    case Pending extends Status
    /** The game has started and hasn't yet finished. */
    case Active extends Status
    /** The game is already finished. */
    case Complete extends Status
    
    /** True if the game hasn't yet started. */
    def isPending: Boolean = this match
      case Pending => true
      case _ => false
    
    /** True if the game has started and hasn't yet finished. */
    def isActive: Boolean = this match
      case Active => true
      case _ => false
      
    /** True if the game is already finished. */
    def isComplete: Boolean = this match
      case Complete => true
      case _ => false
      
  enum PlayerIdentity:
    
    case Human (userId: Int, username: String)
    case Bot (botId: String)
    
    def displayName: String = this match
      case Human(_, username) => username
      case Bot(id) => id
    
    def isHuman: Boolean = this match
      case Human(_, _) => true
      case Bot(_) => false
      
    def isBot: Boolean = !isHuman
    
    def asHumanOpt: Option[Human] = this match
      case id: Human => Some(id)
      case _ => None
      
    def asBotOpt: Option[Bot] = this match
      case id: Bot => Some(id)
      case _ => None
      
    def userIdOpt: Option[Int] = asHumanOpt.map(_.userId)
    def botIdOpt: Option[String] = asBotOpt.map(_.botId)
    
    def belongsToUser (userId: Int): Boolean =
      userIdOpt.contains(userId)
      
    def belongsToUser (userId: Option[Int]): Boolean =
      userId.exists(belongsToUser)
  
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
    identity: PlayerIdentity,
    roomId: String,
    position: PlayerId,
    isOwner: Boolean,
    hasResigned: Boolean = false,
    hasOfferedDraw: Boolean = false,
  ):
    
    export identity.{displayName, isHuman, isBot, asHumanOpt, asBotOpt, userIdOpt, botIdOpt, belongsToUser}
  
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
    lazy val displayNameWithSuffix: String = displayName + suffix
    /** A roman numeral to indicate hotseat order, only if this is a hotseat player. */
    lazy val suffix: String =
      if isHotseat then " " + (hotseatOrder + 1).toRomanNumeral else ""
      
    export player.*