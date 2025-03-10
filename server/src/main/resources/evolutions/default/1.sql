-- !Ups

CREATE TABLE USERS (
  ID INT NOT NULL AUTO_INCREMENT,
  USERNAME VARCHAR(20) NOT NULL,
  EMAIL VARCHAR(60) NOT NULL,
  PASSWORD_HASH VARCHAR(60) NOT NULL,
  JOINED LONG NOT NULL,
  PRIMARY KEY (ID)
);

CREATE TABLE ROOMS (
  ID VARCHAR(5) NOT NULL,
  GAME_ID VARCHAR(40) NOT NULL,
  STATUS VARCHAR(10) NOT NULL,
  PROPERTIES TINYTEXT NOT NULL,
  SEED LONG NOT NULL,
  FORKED_FROM VARCHAR(5) NULL,
  FORKED_TURN INT NULL,
  REMATCH_OF VARCHAR(5) NULL,
  REMATCH VARCHAR(5) NULL,
  PRIMARY KEY (ID),
  FOREIGN KEY (FORKED_FROM) REFERENCES ROOMS(ID) ON DELETE SET NULL,
  FOREIGN KEY (REMATCH_OF) REFERENCES ROOMS(ID) ON DELETE SET NULL,
  FOREIGN KEY (REMATCH) REFERENCES ROOMS(ID) ON DELETE SET NULL
);

CREATE TABLE PLAYERS (
  USER_ID INT NULL,
  BOT_ID VARCHAR(40) NULL,
  ROOM_ID VARCHAR(5) NOT NULL,
  POSITION INT NOT NULL,
  IS_OWNER BOOLEAN NOT NULL,
  HAS_RESIGNED BOOLEAN NOT NULL,
  HAS_OFFERED_DRAW BOOLEAN NOT NULL,
  PRIMARY KEY (ROOM_ID, POSITION),
  FOREIGN KEY (USER_ID) REFERENCES USERS(ID) ON DELETE CASCADE,
  FOREIGN KEY (ROOM_ID) REFERENCES ROOMS(ID) ON DELETE CASCADE
);

CREATE TABLE INPUTS (
  ROOM_ID VARCHAR(5) NOT NULL,
  TURN_ID INT NOT NULL,
  INPUT_ID VARCHAR(60) NOT NULL,
  TIMESTAMP LONG NOT NULL,
  PRIMARY KEY (ROOM_ID, TURN_ID),
  FOREIGN KEY (ROOM_ID) REFERENCES ROOMS(ID) ON DELETE CASCADE
);

-- !Downs

DROP TABLE INPUTS;
DROP TABLE PLAYERS;
DROP TABLE ROOMS;
DROP TABLE USERS;