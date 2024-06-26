-- !Ups

CREATE TABLE USERS (
  ID INT NOT NULL AUTO_INCREMENT,
  USERNAME VARCHAR(20) NOT NULL,
  EMAIL VARCHAR(60) NOT NULL,
  PASSWORD_HASH VARCHAR(60) NOT NULL,
  PRIMARY KEY (ID)
);

CREATE TABLE GAMES (
  ID INT NOT NULL AUTO_INCREMENT,
  GAME TEXT NOT NULL,
  STATE TEXT NOT NULL
);

CREATE TABLE PLAYERS (
  USER_ID INT NOT NULL,
  GAME_ID INT NOT NULL,
  POSITION INT NOT NULL,
  IS_OWNER BOOLEAN NOT NULL,
  PRIMARY KEY (USER_ID, GAME_ID),
  FOREIGN KEY (USER_ID) REFERENCES USERS(ID)
);

-- !Downs

DROP TABLE PLAYERS;
DROP TABLE GAMES;
DROP TABLE USERS;