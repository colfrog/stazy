CREATE TABLE IF NOT EXISTS posts (
       id INTEGER PRIMARY KEY AUTOINCREMENT,
       username VARCHAR NOT NULL,
       title VARCHAR NOT NULL,
       markdown VARCHAR NOT NULL,
       submitted DATETIME DEFAULT CURRENT_TIMESTAMP,
       UNIQUE(id, username)
);
CREATE TABLE IF NOT EXISTS home (
       username VARCHAR PRIMARY KEY UNIQUE,
       markdown VARCHAR
);
CREATE TABLE IF NOT EXISTS users (
       username VARCHAR UNIQUE NOT NULL,
       password VARCHAR NOT NULL
);
CREATE TABLE IF NOT EXISTS images (
	id VARCHAR PRIMARY KEY,
	username VARCHAR NOT NULL,
	image BLOB NOT NULL,
       UNIQUE(id, username)
);
