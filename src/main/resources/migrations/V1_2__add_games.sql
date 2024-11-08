create type STATUS as enum ('Tie', 'Pending', 'Won', 'Crashed');
create type SIZE as enum ('3', '5', '7');
create type SYMBOL as enum ('X','O');

create type SERVER_RESULT AS
(
    response_average_ms double precision,
    response_median_ms  double precision,
    response_p99_ms     double precision,
    response_min_ms     double precision,
    response_max_ms     double precision,
    number_of_moves     int
);

create type MOVE_POSITION AS
(
    "row"    int,
    "column" int
);

create type GAME_MOVE AS
(
    symbol           SYMBOL,
    position         MOVE_POSITION,
    player_server_id varchar(255),
    duration_ms      double precision
);

create table games
(
    id              uuid
        constraint games_pk primary key,
    tournament_id   uuid         not null, -- Because games can be persisted before tournament is complete

    server_a        varchar(255) not null, -- Represents server playing X
    server_a_result SERVER_RESULT,
    server_b        varchar(255) not null,
    server_b_result SERVER_RESULT,

    maybe_winner    varchar(255),          -- If there is a winner
    status          STATUS       not null, -- State of the game
    crashed_by      varchar(255),          -- If crash put ID here
    crashed_message text,                  -- Place for crash message

    duration_ms     double precision,
    size            SIZE         not null,
    moves           GAME_MOVE[],
    created_at      timestamptz default now()
);


