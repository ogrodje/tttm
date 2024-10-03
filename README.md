# TTTM: The machine-to-machine challenge of tic-tac-toe 🕹️

[tttm] is a game of [tic-tac-toe][ttt] played between APIs that players implement themselves.

## Rules

1. Players can use whatever language and whatever technology.
2. The player must submit the URL to a server via the [PR request](https://github.com/ogrodje/tttm/pulls) on this repository. All player servers are listed in [players.yml](./players.yml).
3. The player can submit as many servers as he or she wants.
4. The server needs to follow the [Player Server Protocol](#player-server-protocol) described in this document.
5. The game server will randomly pick player servers and execute gameplay.
6. Games will be played every hour. Each server will play 100 games every hour.
7. The server needs to respond in **3 seconds**. Otherwise, the opposing server wins.
8. `TBD: Scoring`

## Player Server Protocol

Players need to implement an HTTP server that needs to have one endpoint.

### `GET /move`

The game server will pass the following URL `query parameters` to the player server.

- `gid` - `UUID` that represents the given game ID.
  - It can be used on player servers to stick together individual games.
- `size` - A number - size - of [tic-tac-toe][ttt] grid. By default, set to `3`
- `playing` - A symbol that the player server needs to play. Possible values are `X` or `O`
- `moves` - A string that represents the previous moves.
  - Moves are separated by `_` and positions by `-`
  - Example: `X-1-1_O-0-0` means that the `X` symbol was at location `1,1` (centre of grid) and `O` at `0,0` (top-left corner of the grid)

### Body

The content of the successful response (HTTP status 200) needs to be a string that should have the following structure:

```
Move:X_2_2
```

The player server replied by placing the symbol `X` in position `2,2` in the grid—in this case, the very bottom right.

If, for some reason, the server is unable to reply or parse, please use the following payload structure:

```
Error:Sorry. Can't do it bro.
```

## Example Player Servers

- [tttm-randy](https://github.com/otobrglez/tttm-randy) is a sample player server implemented in Python 3 with Flask and uses "random strategy" where it will "play" any field that is empty on the given grid.

## Why?

Why not? We want to see who can build the universe's fastest and most "intelligent" APIs [for the lolz](https://www.urbandictionary.com/define.php?term=for+the+lolz).

## Authors

- [Oto Brglez](https://github.com/otobrglez)
- [Ogrodje Podcast](https://ogrodje.si)

[tttm]: https://github.com/ogrodje/tttm
[ttt]: https://en.wikipedia.org/wiki/Tic-tac-toe
