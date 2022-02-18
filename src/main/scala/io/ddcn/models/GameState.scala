package io.ddcn.models

case class Die(number: Int)

case class Player(colour: String, largestGroup: Int)

case class World(rawMap: Array[Array[Char]], territories: List[Territory])

case class Territory(id: Char, neighbours: List[Territory], dice: List[Die], occupant: Player)

case class GameState(world: World, currentPlayer: Player, alivePlayers: List[Player], deadPlayers: List[Player])