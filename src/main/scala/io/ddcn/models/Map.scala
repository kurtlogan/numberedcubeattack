package io.ddcn.models

import io.ddcn.models.RichOps.MultiArrayOps

import scala.collection.mutable
import scala.util.Random

object RichOps {
  implicit class MultiArrayOps[A](array: Array[Array[A]]) {

    def at(row: Int, col: Int): Option[A] =
      array.lift(row).flatMap(_.lift(col))
  }
}

case class Tile(x: Int, y: Int) {
  val neighbours: mutable.Set[Tile] =mutable.Set()
  var north: Option[Tile] = None
  var south: Option[Tile] = None
  var east: Option[Tile] = None
  var west: Option[Tile] = None

  def link(tile: Tile) = {
    neighbours += tile
  }
}

case class Zone(tiles: Array[Tile]) {

}

case class Territory(zones: Array[Zone]) {

}

case class WorldMap(territories: Array[Territory]) {

}

object WorldMap {
  private var usableTiles: Zone = Zone(Array())
  private var usedTiles: Array[Tile] = Array()

  def apply(height: Int, width: Int, players: Int, zonesPerPlayer: Int): WorldMap = {
    if(players * zonesPerPlayer > height * width) throw new Exception("Won't Fit")

    usableTiles = LinkNeighbours.on(getCoords(height, width))
    val tileCount = usableTiles.tiles.length
    val blankSpace = 0 //for later extension
    val tilesPerPlayer = math.floor((tileCount - blankSpace) / players).toInt

    val territories = (for(p <- 0 until players) yield {
      val zone = makeZones(zonesPerPlayer, tilesPerPlayer)
      Territory(zone)
    }).toArray

    WorldMap(territories)
  }

  def makeZones(zones: Int, tiles: Int): Array[Zone] = {
    var maxTiles = tiles
    (for(z <- 0 until zones) yield {
      val zoneSize = if(z+1 == zones) maxTiles else Math.min(randomZoneSize(maxTiles-1), math.ceil(tiles/2)).toInt
      val territory = makeZone(zoneSize)
      maxTiles = Math.max(maxTiles - zoneSize, 1)
      territory
    }).toArray
  }

  def makeZone(zoneSize: Int): Zone = {
    val start = usableTiles.tiles(Random.nextInt(usableTiles.tiles.length))
    var tiles = Array[Tile](start)
    1 until zoneSize foreach { _ =>
      tiles = addNeighbour(tiles)
    }
    usedTiles = tiles
    usableTiles = Zone(usableTiles.tiles.filter(t => !usedTiles.contains(t)))
    Zone(tiles)
  }

  def addNeighbour(tiles: Array[Tile]): Array[Tile] = {
    tiles :+ Random.shuffle(tiles.flatMap(t => t.neighbours.filter(n => !tiles.contains(n) && !usedTiles.contains(n))).toList).head
  }

  def randomZoneSize(max: Int): Int = {
    Random.between(1, max+1)
  }

  def getCoords(height: Int, width: Int) = {
    val tiles = (for(x <- 0 until width) yield {
      (for(y <- 0 until height) yield {
        Tile(x, y)
      }).toArray
    }).toArray

    tiles.foreach(_.foreach { t =>
      val x = t.x
      val y = t.y

      t.north = tiles.at(x - 1, y)
      t.south = tiles.at(x + 1, y)
      t.east = tiles.at(x, y + 1)
      t.west = tiles.at(x, y - 1)
    })
    Zone(tiles.flatten)
  }
}

object LinkNeighbours {
  def on(input: Zone): Zone = {
    input.tiles.foreach { t =>
      if(t.north.isDefined) t.link(t.north.get)
      if(t.south.isDefined) t.link(t.south.get)
      if(t.east.isDefined) t.link(t.east.get)
      if(t.west.isDefined) t.link(t.west.get)
    }
    input
  }
}