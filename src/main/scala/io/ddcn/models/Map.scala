package io.ddcn.models

import io.ddcn.models.RichOps.MultiArrayOps

import scala.collection.mutable
import scala.util.Random

object RichOps {
  implicit class MultiArrayOps[A](array: Array[Array[A]]) {

    def at(row: Int, col: Int): Option[A] =
      array.lift(row).flatMap(_.lift(col))
  }

  implicit class OptionOps[A](opt: Option[A]) {

    def flip[B](b: => B): Option[B] =
      opt match {
        case Some(_) => None
        case None    => Some(b)
      }
  }
}

case class Tile(x: Int, y: Int) {
  val neighbours: mutable.Set[Tile] = mutable.Set()
  var east: Option[Tile] = None
  var northeast: Option[Tile] = None
  var northwest: Option[Tile] = None
  var west: Option[Tile] = None
  var southeast: Option[Tile] = None
  var southwest: Option[Tile] = None

  def link(tile: Tile) = {
    neighbours += tile
  }
}

case class Zone(tiles: Array[Tile]) {

  val eastBoundry      = tiles.filterNot(t => tiles.contains(Tile(t.x + 10, t.y    )))
  val northeastBoundry = tiles.filterNot(t => tiles.contains(Tile(t.x + 5, t.y - 10)))
  val southeastBoundry = tiles.filterNot(t => tiles.contains(Tile(t.x + 5, t.y + 10)))
  val westBoundry      = tiles.filterNot(t => tiles.contains(Tile(t.x - 10, t.y    )))
  val northwestBoundry = tiles.filterNot(t => tiles.contains(Tile(t.x - 5, t.y - 10)))
  val southwestBoundry = tiles.filterNot(t => tiles.contains(Tile(t.x - 5, t.y + 10)))

}

case class Territory(zones: Array[Zone], owner: Option[Player]) {

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
      Territory(zone, None)
    }).toArray

    WorldMap(territories)
  }

  def makeZones(zones: Int, tiles: Int): Array[Zone] = {
    var maxTiles = tiles
    (for(z <- 0 until zones) yield {
      val zoneSize = if(z+1 == zones) maxTiles else Math.min(randomZoneSize(Math.min(maxTiles-1, 1)), math.ceil(tiles/2)).toInt // this could choose a tile size of zero ???
      val territory = makeZone(zoneSize)
      maxTiles = Math.max(maxTiles - zoneSize, 1) // this could exceed initial max tile size
      territory
    }).toArray
  }

  def makeZone(zoneSize: Int): Zone = {

    val tilesWithNeighbour =
      if(!usedTiles.isEmpty)
        usedTiles.flatMap(_.neighbours).toSet
          .diff(usedTiles.toSet)
          .intersect(usableTiles.tiles.toSet).toArray
      else
        usableTiles.tiles

    val start = tilesWithNeighbour(Random.nextInt(tilesWithNeighbour.length))
    var tiles = Array[Tile](start)
    1 until zoneSize foreach { _ =>
      tiles = addNeighbour(tiles)
    }
    usedTiles = tiles
    usableTiles = Zone(usableTiles.tiles.filter(t => !usedTiles.contains(t)))
    Zone(tiles)
  }

  def addNeighbour(tiles: Array[Tile]): Array[Tile] = {
    Random.shuffle(tiles.flatMap(t => t.neighbours.filter(n => !tiles.contains(n) && !usedTiles.contains(n) && usableTiles.tiles.contains(n))).toList)
      .headOption.map(t => tiles :+ t)
      .getOrElse(tiles)
  }

  def randomZoneSize(max: Int): Int = {
    Random.between(1, max+1)
  }

  def getCoords(height: Int, width: Int) = {
    val tiles = (for(x <- 0 until width) yield {
      (for(y <- 0 until height) yield {
        Tile((x * 10) + ((y % 2) * 5), y * 10)
      }).toArray
    }).toArray

    tiles.foreach(_.foreach { t =>
      val x = Math.abs(t.x / 10)
      val y = t.y / 10

      t.east = tiles.at(x + 1, y)
      t.west = tiles.at(x - 1, y)

      if (t.x % 10 > 0) {
        t.northeast = tiles.at(x + 1, y - 1)
        t.northwest = tiles.at(x, y - 1)
        t.southeast = tiles.at(x + 1, y + 1)
        t.southwest = tiles.at(x, y + 1)
      } else {
        t.northeast = tiles.at(x, y - 1)
        t.northwest = tiles.at(x - 1, y - 1)
        t.southeast = tiles.at(x, y + 1)
        t.southwest = tiles.at(x -1, y + 1)
      }
    })
    Zone(tiles.flatten)
  }
}

object LinkNeighbours {
  def on(input: Zone): Zone = {
    input.tiles.foreach { t =>
      if(t.east.isDefined) t.link(t.east.get)
      if(t.northeast.isDefined) t.link(t.northeast.get)
      if(t.southeast.isDefined) t.link(t.southeast.get)
      if(t.west.isDefined) t.link(t.west.get)
      if(t.northwest.isDefined) t.link(t.northwest.get)
      if(t.southwest.isDefined) t.link(t.southwest.get)
    }
    input
  }
}