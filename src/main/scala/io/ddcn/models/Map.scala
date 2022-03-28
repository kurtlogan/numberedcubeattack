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
  private val usedTiles: mutable.Set[Tile] = mutable.Set()

  def apply(height: Int, width: Int, players: Int, zonesPerPlayer: Int, minZoneSize: Int = 1, maxZoneSize: Int = 100): WorldMap = {
    if(players * zonesPerPlayer * maxZoneSize > height * width) throw new Exception("Won't Fit")

    val usableTiles = LinkNeighbours.on(getCoords(height, width))

    val zones: Array[Zone] =
      (for(p <- 0 until (players * zonesPerPlayer)) yield {
        makeZone(Random.between(minZoneSize, maxZoneSize + 1), usableTiles)
      }).toArray

    val territories =
      Random.shuffle(zones)
        .grouped(zonesPerPlayer)
        .map(zs => Territory(zs.toArray, None))
        .toArray

    WorldMap(territories)
  }

  def makeZone(zoneSize: Int, startingTiles: Zone): Zone = {

    val tilesWithNeighbour =
      if(usedTiles.nonEmpty)
        usedTiles.flatMap(_.neighbours).toSet
          .diff(usedTiles.toSet).toArray
      else
        startingTiles.tiles

    var zone: Array[Tile] = null
    val blockedTiles: mutable.Set[Tile] = mutable.Set()
    while(zone == null) {
      val start = tilesWithNeighbour(Random.nextInt(tilesWithNeighbour.length))
      var tiles = Array[Tile](start)
      1 until zoneSize foreach { _ =>
        tiles = addNeighbour(tiles, blockedTiles.toSet)
      }

      if(tiles.length != zoneSize) {
        // add to blocked tiles set as this area
        // is not large enough for this particular zone
        blockedTiles ++= tiles
      } else {
        usedTiles ++= tiles
        zone = tiles
      }
    }

    Zone(zone)
  }

  def addNeighbour(tiles: Array[Tile], blockedTiles: Set[Tile]): Array[Tile] = {

    def nfilter(tile: Tile): Set[Tile] =
      tile.neighbours.filter(n => !tiles.contains(n) && !blockedTiles.contains(n) && !usedTiles.contains(n)).toSet

    val validTiles =
      tiles
        .flatMap(nfilter)
        .groupBy(t => t.neighbours.count(!tiles.contains(_))) // favour tiles with more neighbours to remove gaps and prevent corridors
        //.groupBy(t => nfilter(t).size) // no "lakes"
        .toList
        .sortBy(_._1)//(Ordering.Int.reverse) // corridor mode ;)
        .map(_._2)
        .headOption

    validTiles.flatMap { ts =>
      Random
        .shuffle(ts.toList)
        .headOption
        .map(t => tiles :+ t)
    }.getOrElse(tiles)
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