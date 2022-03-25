package io.ddcn

import indigo.*
import indigo.scenes.*
import indigo.shared.scenegraph.Shape.Line
import io.ddcn.models._
import io.ddcn.models.RichOps.*

object GameScene extends Scene[Unit, Unit, Unit]:

  type SceneModel     = Unit
  type SceneViewModel = Unit

  val name: SceneName =
    SceneName("game")

  val modelLens: Lens[Unit, Unit] =
    Lens.keepLatest

  val viewModelLens: Lens[Unit, Unit] =
    Lens.keepLatest

  val eventFilters: EventFilters =
    EventFilters.Permissive

  val subSystems: Set[SubSystem] =
    Set()

  def updateModel(
      context: FrameContext[Unit],
      model: Unit
  ): GlobalEvent => Outcome[Unit] =
    _ => Outcome(model)

  def updateViewModel(
      context: FrameContext[Unit],
      model: Unit,
      viewModel: Unit
  ): GlobalEvent => Outcome[Unit] =
    _ => Outcome(viewModel)

  def present(
      context: FrameContext[Unit],
      model: Unit,
      viewModel: Unit
  ): Outcome[SceneUpdateFragment] =
    Outcome(
      SceneUpdateFragment(Layer(world))
    )

  lazy val gen = WorldMap(30, 30, 3, 1)

  //gen.territories.foreach{t => println("t"); t.zones.foreach{z => println("z"); z.tiles.foreach(println)}}

  lazy val world = // must be a val to prevent calculations occurring on every render
    gen
      .territories
      .zip(List(RGBA.Coral, RGBA.Orange, RGBA.Plum, RGBA.Crimson, RGBA.Teal, RGBA.SlateGray, RGBA.Indigo))
      .flatMap { case (t, c) =>
        t.zones.flatMap { z =>
          z.tiles.map { case t@Tile(x, y) =>
            ViewTile.placeAt(ViewTile.tile(c), x, y)
          }
//           ++ z.eastBoundry.map      { case Tile(x, y) => ViewTile.eastBorder(x, y) }
//            ++ z.westBoundry.map      { case Tile(x, y) => ViewTile.westBorder(x, y) }
//            ++ z.southwestBoundry.map { case Tile(x, y) => ViewTile.southWestBorder(x, y) }
//            ++ z.southeastBoundry.map { case Tile(x, y) => ViewTile.southEastBorder(x, y) }
//            ++ z.northwestBoundry.map { case Tile(x, y) => ViewTile.northWestBorder(x, y) }
//            ++ z.northeastBoundry.map { case Tile(x, y) => ViewTile.northEastBorder(x, y) }
        }
      }.toList

  object ViewTile {

    val scale: Double = 2
    val width: Int = (20 * scale).toInt
    val totalHeight: Int = (17 * scale).toInt
    val sideHeight: Int = (7 * scale).toInt
    val lineSize: Int = 4

    def tile(color: RGBA) =
      Shape.Polygon(
        List(
          Point(width / 2, totalHeight),
          Point(0, totalHeight - ((totalHeight - sideHeight) / 2)),
          Point(0,  (totalHeight - sideHeight) / 2),
          Point(width / 2, 0),
          Point(width, (totalHeight - sideHeight) / 2),
          Point(width, totalHeight - ((totalHeight - sideHeight) / 2))
        ),
        Fill.Color(color)
      )

    def border(from: Point, to: Point, x: Int, y: Int) =
      placeAt(Shape.Line(from, to, Stroke(lineSize, RGBA.Black)).withDepth(Depth(-1)), x, y)

    def westBorder(x: Int, y: Int) = border(
      Point(0, 0),
      Point(0, sideHeight),
      x, y
    ).moveBy(0, (totalHeight - sideHeight) / 2).moveBy(-2, -2)

    def eastBorder(x: Int, y: Int) = border(
      Point(0, 0),
      Point(0, sideHeight),
      x, y
    ).moveBy(width, (totalHeight - sideHeight) / 2).moveBy(-2, -2)

    def southWestBorder(x: Int, y: Int) = border(
      Point(0, (totalHeight - sideHeight) / 2 + sideHeight),
      Point(width / 2, totalHeight),
      x, y
    ).moveBy(0, (totalHeight - sideHeight) / 2 + sideHeight).moveBy(-2, -2)

    def southEastBorder(x: Int, y: Int) = border(
      Point(width, (totalHeight - sideHeight) / 2 + sideHeight),
      Point(width / 2, totalHeight),
      x, y
    ).moveBy(width, (totalHeight - sideHeight) / 2 + sideHeight).moveBy(-2, -2)

    def northWestBorder(x: Int, y: Int) = border(
      Point(width / 2, 0),
      Point(0, (totalHeight - sideHeight) / 2),
      x, y
    ).moveBy(width / 2, 0).moveBy(-2, -2)

    def northEastBorder(x: Int, y: Int) = border(
      Point(width / 2, 0),
      Point(width, (totalHeight - sideHeight) / 2),
      x, y
    ).moveBy(width / 2, 0).moveBy(-2, -2)

    def placeAt(shape: Shape, x: Int, y: Int): Shape =
      shape
        .moveTo(x * width, y * (totalHeight - ((totalHeight - sideHeight) / 2)))
        .moveBy((y % 2) * (width / 2), 0) // offset x for every second y
        .moveBy(40, 40) // add spacing to map
  }

  val testData =
    WorldMap(Array(Territory(Array(
      Zone(Array(
        Tile(6,4),
        Tile(7,3),
        Tile(7,4),
        Tile(8,3),
        Tile(6,5),
        Tile(8,2),
        Tile(6,3),
        Tile(7,5),
        Tile(5,4),
        Tile(7,2),
        Tile(8,5),
        Tile(9,4),
        Tile(6,1),
        Tile(8,6),
        Tile(8,4),
        Tile(9,6),
        Tile(4,3),
        Tile(10,5),
        Tile(9,5),
        Tile(5,3),
        Tile(11,6),
        Tile(7,6),
        Tile(5,5),
        Tile(9,2),
        Tile(10,4),
        Tile(11,5),
        Tile(12,6),
        Tile(8,7),
        Tile(4,4),
        Tile(4,6),
        Tile(6,6),
        Tile(7,1),
        Tile(10,7),
        Tile(10,3),
        Tile(3,7),
        Tile(7,7),
        Tile(8,1),
        Tile(3,5),
        Tile(10,6),
        Tile(9,7),
        Tile(9,3),
        Tile(3,2),
        Tile(5,2),
        Tile(3,6),
        Tile(7,0),
        Tile(2,2),
        Tile(5,7),
        Tile(1,1),
        Tile(2,0),
        Tile(2,4),
        Tile(7,8),
        Tile(12,4),
        Tile(3,3),
        Tile(2,6),
        Tile(6,2),
        Tile(0,2),
        Tile(9,1)))), None),
    Territory(Array(
      Zone(Array(
        Tile(7,0),
        Tile(8,0),
        Tile(9,0),
        Tile(10,1),
        Tile(11,2),
        Tile(6,0),
        Tile(11,1),
        Tile(12,2),
        Tile(10,2),
        Tile(10,0),
        Tile(12,0),
        Tile(11,0),
        Tile(13,1),
        Tile(14,2),
        Tile(11,3),
        Tile(12,1),
        Tile(5,1),
        Tile(13,2),
        Tile(15,3),
        Tile(16,4),
        Tile(17,4),
        Tile(4,0),
        Tile(14,1),
        Tile(16,2),
        Tile(4,1),
        Tile(14,3),
        Tile(18,3),
        Tile(15,1),
        Tile(17,3),
        Tile(14,0),
        Tile(3,1),
        Tile(16,5),
        Tile(18,5),
        Tile(19,5),
        Tile(17,5),
        Tile(15,0),
        Tile(18,2),
        Tile(16,3),
        Tile(19,3),
        Tile(12,3),
        Tile(18,4),
        Tile(19,2),
        Tile(13,0),
        Tile(13,3),
        Tile(15,2),
        Tile(18,6),
        Tile(15,4),
        Tile(17,1),
        Tile(14,5),
        Tile(15,6),
        Tile(13,6),
        Tile(17,2),
        Tile(17,6),
        Tile(16,0),
        Tile(15,5),
        Tile(14,4),
        Tile(19,6)))), None),
      Territory(Array(
        Zone(Array(
          Tile(3,1),
          Tile(4,2),
          Tile(2,1),
          Tile(3,2),
          Tile(3,3),
          Tile(3,0),
          Tile(5,3),
          Tile(4,3),
          Tile(2,0),
          Tile(4,4),
          Tile(2,3),
          Tile(3,4),
          Tile(5,4),
          Tile(5,2),
          Tile(6,3),
          Tile(1,1),
          Tile(6,2),
          Tile(1,2),
          Tile(0,2),
          Tile(3,5),
          Tile(4,5),
          Tile(5,6),
          Tile(6,4),
          Tile(7,3),
          Tile(2,4),
          Tile(7,5),
          Tile(2,2),
          Tile(1,3),
          Tile(2,5),
          Tile(6,6),
          Tile(5,7),
          Tile(1,5),
          Tile(8,5),
          Tile(7,2),
          Tile(4,6),
          Tile(0,4),
          Tile(7,4),
          Tile(9,4),
          Tile(8,1),
          Tile(2,6),
          Tile(7,6),
          Tile(10,4),
          Tile(9,6),
          Tile(0,1),
          Tile(8,4),
          Tile(3,6),
          Tile(4,7),
          Tile(8,7),
          Tile(0,3),
          Tile(10,7),
          Tile(10,6),
          Tile(9,1),
          Tile(9,5),
          Tile(1,0),
          Tile(8,3),
          Tile(8,6),
          Tile(9,2)))), None),
      Territory(Array(
        Zone(Array(
            Tile(0,4),
            Tile(1,4),
            Tile(0,5),
            Tile(1,6),
            Tile(0,7),
            Tile(2,7),
            Tile(1,7),
            Tile(1,8),
            Tile(3,7),
            Tile(0,6),
            Tile(2,8),
            Tile(3,9),
            Tile(0,8),
            Tile(2,9),
            Tile(3,8),
            Tile(1,9),
            Tile(2,10),
            Tile(3,11),
            Tile(2,11),
            Tile(0,9),
            Tile(1,10),
            Tile(4,10),
            Tile(3,10),
            Tile(4,9),
            Tile(1,12),
            Tile(2,12),
            Tile(3,13),
            Tile(2,13),
            Tile(1,14),
            Tile(4,11),
            Tile(0,12),
            Tile(1,11),
            Tile(0,10),
            Tile(0,11),
            Tile(0,15),
            Tile(5,10),
            Tile(3,12),
            Tile(5,9),
            Tile(4,8),
            Tile(1,15),
            Tile(4,12),
            Tile(5,11),
            Tile(2,15),
            Tile(0,16),
            Tile(6,9),
            Tile(7,9),
            Tile(8,8),
            Tile(5,8),
            Tile(7,7),
            Tile(6,8),
            Tile(1,17),
            Tile(1,16),
            Tile(7,8),
            Tile(6,10),
            Tile(7,10),
            Tile(4,13),
            Tile(2,16)))), None),
        Territory(Array(
          Zone(Array(
              Tile(7,10),
              Tile(8,9),
              Tile(8,10),
              Tile(6,11),
              Tile(5,12),
              Tile(9,10),
              Tile(7,11),
              Tile(8,11),
              Tile(6,12),
              Tile(9,8),
              Tile(5,13),
              Tile(9,9),
              Tile(4,14),
              Tile(10,9),
              Tile(6,13),
              Tile(10,11),
              Tile(7,12),
              Tile(8,13),
              Tile(8,7),
              Tile(9,12),
              Tile(6,14),
              Tile(11,11),
              Tile(10,7),
              Tile(11,7),
              Tile(9,11),
              Tile(12,11),
              Tile(8,12),
              Tile(11,8),
              Tile(5,14),
              Tile(12,7),
              Tile(9,13),
              Tile(7,15),
              Tile(12,12),
              Tile(13,6),
              Tile(10,10),
              Tile(9,14),
              Tile(6,15),
              Tile(13,12),
              Tile(3,14),
              Tile(10,13),
              Tile(13,7),
              Tile(9,6),
              Tile(14,6),
              Tile(13,10),
              Tile(10,8),
              Tile(13,11),
              Tile(9,7),
              Tile(11,12),
              Tile(14,8),
              Tile(14,12),
              Tile(14,10),
              Tile(13,9),
              Tile(15,5),
              Tile(8,15),
              Tile(11,10),
              Tile(9,16),
              Tile(8,14)))), None),
          Territory(Array(
            Zone(Array(
                Tile(8,15),
                Tile(7,14),
                Tile(9,15),
                Tile(10,15),
                Tile(10,14),
                Tile(8,16),
                Tile(10,16),
                Tile(11,14),
                Tile(9,17),
                Tile(11,16),
                Tile(8,18),
                Tile(7,16),
                Tile(9,18),
                Tile(6,16),
                Tile(10,19),
                Tile(5,16),
                Tile(11,15),
                Tile(6,17),
                Tile(12,16),
                Tile(5,17),
                Tile(13,17),
                Tile(11,19),
                Tile(11,17),
                Tile(7,18),
                Tile(10,18),
                Tile(6,18),
                Tile(12,13),
                Tile(8,17),
                Tile(9,19),
                Tile(14,17),
                Tile(11,13),
                Tile(14,18),
                Tile(7,17),
                Tile(4,16),
                Tile(12,18),
                Tile(12,14),
                Tile(3,15),
                Tile(4,17),
                Tile(7,19),
                Tile(5,15),
                Tile(13,16),
                Tile(13,18),
                Tile(13,19),
                Tile(14,19),
                Tile(8,19),
                Tile(15,16),
                Tile(11,18),
                Tile(12,17),
                Tile(13,13),
                Tile(15,18),
                Tile(16,16),
                Tile(13,14),
                Tile(4,15),
                Tile(15,19),
                Tile(16,19),
                Tile(12,19),
                Tile(14,14)))), None),
            Territory(Array(
              Zone(Array(
                  Tile(13,13),
                  Tile(12,12),
                  Tile(14,13),
                  Tile(15,14),
                  Tile(14,15),
                  Tile(14,12),
                  Tile(11,11),
                  Tile(16,14),
                  Tile(15,15),
                  Tile(13,12),
                  Tile(17,15),
                  Tile(15,11),
                  Tile(15,12),
                  Tile(15,13),
                  Tile(18,16),
                  Tile(16,15),
                  Tile(16,13),
                  Tile(14,11),
                  Tile(12,11),
                  Tile(13,11),
                  Tile(17,14),
                  Tile(17,12),
                  Tile(18,14),
                  Tile(18,11),
                  Tile(18,15),
                  Tile(16,12),
                  Tile(10,10),
                  Tile(19,13),
                  Tile(10,12),
                  Tile(12,10),
                  Tile(9,9),
                  Tile(14,10),
                  Tile(17,16),
                  Tile(19,12),
                  Tile(18,13),
                  Tile(19,15),
                  Tile(9,10),
                  Tile(19,10),
                  Tile(16,11),
                  Tile(11,10),
                  Tile(17,13),
                  Tile(16,10),
                  Tile(18,12),
                  Tile(15,10),
                  Tile(18,9),
                  Tile(13,9),
                  Tile(17,11),
                  Tile(17,10),
                  Tile(10,9),
                  Tile(19,11),
                  Tile(10,11),
                  Tile(9,12),
                  Tile(13,10),
                  Tile(18,10),
                  Tile(16,9),
                  Tile(11,12),
                  Tile(13,15)))), None)))
