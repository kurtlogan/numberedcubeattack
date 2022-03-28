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

  lazy val gen = WorldMap(30, 37, 8, 1, 14, 32)

  //gen.territories.foreach{t => println("t"); t.zones.foreach{z => println("z"); z.tiles.foreach(println)}}

  val playerColours =
    List(
      RGBA.Purple,
      RGBA.Green,
      RGBA.Orange,
      RGBA.Magenta,
      RGBA.SeaGreen,
      RGBA.Yellow,
      RGBA.Cyan,
      RGBA.Crimson
    )

  lazy val world = // must be a val to prevent calculations occurring on every render
    gen
      .territories
      .zip(playerColours)
      .flatMap { case (t, c) =>
        t.zones.flatMap { z =>
          z.tiles.map { case t@Tile(x, y) =>
            ViewTile.placeAt(ViewTile.tile(c), x, y)
          }
           ++ z.eastBoundry.map      { case Tile(x, y) => ViewTile.eastBorder(x, y) }
            ++ z.westBoundry.map      { case Tile(x, y) => ViewTile.westBorder(x, y) }
            ++ z.southwestBoundry.map { case Tile(x, y) => ViewTile.southWestBorder(x, y) }
            ++ z.southeastBoundry.map { case Tile(x, y) => ViewTile.southEastBorder(x, y) }
            ++ z.northwestBoundry.map { case Tile(x, y) => ViewTile.northWestBorder(x, y) }
            ++ z.northeastBoundry.map { case Tile(x, y) => ViewTile.northEastBorder(x, y) }
        }
      }.toList

  object ViewTile {

    val width: Int = 30
    val totalHeight: Int = 26
    val sideHeight: Int = 10
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
        .moveTo((x * width) / 10, (y * (totalHeight - ((totalHeight - sideHeight) / 2)) / 10))
        .moveBy(40, 40) // add spacing to map
  }
