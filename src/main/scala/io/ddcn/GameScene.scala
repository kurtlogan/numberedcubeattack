package io.ddcn

import indigo.*
import indigo.scenes.*
import io.ddcn.models.{Tile, WorldMap}

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

  val world = WorldMap(20, 20, 7, 4)
    .territories
    .zip(List(RGBA.Coral, RGBA.Orange, RGBA.Plum, RGBA.Crimson, RGBA.Teal, RGBA.SlateGray, RGBA.Indigo))
    .flatMap { case (t, c) =>
      t.zones.flatMap { z =>
        z.tiles.flatMap { case t@Tile(x, y) =>
          List(
            Some(ViewTile.placeAt(ViewTile.tile(c), x, y)),
            t.east.map(_ => ViewTile.eastBorder),
            t.west.map(_ => ViewTile.westBorder),
            t.southEast.map(_ => ViewTile.eastBorder),
            t.east.map(_ => ViewTile.eastBorder),
            t.east.map(_ => ViewTile.eastBorder),
            t.east.map(_ => ViewTile.eastBorder),
          ).flatten
        }
      }
    }.toList

  object ViewTile {

    val scale: Double = 2
    val width: Int = (20 * scale).toInt
    val totalHeight: Int = (17 * scale).toInt
    val sideHeight: Int = (7 * scale).toInt
    val lineSize: Int = 5

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

    def border(from: Point, to: Point) = Shape.Line(from, to, Stroke(lineSize, RGBA.Black))

    val westBorder = border(
      Point(0, totalHeight - ((totalHeight - sideHeight) / 2)),
      Point(0,  (totalHeight - sideHeight) / 2)
    )

    val eastBorder = border(
      Point(width, (totalHeight - sideHeight) / 2),
      Point(width, totalHeight - ((totalHeight - sideHeight) / 2))
    )

    val southWestBorder = border(
      Point(width / 2, totalHeight),
      Point(0, totalHeight - ((totalHeight - sideHeight) / 2))
    )

    val southEastBorder = border(
      Point(width / 2, totalHeight),
      Point(width, totalHeight - ((totalHeight - sideHeight) / 2))
    )

    val northWestBorder = border(
      Point(0,  (totalHeight - sideHeight) / 2),
      Point(width / 2, 0),
    )

    val northEastBorder = border(
      Point(width / 2, 0),
      Point(width, (totalHeight - sideHeight) / 2)
    )

    def placeAt(shape: Shape, x: Int, y: Int): Shape =
      shape
        .moveTo(x * width, y * (totalHeight - ((totalHeight - sideHeight) / 2)))
        .moveBy(y % 2 * -(width / 2), 0) // offset x for every second y
        .moveBy(40, 40) // add spacing to map
  }
