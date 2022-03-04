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
      SceneUpdateFragment(world)
    )

  val world = WorldMap(20, 20, 7, 4)
    .territories
    .zip(List(RGBA.Coral, RGBA.Orange, RGBA.Plum, RGBA.Crimson, RGBA.Teal, RGBA.White, RGBA.Indigo))
    .flatMap { case (t, c) =>
      t.zones.flatMap { z =>
        z.tiles.map { case Tile(x, y) =>
          polygon(c).moveTo(x * 20, y * 17).moveBy(y % 2 * -10, 0).moveBy(20, 20)
        }
      }
    }.toList

  def polygon(color: indigo.shared.datatypes.RGBA) =
    Shape.Polygon(
      List(
        Point(50, 27),
        Point(40, 20),
        Point(40, 10),
        Point(50, 3),
        Point(60, 10),
        Point(60, 20)
      ),
      Fill.Color(color)
    )
