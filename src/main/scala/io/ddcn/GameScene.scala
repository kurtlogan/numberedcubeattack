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
      SceneUpdateFragment(polygon(RGBA.White), polygon(RGBA.Blue).moveBy(20, 20), polygon(RGBA.Teal).moveTo(100, 100))
    )

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
