package io.ddcn

import indigo._
import indigo.scenes._
import io.ddcn.models._

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
      SceneUpdateFragment(
        polygon.moveTo(
          )

        (w/2) + (x*w),
        (h/2) + (y*h)

//        polygon.moveTo(40,0),
//        polygon.moveTo(100,0),
//        polygon.moveTo(120,0),

      )
    )
  val worldMap = WorldMap(Array(Territory(Array(Zone(
      Array(
        Tile(0,0),
        Tile(0,1),
        Tile(0,2),
        Tile(0,3),
        Tile(1,1),
        Tile(1,2),
        Tile(1,3),
        Tile(2,0),
        Tile(2,1),
        Tile(2,2),
      )
    )))))

  val polygon =
    Shape.Polygon(
      List(
        Point(50, 27),
        Point(40, 20),
        Point(40, 10),
        Point(50, 3),
        Point(60, 10),
        Point(60, 20)
      ),
      Fill.Color(RGBA.Magenta)
    )
