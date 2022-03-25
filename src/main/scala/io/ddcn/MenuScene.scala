package io.ddcn

import com.sun.corba.se.impl.naming.cosnaming.InternalBindingKey
import indigo.scenes.*
import indigo.shared.assets.AssetName
import indigo.shared.constants.Key
import indigo.shared.datatypes.*
import indigo.shared.{FrameContext, Outcome}
import indigo.shared.events.*
import indigo.shared.materials.Material
import indigo.shared.materials.Material.ImageEffects
import indigo.shared.scenegraph.*
import indigo.shared.subsystems.SubSystem
import indigoextras.ui.{Button, ButtonAssets}

object MenuScene extends Scene[Unit, Unit, Unit]:
  type SceneModel = Unit
  type SceneViewModel = Unit

  val name: SceneName = SceneName("menu")

  private def playerNumber(players: String, colour: RGBA, shiftRight: Int) =
    TextBox(players)
      .withFontSize(Pixels(25))
      .withColor(colour)
      .moveBy(shiftRight, 64)

  val playerButton1: TextBox = playerNumber("3", RGBA.Coral, 0)
  val playerButton2: TextBox = playerNumber("4", RGBA.Orange, 30)
  val playerButton3: TextBox = playerNumber("5", RGBA.Plum, 60)

  def modelLens: Lens[Unit, Unit] = Lens.unit
  def viewModelLens: Lens[Unit, Unit] = Lens.unit
  def eventFilters: EventFilters = EventFilters.Permissive
  def subSystems: Set[SubSystem] = Set()

  def updateModel(context: FrameContext[Unit], model: Unit): GlobalEvent => Outcome[SceneModel] =
    case KeyboardEvent.KeyUp(Key.SPACE) => Outcome(model).addGlobalEvents(SceneEvent.JumpTo(GameScene.name))
    case MouseEvent.Click(Point(playerButton1.x, playerButton1.y)) => Outcome(model).addGlobalEvents(SceneEvent.JumpTo(GameScene.name))
    case _ => Outcome(model)

  def updateViewModel(
                       context: FrameContext[Unit],
                       model: Unit,
                       viewModel: Unit
                     ): GlobalEvent => Outcome[Unit] = _ => Outcome(viewModel)
  def present(
               context: FrameContext[Unit],
               model: Unit,
               viewModel: Unit
             ): Outcome[SceneUpdateFragment] =
    Outcome {
      SceneUpdateFragment.empty
        .addLayer {
          Layer(
            BindingKey("ui"),
            List(
              TextBox("Numbered Cube Attack!")
                .withFontFamily(FontFamily.monospace)
                .withFontSize(Pixels(16))
                .withColor(RGBA.Red)
                .alignLeft,
              TextBox("Please Select the Number of Players")
                .withFontFamily(FontFamily.monospace)
                .withFontSize(Pixels(12))
                .withColor(RGBA.Red)
                .moveBy(0, 32),
              playerNumber("3", RGBA.Coral, 0),
              playerNumber("4", RGBA.Orange, 30),
              playerNumber("5", RGBA.Plum, 60)
            )
          )
        }
    }