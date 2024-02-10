package sokoban.app.operations

import scala.swing.{BorderPanel, Button, GridPanel, Label, Panel}
import sokoban.lib.operations.Operation

import scala.swing.event.ButtonClicked

abstract class OperationPanel extends BorderPanel with Operation {

  val prevButton: Button = new Button("<=") {
    enabled = false
  }
  val operationNumLabel = new Label(operationNumLabelString(1))
  val nextButton: Button = new Button("=>") {
    enabled = numOfOperations > 1
  }
  val buttonsPanel = new GridPanel(1, 0) {
    contents += prevButton
    contents += operationNumLabel
    contents += nextButton
  }
  add(buttonsPanel, BorderPanel.Position.North)

  val operationInnerPanel = new GridPanel(0, 1) {
    contents += operationFromNum(currNumFromLabel)
  }
  add(operationInnerPanel, BorderPanel.Position.Center)

  prevButton.reactions += {
    case ButtonClicked(_) => {
      val nextNum = currNumFromLabel - 1
      operationNumLabel.text = operationNumLabelString(nextNum)

      prevButton.enabled = nextNum > 1
      nextButton.enabled = nextNum < numOfOperations
      operationInnerPanel.contents.clear()
      operationInnerPanel.contents += operationFromNum(nextNum)
      operationInnerPanel.revalidate()
      operationInnerPanel.repaint()
      OperationPanel.this.revalidate()
      OperationPanel.this.repaint()
    }
  }

  nextButton.reactions += {
    case ButtonClicked(_) => {
      val nextNum = currNumFromLabel + 1
      operationNumLabel.text = operationNumLabelString(nextNum)

      prevButton.enabled = nextNum > 1
      nextButton.enabled = nextNum < numOfOperations
      operationInnerPanel.contents.clear()
      operationInnerPanel.contents += operationFromNum(nextNum)
      operationInnerPanel.revalidate()
      operationInnerPanel.repaint()
      OperationPanel.this.revalidate()
      OperationPanel.this.repaint()
    }
  }

  def operationNumLabelString(currNum: Int): String = currNum.toString + "/" + numOfOperations

  def currNumFromLabel: Int = operationNumLabel.text.split("/")(0).toInt

  def operationFromNum(currNum: Int): Panel

  def numOfOperations: Int
}
