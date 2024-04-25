package ex2

import ex2.Direction.{North, South}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RobotRepeatedTest extends AnyFlatSpec with Matchers:

  "A robot tha repeat an action" should "act correctly" in :
    val robot = RobotRepeated((SimpleRobot((0, 0), North)), 2)

    robot.act()
    robot.position should be((0, 2))

    robot.act()
    robot.position should be((0, 4))

    robot.turn(South)
    robot.act()
    robot.act()
    robot.position should be((0, 0))