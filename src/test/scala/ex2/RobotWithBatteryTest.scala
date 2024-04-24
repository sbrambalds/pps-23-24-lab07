package ex2

import ex2.Direction
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RobotWithBatteryTest extends AnyFlatSpec with Matchers:

  it should "consume battery" in :
    val robot = new RobotWithbattery(SimpleRobot((0, 0), Direction.North), 20)

    robot.battery should be(100)

    robot.act()
    robot.battery should be(80)

    robot.act()
    robot.battery should be(60)

    robot.act()
    robot.battery should be(40)

    robot.act()
    robot.battery should be(20)

    robot.act()
    robot.battery should be(0)

  it should "not turn if the battery is low" in :
    val robot = new RobotWithbattery(SimpleRobot((0, 0), Direction.North), 100)

    robot.battery should be(100)

    robot.act()
    robot.battery should be(0)

    robot.turn(Direction.South)
    robot.direction should be(Direction.North)

    robot.turn(Direction.West)
    robot.direction should be(Direction.North)

    robot.turn(Direction.South)
    robot.direction should be(Direction.North)

  it should "not act if the battery is low" in :
    val robot = new RobotWithbattery(SimpleRobot((0, 0), Direction.North), 100)

    robot.battery should be(100)

    robot.act()
    robot.battery should be(0)

    robot.act()
    robot.position should be((0, 1))

    robot.act()
    robot.position should be((0, 1))

