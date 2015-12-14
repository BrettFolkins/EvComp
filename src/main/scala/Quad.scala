package com

import com.Entropy.rand
import scala.collection.mutable.Buffer
import scala.collection.mutable.ListBuffer

class Quad(
    val dt: Double = 1.0/200.0,  //seconds
    val thrustTW: Double = 80.0, //Thrust To Weight
    val accelSDV: Double = 0.1,  //feet per second^2
    val baroSDV: Double = 1      //feet
  ) {
    var position: Double = 0.0
    var velocity: Double = 0.0
    var acceleration: Double = 0.0
    var time: Double = 0.0

    def update(throttle: Double): Unit = {
        val th = Math.max(0.0, Math.min(1.0, throttle))
        acceleration = th*thrustTW - 32.17  //feet per second^2
        velocity     = velocity + acceleration*dt //feet per second
        position     = Math.max(0, position + velocity*dt) //feet
        time += dt //seconds

        assert(!position.isNaN)
        assert(!velocity.isNaN)
        assert(!acceleration.isNaN)
    }

    def accelerometer: Double = { //feet per second^2
        acceleration + rand.nextGaussian()*accelSDV
    }

    def barometer: Double = { //feet
        position + rand.nextGaussian()*baroSDV
    }

    override def toString: String =
        "Pos: %f,\tVel: %f,\tAcc: %f,\tt: %f".format(position, velocity, acceleration, time)
}

object Quad{
    def simulate(model: Quad, time: Double, control: Quad => Double): Seq[Double] = {
        var quad = model
        val positions = new ListBuffer[Double]()
        while(quad.time < time) {
            quad.update(control(quad))
            positions += quad.position
        }
        positions
    }
}
