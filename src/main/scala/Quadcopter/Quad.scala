package com.Quadcopter

import com.util.Entropy.rand
import com.util.Chart._
import com.util.Chart
import com.graph._
import scala.collection.mutable.Buffer
import scala.collection.mutable.ListBuffer
import java.lang.Math._
/*
model accelerometer drift
model barometer weirdness
*/

class Quad(
    val dt: Double = 1.0/100.0,  //seconds
    val ThrustToWeight: Double = 64.34, //Thrust To Weight
    val accelSDV: Double = 0.271, //feet per second^2
    val baroSDV: Double  = 0.9,   //feet
    val motorDelay:Double= 1.0    //how quickly the motors spool up
    //SDV's Measured from running quad
  ) {
    val gravity: Double = 32.17; //feet per second^2
    var position: Double = 0.0
    var velocity: Double = 0.0
    var acceleration: Double = 0.0
    var time: Double = 0.0
    var thrust: Double = 0.0

    def update(throttle: Double): Unit = {
        val motorSetpoint = max(0.0, min(1.0, throttle)) //constrain the throttle to 0-1
        thrust       = (motorDelay)*motorSetpoint + (1.0-motorDelay)*thrust
        acceleration = thrust*ThrustToWeight - gravity //feet per second^2
        velocity     = velocity + acceleration*dt //feet per second
        position     = position + velocity*dt //feet
        time        += dt //seconds

        assert(!position.isNaN)
        assert(!velocity.isNaN)
        assert(!acceleration.isNaN)
    }

    def accelerometer: Double = { //feet per second^2
        acceleration + gravity + rand.nextGaussian()*accelSDV
    }

    def barometer: Double = { //feet
        position + rand.nextGaussian()*baroSDV
    }

    import Quad._

    def telemetry: Telemetry = new Telemetry(position, velocity, acceleration)

    def sensors: Sensors = new Sensors(barometer, accelerometer)

    override def toString: String =
        s"Pos: $position, Vel: $velocity, Acc: $acceleration, t: $time"
}

object Quad{
    class Telemetry(val position: Double,
                    val velocity: Double,
                    val acceleration: Double) {
    }
    class Sensors(val barometer: Double, val accelerometer: Double)

    /** Given a (time)=>throttle function, and a maximum time to simulate for,
      * return associated flight data
      */
    def flightData(throttleAtT: (Double) => Double, maxT: Double): Seq[(Telemetry, Sensors)] = {
        val quad = new Quad()
        val data = new ListBuffer[(Telemetry, Sensors)]()
        while(quad.time <= maxT){
            quad.update(throttleAtT(quad.time))
            data append ( (quad.telemetry, quad.sensors) )
        }
        data.toList
    }

    /** Given a function for controlling the quad based on its sensors,
      * return the resulting flight data
      */
    def flightData(throttle: (Sensors, Double) => Double, maxT: Double): Seq[Telemetry] = {
        val quad = new Quad()
        val data = new ListBuffer[Telemetry]()
        while(quad.time <= maxT){
            quad.update(throttle(quad.sensors, quad.time))
            data += quad.telemetry
        }
        data.toList
    }

    class TelemetryFilter(val runningTime: Double) extends FitnessEvalwShow{
        /** Consider overriding the next 4 definitions to change the flight profile
          * or filter output you want to test
          */

        /** throttle curve used to generate data */
        def throttleAt(time: Double): Double = 0.05*cos(time)+0.5
        /** number of components a function needs to output to be interpreted correctly */
        val numberOfTargets = 3
        /** translation from function output seq[Double] to a Telemetry object */
        def interpret(res: Seq[Double]): Telemetry = new Telemetry(res(0),res(1),res(2))
        /** distance between two telemetry logs to use for fitness calculation */
        def sqrDist(a: Telemetry, b: Telemetry): Double = {
            val pdist = a.position - b.position
            val vdist = a.velocity - b.velocity
            val adist = a.acceleration - b.acceleration
            pdist*pdist + vdist*vdist + adist*adist
        }

        /** the number of recurant terms to wrap for the function under test */
        val recCount    = 3

        val range       = 100.0
        val outputCount = numberOfTargets + recCount
        val inputCount  = 2 + outputCount //baro, accel, recurant terms

        val (correctData, sensorData) = flightData(throttleAt(_), runningTime).unzip
        val sensorInput = sensorData.map(d => Seq(d.barometer, d.accelerometer))

        override def toString =
            s"Telemetry filter for $runningTime seconds with $recCount recurrent terms"

        def clean(d: Double): Double =
            if( (d isInfinity) || (d isNaN) ) 0.0 else d

        def calc(func: Seq[Double] => Seq[Double]) : Seq[Telemetry] = {
            val results = sensorInput.scanLeft(Seq.fill[Double](outputCount)(0.0)){
                case(prev,sensors) => func( (sensors ++ prev) ).map(clean(_))
            }.drop(1) //remove the inital 0's from the outputs

            val answer = results.map(_.take(numberOfTargets))

            answer.map(interpret)
        }

        def apply(func: Seq[Double] => Seq[Double]) : Double = {
            val filtered = calc(func)
            val distance = filtered.zip(correctData).map{
                    case(f,c) => sqrDist(f,c)
                }.sum
            sqrt(distance)
        }

        def show(func: Seq[Double] => Seq[Double]): Graph = {
            val results = calc(func)
            Chart( ("True Altitude", correctData.map(_.position)),
                   ("True Velocity", correctData.map(_.velocity)),
                   ("True Acceleration", correctData.map(_.acceleration)),
                   ("Filtered Altitude", results.map(_.position)),
                   ("Filtered Velocity", results.map(_.velocity)),
                   ("Filtered Acceleration", results.map(_.acceleration)) )
        }
    }

    class AltitudeHold(val runningTime: Double) extends FitnessEvalwShow{
        def setpoint(time: Double): Double = 10.0

        val range       = 100.0
        val recCount    = 3
        val inputCount  = 3 + recCount
        val outputCount = 1 + recCount
        val numAverage  = 50

        override def toString =
            s"Altitude hold Sim for $runningTime seconds with $recCount recurrent terms, averaging $numAverage trials"

        def clean(d: Double): Double =
            if( (d isInfinity) || (d isNaN) ) 0.0 else d

        def calc(func: Seq[Double] => Seq[Double]) : Seq[(Double,Double)] = {
            var momento   = Seq.fill[Double](recCount)(0.0)
            var setpoints = new ListBuffer[Double]()
            def fly(s: Sensors, t: Double): Double = {
                val setp = setpoint(t)
                setpoints += setp
                val input = Seq(setp, s.barometer, s.accelerometer) ++ momento
                val rtn = func(input).map(clean)
                momento = rtn.takeRight(recCount)
                rtn(0)
            }
            val positions = flightData(fly(_,_), runningTime).map(_.position)
            positions.zip(setpoints.toList)
        }

        def apply(func: Seq[Double] => Seq[Double]): Double = {
            def score(): Double = {
                val results = calc(func)
                val dist = results.map{
                        case (f,s) => (f-s)*(f-s)
                    }.sum
                sqrt(dist)
            }
            (1 to numAverage).map(x => score()).sum / numAverage.toDouble
        }

        def show(func: Seq[Double] => Seq[Double]): Graph = {
            val (pos, set) = calc(func).unzip
            Chart(("Position", pos), ("Setpoint", set))
        }
    }








}
