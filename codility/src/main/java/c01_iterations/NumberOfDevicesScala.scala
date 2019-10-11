package c01_iterations

/*
 Your first input is a list of connections. Each connection is a pair of devices, where each device is represented by its IPv4 address.
 Each distinct device (IP address) may be in one of two states: active or inactive. All devices begin inactive.

 connections = [["192.167.0.0", "192.167.0.1"], ["192.167.0.2", "192.167.0.0"], ["192.167.0.0", "192.167.0.3"]]

 Each connection can also be in one of two states: active or inactive. A connection is active only when both of its devices are active.
 Since all devices begin inactive, all connections also begin inactive.

 Your second input is a sequence of single IP addresses that is used to toggle device states, and therefore also connection states, in order.

 toggleIps = ["192.167.0.1", "192.167.0.0", "192.167.0.2", "192.167.0.0", "0.0.0.0"]

 For each IP in toggleIps, toggle the state of any matching devices and connections. After each toggle, calculate the "impact count":
 the number of connections which the device participates in and have changed from active to inactive, or inactive to active.
 Return an array, where each element contains the impact count after the corresponding toggle. This gives us a timeline of
 connection/disconnection impact throughout the simulation.

 Example

 "192.167.0.1" would become active and is a part of connection: ["192.167.0.0", "192.167.0.1"]. However, since the other device
 is still inactive the connection remains inactive. The first count would be 0 because the connection has not changed state
 When "192.167.0.0" becomes active there is already an active device connected to it - "192.167.0.1" which means there
 is now 1 active connection that was previously inactive
 "192.167.0.2" becomes active which activates 1 connection between it and "192.167.0.0"
 "192.167.0.0" becomes inactive, it exists in 2 connections, which were both active and now become inactive
 "0.0.0.0" becomes active, but it has 0 connections
 So overall for the sample inputs above:
 numberOfDevices(connections, toggleIps) = [0, 1, 1, 2, 0]

 Input/Output

 [execution time limit] 20 seconds (scala)

 [input] array.array.string connections

 A list of connections, where each connection is described by 2 IPv4 addresses of a pair of connected devices.
 It is guaranteed that given IPv4 addresses are correct.

 Guaranteed constraints:
 0 ≤ connections.length ≤ 103,
 connections[i].length = 2.

 [input] array.string toggleIps

 A list of IPv4 addresses of devices that change their state. It is guaranteed that given IPv4 addresses are correct.

 Guaranteed constraints:
 1 ≤ toggleIps.length ≤ 103.

 [output] array.integer

 An array, where ith element is equal to the number of connections which the device toggleIps[i] participates in
 and have changed from active to inactive, or inactive to active.Your first input is a list of connections.
 Each connection is a pair of devices, where each device is represented by its IPv4 address.
 Each distinct device (IP address) may be in one of two states: active or inactive. All devices begin inactive.

 connections = [["192.167.0.0", "192.167.0.1"], ["192.167.0.2", "192.167.0.0"], ["192.167.0.0", "192.167.0.3"]]

 Each connection can also be in one of two states: active or inactive. A connection is active only when both
 of its devices are active. Since all devices begin inactive, all connections also begin inactive.

 Your second input is a sequence of single IP addresses that is used to toggle device states, and therefore also connection states, in order.

 toggleIps = ["192.167.0.1", "192.167.0.0", "192.167.0.2", "192.167.0.0", "0.0.0.0"]

 For each IP in toggleIps, toggle the state of any matching devices and connections. After each toggle,
 calculate the "impact count": the number of connections which the device participates in and
 have changed from active to inactive, or inactive to active.
 Return an array, where each element contains the impact count after the corresponding toggle.
 This gives us a timeline of connection/disconnection impact throughout the simulation.

 Example

 "192.167.0.1" would become active and is a part of connection: ["192.167.0.0", "192.167.0.1"]. However, since the
 other device is still inactive the connection remains inactive. The first count would be 0 because the connection has not changed state
 When "192.167.0.0" becomes active there is already an active device connected to it - "192.167.0.1" which
 means there is now 1 active connection that was previously inactive
 "192.167.0.2" becomes active which activates 1 connection between it and "192.167.0.0"
 "192.167.0.0" becomes inactive, it exists in 2 connections, which were both active and now become inactive
 "0.0.0.0" becomes active, but it has 0 connections
 So overall for the sample inputs above:
 numberOfDevices(connections, toggleIps) = [0, 1, 1, 2, 0]

 Input/Output

 [execution time limit] 20 seconds (scala)

 [input] array.array.string connections

 A list of connections, where each connection is described by 2 IPv4 addresses of a pair of connected devices.
 It is guaranteed that given IPv4 addresses are correct.

 Guaranteed constraints:
 0 ≤ connections.length ≤ 103,
 connections[i].length = 2.

 [input] array.string toggleIps

 A list of IPv4 addresses of devices that change their state. It is guaranteed that given IPv4 addresses are correct.

 Guaranteed constraints:
 1 ≤ toggleIps.length ≤ 103.

 [output] array.integer

 An array, where ith element is equal to the number of connections which the device toggleIps[i] participates in
 and have changed from active to inactive, or inactive to active.
*/

object NumberOfDevicesScala extends App {

  def numberOfDevices(connections: Array[Array[String]], toggleIps: Array[String]): Array[Int] = {

    import scala.collection.mutable.ListBuffer
    val listResults = new ListBuffer[String]()

    val results =
      toggleIps.map(toggleIP => {

        listResults += toggleIP
        val ctrToggleIP = listResults.count(_ == toggleIP)
        val isIPToggled = if (ctrToggleIP % 2 == 0) false else true

        val connectionsToToggle =
          connections
            .filter(connection => connection.contains(toggleIP))
            .toList
            .map(connection => connection.toList)

        val otherIPConnections =
          connectionsToToggle.map(c => c.filter(_ != toggleIP).head)

        println(s"toggleIP $toggleIP, isIPToggled $isIPToggled, ctrToggleIP $ctrToggleIP, otherIPConnections $otherIPConnections")

        val ctrConnections =
          otherIPConnections.count(o => {
            val oCtrToggleIP = listResults.count(_ == o)
            val isOtherIPToggled = if (oCtrToggleIP % 2 == 0) false else true
            isOtherIPToggled
          })
        ctrConnections
      })
    results
  }

  val connections = Array(
    Array("192.167.0.0","192.167.0.1"),
    Array("192.167.0.2","192.167.0.0"),
    Array("192.167.0.0","192.167.0.3")
  )
  val toggleIPs = Array(
    "192.167.0.1",
    "192.167.0.0",
    "192.167.0.2",
    "192.167.0.0",
    "0.0.0.0"
  )

  val result = numberOfDevices(connections, toggleIPs)
  println(s"numberOfDevices results ${result.toList}")

}
