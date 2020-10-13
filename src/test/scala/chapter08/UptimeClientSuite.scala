package chapter08

import org.scalatest.funsuite.AnyFunSuite


class UptimeClientSuite extends AnyFunSuite {
  test("total uptime") {
    val hosts = Map("host1" -> 10, "host2" -> 6)
    val client = new TestUptimeClientImpl(hosts)
    val service = new UptimeService(client)
    val actual = service.getTotalUptime(hosts.keys.toList)
    val expected = hosts.values.sum 
    assert(actual == expected)
  }
}
