import kotlinx.coroutines.*

fun getResponse(code: String): String {
  return java.net.URL("http://httpstat.us/$code?sleep=2000")
    .readText()
}

fun getIPAddress() =
  java.net.URL("https://api.ipify.org").readText()

runBlocking<Unit> {
  val job = GlobalScope.launch {   
    try {
      println("get IP Address")
      val ip = getIPAddress()
      
      try {
        val code = if(Math.random() > 0.5) "200" else "404"
                      
        println("get response")
        val response = getResponse(code)
                      
        println("request from ${ip} response: ${response}")
      } catch(ex: Exception) {
        println("Error getting response: $ex")
      }
    } catch(ex: Exception) {
      println("Error getting IP: $ex")
    }
  }         
            
  println("Started...")
  job.join()
}

/*
This code takes some time, depending on network speed, to make the two requests.

For example, when run with time command on Unix-like systems:

Started...
get IP Address
get response
request from 217.76.206.233 response: 200 OK
        5.95 real         8.63 user         0.36 sys

*/