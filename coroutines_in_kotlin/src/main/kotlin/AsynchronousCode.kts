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
      val ip = async { getIPAddress() }
      
      try {
        val code = if(Math.random() > 0.5) "200" else "404"
                      
        println("get response")
        val response = async { getResponse(code) }
                      
        println("request from ${ip.await()} response: ${response.await()}")
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
The two requests are sent asynchronously. We do not have to wait for the reponse from the first to start the second.
For example, when run with time command on Unix-like systems:

Started...
get IP Address
get response
request from 217.76.206.233 response: 200 OK
        5.06 real         8.04 user         0.34 sys
*/