1. On command prompt, compile code by running

gradlew 

2. On a command window, run

gradlew runServer
                 
3. If you have access to curl, test the server with
curl --no-buffer http://localhost:8080 -d '10'

4. On another command window, run

gradlew runClient