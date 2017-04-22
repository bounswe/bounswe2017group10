Entry Point for API Project
===
This is just a dummy Spring application to get you started with the implementation of the actual api project.
To build and serve the application, just type `gradle bootRun` from the console while you are in the project folder.

This will compile the project into `build/` directory and
start serving it from `localhost:8080`. After you run the application, don't close the console and go to
`http://localhost:8080/greeting?name=yigit` from your browser, you should see a small json
object with `content` key being "Hello, yigit!". You can change the `GET` parameter to manipulate the response.

You can find the code managing the `/greeting` endpoint in `src/main/java/hello/GreetingController.java`.
There are also `/get-users` and `add-user` controller methods, which eventually resolve into endpoints you can use
just like `/greeting`.
