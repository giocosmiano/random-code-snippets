import java.util.*;
import java.util.function.Consumer;

//class Mailer {
//  public void from(String addr) { System.out.println("from"); }
//  public void to(String addr) { System.out.println("to"); }
//  public void subject(String subjectLine) { System.out.println("subject"); }
//  public void body(String message) { System.out.println("body"); }
//  public void send() { System.out.println("sending..."); }
//}
//public class Sample {
//  public static void main(String[] args) {
//    Mailer mailer = new Mailer();
//    mailer.from("from_test@gmail.com");
//    mailer.to("to_test@gmail.com");
//    mailer.subject("Your code sucks");
//    mailer.body("Hello world...");
//    mailer.send();
//  }
//}


class Mailer {
  public Mailer from(String addr) { System.out.println(String.format("from: %s", addr)); return this; }
  public Mailer to(String addr) { System.out.println(String.format("to: %s", addr)); return this; }
  public Mailer subject(String subject) { System.out.println(String.format("subject: %s", subject)); return this; }
  public Mailer body(String body) { System.out.println(String.format("body: %s", body)); return this; }
  public static void send(Consumer<Mailer> block) { 
    Mailer mailer = new Mailer();
    block.accept(mailer);
    System.out.println("sending..."); 
  }
}

public class FluentInterfaces {
  public static void main(String[] args) {
    Mailer.send(mailer -> {
      mailer.from("from_test@gmail.com")
            .to("to_test@gmail.com")
            .subject("Your code sucks")
            .body("Hello world...");
    });
  }
}

