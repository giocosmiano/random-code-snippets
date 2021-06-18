package kidsSchoolProjects;

public class LimitationsOfThreads {
  public static final int MAX = 5000; //try 2000, ... 4000 works, but then fails for higher values like 5000
  
  public static void doWork() {
    try { Thread.sleep(2000); } catch(Exception ex) {}
  }

  public static void main(String[] args) throws Exception {
    Thread th = null;
    
    for(int i = 0; i < MAX; i++) {
      th = new Thread(LimitationsOfThreads::doWork);
      th.start();
    }
    
    th.join();
  }
}