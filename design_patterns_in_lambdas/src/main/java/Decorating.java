import java.util.*;
import java.awt.Color;
import java.util.function.*;

@SuppressWarnings("unchecked")
class Camera {
  private Function<Color, Color> filter;
  
  public Camera() { setFilters(); }
  
  public Color capture(final Color inputColor) {
    final Color processedColor = filter.apply(inputColor);
    //... more processing...
    return processedColor;
  }
  
  public void setFilters(final Function<Color, Color>... filters) {
    filter = 
      Arrays.asList(filters).stream()
                           .reduce((filter, next) -> filter.compose(next))
                           .orElse(color -> color);
  }
}

@SuppressWarnings("unchecked")
public class Decorating {
  public static void main(String[] args) {
    final Camera camera = new Camera();
    
    final Consumer<String> printCaptured = (filterInfo) ->
      System.out.println(String.format("with %s : %s", filterInfo,
        camera.capture(new Color(200, 100, 200))));

    
    printCaptured.accept("NO filters");    
    
    camera.setFilters(Color::brighter);
    printCaptured.accept("bright filter");

    camera.setFilters(Color::darker);
    printCaptured.accept("bright darker");

    camera.setFilters(Color::brighter, Color::darker);
    printCaptured.accept("bright and darker");
  }
}

