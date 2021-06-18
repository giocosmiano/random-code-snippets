/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package kidsSchoolProjects;

/**
 *
 * @author gabriellecosmiano
 */
public class FahrenheightToCelsiusConverter {

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {

        double Tf = -40;
        double Tc = Tc = 5.0 / 9 * (Tf - 32);

        System.out.println(Tc);

        // Tc = 5.0 / 9 * (Tf-32)
        
        
        //sin(x) = x - x3 / 6 + x5 / 120 - x7 / 5040 + x9 / 362880
        double angleInDegrees = 90;
        double angleInRadians = (angleInDegrees / 360) * 2 * Math.PI;
        System.out.println(angleInRadians); 
        //double newRadians = Math.toRadians(angleInDegrees); 

        double sin = angleInRadians - Math.pow(angleInRadians, 3) 
                / 6 + Math.pow(angleInRadians, 5) 
                / 120 - Math.pow(angleInRadians, 7) 
                / 5040 + Math.pow(angleInRadians, 9) 
                / (362880);
System.out.println (angleInDegrees + " degrees is equivalent to " + angleInRadians + " radians. "); 
System.out.println ("sin(" + angleInDegrees + ") =" + sin );

//180 degrees is equivalent to 3.1415 radians. 
//sin(180) = 0.0


    }
}
