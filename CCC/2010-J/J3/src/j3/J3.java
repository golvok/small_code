/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package j3;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

/**
 *
 * @author 088241930
 */
public class J3 {

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        boolean running = true;
        int[] vars = new int[2];
        do {
            try { 
                String[] inArr = br.readLine().split(" ");
                //original didn't work w/ neg. #s
                // ln 29 & 42 originally used _ = inArr[_].charAt(0)-48
                int oT = Integer.parseInt(inArr[0]);
                if (oT == 7) {
                    System.exit(0);
                } else {
                    int X = inArr[1].charAt(0) - 65;
                    if (oT == 1 || oT == 2) {
                        if (oT == 1) {
                            int n = Integer.parseInt(inArr[2]);
                            vars[X] = n;
                        } else {
                            System.out.println(vars[X]);
                        }
                    } else {
                        int Y = inArr[2].charAt(0) - 65;
                        if (oT == 3){
                            vars[X] = vars[X]+vars[Y];
                        }else if (oT == 4){
                            vars[X] = vars[X]*vars[Y];
                        }else if (oT == 5){
                            vars[X] = vars[X]-vars[Y];
                        }else{
                            vars[X] = (int) Math.floor(vars[X]/vars[Y]);
                        }
                    }
                }
            } catch (IOException ex) {
            }
        } while (running);
    }
}
