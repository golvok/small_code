/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package j1;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

/**
 *
 * @author 088241930
 */
public class J1 {

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));

        try {
            int num = Integer.parseInt(br.readLine());
            int numCombos = 0;
                if (num<=5){
                    numCombos = num-1;
                }else{
                    numCombos=9-num;
                }
            System.out.println(numCombos);
        } catch (IOException ex) {
        }
    }

}
