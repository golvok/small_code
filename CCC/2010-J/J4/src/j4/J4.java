/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package j4;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

/**
 *
 * @author 088241930
 */
public class J4 {

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        boolean running = true;
        do {
            try {
                String[] strInput = br.readLine().split(" ");
                char[] input = new char[strInput.length-1];
                int numTemps = strInput[0].charAt(0)-48;
                for (int i = 0;i<input.length;i++){
                    input[i] = strInput[i+1].charAt(0);
                }
                //String StrChanges = new String(input);
                if (input[0] == 0) {
                    System.exit(0);
                }
                int[] changes = new int[input.length - 1];
                for (int j = 0; j+1 < input.length; j++) {
                    changes[j] = input[j+1] - input[j];
                }
                for (int k = 0; k< changes.length;k++){

                    for (int l = 1;l<=changes.length;l++){
                        //for (m)
                    }
                }
            } catch (IOException ex) {
            }
        } while (running);
    }
}
