/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package j2;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

/**
 *
 * @author 088241930
 */
public class J2 {

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        try {
            int nDist = 0;
            int nCDist = 0;
            int bDist = 0;
            int bCDist = 0;
            int a = Integer.parseInt(br.readLine());
            int b = Integer.parseInt(br.readLine());
            int c = Integer.parseInt(br.readLine());
            int d = Integer.parseInt(br.readLine());
            int s = Integer.parseInt(br.readLine());
            boolean nDir = true;
            boolean bDir = true;
            for (int i = 0;i<s;i++){
                if (nDir){
                    //System.out.println("n+");
                    nDist++;
                    nCDist++;
                    if (nCDist==a){
                        nCDist = 0;
                        nDir = !nDir;
                    }
                }else{
                    //System.out.println("n-");
                    nDist--;
                    nCDist++;
                    if (nCDist==b){
                        nCDist = 0;
                        nDir = !nDir;
                    }
                }

                if (bDir){
                    //System.out.println("b+");
                    bDist++;
                    bCDist++;
                    if (bCDist==c){
                        bCDist = 0;
                        bDir = !bDir;
                    }
                }else{
                    //System.out.println("b-");
                    bDist--;
                    bCDist++;
                    if (bCDist==d){
                        bCDist = 0;
                        bDir = !bDir;
                    }
                }
            }
            if (nDist>bDist){
                System.out.println("Nikky");
            }else if(nDist == bDist){
                System.out.println("Tied");
            }else{
                System.out.println("Byron");
            }
        } catch (IOException ex) {
        }
    }

}
