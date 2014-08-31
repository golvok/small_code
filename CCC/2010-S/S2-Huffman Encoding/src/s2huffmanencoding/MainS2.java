/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package s2huffmanencoding;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;

/**
 *
 * @author matt
 */
public class MainS2 {

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        try {
			BufferedReader cmdIn = new BufferedReader(new InputStreamReader(System.in));
			BufferedReader br = new BufferedReader(new FileReader("/users/matt/documents/java/ccc s 2010/s2-Huffman Encoding/s2."+cmdIn.readLine()+".in"));
			String line = br.readLine();
			String[] entries;
			int numletters = Integer.parseInt(line);
			
			String[] letters = new String[20];
			String[] codes = new String[20];
			String codeIn = "";
			String output = "";
			for (int i = 0;i<numletters;i++){
				entries = br.readLine().split(" ");
				letters[i] = entries[0];
				codes[i] = entries[1];
			}
			codeIn = br.readLine();
			String encoded = "";
			String decoded = "";
			
			while (codeIn.length() > 0){
				encoded += codeIn.charAt(0);
				codeIn = codeIn.substring(1);
				for (int j = 0; j < codes.length; j++) {
					if (encoded.equals(codes[j])){
						encoded = "";
						output += letters[j];
						break;
					}
				}
			}
			System.out.println(output);
			br.close();
		} catch (IOException ex) {
			ex.printStackTrace();
		}
    }

}
