/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package s1computerpurchase;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

/**
 *
 * @author matt
 */
public class MainS1 {

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
		try {
			BufferedReader br = new BufferedReader(new FileReader("/users/matt/documents/java/ccc s 2010/s1-computer purchase/s1.in"));
			String line = br.readLine();
			String[] entries;
			int numComps = Integer.parseInt(line);
			Computer[] comps = new Computer[numComps];
			for (int i = 0;i<numComps;i++){
				entries = br.readLine().split(" ");
				comps[i] = new Computer(entries[0],Integer.parseInt(entries[1]),Integer.parseInt(entries[2]),Integer.parseInt(entries[3]));
			}

			boolean sorted = false;
			Computer temp;
			while (!sorted){
				sorted = true;
				for (int j = 0; j+1 < comps.length; j++) {
					if (comps[j].value < comps[j+1].value){
						sorted = false;
						temp = comps[j];
						comps[j] = comps[j+1];
						comps[j+1] = temp;
						break;
					}
				}
			}
			if (numComps == 0){
				
			}else if (numComps == 1){
				System.out.println(comps[0].name);
			} else if (comps[0].value == comps[1].value){
				if (comps[0].name.compareTo(comps[1].name) < 0){
					System.out.println(comps[0].name);
					System.out.println(comps[1].name);
				} else if (comps[0].name.compareTo(comps[1].name) > 0){
					System.out.println(comps[1].name);
					System.out.println(comps[0].name);
				} else {
					System.out.println(comps[0].name);
					System.out.println(comps[1].name);
				}
			} else {
					System.out.println(comps[0].name);
					System.out.println(comps[1].name);
			}
			br.close();
		} catch (IOException ex) {
			ex.printStackTrace();
		}
    }

}
class Computer {
	public String name = "";
	public int value = 0;
	public Computer(String name,int r, int s, int d){
		value = 2*r + 3*s + d;
		this.name = name;
	}

	@Override
	public String toString() {
		return Integer.toString(value);
	}

}