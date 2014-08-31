/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package s4animalfarm;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;

/**
 *
 * @author matt
 */
public class MainS4 {

	/**
	 * this is a complete implementation of prim's algortithm but
	 * it does not do what was intended because it treats the outside
	 * as if it were another pen  and will break down a fence to get
	 * to it unless it is the last one checked
	 * @see MainS4.prim()
	 */
	public static void main(String[] args) {
		BufferedReader br = null;

		try {
			BufferedReader cmdIn = new BufferedReader(new InputStreamReader(System.in));
			br = new BufferedReader(new FileReader("/Users/matt/Documents/JAVA/CCC S 2010/S4-AnimalFarm/s4." + cmdIn.readLine() + ".in"));
			//br = new BufferedReader(new FileReader("/Users/matt/Documents/JAVA/CCC S 2010/S4-AnimalFarm/s4.c.in"));
			String line = br.readLine();
			int numPens = Integer.parseInt(line);
			String[] entries;
			int[] intEntries;
			Pen[] pens = new Pen[numPens + 1];//to accomodate for outside pen
			Pen[] pensWithoutOutside = new Pen[numPens];
			Node[] nodes;
			Node[] nodesWithoutOutside;

			for (int i = 0; i < numPens; i++) {
				entries = br.readLine().split(" ");
				intEntries = new int[entries.length];
				for (int j = 0; j < entries.length; j++) {
					intEntries[j] = Integer.parseInt(entries[j]);
				}
				int[] points = new int[intEntries[0]];
				int[] costs = new int[intEntries[0]];
				System.arraycopy(intEntries, 1, points, 0, intEntries[0]);
				System.arraycopy(intEntries, intEntries[0] + 1, costs, 0, intEntries[0]);
				pens[i] = new Pen(points, costs, i);

				//System.out.println(pens[i]);

			}

			System.arraycopy(pens, 0, pensWithoutOutside, 0, numPens);//copies everything except the outside

			nodes = makenodes(pens, true);
			nodesWithoutOutside = makenodes(pensWithoutOutside, false);
			System.out.println();
			
			
			int minCost = prim(nodes, nodes[0]);
			System.out.println("^ is WITH outside, minCost = " + minCost + "\n");
			int minCostWithoutOutside = prim(nodesWithoutOutside, nodesWithoutOutside[0]);
			System.out.println("^ is WITHOUT outside, minCost = " + minCostWithoutOutside + "; Integer.MAX_VALUE = " + Integer.MAX_VALUE);


			System.out.println(Math.min(minCost, minCostWithoutOutside));
		}// <editor-fold defaultstate="collapsed" desc="catches">
		catch (IOException ex) {
			ex.printStackTrace();
		} finally {
			try {
				br.close();
			} catch (IOException ex) {
				System.out.println("IOEXCEPTION @ backup");
			}
		}// </editor-fold>


	}

	private static Node[] makenodes(Pen[] pens, boolean incldOutsidePen) {
		Node[] nodes = new Node[pens.length];
		//create a node for each pen
		for (int i = 0; i < pens.length; i++) {
			nodes[i] = new Node(new Node[nodes.length], new int[nodes.length], i);
		}
		// build up links between nodes
		for (int i = 0; i < pens.length - (incldOutsidePen ? 1 : 0); i++) {//loop over pens(not the last pen (would be outside) if incldOutsidePen == true)
			Pen pen = pens[i];
			boolean alreadyHasLinkToOutside = false;
			for (int j = 0; pen != null && j < pen.points.length; j++) {//loop over a node's sides
				boolean fndPrtnr = false; //does this side link to another pen?
				int pt1 = pen.points[j];
				int pt2;
				if (j + 1 >= pen.points.length) {
					pt2 = pen.points[0];//loop around
				} else {
					pt2 = pen.points[j + 1];//next point
				}
				for (int k = 0; k < pens.length - (incldOutsidePen ? 1 : 0); k++) {//loop over pens to test for connectivity(not the last pen (would be outside) if incldOutsidePen == true)
					if (i != k) {//checking itself would cause problems
						Pen testP = pens[k];
						if (testP != null && testP.hasSide(pt1, pt2)) {//if the test has the side as well....
							nodes[i].nodes[k] = nodes[k];
							nodes[i].costs[k] = pen.costs[j];
							fndPrtnr = true;//it has been linked!
							break;//a side can only link to ONE other pen
						}
					}
				}
				//if haveNotFoundPartner && includingOutsidePen && (doesNotAlreadyHaveLinkToOutside || isCheaper)
				if (!fndPrtnr && incldOutsidePen && (!alreadyHasLinkToOutside || pen.costs[j] < nodes[i].costs[nodes.length - 1])) {
					//the outside one is allways the last one
					//add the outside to the node
					nodes[i].nodes[nodes.length - 1] = nodes[nodes.length - 1];
					nodes[i].costs[nodes.length - 1] = pen.costs[j];
					//add the node to the outside
					nodes[nodes.length - 1].nodes[i] = nodes[i];
					nodes[nodes.length - 1].costs[i] = pen.costs[j];
					alreadyHasLinkToOutside = true;//now it will check if the new link is cheaper
				}
			}
		}
		//ouputs links between nodes, the pens they came from and the costs
		System.out.println("with incldOutsidePen = " + incldOutsidePen);
		for (int i = 0; i < nodes.length; i++) {
			System.out.println("\tN(" + i + ")= " + nodes[i]);
			System.out.println("\tP(" + i + ")= " + pens[i] + "\n");
		}
		return nodes;
	}

	/**
	 * PRIM'S ALGORITHM
	 * 
	 * @param nodes
	 * @param start
	 * @param closed
	 * @param cheapestKnown
	 * @return the weight of the minimum spanning tree
	 */
	static private int prim(Node[] nodes, Node start) {

		boolean[] closed = new boolean[nodes.length];//never make a connection to these
		int numClosed;
		boolean[] open = new boolean[nodes.length];//can make connections to other nodes

		int totalCost = 0;



		//initialize
		closed[start.index] = true;
		open[start.index] = true;
		numClosed = 1;

		while (true) {

			int lowestCost = Integer.MAX_VALUE;
			Node adjacentToCheapest = null;//the node to the animals start at and will beak into cheapestNode
			Node cheapestNode = null;

			for (int i = 0; i < open.length; i++) {
				if (open[i]) {//if this node can make connections...
					Node check = nodes[i];
					for (int j = 0; j < check.nodes.length; j++) {//...iterate over it's nodes...
						Node compare = check.nodes[j];
						if (compare != null && !closed[compare.index] && check.costs[compare.index] < lowestCost) {//...compare to the lowest cost...
							//...if it is the cheapest, make it so.
							adjacentToCheapest = check;
							cheapestNode = compare;
							lowestCost = adjacentToCheapest.costs[compare.index];
						}
					}
				}
			}
			if (cheapestNode != null && adjacentToCheapest != null) {
				totalCost += lowestCost;
				closed[cheapestNode.index] = true;
				++numClosed;
				open[cheapestNode.index] = true;

				System.out.println(adjacentToCheapest.index + " --(" + lowestCost + ")--> " + cheapestNode.index + " t=" + totalCost);
			}else{
				//there are no adjacent nodes so stop. hopefully withOutside has better luck
				if (numClosed < nodes.length){
					totalCost = Integer.MAX_VALUE;
				}
				break;
			}

		}

		return totalCost;

	}
}

class Node {

	public Node[] nodes;
	public int[] costs;
	public int index;

	public Node(Node[] connectedTo, int[] costs, int index) {
		nodes = connectedTo;
		this.costs = costs;
		this.index = index;
	}

	@Override
	public String toString() {
		String out = new String();
		for (int i = 0; i < nodes.length; i++) {
			if (nodes[i] != null) {
				out += nodes[i].index + "@" + costs[i] + "\t  ";
			}
		}
		return out;
	}
}

class Pen {

	public int[] points;
	public int[] costs;
	public int index;

	public Pen(int[] points, int[] costs, int index) {
		this.points = points;
		this.costs = costs;
		this.index = index;
	}

	@Override
	public String toString() {
		String out = new String();
		for (int i = 0; i < points.length; i++) {
			int pt1 = points[i];
			int pt2;
			if (i + 1 >= points.length) {
				pt2 = points[0];
			} else {
				pt2 = points[i + 1];
			}
			out += pt1 + "," + pt2 + "@" + costs[i] + "\t";
		}
		return out;
	}

	boolean hasPoint(int point) {
		for (int i = 0; i < points.length; i++) {
			if (points[i] == point) {
				return true;
			}
		}
		return false;
	}

	boolean hasSide(int point1, int point2) {
		return (hasPoint(point1) && hasPoint(point2));
	}
}
