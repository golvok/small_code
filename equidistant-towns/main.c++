// solves: http://community.topcoder.com/stat?c=problem_statement&pm=13284

#include <vector>
#include <unordered_map>
#include <algorithm>
#include <iostream>

using namespace std;


class Graph {
public:
	typedef size_t id_type;
	typedef size_t weight_type;
	
	class Node {
	public:
		typedef vector<Node*> node_list;
		typedef node_list::iterator node_iterator;
		typedef node_list::const_iterator node_const_iterator;
		typedef unordered_map<Node*,weight_type> weight_map;
		typedef weight_map::iterator weight_map_iterator;
		typedef weight_map::value_type weight_pair;

		static const weight_type not_found = -1; 
		
		Node(id_type id_, Graph& graph_)
			: id(id_)
			, parent(0)
			, children()
			, node2weight()
			, weight_counts()
			, graph(graph_) { }

		void addChild(Node* n, weight_type w) {
			children.push_back(n);
			addWeight(n,w);
		}

		void setParent(Node* n) {
			auto found_node = std::find(
				children.begin(),
				children.end(),
				n
			);
			parent = n;
			if (found_node != children.end()) {
				// node2weight.erase(n);
				children.erase(found_node);
			}
		}
		Node* getParent() { return parent; }
		id_type getId() { return id; }

		weight_type getWeightTo(Node* n) {
			return node2weight.find(n)->second;
		}

		const node_list& getChildren() { return children; }

		const weight_map& getWeightMap() { return node2weight; }

		node_list getNodesWithWeight(size_t w) {
			node_list result;
			for(const auto node_and_weight : getWeightMap()) {
				if (node_and_weight.second == w) {
					result.push_back(node_and_weight.first);
				}
			}
			return result;
		}

		size_t getWeightCount(size_t w) {
			auto found_count = weight_counts.find(w);
			if (found_count != weight_counts.end()) {
				return found_count->second;
			} else {
				return Node::not_found;
			}
		}

		void addWeight(Node* to, weight_type w) {
			cout << "add " << getId() << "--(" << w << ")->" << to->getId() << "\n";
			node2weight.insert(make_pair(to,w));
			size_t current_count;
			{
				decltype(weight_counts)::iterator found_weight = weight_counts.find(w);
				if (found_weight == weight_counts.end()) {
					current_count = 1;
				} else {
					current_count = found_weight->second + 1;
					weight_counts.erase(found_weight);
				}
			}
			weight_counts.insert(make_pair(w, current_count));
			graph.proposeBetterCandidate(w,current_count,this);
		}
		
		weight_type getWeight(Node* n) {
			weight_map_iterator w = node2weight.find(n);
			if (w == node2weight.end()) {
				return not_found;
			} else {
				return w->second;
			}
		}
	private:
		id_type id;
		Node* parent;
		node_list children;
		weight_map node2weight;
		unordered_map<weight_type,size_t> weight_counts;
		Graph& graph;

		Node& operator=(const Node&) = delete;
		Node(const Node&) = delete;
	};

	struct BestResultData {
		weight_type weight;
		size_t count;
		Node* center;
		BestResultData() : weight(-1), count(0), center(0) { }
		void set(weight_type w, size_t count, Node* center) {
			this->weight = w;
			this->count  = count;
			this->center   = center;
		}
	};
	
	Graph() : id2node(), best_results() {}

	void addLink(std::pair<id_type,id_type> link, weight_type weight) {
		pair<Node*,Node*> nodes_in_link {
			getOrCreateNode(link.first),
			getOrCreateNode(link.second),
		};
		nodes_in_link.first ->addChild(nodes_in_link.second, weight);
		nodes_in_link.second->addChild(nodes_in_link.first,  weight);
	}

	void proposeBetterCandidate(weight_type w, size_t count, Node* center) {
		if (count > best_results.count) {
			best_results.set(w,count,center);
		}
	}

	Node* getRoot() { return id2node.begin()->second; }
	const BestResultData& getBestResultData() { return best_results; }
	Node* getNode(id_type id) { return id2node.find(id)->second; }

	~Graph() {
		for (auto id_and_node : id2node) {
			delete id_and_node.second;
		}
	}
private:

	unordered_map<id_type,Node*> id2node;
	BestResultData best_results;
	
	Node* getOrCreateNode(id_type id) {
		auto found_node = id2node.find(id);
		if (found_node == id2node.end()) {
			Node* new_node = new Node(id,*this);
			id2node.insert(make_pair(id,new_node));
			return new_node;
		} else {
			return found_node->second;
		}
	}

	Graph& operator=(const Graph&) = delete;
	Graph(const Graph&) = delete;
};

typedef Graph::Node Node;

// {{n1, n2}, weight}
vector<pair<pair<size_t,size_t>,size_t>> links {
// 	{{1,2},1},
// 	{{1,3},1},
// 	{{1,4},1},
// };
// 	{{1,2},2},
// 	{{2,3},1},
// 	{{3,4},3},
// 	{{2,5},2},
// 	{{3,6},3},
// };
// 	{{1,2},1000},
// 	{{1,3},1000},
// 	{{1,4},1000},
// 	{{1,5},1000},
// 	{{1,6},1000},
// 	{{1,7},1000},
// 	{{1,8},1000},
// 	{{1,9},1000},
// 	{{1,10},1000},
// };
	{{1,2},2},
	{{2,3},1},
	{{3,6},3},
};


// A1:
// keep global of greatest number, and where (vector of links?)
// start at top node
// recurse on nodes.
// at each node find distance of all children (put in multimap, and global memorize list)
// keep track of totals of children of a given distance.
// when one is bigger than global greatest, set it greatest.

void setParentsAndGetWeightsFromChildren(Node* me, Node* parent);
void getWeightsFromParent(Node* me, Graph::weight_type weight_to_parent);
void end() { return; }

int main() {
	Graph graph;

	for (auto& link : links) {
		graph.addLink(
			link.first,
			link.second
		);
	}

	Node* root = graph.getRoot();
	
	// up then down; gather from the children first, then distribute.

	cout << "get from children\n";

	setParentsAndGetWeightsFromChildren(root,nullptr);

	cout << "get from parent\n";

	for (Node* child : root->getChildren()) {
		getWeightsFromParent(child, root->getWeightTo(child));
	}

	const Graph::BestResultData& best_results = graph.getBestResultData();

	cout 
		<< "count <= " << best_results.count
		<< ", weight = " << best_results.weight
		<< ", center = " << best_results.center->getId() << "\n"
	;

	auto superset_of_solution = 
		best_results.center->getNodesWithWeight(best_results.weight);

	size_t new_count = (size_t)-1;

	for(Node* node : superset_of_solution) {
		size_t temp_weight_count = node->getWeightCount(best_results.weight*2) + 1;
		if (temp_weight_count < new_count) {
			new_count = temp_weight_count;
		}
	}

	cout << "count = " << new_count << "\n";

	end();
	return 0;
}

void setParentsAndGetWeightsFromChildren(Node* me, Node* parent) {
	me->setParent(parent); // also removes parent from weight map and child list
	// iterate over children
	for (Node* child : me->getChildren()) {
		// tell children to gather weights from their children.
		setParentsAndGetWeightsFromChildren(child,me);
		// add all weights from this child's weight map
		Graph::weight_type weight_to_child = me->getWeightTo(child);
		cout << "@" << me->getId() << ", from " << child->getId() << " get:\n";
		for (const Node::weight_pair& node_and_weight : child->getWeightMap()) {
			if (node_and_weight.first == me) { continue; }
			me->addWeight(
				node_and_weight.first,
				node_and_weight.second + weight_to_child
			);
		}
	}
}

void getWeightsFromParent(Node* me, Graph::weight_type weight_to_parent) {
	// add all weights from parent
	cout << "@" << me->getId() << ", from " << me->getParent()->getId() << " get:\n";
	for (const Node::weight_pair& child_and_weight : me->getParent()->getWeightMap()) {
		if (
			child_and_weight.first != me
			&& me->getWeight(child_and_weight.first) == Node::not_found
		) {
			me->addWeight(
				child_and_weight.first,
				child_and_weight.second + weight_to_parent
			);
		}
	}
	// tell children to do the same
	for (Node* child : me->getChildren()) {
		getWeightsFromParent(child, me->getWeightTo(child));
	}
}